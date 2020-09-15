##' @title annotate SMRU tables with SSM-estimated locations
##'
##' @description annotate SMRU tables so each event has SSM-derived lon, lat, x, y, x.se, y.se.
##'
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param fit final \code{foieGras} fit object
##' @param meta metadata used to truncate start of diag data for each individual
##' @param drop.refs SMRU refs to be dropped
##' @param crs CRS to be applied when interpolating SSM-estimated locations and re-projecting back from Cartesian coords to longlat
##'
##' @examples
##'
##' @importFrom dplyr filter select mutate group_by "%>%" summarise left_join distinct pull arrange ungroup
##' @importFrom tidyr unnest
##' @importFrom lubridate mdy_hms
##' @importFrom foieGras grab
##' @importFrom sf st_as_sf st_coordinates st_transform st_geometry<-
##'
##' @export

annotate_smru_tables <- function(smru, fit, meta, drop.refs = NULL,
                                 crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +k=1 +ellps=WGS84 +units=km +no_defs") {

  ## general approx fun
  approx.fn <- function(x, smru.table, date.var) {
    dt <- smru.table %>% filter(ref == x$ref[1]) %>% pull(date.var)
    lon1 <- approx(x = cbind(x$date, x$lon), xout = dt)$y
    lat1 <- approx(x = cbind(x$date, x$lat), xout = dt)$y
    x1 <- approx(x = cbind(x$date, x$x), xout = dt)$y
    y1 <- approx(x = cbind(x$date, x$y), xout = dt)$y
    x.se1 <- approx(x = cbind(x$date, x$x.se), xout = dt)$y
    y.se1 <- approx(x = cbind(x$date, x$y.se), xout = dt)$y
  data.frame(
      date = dt,
      ssm_lon = lon1,
      ssm_lat = lat1,
      ssm_x = x1,
      ssm_y = y1,
      ssm_x.se = x.se1,
      ssm_y.se = y.se1
    )
  }

  approx.ctd.fn <- function(x, smru.table, date.var) {
    dt <- smru.table %>% filter(ref == x$ref[1]) %>% pull(date.var)
    x1 <- approx(x = cbind(x$date, x$x), xout = dt)$y
    y1 <- approx(x = cbind(x$date, x$y), xout = dt)$y
    x.se1 <- approx(x = cbind(x$date, x$x.se), xout = dt)$y
    y.se1 <- approx(x = cbind(x$date, x$y.se), xout = dt)$y
    data.frame(
      date = dt,
      ssm_x = x1,
      ssm_y = y1,
      ssm_x.se = x.se1,
      ssm_y.se = y.se1
    )
  }

  f <- grab(fit, "fitted", as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.refs)

  p <- grab(fit, "predicted", as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.refs)

  deploy_meta <- meta %>%
    select(device_id, release_date) %>%
    filter(!device_id %in% drop.refs)

  ## ctd table
  ctd <- smru$ctd %>%
    mutate(ref = as.character(ref),
           end.date = mdy_hms(end.date, tz = "UTC")) %>%
    filter(!ref %in% drop.refs) %>%
    group_by(ref) %>%
    arrange(end.date, .by_group = TRUE) %>%
    ungroup()

  ctd <- p %>%
    group_by(ref) %>%
    do(locs = approx.ctd.fn(., smru.table = ctd, date.var = "end.date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(ctd, ., by = c("ref", c("end.date" = "date"))) %>%
    distinct()

  ## re-project ctd interpolated ssm_x,y locs back to longlat
  ssm_sf <- st_as_sf(ctd %>% filter(!is.na(ssm_x)), coords = c("ssm_x", "ssm_y"), crs = crs)
  ssm_ll <- ssm_sf %>% st_transform(4326) %>%
    st_coordinates() %>%
    as.data.frame()
  names(ssm_ll) <- c("ssm_lon","ssm_lat")
  ssm_xy <- st_coordinates(ssm_sf) %>%
    as.data.frame()
  names(ssm_xy) <- c("ssm_x", "ssm_y")
  st_geometry(ssm_sf) <- NULL
  ctd <- cbind(ssm_sf, ssm_ll, ssm_xy) %>%
    select(1:29, lat, lon, ssm_lon, ssm_lat, ssm_x, ssm_y, ssm_x.se, ssm_y.se)

  ## dive table
  dive <- smru$dive %>%
    mutate(ref = as.character(ref),
           de.date = mdy_hms(de.date, tz = "UTC")) %>%
    filter(!ref %in% drop.refs)

  dive <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = dive, date.var = "de.date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(dive, ., by = c("ref", c("de.date" = "date")))


  ## haulout table
  haulout <- smru$haulout %>%
    mutate(ref = as.character(ref),
           s.date = mdy_hms(s.date, tz = "UTC")) %>%
    filter(!ref %in% drop.refs)

  haulout <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = haulout, date.var = "s.date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(haulout, ., by = c("ref", c("s.date" = "date")))

  ## summary table
  ssummary <- smru$summary %>%
    mutate(ref = as.character(ref),
           e.date = mdy_hms(e.date, tz = "UTC")) %>%
    filter(!ref %in% drop.refs)

  ssummary <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = ssummary, date.var = "e.date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(ssummary, ., by = c("ref", c("e.date" = "date")))

  ## diag table
  diag <- smru$diag %>%
    mutate(d.date = mdy_hms(d.date, tz = "UTC"),
           ref = as.character(ref)) %>%
    left_join(. , deploy_meta, by = c("ref" = "device_id")) %>%
    mutate(release_date = ifelse(is.na(release_date), d.date, release_date)) %>%
    mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
    filter(d.date >= release_date) %>%
    select(-release_date) %>%
    filter(!ref %in% drop.refs)

  diag <- f %>%
    group_by(ref) %>%
    unnest(cols = c()) %>%
    left_join(diag, ., by = c("ref", c("d.date" = "date"))) %>%
    distinct()

  list(diag = diag, ctd = ctd, dive = dive, haulout = haulout, ssummary = ssummary)
}
