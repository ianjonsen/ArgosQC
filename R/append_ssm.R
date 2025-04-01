##' @title annotate SMRU tables with SSM-estimated locations
##'
##' @description annotate SMRU tables so each event has SSM-derived lon, lat, x, y, x.se, y.se.
##'
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param fit final \code{foieGras} fit object
##' @param what choose which locations to use for annotating SMRU tables (default = "predicted")
##' @param meta metadata used to truncate start of diag data for each individual
##' @param gps.tab logical; does a SMRU GPS table exist, if so append to this as well
##' @param cut drop predicted locations if keep = FALSE, ie. locations in a large data gap
##' @param drop.refs SMRU refs to be dropped
##' @param crs CRS to be applied when interpolating SSM-estimated locations and re-projecting back from Cartesian coords to longlat
##'
##' @examples
##'
##' @importFrom dplyr filter select mutate group_by %>% summarise left_join
##' @importFrom dplyr distinct pull arrange ungroup
##' @importFrom tidyr unnest
##' @importFrom stringr str_detect regex
##' @importFrom lubridate mdy_hms ymd_hms
##' @importFrom sf st_as_sf st_coordinates st_transform st_geometry<-
##' @importFrom snakecase to_snake_case
##'
##' @export

annotate_smru_tables <- function(smru,
                                 fit,
                                 what = "p",
                                 meta,
                                 gps.tab = FALSE,
                                 cut = FALSE,
                                 drop.refs = NULL,
                                 crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +k=1 +ellps=WGS84 +units=km +no_defs") {

  ## general approx fun
  approx.fn <- function(x, smru.table, date.var) {
    dt <- smru.table %>% filter(ref == x$ref[1]) %>% pull(date.var)
    lon1 <- approx(x = cbind(x$date, x$lon), xout = dt)$y
    lat1 <- approx(x = cbind(x$date, x$lat), xout = dt)$y
    x1 <- approx(x = cbind(x$date, x$x), xout = dt)$y
    y1 <- approx(x = cbind(x$date, x$y), xout = dt)$y
    x_se1 <- approx(x = cbind(x$date, x$x_se), xout = dt)$y
    y_se1 <- approx(x = cbind(x$date, x$y_se), xout = dt)$y
  data.frame(
      date = dt,
      ssm_lon = round(lon1,6),
      ssm_lat = round(lat1,6),
      ssm_x = round(x1,6),
      ssm_y = round(y1,6),
      ssm_x_se = round(x_se1,6),
      ssm_y_se = round(y_se1,6)
    )
  }

  approx.ctd.fn <- function(x, smru.table, date.var) {
    dt <- smru.table %>% filter(ref == x$ref[1]) %>% pull(date.var)
    x1 <- approx(x = cbind(x$date, x$x), xout = dt)$y
    y1 <- approx(x = cbind(x$date, x$y), xout = dt)$y
    x_se1 <- approx(x = cbind(x$date, x$x_se), xout = dt)$y
    y_se1 <- approx(x = cbind(x$date, x$y_se), xout = dt)$y
    data.frame(
      date = dt,
      ssm_x = round(x1,6),
      ssm_y = round(y1,6),
      ssm_x_se = round(x_se1,6),
      ssm_y_se = round(y_se1,6)
    )
  }

  f <- grab_QC(fit, "fitted", as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.refs)
  names(f) <- to_snake_case(names(f))

  if(what == "p") {
  p <- grab_QC(fit, "predicted", cut = cut, as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.refs)
  names(p) <- to_snake_case(names(p))

  } else if (what == "r") {
    r <- grab_QC(fit, "rerouted", cut = cut, as_sf = FALSE) %>%
      rename(ref = id) %>%
      filter(!ref %in% drop.refs)
    names(r) <- to_snake_case(names(r))
  }

  if("device_id" %in% names(meta)) {
    deploy_meta <- meta %>%
      select(device_id, release_date) %>%
      filter(!device_id %in% drop.refs)

  } else if ("DeploymentID" %in% names(meta)) {
    deploy_meta <- meta %>%
      select(device_id = DeploymentID, release_date = DeploymentStartDateTime) %>%
      filter(!device_id %in% drop.refs)

  }


  ## ctd table
  ctd <- smru$ctd %>%
    mutate(ref = as.character(ref)) %>%
    filter(!ref %in% drop.refs) %>%
    group_by(ref) %>%
    arrange(end_date, .by_group = TRUE) %>%
    ungroup()

  if (what == "p") {
    ctd <- p %>%
      group_by(ref) %>%
      do(locs = approx.ctd.fn(., smru.table = ctd, date.var = "end_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(ctd, ., by = c("ref", c("end_date" = "date"))) %>%
      distinct()

  } else if (what == "r") {
    ctd <- r %>%
      group_by(ref) %>%
      do(locs = approx.ctd.fn(., smru.table = ctd, date.var = "end_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(ctd, ., by = c("ref", c("end_date" = "date"))) %>%
      distinct()
  }

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
    select(1:29, lat, lon, ssm_lon, ssm_lat, ssm_x, ssm_y, ssm_x_se, ssm_y_se)

  ## dive table
  dive <- smru$dive %>%
    mutate(ref = as.character(ref)) %>%
    mutate(ds_date = ifelse(str_detect(ds_date, regex("[a-z]", TRUE)),
                            ymd_hms(ds_date, tz = "UTC"),
                            mdy_hms(ds_date, tz = "UTC"))) %>%
    mutate(ds_date = as.POSIXct(ds_date, tz = "UTC", origin = "1970-01-01")) %>%
    filter(!ref %in% drop.refs) %>%
    mutate(lon = round(lon,6),
           lat = round(lat,6))

  class(dive$ds_date) <- c("POSIXct","POSIXt")

  if (what == "p") {
    dive <- p %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = dive, date.var = "de_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(dive, ., by = c("ref", c("de_date" = "date")))

  } else if (what == "r") {
    dive <- r %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = dive, date.var = "de_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(dive, ., by = c("ref", c("de_date" = "date")))
  }


  ## haulout table
  haulout <- smru$haulout %>%
    mutate(ref = as.character(ref)) %>%
    filter(!ref %in% drop.refs) %>%
    mutate(lon = round(lon,6),
           lat = round(lat,6))

  if (what == "p") {
    haulout <- p %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = haulout, date.var = "s_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(haulout, ., by = c("ref", c("s_date" = "date")))

  } else if (what == "r") {
    haulout <- r %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = haulout, date.var = "s_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(haulout, ., by = c("ref", c("s_date" = "date")))
  }

  ## summary table
  ssummary <- smru$summary %>%
    mutate(ref = as.character(ref)) %>%
    filter(!ref %in% drop.refs)

  if (what == "p") {
    ssummary <- p %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = ssummary, date.var = "e_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(ssummary, ., by = c("ref", c("e_date" = "date")))

  } else if (what == "r") {
    ssummary <- r %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = ssummary, date.var = "e_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(ssummary, ., by = c("ref", c("e_date" = "date")))
  }


  ## diag table
  diag <- smru$diag %>%
    mutate(ref = as.character(ref)) %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    mutate(release_date = ifelse(is.na(release_date), d_date, release_date)) %>%
    mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
    filter(d_date >= release_date) %>%
    select(-release_date) %>%
    filter(!ref %in% drop.refs)

  diag <- f %>%
    group_by(ref) %>%
    unnest(cols = c()) %>%
    left_join(diag, ., by = c("ref", c("d_date" = "date"))) %>%
    distinct() %>%
    rename(
      lat = lat.x,
      lon = lon.x,
      ssm_lon = lon.y,
      ssm_lat = lat.y,
      ssm_x = x,
      ssm_y = y,
      ssm_x_se = x_se,
      ssm_y_se = y_se
    ) %>%
    mutate(ssm_lon = round(ssm_lon,6),
           ssm_lat = round(ssm_lat,6),
           ssm_x = round(ssm_x,6),
           ssm_y = round(ssm_y,6),
           ssm_x_se = round(ssm_x_se,6),
           ssm_y_se = round(ssm_y_se,6)
    )
  ## remove CRW-specific model params from diag files
  ##  these only need to appear in ssmoutputs files
  if(all(c("u","u_se","v","v_se","s","s_se") %in% names(diag))) {
    diag <- diag %>%
      select(-c("u","u_se","v","v_se","s","s_se"))
  }

  if (gps.tab) {
    ## gps table
    gps <- smru$gps %>%
      mutate(ref = as.character(ref)) %>%
      left_join(. , deploy_meta, by = c("ref" = "device_id")) %>%
      mutate(release_date = ifelse(is.na(release_date), d_date, release_date)) %>%
      mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
      filter(d_date >= release_date) %>%
      select(-release_date) %>%
      filter(!ref %in% drop.refs)

    gps <- f %>%
      group_by(ref) %>%
      unnest(cols = c()) %>%
      left_join(gps, ., by = c("ref", c("d_date" = "date"))) %>%
      distinct() %>%
      rename(
        lat = lat.x,
        lon = lon.x,
        ssm_lon = lon.y,
        ssm_lat = lat.y,
        ssm_x = x,
        ssm_y = y,
        ssm_x_se = x_se,
        ssm_y_se = y_se
      ) %>%
      mutate(
        ssm_lon = round(ssm_lon, 6),
        ssm_lat = round(ssm_lat, 6),
        ssm_x = round(ssm_x, 6),
        ssm_y = round(ssm_y, 6),
        ssm_x_se = round(ssm_x_se, 6),
        ssm_y_se = round(ssm_y_se, 6)
      )
    ## remove CRW-specific model params from diag files
    ##  these only need to appear in ssmoutputs files
    if (all(c("u", "u_se", "v", "v_se", "s", "s_se") %in% names(gps))) {
      gps <- gps %>%
        select(-c("u", "u_se", "v", "v_se", "s", "s_se"))
    }
  }

  if(gps.tab) {
    list(diag = diag, gps = gps, ctd = ctd, dive = dive, haulout = haulout, ssummary = ssummary)
  } else {
  list(diag = diag, ctd = ctd, dive = dive, haulout = haulout, ssummary = ssummary)
  }
}
