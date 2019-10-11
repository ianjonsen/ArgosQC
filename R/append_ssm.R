##' @title annotate SMRU tables with SSM-estimated locations
##'
##' @description annotate SMRU tables so each event has SSM-derived lon,lat,x,y,x.se,y.se
##'
##' @param cids SMRU campaign id(s)
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param fit final \code{foieGras} fit object
##'
##' @examples
##'
##' @importFrom dplyr filter select mutate group_by "%>%" summarise left_join
##' @importFrom lubridate mdy_hms
##' @importFrom foieGras grab
##'
##' @export

annotate_smru_tables <- function(cids, smru, fit) {

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

  ## ctd table
  ctd <- smru$ctd %>%
    mutate(ref = as.character(ref),
           end.date = lubridate::mdy_hms(end.date, tz = "UTC")) %>%
    filter(!ref %in% c("ct150-440BAT-16", "ct144-184BAT2-14"))

  ctd <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = ctd, date.var = "end.date")) %>%
    unnest() %>%
    left_join(ctd, ., by = c("ref", c("end.date" = "date"))) %>%
    distinct()


  ## dive table
  dive <- smru$dive %>%
    mutate(ref = as.character(ref),
           de.date = lubridate::mdy_hms(de.date, tz = "UTC")) %>%
    filter(!ref %in% c("ct150-440BAT-16", "ct144-184BAT2-14"))

  dive <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = dive, date.var = "de.date")) %>%
    unnest() %>%
    left_join(dive, ., by = c("ref", c("de.date" = "date")))


  ## haulout table
  haulout <- smru$haulout %>%
    mutate(ref = as.character(ref),
           s.date = lubridate::mdy_hms(s.date, tz = "UTC")) %>%
    filter(!ref %in% c("ct150-440BAT-16", "ct144-184BAT2-14"))

  haulout <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = haulout, date.var = "s.date")) %>%
    unnest() %>%
    left_join(haulout, ., by = c("ref", c("s.date" = "date")))

  ## summary table
  ssummary <- smru$summary %>%
    mutate(ref = as.character(ref),
           e.date = lubridate::mdy_hms(e.date, tz = "UTC")) %>%
    filter(!ref %in% c("ct150-440BAT-16", "ct144-184BAT2-14"))

  ssummary <- p %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = ssummary, date.var = "e.date")) %>%
    unnest() %>%
    left_join(ssummary, ., by = c("ref", c("e.date" = "date")))

  ## diag table
  diag <- smru$diag %>%
    mutate(d.date = lubridate::mdy_hms(d.date, tz = "UTC")) %>%
    left_join(. , deploy_meta, by = c("ref" = "device_id")) %>%
    mutate(release_date = ifelse(is.na(release_date), d.date, release_date)) %>%
    mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
    filter(d.date >= release_date) %>%
    select(-release_date) %>%
    filter(!ref %in% c("ct150-440BAT-16", "ct144-184BAT2-14"))

  diag <- f %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = diag, date.var = "d.date")) %>%
    unnest() %>%
    left_join(diag, ., by = c("ref", c("d.date" = "date"))) %>%
    distinct()
}
