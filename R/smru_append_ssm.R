##' @title append SMRU tables with SSM-estimated locations
##'
##' @description append SMRU tables so each event has SSM-derived lon, lat, x, y, x.se, y.se.
##'
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param fit final \code{foieGras} fit object
##' @param what choose which locations to use for annotating SMRU tables (default = "predicted")
##' @param meta metadata used to truncate start of diag data for each individual
##' @param cut drop predicted locations if keep = FALSE, ie. locations in a large data gap
##' @param dropIDs SMRU refs to be dropped
##'
##' @importFrom dplyr filter select mutate group_by %>% summarise left_join
##' @importFrom dplyr distinct pull arrange ungroup
##' @importFrom tidyr unnest
##' @importFrom vctrs list_drop_empty
##' @importFrom stringr str_detect regex
##' @importFrom lubridate mdy_hms ymd_hms
##' @importFrom sf st_as_sf st_coordinates st_transform st_geometry<-
##' @importFrom snakecase to_snake_case
##'
##' @export

smru_append_ssm <- function(smru,
                          fit,
                          what = "p",
                          meta,
                          cut = FALSE,
                          dropIDs = NULL) {

  ## general approx fun
  approx.fn <- function(x, smru.table, date.var) {
    dt <- smru.table |>
      filter(ref == x$ref[1]) |>
      pull(date.var)
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

  f <- grab_QC(fit, "fitted", as_sf = FALSE) |>
    rename(ref = id) |>
    filter(!ref %in% dropIDs)
  names(f) <- to_snake_case(names(f))

  ssm_locs <- switch(what,
                     p = {
                       grab_QC(fit, "predicted", cut = cut, as_sf = FALSE)
                     },
                     r = {
                       grab_QC(fit, "rerouted", cut = cut, as_sf = FALSE)
                     }) |>
    rename(ref = id) |>
    filter(!ref %in% dropIDs)
  names(ssm_locs) <- to_snake_case(names(ssm_locs))

  if("device_id" %in% names(meta)) {
    deploy_meta <- meta |>
      select(device_id, release_date) |>
      filter(!device_id %in% dropIDs)

  } else if ("DeploymentID" %in% names(meta)) {
    deploy_meta <- meta |>
      select(device_id = DeploymentID, release_date = DeploymentStartDateTime) |>
      filter(!device_id %in% dropIDs)

  }

  out <- list(ctd=NULL,
              dive=NULL,
              haulout=NULL,
              cruise=NULL,
              ssummary=NULL,
              diag=NULL,
              gps=NULL)

  ## ctd table
  if ("ctd" %in% names(smru)) {
    ctd <- smru$ctd |>
      mutate(ref = as.character(ref)) |>
      filter(!ref %in% dropIDs) |>
      group_by(ref) |>
      arrange(end_date, .by_group = TRUE) |>
      ungroup()

    tmp <- ssm_locs |>
      group_by(ref) |>
      do(locs = approx.fn(., smru.table = ctd, date.var = "end_date")) |>
      unnest(cols = c(locs)) |>
      suppressWarnings()

    ctd <- left_join(ctd, tmp, by = c("ref", c("end_date" = "date"))) |>
      distinct()

    out$ctd <- ctd
  }

  ## dive table
  if("dive" %in% names(smru)) {
    dive <- smru$dive %>%
      mutate(ref = as.character(ref)) %>%
      mutate(ds_date = ifelse(
        str_detect(ds_date, regex("[a-z]", TRUE)),
        ymd_hms(ds_date, tz = "UTC"),
        mdy_hms(ds_date, tz = "UTC")
      )) %>%
      mutate(ds_date = as.POSIXct(ds_date, tz = "UTC", origin = "1970-01-01")) %>%
      filter(!ref %in% dropIDs) %>%
      mutate(lon = round(lon, 6), lat = round(lat, 6))

    class(dive$ds_date) <- c("POSIXct", "POSIXt")

    dive <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = dive, date.var = "de_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(dive, ., by = c("ref", c("de_date" = "date"))) |>
      suppressWarnings()

    out$dive <- dive
  }

  ## haulout table
  if ("haulout" %in% names(smru)) {
    haulout <- smru$haulout %>%
      mutate(ref = as.character(ref)) %>%
      filter(!ref %in% dropIDs) %>%
      mutate(lon = round(lon, 6), lat = round(lat, 6))

    haulout <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = haulout, date.var = "s_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(haulout, ., by = c("ref", c("s_date" = "date"))) |>
      suppressWarnings()

    out$haulout <- haulout
  }

  ## cruise table
  if ("cruise" %in% names(smru)) {
    cruise <- smru$cruise |>
      mutate(ref = as.character(ref)) |>
      filter(!ref %in% dropIDs) |>
      mutate(lon = round(lon, 6), lat = round(lat, 6))

    cruise <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = cruise, date.var = "s_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(cruise, ., by = c("ref", c("s_date" = "date"))) |>
      suppressWarnings()

    out$cruise <- cruise
  }

  ## summary table
  if ("summary" %in% names(smru)) {
    ssummary <- smru$summary %>%
      mutate(ref = as.character(ref)) %>%
      filter(!ref %in% dropIDs)

    ssummary <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = ssummary, date.var = "e_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(ssummary, ., by = c("ref", c("e_date" = "date"))) |>
      suppressWarnings()

    out$ssummary <- ssummary
  }

  ## diag table
  if ("diag" %in% names(smru)) {
    diag <- smru$diag %>%
      mutate(ref = as.character(ref)) %>%
      left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
      mutate(release_date = ifelse(is.na(release_date), d_date, release_date)) %>%
      mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
      filter(d_date >= release_date) %>%
      select(-release_date) %>%
      filter(!ref %in% dropIDs)

    diag <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = diag, date.var = "d_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(diag, ., by = c("ref", c("d_date" = "date"))) |>
      suppressWarnings()

    out$diag <- diag
  }

  ## gps table
  if ("gps" %in% names(smru)) {
    gps <- smru$gps %>%
      mutate(ref = as.character(ref)) %>%
      left_join(. , deploy_meta, by = c("ref" = "device_id")) %>%
      mutate(release_date = ifelse(is.na(release_date), d_date, release_date)) %>%
      mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
      filter(d_date >= release_date) %>%
      select(-release_date) %>%
      filter(!ref %in% dropIDs)

    gps <- ssm_locs %>%
      group_by(ref) %>%
      do(locs = approx.fn(., smru.table = gps, date.var = "d_date")) %>%
      unnest(cols = c(locs)) %>%
      left_join(gps, ., by = c("ref", c("d_date" = "date"))) |>
      suppressWarnings()

    out$gps <- gps
  }

  out <- list_drop_empty(out)

  return(out)

}
