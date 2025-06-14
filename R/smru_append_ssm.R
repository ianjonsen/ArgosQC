##' @title append SMRU tables with SSM-estimated locations
##'
##' @description append SMRU tables so each event has SSM-derived lon, lat, x, y, x.se, y.se.
##'
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param fit final \code{foieGras} fit object
##' @param what choose which locations to use for annotating SMRU tables (default = "predicted")
##' @param meta metadata used to truncate start of diag data for each individual
##' @param gps.tab logical; does a SMRU GPS table exist, if so append to this as well
##' @param cut drop predicted locations if keep = FALSE, ie. locations in a large data gap
##' @param dropIDs SMRU refs to be dropped
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

smru_append_ssm <- function(smru,
                          fit,
                          what = "p",
                          meta,
                          gps.tab = FALSE,
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

  ## ctd table
  ctd <- smru$ctd |>
    mutate(ref = as.character(ref)) |>
    filter(!ref %in% dropIDs) |>
    group_by(ref) |>
    arrange(end_date, .by_group = TRUE) |>
    ungroup()

  tmp <- ssm_locs |>
    group_by(ref) |>
    do(locs = approx.fn(., smru.table = ctd, date.var = "end_date")) |>
    unnest(cols = c(locs))

  ctd <- left_join(ctd, tmp, by = c("ref", c("end_date" = "date"))) |>
    distinct()


  ## dive table
  dive <- smru$dive %>%
    mutate(ref = as.character(ref)) %>%
    mutate(ds_date = ifelse(str_detect(ds_date, regex("[a-z]", TRUE)),
                            ymd_hms(ds_date, tz = "UTC"),
                            mdy_hms(ds_date, tz = "UTC"))) %>%
    mutate(ds_date = as.POSIXct(ds_date, tz = "UTC", origin = "1970-01-01")) %>%
    filter(!ref %in% dropIDs) %>%
    mutate(lon = round(lon,6),
           lat = round(lat,6))

  class(dive$ds_date) <- c("POSIXct","POSIXt")

  dive <- ssm_locs %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = dive, date.var = "de_date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(dive, ., by = c("ref", c("de_date" = "date")))


  ## haulout table
  haulout <- smru$haulout %>%
    mutate(ref = as.character(ref)) %>%
    filter(!ref %in% dropIDs) %>%
    mutate(lon = round(lon,6),
           lat = round(lat,6))

  haulout <- ssm_locs %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = haulout, date.var = "s_date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(haulout, ., by = c("ref", c("s_date" = "date")))


  ## summary table
  ssummary <- smru$summary %>%
    mutate(ref = as.character(ref)) %>%
    filter(!ref %in% dropIDs)

  ssummary <- ssm_locs %>%
    group_by(ref) %>%
    do(locs = approx.fn(., smru.table = ssummary, date.var = "e_date")) %>%
    unnest(cols = c(locs)) %>%
    left_join(ssummary, ., by = c("ref", c("e_date" = "date")))


  ## diag table
  diag <- smru$diag %>%
    mutate(ref = as.character(ref)) %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    mutate(release_date = ifelse(is.na(release_date), d_date, release_date)) %>%
    mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
    filter(d_date >= release_date) %>%
    select(-release_date) %>%
    filter(!ref %in% dropIDs)

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
      filter(!ref %in% dropIDs)

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
