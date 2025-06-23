##' @title append WC tag datafiles with SSM-estimated locations
##'
##' @description append WC tag datafiles so each event has SSM-derived lon, lat, x, y, x.se, y.se.
##'
##' @param wc WC tag datafiles - output of \code{pull_wc}
##' @param fit final \code{aniMotum} fit object(s)
##' @param what choose which locations to use for annotating WC tag datafiles (default = "predicted")
##' @param meta metadata used to truncate start of diag data for each individual
##' @param cut drop predicted locations if keep = FALSE, ie. locations in a large data gap
##' (currently, only used in DM QC mode)
##' @param dropIDs SMRU DeploymentIDs to be dropped
##' @param crs CRS to be applied when interpolating SSM-estimated locations and re-projecting back from Cartesian coords to longlat
##'
##' @importFrom dplyr filter select mutate group_by summarise left_join
##' @importFrom dplyr distinct pull arrange ungroup
##' @importFrom tidyr unnest
##' @importFrom stringr str_detect regex
##' @importFrom vctrs list_drop_empty
##' @importFrom lubridate mdy_hms ymd_hms
##' @importFrom sf st_as_sf st_coordinates st_transform st_geometry<-
##' @importFrom snakecase to_snake_case
##'
##' @export

wc_append_ssm <- function(wc,
                          fit,
                          what = "p",
                          meta,
                          cut = FALSE,
                          dropIDs = NULL,
                          crs = "+proj=merc +units=km +ellps=WGS84 +no_defs") {

  if(any(class(fit) %in% "list")) {
    f <- lapply(fit, function(x) {
      grab_QC(x, "fitted", as_sf = FALSE) |>
        rename(DeploymentID = id) |>
        filter(!DeploymentID %in% dropIDs)
    }) |>
      bind_rows()

    locs <- switch(what,
           p = {
             lapply(fit, function(x) {
               grab_QC(x, "predicted", cut = cut, as_sf = FALSE) |>
                 rename(DeploymentID = id) |>
                 filter(!DeploymentID %in% dropIDs)
             }) |>
               bind_rows()
           },
           r = {
             lapply(fit, function(x) {
               grab_QC(x, "rerouted", cut = cut, as_sf = FALSE) |>
                 rename(DeploymentID = id) |>
                 filter(!DeploymentID %in% dropIDs)
             }) |>
               bind_rows()
           })

  } else if(inherits(fit, "ssm_df")) {
    f <- grab_QC(fit, "fitted", as_sf = FALSE) |>
      rename(DeploymentID = id) |>
      filter(!DeploymentID %in% dropIDs)

    locs <- switch(what, p = {
      grab_QC(fit, "predicted", cut = cut, as_sf = FALSE) |>
        rename(DeploymentID = id) |>
        filter(!DeploymentID %in% dropIDs)
    }, r = {
      grab_QC(fit, "rerouted", cut = cut, as_sf = FALSE) |>
        rename(DeploymentID = id) |>
        filter(!DeploymentID %in% dropIDs)
    })

  }

  deploy_meta <- meta |>
    select(DeploymentID, dive_start) |>
    filter(!DeploymentID %in% dropIDs)

  # get tag-specific file names
  wc.fnms <- sapply(wc, nrow) |>
    list_drop_empty() |>
    names()

  out <- lapply(1:length(wc.fnms), function(i) {

    if(wc.fnms[i] == "Locations") {
      x <- wc_append_Locations(wc$Locations,
                          f,
                          deploy_meta,
                          dropIDs)

    } else if(wc.fnms[i] == "FastGPS") {
      wc$FastGPS <- wc$FastGPS |>
        mutate(Date = paste(Day, Time)) |>
        mutate(Date = dmy_hms(Date, tz = "UTC"))

      x <- wc_append_datafile(wc$FastGPS,
                         locs,
                         deploy_meta,
                         dropIDs) |>
        select(-Date)

    } else if(wc.fnms[i] %in% c("ECDHistos_SCOUT_DSA")) {
      x <- wc_append_ECDHistosDSA(eval(parse(text = paste0("wc$", wc.fnms[i]))),
                                  locs,
                                  deploy_meta,
                                  dropIDs)

    } else if(wc.fnms[i] == "DSA") {
      x <- wc_append_DSA(eval(parse(text = paste0("wc$", wc.fnms[i]))),
                              locs,
                              deploy_meta,
                              dropIDs)

    } else {
      x <- wc_append_datafile(eval(parse(text = paste0("wc$", wc.fnms[i]))),
                         locs,
                         deploy_meta,
                         dropIDs)
    }

    x |>
      select(-dive_start)
  })

  names(out) <- wc.fnms

  return(out)
}


##' @title Interpolate & append SSM locations to WC Locations file
##'
##' @description interpolate SSM locations to Locations event times &
##' append to Locations WC tag datafile
##'
##' @param wc the wc tag datafile list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

wc_append_Locations <- function(wc,
                                locs,
                                deploy_meta,
                                dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(Date >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(Date, .by_group = TRUE) |>
    ungroup()

  wc.out <- left_join(wc, locs, by = c("DeploymentID", "Date" = "date")) |>
    group_by(DeploymentID) |>
    arrange(.by_group = TRUE) |>
    distinct() |>
    rename(
      ssm_lon = lon,
      ssm_lat = lat,
      ssm_x = x,
      ssm_y = y,
      ssm_x_se = x.se,
      ssm_y_se = y.se
    )


  return(wc.out)
}


##' @title Interpolate & append SSM locations to WC ECDHistos_SCOUT_DSA datafiles
##'
##' @description interpolate SSM locations to WC ECDHistos_SCOUT_DSA datafiles event times &
##' append to WC ECDHistos_SCOUT_DSA datafiles
##'
##' @param wc the WC ECDHistos_SCOUT_DSA datafiles list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

wc_append_ECDHistosDSA <- function(wc,
                                   locs,
                                   deploy_meta,
                                   dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(End >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(End, .by_group = TRUE) |>
    ungroup()

  out <- locs |>
    group_by(DeploymentID) |>
    do(locs = approx_ssm(., wc = wc)) |>
    unnest(cols = c(locs))

  out <- left_join(wc, out, by = c("DeploymentID", "End" = "Date")) |>
    distinct()

  out

}


##' @title Interpolate & append SSM locations to WC DSA datafiles
##'
##' @description interpolate SSM locations to WC DSA datafiles event times &
##' append to WC DSA datafiles
##'
##' @param wc the WC DSA datafiles list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

wc_append_DSA <- function(wc,
                          locs,
                          deploy_meta,
                          dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(DiveStart >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(DiveStart, .by_group = TRUE) |>
    ungroup()

  out <- locs |>
    group_by(DeploymentID) |>
    do(locs = approx_ssm(., wc = wc)) |>
    unnest(cols = c(locs))

  out <- left_join(wc, out, by = c("DeploymentID", "DiveStart" = "Date")) |>
    distinct()

  out

}



##' @title Interpolate & append SSM locations to WC tag datafile
##'
##' @description interpolate SSM locations to WC tag datafile event times &
##' append to WC tag datafile
##'
##' @param wc the WC tag datafile list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

wc_append_datafile <- function(wc,
                               locs,
                               deploy_meta,
                               dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(Date >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(Date, .by_group = TRUE) |>
    ungroup()

  out <- locs |>
    group_by(DeploymentID) |>
    do(locs = approx_ssm(., wc = wc)) |>
    unnest(cols = c(locs))

  out <- left_join(wc, out, by = c("DeploymentID", "Date")) |>
    distinct()

  out

}

