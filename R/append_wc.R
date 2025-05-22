##' @title annotate WC tag datafiles with SSM-estimated locations
##'
##' @description annotate WC tag datafiles so each event has SSM-derived lon, lat, x, y, x.se, y.se.
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
##' @examples
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

annotate_wc <- function(wc,
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
      x <- append_wc_Locations(wc$Locations,
                          f,
                          deploy_meta,
                          dropIDs)

    } else if(wc.fnms[i] == "FastGPS") {
      wc$FastGPS <- wc$FastGPS |>
        mutate(Date = paste(Day, Time)) |>
        mutate(Date = dmy_hms(Date, tz = "UTC"))

      x <- append_wc_datafile(wc$FastGPS,
                         locs,
                         deploy_meta,
                         dropIDs) |>
        select(-Date)

    } else if(wc.fnms[i] %in% c("ECDHistos_SCOUT_DSA")) {
      x <- append_wc_ECDHistosDSA(eval(parse(text = paste0("wc$", wc.fnms[i]))),
                                  locs,
                                  deploy_meta,
                                  dropIDs)

    } else if(wc.fnms[i] == "DSA") {
      x <- append_wc_DSA(eval(parse(text = paste0("wc$", wc.fnms[i]))),
                              locs,
                              deploy_meta,
                              dropIDs)

    } else {
      x <- append_wc_datafile(eval(parse(text = paste0("wc$", wc.fnms[i]))),
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
