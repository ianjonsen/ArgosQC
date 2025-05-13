##' @title clean, truncate & convert WC Locations files to sf-tibbles
##'
##' @description restructures Locations files, formats dates & lc's; truncates start
##' (and end for "nrt") of individual deployments using ctd dates; converts to
##' sf geometry - all in preparation for SSM-filtering. Splits resulting truncated
##' diag files by species.
##'
##' @param wc list of WC datafiles
##' @param meta metadata used to truncate start of diag data for each individual
##' @param dropIDs WC DeploymentID's to be dropped (eg. tags were turned on but not deployed)
##' @param crs a proj4string to re-project diag locations from longlat
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm),
##' in latter case diag is not right-truncated & date of first dive is used
##' for the track start date
##'
##' @examples
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do
##' @importFrom dplyr ungroup rename select
##' @importFrom assertthat assert_that
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract
##' @importFrom aniMotum format_data
##'
##' @export
##'

prep_wc <- function(wc,
                    meta,
                    dropIDs,
                    crs = "+proj=merc +units=km +ellps=WGS84 +no_defs",
                    QCmode = "nrt") {
  assert_that(is.list(wc))

  ## clean step
  if ("Error Semi-major axis" %in% names(wc$Locations)) {
    locs <- wc$Locations |>
      dplyr::select(
        DeploymentID,
        Date,
        Quality,
        Longitude,
        Latitude,
        'Error Semi-major axis',
        'Error Semi-minor axis',
        'Error Ellipse orientation'
      )
  } else {
    locs <- wc$Locations |>
      dplyr::select(DeploymentID, Date, Quality, Longitude, Latitude) |>
      mutate(
        'Error Semi-major axis' = NA,
        'Error Semi-minor axis' = NA,
        'Error Ellipse orientation' = NA
      )
  }

  locs <- locs |>
    rename(
      date = Date,
      lc = Quality,
      lon = Longitude,
      lat = Latitude,
      smaj = 'Error Semi-major axis',
      smin = 'Error Semi-minor axis',
      eor = 'Error Ellipse orientation'
    ) |>
    mutate(lc = factor(
      lc,
      levels = c("G", 3, 2, 1, 0, "A", "B", "Z"),
      labels = c("G", "3", "2", "1", "0", "A", "B", "Z"),
      ordered = TRUE
    )) |>
    mutate(
      lon = as.numeric(lon),
      lat = as.numeric(lat),
      smaj = as.numeric(smaj),
      smin = as.numeric(smin),
      eor = as.numeric(eor)
    )

  locs <- locs |>
    filter(!DeploymentID %in% dropIDs)

  ## truncate by dive_start/dive_end datetimes & convert to sf geometry steps
  deploy_meta <- meta |>
    dplyr::select(
      DeploymentID,
      DeploymentStartDateTime,
      dive_start,
      dive_end
    ) |>
    mutate(
      dive_start = ifelse(
        DeploymentStartDateTime > dive_start,
        DeploymentStartDateTime,
        dive_start))

  ## only left-truncate tracks
  locs <- locs |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(date >= dive_start) |>
    dplyr::select(-DeploymentStartDateTime, -dive_start, -dive_end)


  ## project lon,lat coords
  locs_sf <- locs |>
    mutate(id = DeploymentID) |>
    dplyr::select(DeploymentID, id, everything()) |>
    group_by(DeploymentID) |>
    do(
      d_sf =
        sf::st_as_sf(., coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") |>
        sf::st_transform(., crs = crs) |>
        select(-DeploymentID)
    ) |>
    ungroup() |>
    left_join(meta |> select(DeploymentID, ADRProjectID), by = "DeploymentID")

  ## add species code
  load(system.file("extdata/spcodes.rda", package = "ArgosQC"))

  msp <- meta |> select(DeploymentID, AnimalScientificName)
  msp <- left_join(msp, spcodes, by = c("AnimalScientificName" = "species"))

  locs_sf <- locs_sf |>
    left_join(msp, by = "DeploymentID")

  locs_sf <- locs_sf |>
    rename(sp = code) |>
    mutate(sp = factor(sp, levels = spcodes$code, ordered = TRUE)) |>
    mutate(sp = droplevels(sp)) |>
    dplyr::select(DeploymentID, ADRProjectID, sp, d_sf)
  locs_sf <- split(locs_sf, locs_sf$sp)

  return(locs_sf)


}
