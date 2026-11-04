##' @title clean, truncate & convert WC Locations files to sf-tibbles
##'
##' @description restructures Locations files, formats dates & lc's; keeps the
##' deployments in the metadata and starts each track at its QC start (the
##' deployment start, T0, or a later manual QC start; see
##' `wc_deployment_start()`); converts to sf geometry - all in preparation for
##' SSM-filtering. Tracks are not truncated at the first or last dive record.
##'
##' @param wc list of WC datafiles
##' @param meta metadata; its `QC_start_date` column sets the start of each track
##' @param dropIDs WC DeploymentID's to be dropped (eg. tags were turned on but not deployed)
##' @param crs a proj4string to re-project diag locations from longlat. Default is NULL
##' which results in one of 4 possible projections applied automatically, based on
##' the centroid of the tracks. See `overview` vignette for details.
##' @param program specify the aniBOS program contributing data (currently: 'atn', 'irap')
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm).
##' In delayed-mode QC, tracks end at a manual QC end or, for ATN, the metadata
##' deployment stop, where given.
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do
##' @importFrom dplyr ungroup rename select n lag first
##' @importFrom assertthat assert_that
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract
##' @importFrom aniMotum format_data
##' @importFrom traipse track_distance_to
##'
##' @export
##'

wc_prep_loc <- function(wc,
                    meta,
                    dropIDs,
                    crs = NULL,
                    program = "atn",
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

  ## keep the deployments in the metadata and start each track at its QC start:
  ##  the deployment start (T0) or a later manual QC start (see
  ##  wc_deployment_start()). Tracks are not truncated at the first or last
  ##  dive record. Delayed-mode QC only: tracks end at a manual QC end
  ##  (QC_end_datetime in the metadata file) or, for ATN, the metadata
  ##  deployment stop (DeploymentStopDateTime), where given
  locs <- locs |>
    filter(!DeploymentID %in% dropIDs,
           DeploymentID %in% meta$DeploymentID,
           !is.na(lon), !is.na(lat))
  i <- match(locs$DeploymentID, meta$DeploymentID)
  qs <- as.POSIXct(meta$QC_start_date, tz = "UTC")[i]
  locs <- locs[is.na(qs) | locs$date >= qs, ]
  if (identical(as.character(QCmode), "dm")) {
    i <- match(locs$DeploymentID, meta$DeploymentID)
    qe <- if ("QC_end_date" %in% names(meta)) as.POSIXct(meta$QC_end_date, tz = "UTC")[i] else
      as.POSIXct(rep(NA_real_, nrow(locs)), tz = "UTC")
    if ("DeploymentStopDateTime" %in% names(meta)) {
      ds <- as.POSIXct(meta$DeploymentStopDateTime, tz = "UTC")[i]
      qe[is.na(qe)] <- ds[is.na(qe)]
    }
    locs <- locs[is.na(qe) | locs$date <= qe, ]
  }
  locs <- locs |>
    select(DeploymentID, everything())

  ## apply simple distance - time filter on first 10 locations to remove potentially
  ##  spurious locations immediately after the first location, which is often
  ##  a user-measured GPS location. Assume dist >= 1000km + spd>=500km/h imply
  ##  spurious locations

  locs <- locs |>
    group_by(DeploymentID) |>
    mutate(dist = track_distance_to(lon, lat, first(lon), first(lat))/1000) |>
    mutate(dt = as.numeric(difftime(date, lag(date), units = "hours"))) |>
    mutate(spd = dist/dt) |>
    mutate(test = c(rep(TRUE, 10), rep(FALSE, n()-10))) |>
    filter((dist < 1000 & spd < 500 & test) | !test) |>
    select(-dist, -dt, -spd, -test)


  if (is.null(crs)) {
    ## project lon,lat coords - use different projections depending where
    ##  centroid of tracks lies latitudinally
    proj.fn <- function(x) {
      if ((mean(x$lat, na.rm = T, trim=0.05) > 25 &
          mean(x$lat, na.rm = T, trim=0.05) <= 55) |
          (mean(x$lat, na.rm = T, trim=0.05) < -25 &
          mean(x$lat, na.rm = T, trim=0.05) >= -55)) {
        lat25 <- quantile(x$lat, 0.25, na.rm = TRUE) |> round(0)
        lat75 <- quantile(x$lat, 0.75, na.rm = TRUE) |> round(0)
        mlon <- mean(ifelse(x$lon < 0, x$lon + 360, x$lon),
                     na.rm = T,
                     trim = 0.1) |>
          round(0)

        x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
          sf::st_transform(crs = paste0("+proj=eqdc +lat_1=",
                                        lat25,
                                        " +lat_2=",
                                        lat75,
                                        " +lon_0=",
                                        mlon,
                                        " +units=km +ellps=WGS84"))

      } else if (mean(x$lat, na.rm = T, trim = 0.05) > 55 |
                 mean(x$lat, na.rm = T, trim = 0.05) < -55) {
        mlon <- mean(ifelse(x$lon < 0, x$lon + 360, x$lon),
                     na.rm = T,
                     trim = 0.1) |>
          round(0)
        mlat <- mean(x$lat, na.rm = T, trim = 0.05) |>
          round(0)
        x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
          sf::st_transform(crs = paste0(
            "+proj=stere +lon_0=",
            mlon,
            " +lat_0=",
            mlat,
            " +units=km +ellps=WGS84"
          ))

      } else if(mean(x$lat, na.rm = T, trim=0.05) <= 25 &
                 mean(x$lat, na.rm = T, trim=0.05) >= -25) {

        if(diff(range(x$lon, na.rm = T)) > 180) {
          x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
            sf::st_shift_longitude()

        } else {
          x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
        }
      }

      x
    }

  } else {
    proj.fn <- function(x) {
      x |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
        sf::st_transform(., crs = crs)
    }

  }



  ## Reproject
  locs <- locs |>
    mutate(id = DeploymentID) |>
    dplyr::select(DeploymentID, id, everything()) |>
    group_by(DeploymentID) |>
    do(d_sf = proj.fn(.)) |>
    ungroup()

  if (program == "atn") {
    locs <- locs |>
      left_join(meta |> select(DeploymentID, ADRProjectID), by = "DeploymentID") |>
      select(DeploymentID, ADRProjectID, d_sf)

  } else if (program != "atn") {
    locs <- locs |>
      select(DeploymentID, d_sf)

  } else if (!program %in% c("atn", "irap")) {
    stop("program currently not supported")

  }

  return(locs)
}
