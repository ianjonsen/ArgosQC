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
##' @param crs a proj4string to re-project diag locations from longlat. Default is NULL
##' which results in one of 4 possible projections applied automatically, based on
##' the centroid of the tracks. See `overview` vignette for details.
##' @param program specify the aniBOS program contributing data (currently: 'atn', 'irap')
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm),
##' in latter case wc is not right-truncated & date of first dive is used
##' for the track start date.
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

  if (program == "atn") {
    locs <- locs |>
      filter(!DeploymentID %in% dropIDs)

    ## truncate by dive_start/dive_end datetimes & convert to sf geometry steps
    deploy_meta <- meta |>
      dplyr::select(
        DeploymentID,
        DeploymentStartDateTime,
        DeploymentStopDateTime,
        dive_start,
        dive_end
      )

    ## only truncate tracks depending on QCmode
    locs <- locs |>
      left_join(deploy_meta, by = "DeploymentID") |>
      mutate(dt.meta = as.numeric(difftime(
        as.Date(DeploymentStopDateTime),
        as.Date(DeploymentStartDateTime),
        units = "days"
      )),
      dt.dive = as.numeric(difftime(
        as.Date(dive_end), as.Date(dive_start), units = "days"
      )))

    locs.lst <- split(locs, locs$DeploymentID)

    locs <- lapply(locs.lst, function(x) {
      switch(QCmode, nrt = {
        if (x$dive_start[1] >= x$DeploymentStartDateTime[1]) {
          x |> filter(date >= dive_start)
        } else {
          x |> filter(date >= DeploymentStartDateTime)
        }
      }, dm = {
        if (x$dt.meta[1] > x$dt.dive[1] &
            x$dt.dive[1] > x$dt.meta[1] * 0.5) {
          x |> filter(date >= dive_start & date <= dive_end)
        } else {
          x |> filter(date >= DeploymentStartDateTime &
                        date <= DeploymentStopDateTime)
        }
      })
    }) |>
      bind_rows() |>
      dplyr::select(
        -DeploymentStartDateTime,-DeploymentStopDateTime,-dive_start,-dive_end,-dt.meta,-dt.dive
      )

  } else if (program != "atn") {
    locs <- locs |>
      filter(!DeploymentID %in% dropIDs)

    deploy_meta <- meta |>
      dplyr::select(
        DeploymentID, QC_start_date
      )

  locs <- locs |>
    left_join(deploy_meta,
              by = "DeploymentID")

    if (QCmode == "nrt") {
       locs <- locs |>
         filter(date >= QC_start_date & !is.na(lon)) |>
         select(-QC_start_date)

    } else {
      locs <- locs |>
        filter(date >= QC_start_date & !is.na(lon),
               date <= QC_end_date & !is.na(lon)) |>
        select(-QC_start_date, -QC_end_date)
    }

  locs <- locs |>
    select(DeploymentID, everything())

  } else {
    stop("AniBOS program currently not supported")

  }

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
