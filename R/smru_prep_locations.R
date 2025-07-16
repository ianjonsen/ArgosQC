##' @title clean, truncate & convert diag files to sf-tibbles
##'
##' @description restructures diag files, formats dates & lc's; truncates start
##' (and end for "nrt") of individual deployments using ctd dates; converts to
##' sf geometry - all in preparation for SSM-filtering. Splits resulting truncated
##' diag files by species.
##'
##' @param smru list of SMRU tables
##' @param meta metadata used to truncate start of diag data for each individual
##' @param dropIDs SMRU refs to be dropped (eg. tags were turned on but not deployed)
##' @param crs a proj4string to re-project diag locations from longlat. Default is NULL
##' which results in one of 4 possible projections applied automatically, based on
##' the centroid of the tracks. See `overview` vignette for details.
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm),
##' in latter case diag is not right-truncated & date of first dive is used
##' for the track start date
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

smru_prep_loc <- function(smru,
                          meta,
                          dropIDs = NULL,
                          crs = NULL,
                          QCmode = NULL) {

  assert_that(is.list(smru))
  if(is.null(QCmode)) stop("QCmode not specified in config file")

  ## clean step
  if("semi_major_axis" %in% names(smru$diag)) {
    diag <- smru$diag |>
      dplyr::select(ref,
             d_date,
             lq,
             lon,
             lat,
             semi_major_axis,
             semi_minor_axis,
             ellipse_orientation)
  } else {
    diag <- smru$diag |>
      dplyr::select(ref,
             d_date,
             lq,
             lon,
             lat) |>
      mutate(semi_major_axis = NA,
             semi_minor_axis = NA,
             ellipse_orientation = NA)
  }

  diag <- diag |>
    rename(
      ref = ref,
      date = d_date,
      lc = lq,
      smaj = semi_major_axis,
      smin = semi_minor_axis,
      eor = ellipse_orientation
    ) |>
    mutate(ref = as.character(ref)) |>
    mutate(lc = ifelse(lc==-1, "A", lc), lc = ifelse(lc==-2, "B", lc), lc = ifelse(lc==-9, "Z", lc)) |>
    mutate(lc = factor(
      lc,
      levels = c(3, 2, 1, 0, "A", "B", "Z"),
      labels = c("3", "2", "1", "0", "A", "B", "Z"),
      ordered = TRUE
    )) |>
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) |>
    mutate(smaj = as.numeric(smaj),
           smin = as.numeric(smin),
           eor = as.numeric(eor))

  if("gps" %in% names(smru)) {
   gps <- smru$gps |>
     rename(date = d_date) |>
      aniMotum::format_data(id = "ref",
                  coord = c("lon","lat")) |>
      select(ref = id,date,lc,lon,lat,smaj,smin,eor) |>
     filter(!is.na(lon), !is.na(lat))

   diag <- bind_rows(diag, gps) |>
     group_by(ref) |>
     arrange(date, .by_group=TRUE) |>
     ungroup()
  }

  diag <- diag |>
    filter(!ref %in% dropIDs)

  ## truncate & convert to sf geometry steps
  if("device_id" %in% names(meta)) {
    if("dive_start" %in% names(meta)) {
      deploy_meta <- meta |>
        dplyr::select(device_id, ctd_start, dive_start, ctd_end, dive_end)

    } else {
      deploy_meta <- meta |>
        dplyr::select(device_id, ctd_start, ctd_end)
    }


    if(QCmode == "nrt") {
      ## left- and right-truncate tracks
      diag <- diag |>
        left_join(deploy_meta, by = c("ref" = "device_id")) |>
        filter(date >= ctd_start & date <= ctd_end) |>
        dplyr::select(-ctd_start, -ctd_end)
    } else {
      if("dive_start" %in% names(meta)) {
      ## only left-truncate tracks with date of first dive
      diag <- diag |>
        left_join(deploy_meta, by = c("ref" = "device_id")) |>
        filter(date >= dive_start) |>
        dplyr::select(-ctd_start, -dive_start, -ctd_end, -dive_end)

      } else {
        diag <- diag |>
          left_join(deploy_meta, by = c("ref" = "device_id")) |>
          filter(date >= ctd_start) |>
          dplyr::select(-ctd_start, -ctd_end)
      }
    }

  } else if ("DeploymentID" %in% names(meta)) {
    deploy_meta <- meta |>
      dplyr::select(
        DeploymentID,
        DeploymentStartDateTime,
        DeploymentStopDateTime,
        ctd_start,
        dive_start,
        ctd_end,
        dive_end
      ) |>
      mutate(
        ctd_start = ifelse(
          DeploymentStartDateTime > ctd_start,
          DeploymentStartDateTime,
          ctd_start
        )
      ) |>
      mutate(ctd_start = as.POSIXct(ctd_start, origin = "1970-01-01", tz = "UTC")) |>
      mutate(ctd_end = ifelse(
        DeploymentStopDateTime < ctd_end,
        DeploymentStopDateTime,
        ctd_end
      )) |>
      mutate(ctd_end = as.POSIXct(ctd_end, origin = "1970-01-01", tz = "UTC")) |>
      mutate(
        dive_start = ifelse(
          DeploymentStartDateTime > dive_start,
          DeploymentStartDateTime,
          dive_start
        )
      ) |>
      mutate(dive_start = as.POSIXct(dive_start, origin = "1970-01-01", tz = "UTC")) |>
      mutate(dive_end = ifelse(
        DeploymentStopDateTime < dive_end,
        DeploymentStopDateTime,
        dive_end
      )) |>
      mutate(dive_end = as.POSIXct(dive_end, origin = "1970-01-01", tz = "UTC"))

    if(QCmode == "nrt") {
      ## left- and right-truncate tracks
      diag <- diag |>
        left_join(deploy_meta, by = c("ref" = "DeploymentID")) |>
        filter(date >= ctd_start & date <= ctd_end) |>
        dplyr::select(-ctd_start, -ctd_end, -dive_end)
    } else {
      ## only left-truncate tracks with date of first dive
      diag <- diag |>
        left_join(deploy_meta, by = c("ref" = "DeploymentID")) |>
        filter(date >= dive_start) |>
        dplyr::select(-ctd_start, -dive_start, -ctd_end, -dive_end)
    }

  } else {
    stop("Unrecognized ID variable, please check source of metadata")

  }

## Define reprojection functions based on whether crs is NULL or specified
  if (is.null(crs)) {
    proj.fn <- function(x) {
      if ((mean(x$lat, na.rm = T, trim = 0.05) > 25 &
           mean(x$lat, na.rm = T, trim = 0.05) <= 55) |
          (mean(x$lat, na.rm = T, trim = 0.05) < -25 &
           mean(x$lat, na.rm = T, trim = 0.05) >= -55)) {
        lat25 <- quantile(x$lat, 0.25, na.rm = TRUE) |> round(0)
        lat75 <- quantile(x$lat, 0.75, na.rm = TRUE) |> round(0)
        mlon <- mean(ifelse(x$lon < 0, x$lon + 360, x$lon),
                     na.rm = T,
                     trim = 0.1) |>
          round(0)

        x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
          sf::st_transform(
            crs = paste0(
              "+proj=eqdc +lat_1=",
              lat25,
              " +lat_2=",
              lat75,
              " +lon_0=",
              mlon,
              " +units=km +ellps=WGS84"
            )
          )

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

      } else if (mean(x$lat, na.rm = T, trim = 0.05) <= 25 &
                 mean(x$lat, na.rm = T, trim = 0.05) >= -25) {
        if (diff(range(x$lon, na.rm = T)) > 180) {
          x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
            sf::st_shift_longitude()

        } else {
          x <- x |> sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
        }
      }

      x |> select(-ref)

    }
  } else if (!is.null(crs)) {
    proj.fn <- function(x) {
      x |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
        sf::st_transform(., crs = crs) |>
        select(-ref)
    }

  }

  ## Reproject
  diag_sf <- diag |>
    mutate(id = ref) |>
    dplyr::select(ref, id, everything()) |>
    group_by(ref) |>
    do(d_sf = proj.fn(.)) |>
    ungroup() |>
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))
  # }

  diag_sf <- diag_sf |>
    dplyr::select(ref, cid, d_sf)

  return(diag_sf)

}
