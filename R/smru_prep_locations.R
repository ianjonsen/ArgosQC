##' @title clean, truncate & convert diag files to sf-tibbles
##'
##' @description restructures diag files, formats dates & lc's; keeps the
##' deployments in the metadata and starts each track at its deployment start
##' (metadata column `deploy_start`, set by `smru_deployment_start()`); converts
##' to sf geometry - all in preparation for SSM-filtering. Tracks are not
##' truncated at the first or last CTD profile or dive record.
##'
##' @param smru list of SMRU tables
##' @param meta metadata; its `deploy_start` column (if present) sets the start
##' of each track
##' @param dropIDs SMRU refs to be dropped (eg. tags were turned on but not deployed)
##' @param crs a proj4string to re-project diag locations from longlat. Default is NULL
##' which results in one of 4 possible projections applied automatically, based on
##' the centroid of the tracks. See `overview` vignette for details.
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm)
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do
##' @importFrom dplyr ungroup rename select
##' @importFrom assertthat assert_that
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract str_split
##' @importFrom aniMotum format_data
##'
##' @export
##'

smru_prep_loc <- function(smru,
                          meta,
                          dropIDs = NULL,
                          crs = NULL,
#                          extent = NULL,
                          QCmode = NULL) {

  assert_that(is.list(smru))
  if(is.null(QCmode)) stop("QCmode not specified in config file")

  # ## turn extent string into a numeric vector
  # if(!is.null(extent)) {
  #   ex <- unlist(str_split(extent, ",")) |> as.numeric()
  #   if(length(ex) != 4) stop("'subset.region' not properly defined in config file as a lon-lat extent string. See ?smru_qc for details")
  #
  # } else if (is.null(extent)) {
  #     ex <- c(-180,180,-90,90)
  #   }


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

  ## drop unused IDs
  diag <- diag |>
    filter(!ref %in% dropIDs)

  ## drop Argos placeholder locations at exactly 0, 0 (near real-time and
  ##  delayed-mode QC; see qc_drop_null_island())
  diag <- qc_drop_null_island(diag)

  ## keep the deployments in the metadata and start each track at its
  ##  deployment start (T0, metadata column deploy_start; see
  ##  smru_deployment_start()). Tracks are not truncated at the first or last
  ##  CTD profile or dive record, in near real-time or delayed-mode QC
  ##  (delayed-mode track ends are set with trimIDs; see trim_locs())
  id_col <- intersect(c("device_id", "DeploymentID"), names(meta))[1]
  if (is.na(id_col)) stop("Unrecognized ID variable, please check source of metadata")
  ids <- as.character(meta[[id_col]])
  diag <- diag |>
    filter(ref %in% ids)
  if ("deploy_start" %in% names(meta)) {
    T0 <- as.POSIXct(meta$deploy_start, tz = "UTC")[match(diag$ref, ids)]
    diag <- diag[is.na(T0) | diag$date >= T0, ]
  }
  ## ATN: deployment stop date and time from the metadata, where given
  if (id_col == "DeploymentID" && "DeploymentStopDateTime" %in% names(meta)) {
    T1 <- as.POSIXct(meta$DeploymentStopDateTime, tz = "UTC")[match(diag$ref, ids)]
    diag <- diag[is.na(T1) | diag$date <= T1, ]
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
