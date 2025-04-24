##' @title SMRU diag table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU diag table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select any_of bind_rows group_by
##' @importFrom dplyr group_split
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @keywords internal

smru_diag_write <- function(smru_ssm,
                       meta,
                       program = "imos",
                       path = NULL,
                       drop.refs = NULL,
                       suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))


  ## Argos data
  diag <- smru_ssm$diag |>
    filter(!ref %in% drop.refs) |>
    filter(!is.na(ref)) |>
    mutate(cid = str_extract(ref, regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  ## check diag schema compliance to AODN standard
  vars <- c(
    "ref",
    "ptt",
    "d_date",
    "lq",
    "lat",
    "lon",
    "alt_lat",
    "alt_lon",
    "n_mess",
    "n_mess_120",
    "best_level",
    "pass_dur",
    "freq",
    "v_mask",
    "alt",
    "est_speed",
    "km_from_home",
    "iq",
    "nops",
    "deleted",
    "actual_ptt",
    "error_radius",
    "semi_major_axis",
    "semi_minor_axis",
    "ellipse_orientation",
    "hdop",
    "satellite",
    "diag_id",
    "ssm_lon",
    "ssm_lat",
    "ssm_x",
    "ssm_y",
    "ssm_x_se",
    "ssm_y_se",
    "cid"
  )
  diag <- diag |>
    select(any_of(vars))

  diag <- diag |>
    mutate(iq = ifelse(!is.integer(iq), as.integer(iq), iq))

  ## return error if unexpected object mode or value
  tests <- with(
    diag,
    c(
      is.character(ref),
      is.integer(ptt),
      inherits(d_date, "POSIXct"),
      is.integer(lq),
      all(is.double(lat), ((lat >= -90 &
                              lat <= 90) |
                             is.na(lat))),
      all(is.double(lon), ((lon >= -180 &
                              lon <= 180) |
                             (lon >= 0 & lon <= 360) |
                             is.na(lon)
      )),
      all(is.double(alt_lat), ((alt_lat >= -90 &
                                  alt_lat <= 90) |
                                 is.na(alt_lat)
      )),
      all(is.double(alt_lon), ((alt_lon >= -180 &
                                  alt_lon <= 180) |
                                 (alt_lon >= 0 & alt_lon <= 360) |
                                 is.na(alt_lon)
      )),
      all(is.integer(n_mess), n_mess >= 0),
      all(is.integer(n_mess_120), n_mess_120 >= 0),
      is.integer(best_level),
      is.integer(pass_dur),
      all(is.double(freq), freq > 0),
      is.integer(v_mask),
      any(is.numeric(alt), is.na(alt)),
      all(is.double(est_speed), (est_speed >= -1 |
                                   is.na(est_speed))),
      all(is.double(km_from_home), (km_from_home >= 0 |
                                      is.na(km_from_home))),
      any(is.integer(iq), is.na(iq)),
      is.integer(nops),
      any(is.logical(deleted), is.character(deleted)),
      is.integer(actual_ptt),
      if ("error_radius" %in% names(diag)) {
        all(any(is.integer(error_radius), is.na(error_radius)), (error_radius >= 0 |
                                                                   is.na(error_radius)))
      },
      if ("semi_major_axis" %in% names(diag)) {
        all(any(is.integer(semi_major_axis), is.na(semi_major_axis)), (semi_major_axis >= 0 |
                                                                         is.na(semi_major_axis)))
      },
      if ("semi_minor_axis" %in% names(diag)) {
        all(any(is.integer(semi_minor_axis), is.na(semi_minor_axis)), (semi_minor_axis >= 0 |
                                                                         is.na(semi_minor_axis)))
      },
      if ("ellipse_orientation" %in% names(diag)) {
        all(any(
          is.integer(ellipse_orientation),
          is.na(ellipse_orientation)
        ), ((ellipse_orientation >= 0 &
               ellipse_orientation <= 180) | is.na(ellipse_orientation)
        ))
      },
      if ("hdop" %in% names(diag)) {
        any(is.integer(hdop), is.na(hdop))
      },
      if ("satellite" %in% names(diag)) {
        any(is.character(satellite), is.na(satellite))
      },
      if ("diag_id" %in% names(diag)) {
        is.integer(diag_id)
      }
    )
  )

  if (program == "imos") {
    ## Test fails only throw error for IMOS program data
    fails <- names(diag)[which(!tests)]
    if (length(fails) > 0)
      stop(paste0("non-compliant diag records found in: ", fails, "\n"))

  } else if (program == "atn") {
    diag <- left_join(diag,
                       meta |> select(DeploymentID,
                                      AnimalAphiaID,
                                      DeploymentLocation),
                       by = c("ref" = "DeploymentID")) |>
      select(-cid)

  }

  return(diag)
}
