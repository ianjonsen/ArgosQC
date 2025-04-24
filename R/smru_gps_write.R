##' @title SMRU gps table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU gps table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter mutate select any_of left_join
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_gps_write <- function(smru_ssm,
                            meta,
                            program = "imos",
                            path = NULL,
                            drop.refs = NULL,
                            suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  gps <- smru_ssm$gps |>
    filter(!ref %in% drop.refs) |>
    filter(!is.na(ref)) |>
    mutate(cid = str_extract(ref, regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  ## check gps schema compliance to AODN standard
  vars <- c(
    "ref",
    "ptt",
    "d_date",
    "nsats_detected",
    "nsats_transmitted",
    "gps_mode",
    "pseudoranges",
    "max_csn",
    "cnt",
    "lat",
    "lon",
    "residual",
    "timeshift",
    "submitted",
    "nsats_healthy",
    "nbits",
    "deleted",
    "v_mask",
    "km_from_home",
    "est_speed",
    "gps_number",
    "tx_date",
    "uplink_number",
    "tagging_id",
    "d_date_tag",
    "ssm_lon",
    "ssm_lat",
    "ssm_x",
    "ssm_y",
    "ssm_x_se",
    "ssm_y_se",
    "cid"
  )
  gps <- gps |>
    select(any_of(vars))

  ## return error if unexpected object mode or value
  tests <- with(gps,
                c(
                  is.character(ref),
                  is.integer(ptt),
                  inherits(d_date, "POSIXct"),
                  all(is.double(lat), ((lat >= -90 &
                                          lat <= 90) |
                                         is.na(lat))),
                  all(is.double(lon), ((lon >= -180 &
                                          lon <= 180) |
                                         (lon >= 0 & lon <= 360) |
                                         is.na(lon)
                  )),
                  is.integer(v_mask),
                  all(is.double(est_speed), (est_speed >= -1 |
                                               is.na(est_speed))),
                  all(is.double(km_from_home), (km_from_home >= 0 |
                                                  is.na(km_from_home)))
                ))


  if (program == "imos") {
    ## Test fails only throw error for IMOS program data
    fails <- names(gps)[which(!tests)]
    if (length(fails) > 0)
      stop(paste0("non-compliant gps records found in: ", fails, "\n"))

  } else if (program == "atn") {

    gps <- left_join(
      gps,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)

  }

  return(gps)
}
