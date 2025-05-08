##' @title SMRU haulout table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU haulout table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param test should variables be tested for standards compliance, default is TRUE.
##' Standards compliance is specific to the program. Currently, only program = `imos`
##' has defined variable standard against which output compliance is tested.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter mutate select any_of left_join
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_haulout_write <- function(smru_ssm,
                            meta,
                            program = "imos",
                            test = TRUE,
                            path = NULL,
                            drop.refs = NULL,
                            suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  haulout <- smru_ssm$haulout |>
    filter(!ref %in% drop.refs) |>
    filter(!is.na(ref)) |>
    mutate(cid = str_extract(ref,
                             regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  ## check haulout schema compliance to AODN standard
  vars <- c("ref",
            "ptt",
            "s_date",
            "e_date",
            "haulout_number",
            "cnt",
            "phosi_secs",
            "wet_n",
            "wet_min",
            "wet_max",
            "wet_mean",
            "wet_sd",
            "tagging_id",
            "s_date_tag",
            "e_date_tag",
            "end_number",
            "lat",
            "lon",
            "ssm_lon",
            "ssm_lat",
            "ssm_x",
            "ssm_y",
            "ssm_x_se",
            "ssm_y_se",
            "cid"
  )
  haulout <- haulout |>
    select(any_of(vars))

  if(any(!c("phosi_secs","wet_n","wet_min","wet_max","wet_mean","wet_sd","tagging_id","s_date_tag","e_date_tag") %in%
         names(haulout))) {
    tests <- with(haulout,
                 c(is.character(ref),
                   is.integer(ptt),
                   inherits(s_date, "POSIXct"),
                   inherits(e_date, "POSIXct"),
                   is.integer(haulout_number),
                   all(is.integer(cnt), cnt >= 0),
                   is.integer(end_number)))
  } else {
    tests <- with(haulout,
                 c(is.character(ref),
                   is.integer(ptt),
                   inherits(s_date, "POSIXct"),
                   inherits(e_date, "POSIXct"),
                   is.integer(haulout_number),
                   all(is.integer(cnt), cnt >= 0),
                   all(is.integer(phosi_secs), (phosi_secs >= 0 |
                                                  is.na(phosi_secs))),
                   all(is.integer(wet_n), (wet_n >= 0 |
                                             is.na(wet_n))),
                   all(is.double(wet_min), (wet_min >= 0 |
                                              is.na(wet_min))),
                   all(is.double(wet_max), (wet_max >= 0 |
                                              is.na(wet_max))),
                   all(is.double(wet_mean), (wet_mean >= 0 |
                                               is.na(wet_mean))),
                   all(is.double(wet_sd), (wet_sd >= 0 |
                                             is.na(wet_sd))),
                   any(is.integer(tagging_id) | is.na(tagging_id)),
                   any(inherits(s_date_tag, "POSIXct") | is.na(s_date_tag)),
                   any(inherits(e_date_tag, "POSIXct") | is.na(e_date_tag)),
                   is.integer(end_number)))

    if(any(!tests[8:12])) {
      haulout <- haulout |>
        mutate(wet_n = as.integer(wet_n, na.rm = TRUE),
               wet_min = as.double(wet_min, na.rm = TRUE),
               wet_max = as.double(wet_max, na.rm = TRUE),
               wet_mean = as.double(wet_mean, na.rm = TRUE),
               wet_sd = as.double(wet_sd, na.rm = TRUE))
      tests[8:12] <- TRUE
    }

  }

  if(program == "imos") {
    haulout <- haulout |>
      filter(ref %in% meta$device_id)
  }

  if (program == "imos" & test) {

    ## Test fails only throw error for IMOS program data
    fails <- names(haulout)[which(!tests)]
    if (length(fails) > 0)
      stop(paste0("non-compliant haulout records found in: ", fails, "\n"))

  } else if (program == "atn") {
    haulout <- left_join(
      haulout,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)
  }

  return(haulout)
}


