##' @title SMRU ctd table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU ctd table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select any_of left_join
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_ctd_write <- function(smru_ssm,
                               meta,
                               program = "imos",
                               path = NULL,
                               drop.refs = NULL,
                               suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ctd <- smru_ssm$ctd <- smru_ssm$ctd |>
    filter(!ref %in% drop.refs) |>
    filter(!is.na(ref)) |>
    mutate(cid = str_extract(ref,
                             regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  ## check ctd schema compliance to AODN standard
  vars <- c("ref",
            "ptt",
            "end_date",
            "max_dbar",
            "num",
            "n_temp",
            "n_cond",
            "n_sal",
            "temp_dbar",
            "temp_vals",
            "cond_dbar",
            "cond_vals",
            "sal_dbar",
            "sal_vals",
            "n_fluoro",
            "fluoro_dbar",
            "fluoro_vals",
            "n_oxy",
            "oxy_dbar",
            "oxy_vals",
            "qc_profile",
            "qc_temp",
            "qc_sal",
            "sal_corrected_vals",
            "created",
            "modified",
            "n_photo",
            "photo_dbar",
            "photo_vals",
            "lat",
            "lon",
            "ssm_lon",
            "ssm_lat",
            "ssm_x",
            "ssm_y",
            "ssm_x_se",
            "ssm_y_se",
            "cid")

  ctd <- ctd |>
    select(any_of(vars))

  if (any(!c("photo_dbar", "photo_vals", "created", "modified", "n_photo")
          %in% ctd)) {
    tests <- with(
      ctd,
      c(
        is.character(ref),
        is.integer(ptt),
        inherits(end_date, "POSIXct"),
        all(is.double(max_dbar), max_dbar >= 0),
        is.integer(num),
        all(is.integer(n_temp), (n_temp >= 0 |
                                   is.na(n_temp))),
        all(is.integer(n_cond), (n_cond >= 0 |
                                   is.na(n_cond))),
        all(is.integer(n_sal), (n_sal >= 0 |
                                  is.na(n_sal))),
        any(is.character(temp_dbar), all(is.na(temp_dbar))),
        any(is.character(temp_vals), all(is.na(temp_vals))),
        any(is.character(cond_dbar), all(is.na(cond_dbar))),
        any(is.character(cond_vals), all(is.na(cond_vals))),
        any(is.character(sal_dbar), all(is.na(sal_dbar))),
        any(is.character(sal_vals), all(is.na(sal_vals))),
        any(is.integer(n_fluoro), all(is.na(n_fluoro))),
        any(is.character(fluoro_dbar), all(is.na(fluoro_dbar))),
        any(is.character(fluoro_vals), all(is.na(fluoro_vals))),
        any(is.integer(n_oxy), all(is.na(n_oxy))),
        any(is.character(oxy_dbar), all(is.na(oxy_dbar))),
        any(is.character(oxy_vals), all(is.na(oxy_vals))),
        is.integer(qc_profile),
        any(is.character(qc_temp), all(is.na(qc_temp))),
        any(is.character(qc_sal), all(is.na(qc_sal))),
        any(is.character(sal_corrected_vals),
            all(is.na(
              sal_corrected_vals
            )))
      )
    )
  } else {
    tests <- with(
      ctd,
      c(
        is.character(ref),
        is.integer(ptt),
        inherits(end_date, "POSIXct"),
        all(is.double(max_dbar), max_dbar >= 0),
        is.integer(num),
        all(is.integer(n_temp), (n_temp >= 0 |
                                   is.na(n_temp))),
        all(is.integer(n_cond), (n_cond >= 0 |
                                   is.na(n_cond))),
        all(is.integer(n_sal), (n_sal >= 0 |
                                  is.na(n_sal))),
        any(is.character(temp_dbar), all(is.na(temp_dbar))),
        any(is.character(temp_vals), all(is.na(temp_vals))),
        any(is.character(cond_dbar), all(is.na(cond_dbar))),
        any(is.character(cond_vals), all(is.na(cond_vals))),
        any(is.character(sal_dbar), all(is.na(sal_dbar))),
        any(is.character(sal_vals), all(is.na(sal_vals))),
        any(is.integer(n_fluoro), all(is.na(n_fluoro))),
        any(is.character(fluoro_dbar), all(is.na(fluoro_dbar))),
        any(is.character(fluoro_vals), all(is.na(fluoro_vals))),
        any(is.integer(n_oxy), all(is.na(n_oxy))),
        any(is.character(oxy_dbar), all(is.na(oxy_dbar))),
        any(is.character(oxy_vals), all(is.na(oxy_vals))),
        is.integer(qc_profile),
        any(is.character(qc_temp), all(is.na(qc_temp))),
        any(is.character(qc_sal), all(is.na(qc_sal))),
        any(is.character(sal_corrected_vals),
            all(is.na(
              sal_corrected_vals
            ))),
        any(inherits(created, "POSIXct"), all(is.na(created))),
        any(inherits(modified, "POSIXct"), all(is.na(modified))),
        any(is.integer(n_photo), all(is.na(n_photo))),
        any(is.character(photo_dbar), all(is.na(photo_dbar))),
        any(is.character(photo_vals), all(is.na(photo_vals)))
      )
    )
  }


  if (program == "imos") {
    if(which(!tests) == 7) {
      ctd <- ctd |>
        mutate(n_cond = as.integer(n_cond, na.rm = TRUE))

      tests[7] <- TRUE
    }
    ## Test fails only throw error for IMOS program data
    fails <- names(ctd)[which(!tests)]
    if (length(fails) > 0)
      stop(paste0("non-compliant ctd records found in: ", fails, "\n"))

  } else if (program == "atn") {

    ctd <- left_join(
      ctd,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)
  }

  return(ctd)
}
