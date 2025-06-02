##' @title reconfigure annotated SMRU tables, test for expected data schema for AODN
##'
##' @description reconfigure annotated tables - subsample predicted locations to 6-h interval, write to .csv and zip by campaign id
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param fit final \code{aniMotum} fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param test should variables be tested for standards compliance, default is TRUE.
##' Standards compliance is specific to the program. Currently, only program = `imos`
##' has defined variable standard against which output compliance is tested.
##' @param path path to write .csv files
##' @param drop.refs individual SMRU ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##' @examples
##'
##' @importFrom dplyr filter rename mutate select group_by group_split
##' @importFrom stringr str_replace
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @export

smru_write_csv <- function(smru_ssm,
                        fit,
                        what,
                        meta,
                        program = "imos",
                        test = TRUE,
                        path = NULL,
                        drop.refs = NULL,
                        suffix = "_nrt") {


  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## SSM predictions
  ssm_out <- smru_ssm_outputs(fit = fit,
                         what = what,
                         drop.refs = drop.refs,
                         suffix = suffix)

  ## Metadata
  if (program == "imos") {
    meta <- meta |>
      filter(!device_id %in% drop.refs)

    meta <- left_join(meta, ssm_out$qc_se, by = c("device_id" = "ref")) |>
      select(-ctd_start, -ctd_end, -dive_start, -dive_end)

  } else if (program == "atn") {

    ## If ATN data then append QC variables to metadata
    meta <- meta |> filter(!DeploymentID %in% drop.refs)

    meta <- left_join(meta, ssm_out$qc_se, by = c("DeploymentID" = "ref")) |>
      select(-ctd_start, -ctd_end, -dive_start, -dive_end) |>
      rename(QCStartDateTime = qc_start_date, QCStopDateTime = qc_end_date) |>
      mutate(QCMethod = "ArgosQC",
             QCVersion = as.character(packageVersion("ArgosQC")))

    now <- Sys.time()
    attr(now, "tzone") <- "UTC"
    meta <- meta |>
      mutate(QCDateTime = now)

  }

  idx <- rep(FALSE, 8)
  out <- list()

  ## SSM predictions
  ssmoutputs <- smru_write_ssm(
    p_out = ssm_out$p_out,
    meta = meta,
    program = program,
    path = path,
    drop.refs = drop.refs,
    suffix = suffix
  )
  out[[1]] <- ssmoutputs
  idx[1] <- TRUE

  ## Diag data
  diag <- smru_write_diag(
    smru_ssm = smru_ssm,
    meta = meta,
    program = program,
    test = test,
    path = path,
    drop.refs = drop.refs,
    suffix = suffix
  )
  out[[2]] <- diag
  idx[2] <- TRUE

  ## GPS data (if present)
  if ("gps" %in% names(smru_ssm)) {
    gps <- smru_write_gps(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      test = test,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[3]] <- gps
    idx[3] <- TRUE
  }

  ## Haulout data
  if ("haulout" %in% names(smru_ssm)) {
    haulout <- smru_write_haulout(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      test = test,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[4]] <- haulout
    idx[4] <- TRUE
  }

  ## CTD data
  if ("ctd" %in% names(smru_ssm)) {
    ctd <- smru_write_ctd(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      test = test,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[5]] <- ctd
    idx[5] <- TRUE
  }

  ## dive data
  if ("dive" %in% names(smru_ssm)) {
    dive <- smru_write_dive(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[6]] <- dive
    idx[6] <- TRUE
  }

  ## summary data
  if ("ssummary" %in% names(smru_ssm)) {
    ssummary <- smru_write_summary(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[7]] <- ssummary
    idx[7] <- TRUE
  }

## Metadata
  meta <- smru_write_meta(
    meta = meta,
    program = program,
    test = test,
    path = path,
    drop.refs = drop.refs,
    suffix = suffix
  )
  out[[8]] <- meta
  idx[8] <- TRUE

  nms <- c("ssmoutputs","diag","gps","haulout","ctd","dive","summary","metadata")
  names(out) <- nms
  out <- out[idx]
  nms <- nms[idx]

  ## write tables to .csv
  if (program == "imos") {
    lapply(1:length(out), function(i) {
      if (nms[i] != "metadata") {
        out[[i]] |>
          group_by(cid) |>
          group_split() |>
          walk(~ suppressMessages(write_csv(
            .x, file = paste0(file.path(path, nms[i]), "_", .x$cid[1], suffix, ".csv")
          )))
      } else {
        out[[i]] |>
          group_by(sattag_program) |>
          group_split() |>
          walk(~ suppressMessages(write_csv(
            .x, file = paste0(file.path(path, nms[i]), "_", .x$sattag_program[1], suffix, ".csv")
          )))
      }
    })

  } else if (program == "atn") {

    lapply(1:length(out), function(i) {
      out[[i]] |>
        group_by(AnimalAphiaID, DeploymentLocation) |>
        group_split() |>
        walk(~ suppressMessages(write_csv(
          .x,
          file = paste0(
            file.path(path, nms[i]),
            "_",
            .x$AnimalAphiaID[1],
            "_",
            str_replace(.x$DeploymentLocation[1], "\\ ", ""),
            suffix,
            ".csv"
          )
        )))
    })
  }

  return(invisible())
}


##' @title SMRU ctd table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU ctd table, write to .csv - format depends on program (IMOS, ATN)
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
##' @importFrom dplyr filter rename mutate select any_of left_join
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_write_ctd <- function(smru_ssm,
                           meta,
                           program = "imos",
                           test = TRUE,
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

  if(program == "imos") {
    ctd <- ctd |>
      filter(ref %in% meta$device_id)
  }

  if (program == "imos" & test) {
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



##' @title SMRU diag table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU diag table, write to .csv - format depends on program (IMOS, ATN)
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
##' @importFrom dplyr filter rename mutate select any_of bind_rows group_by
##' @importFrom dplyr group_split
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @keywords internal

smru_write_diag <- function(smru_ssm,
                            meta,
                            program = "imos",
                            test = TRUE,
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

  if(program == "imos") {
    diag <- diag |>
      filter(ref %in% meta$device_id)
  }

  if (program == "imos" & test) {
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



##' @title SMRU dive table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU dive table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter mutate select
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_write_dive <- function(smru_ssm,
                            meta,
                            program = "imos",
                            path = NULL,
                            drop.refs = NULL,
                            suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  dive <- smru_ssm$dive |>
    filter(!ref %in% drop.refs) |>
    mutate(cid = str_extract(ref,
                             regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  if(program == "imos") {
    dive <- dive |>
      filter(ref %in% meta$device_id)
  }

  if (program == "atn") {

    dive <- left_join(
      dive,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)
  }

  ## strip out any records that are all NA's
  dive <- dive |>
    filter(!is.na(ref))

  return(dive)
}



##' @title SMRU gps table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU gps table, write to .csv - format depends on program (IMOS, ATN)
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

smru_write_gps <- function(smru_ssm,
                           meta,
                           program = "imos",
                           test = TRUE,
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

  if(program == "imos") {
    gps <- gps |>
      filter(ref %in% meta$device_id)
  }

  if (program == "imos" & test) {
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

smru_write_haulout <- function(smru_ssm,
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




##' @title metadata table tests & write to .csv
##'
##' @description Apply AODN tests to metadata table, write to .csv - format depends on program (IMOS, ATN)
##'
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
##' @importFrom dplyr filter rename mutate select any_of bind_rows group_by
##' @importFrom dplyr group_split
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @keywords internal

smru_write_meta <- function(meta,
                            program = "imos",
                            test = TRUE,
                            path = NULL,
                            drop.refs = NULL,
                            suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## remove dive, ctd start/end dates columns, add 'state_country' for AODN (based on deployment location)
  if (program == "imos") {

    meta <- meta |>
      mutate(
        state_country = ifelse(
          release_site == "Dumont d'Urville",
          "French Antarctic Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Dumont D'Urville",
          "French Antarctic Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Iles Kerguelen",
          "French Overseas Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Scott Base",
          "New Zealand Antarctic Territory",
          state_country
        )
      ) |>
      mutate(state_country = ifelse(
        release_site == "Campbell Island",
        "New Zealand",
        state_country
      )) |>
      mutate(state_country = ifelse(release_site == "Montague Island", "Australia", state_country)) |>
      mutate(state_country = ifelse(
        release_site == "Macquarie Island",
        "Australia",
        state_country
      )) |>
      mutate(
        state_country = ifelse(
          release_site == "Casey",
          "Australian Antarctic Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Davis",
          "Australian Antarctic Territory",
          state_country
        )
      ) |>
      mutate(state_country = ifelse(is.na(state_country), "Unknown", state_country))


    ## check metadata schema compliance to AODN standard
    meta <- meta |>
      select(
        sattag_program,
        device_id,
        ptt,
        body,
        device_wmo_ref,
        tag_type,
        common_name,
        species,
        release_longitude,
        release_latitude,
        release_site,
        release_date,
        recovery_date,
        age_class,
        sex,
        length,
        estimated_mass,
        actual_mass,
        state_country,
        qc_start_date,
        qc_end_date
      ) |>
      filter(!is.na(qc_start_date),
             !is.na(qc_end_date))

    ## return error if unexpected object mode or value
    tests <- with(
      meta,
      c(
        is.character(sattag_program),
        is.character(device_id),
        is.integer(ptt),
        is.integer(body),
        is.character(device_wmo_ref),
        is.character(tag_type),
        is.character(common_name),
        is.character(species),
        all(is.double(release_longitude), ((release_longitude >= -180 &
                                              release_longitude <= 180) |
                                             (release_longitude >= 0 &
                                                release_longitude <= 360) |
                                             is.na(release_longitude)
        )),
        all(is.double(release_latitude), ((release_latitude >= -90 &
                                             release_latitude <= 90) |
                                            is.na(release_latitude)
        )),
        is.character(release_site),
        any(inherits(release_date, "POSIXct"), is.na(release_date)),
        any(inherits(recovery_date, "POSIXct"), is.na(recovery_date)),
        all(
          unique(age_class) %in% c("adult", "subadult", "juvenille", "juvenile", "weaner", NA)
        ),
        all(unique(sex) %in% c("female", "male", "f", "m", NA)),
        all(is.double(length), (length > 0 |
                                  is.na(length))),
        all(is.integer(estimated_mass), (
          estimated_mass > 0 | is.na(estimated_mass)
        )),
        all(is.double(actual_mass), (actual_mass > 0 |
                                       is.na(actual_mass))),
        is.character(state_country),
        any(inherits(qc_start_date, "POSIXct"), is.na(qc_start_date)),
        any(inherits(qc_end_date, "POSIXct"), is.na(qc_end_date))
      )
    )
    if(test) {
      fails <- names(meta)[which(!tests)]
      if (length(fails) > 0)
        stop(paste0("non-compliant metadata records found in: ", fails, "\n"))
    }

    ## If metadata is compliant then write to .csv by sattag_program (SMRU campaign id)
    meta <- meta |>
      mutate(age_class = ifelse(age_class == "juvenile", "juvenille", age_class))

  } else if (program == "atn") {

    meta <- meta |> filter(!DeploymentID %in% drop.refs)

  }

  return(meta)
}



##' @title ssmoutputs table write to .csv
##'
##' @description write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select left_join group_split group_by
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @keywords internal

smru_write_ssm <- function(p_out,
                           meta,
                           program = "imos",
                           path = NULL,
                           drop.refs = NULL,
                           suffix = "_nrt") {


  p_out <- p_out |>
    mutate(
      lon = round(lon, 6),
      lat = round(lat, 6),
      x = round(x, 6),
      y = round(y, 6),
      x_se = round(x_se, 6),
      y_se = round(y_se, 6),
      u = round(u, 6),
      v = round(v, 6),
      u_se = round(u_se, 6),
      v_se = round(v_se, 6),
      s = round(s, 6),
      s_se = round(s_se, 6)
    )

  if (suffix != "_nrt" & "keep" %in% names(p_out)) {
    ## cut predicted locs from large data gaps
    p_out <- p_out |>
      filter(keep) |>
      select(-keep)

  }

  if (program == "atn") {
    p_out <- left_join(
      p_out,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)

  }

  return(p_out)
}



##' @title SMRU summary table tests & write to .csv
##'
##' @description Apply AODN tests to SMRU summary table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter mutate select
##' @importFrom stringr str_extract regex
##'
##' @keywords internal

smru_write_summary <- function(smru_ssm,
                               meta,
                               program = "imos",
                               path = NULL,
                               drop.refs = NULL,
                               suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ssummary <- smru_ssm$ssummary |>
    filter(!ref %in% drop.refs) |>
    mutate(cid = str_extract(ref,
                             regex("[a-z]{1,2}[0-9]{2,3}", ignore_case = TRUE)))

  if(program == "imos") {
    ## double check only device_id's in metadata are written
    ssummary <- ssummary |>
      filter(ref %in% meta$device_id)
  }

  if (program == "atn") {

    ssummary <- left_join(
      ssummary,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)

  }

  return(ssummary)
}
