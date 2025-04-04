##' @title reconfigure annotated SMRU tables, test for expected data schema for AODN
##'
##' @description reconfigure annotated tables - subsample predicted locations to 6-h interval, write to .csv and zip by campaign id
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param fit final \code{aniMotum} fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
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

write_2_csv <- function(smru_ssm,
                        fit,
                        what,
                        meta,
                        program = "imos",
                        path = NULL,
                        drop.refs = NULL,
                        suffix = "_nrt") {


  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## SSM predictions
  ssm_out <- ssm_outputs(fit = fit,
                         what = "p",
                         drop.refs = drop.refs)

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
  ssmoutputs <- ssm_write(
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
  diag <- smru_diag_write(
    smru_ssm = smru_ssm,
    meta = meta,
    program = program,
    path = path,
    drop.refs = drop.refs,
    suffix = suffix
  )
  out[[2]] <- diag
  idx[2] <- TRUE

  ## GPS data (if present)
  if ("gps" %in% names(smru_ssm)) {
    gps <- smru_gps_write(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[3]] <- gps
    idx[3] <- TRUE
  }

  ## Haulout data
  if ("haulout" %in% names(smru_ssm)) {
    haulout <- smru_haulout_write(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[4]] <- haulout
    idx[4] <- TRUE
  }

  ## CTD data
  if ("ctd" %in% names(smru_ssm)) {
    ctd <- smru_ctd_write(
      smru_ssm = smru_ssm,
      meta = meta,
      program = program,
      path = path,
      drop.refs = drop.refs,
      suffix = suffix
    )
    out[[5]] <- ctd
    idx[5] <- TRUE
  }

  ## dive data
  if ("dive" %in% names(smru_ssm)) {
    dive <- smru_dive_write(
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
    ssummary <- smru_summary_write(
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
  meta <- meta_write(
    meta = meta,
    program = program,
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


