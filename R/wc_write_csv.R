##' @title write annotated WC tag datafiles & annotated metadata
##'
##' @description subsample SSM-predicted locations to 6-h intervals, write annotated files to .csv
##'
##' @param wc_ssm SSM-appended WC tag datafiles - output of \code{append_wc_ssm}
##' @param fit final SSM fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual WC DeploymentID's to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##' @param pred.int prediction interval to use for sub-sampling predicted locations
##' (default = 6 h)
##'
##' @examples
##'
##' @importFrom dplyr filter rename mutate select inner_join
##' @importFrom stringr str_replace
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @export

wc_write_csv <- function(wc_ssm,
                        fit,
                        what,
                        meta,
                        program = "atn",
                        path = NULL,
                        dropIDs = NULL,
                        suffix = "_nrt",
                        pred.int = 6) {


  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## SSM predictions
  ssm_out <- wc_ssm_outputs(fit = fit,
                            what = what,
                            dropIDs = dropIDs,
                            suffix = suffix,
                            pred.int = pred.int)

  ## Metadata
  ## If ATN data then append QC variables to metadata
  meta <- meta |> filter(!DeploymentID %in% dropIDs)

  meta <- inner_join(meta, ssm_out$qc_se, by = "DeploymentID") |>
    select(-dive_start, -dive_end) |>
    rename(QCStartDateTime = qc_start_date, QCStopDateTime = qc_end_date) |>
    mutate(QCMethod = "ArgosQC",
            QCVersion = as.character(packageVersion("ArgosQC"))) |>
    rename(QCproj4string = proj4string)

  now <- Sys.time()
  attr(now, "tzone") <- "UTC"
  meta <- meta |>
    mutate(QCDateTime = now)


  wc.fnms <- names(wc_ssm)

  out <- lapply(1:length(wc_ssm), function(i) {
    wc_write_datafile(wc_ssm = wc_ssm[[i]],
                      meta = meta,
                      program = program,
                      path = path,
                      dropIDs = dropIDs,
                      suffix = suffix)
  })

  names(out) <- wc.fnms

  ## SSM predictions
  out$ssmoutputs <- wc_write_ssm(
    locs_out = ssm_out$locs_out,
    meta = meta,
    program = program,
    path = path,
    dropIDs = dropIDs,
    suffix = suffix
  )

  ## QC-annotated Metadata
  out$metadata <- meta

  nms <- names(out)


  ## write tables to .csv files
  lapply(1:length(out), function(i) {
    out[[i]] |>
      group_by(AnimalAphiaID, ADRProjectID) |>
      group_split() |>
      walk( ~ suppressMessages(write_csv(
        .x,
        file = paste0(
          file.path(path, nms[i]),
          "_",
          .x$AnimalAphiaID[1],
          "_",
          str_replace(.x$ADRProjectID[1], "\\ ", ""),
          suffix,
          ".csv"
        )
      )))
  })


  return(invisible())

}
