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
##' @param dropIDs individual WC DeploymentID's to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##' @param pred.int prediction interval to use for sub-sampling predicted locations
##' (default = 6 h)
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
    select(-QC_start_date, -QC_end_date) |>
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
    if (program == "atn") {
      if (all(c("TagID", "TagModel") %in% names(out[[i]]))) {
        tmp <- out[[i]] |>
          select(-TagID, -TagModel)
      } else {
        tmp <- out[[i]]
      }
      tmp |>
        group_by(AnimalAphiaID, ADRProjectID) |>
        group_split() |>
        walk(~ suppressMessages(write_csv(
          .x |> select(-AnimalAphiaID, -ADRProjectID),
          file = paste0(
            file.path(path, nms[i]),
            "_",
            .x$AnimalAphiaID[1],
            "_",
            .x$ADRProjectID[1],
            suffix,
            ".csv"
          )
        )))

    } else if(program != "atn") {
      out[[i]] |>
#        mutate(DeploymentID = irapID) |>
        group_by(common_name) |>
        group_split() |>
        walk( ~ suppressMessages(write_csv(
          .x,
          file = paste0(
            file.path(path, nms[i]),
            "_",
            .x$common_name[1],
            suffix,
            ".csv"
          )
        )))

    }

  })


  return(invisible())

}



##' @title WC generic datafile write to .csv
##'
##' @description Write WC generic datafile to .csv - format specific to ATN program
##'
##' @param wc_ssm SSM-appended WC tag datafile - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, `atn` or
##' `irap`.
##' @param path path to write .csv files
##' @param dropIDs individual ids to be dropped
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

wc_write_datafile <- function(wc_ssm,
                              meta,
                              program = "atn",
                              path = NULL,
                              dropIDs = NULL,
                              suffix = "_nrt") {
  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## file
  tmp <- wc_ssm |>
    filter(!DeploymentID %in% dropIDs) |>
    filter(!is.na(DeploymentID))

if(program == "atn") {
  out <- inner_join(tmp,
                    meta |> select(DeploymentID, TagID, TagModel, AnimalAphiaID, ADRProjectID),
                    by = "DeploymentID")

} else if(program != "atn") {
  out <- inner_join(tmp,
                    meta |> select(DeploymentID, common_name),
                    by = "DeploymentID")
}

  return(out)
}



##' @title ssmoutputs table write to .csv
##'
##' @description write to .csv - format depends on program (IMOS, ATN)
##'
##' @param locs_out SSM-appended WC tag datafile - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param dropIDs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select left_join
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @keywords internal

wc_write_ssm <- function(locs_out,
                         meta,
                         program = "atn",
                         path = NULL,
                         dropIDs = NULL,
                         suffix = "_nrt") {


  locs_out <- locs_out |>
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

  if (suffix != "_nrt" & "keep" %in% names(locs_out)) {
    ## cut predicted locs from large data gaps
    locs_out <- locs_out |>
      filter(keep) |>
      select(-keep)

  }

  if(program == "atn") {
    locs_out <- left_join(
      locs_out,
      meta |> select(DeploymentID, AnimalAphiaID, ADRProjectID),
      by = "DeploymentID")

  } else if(program != "atn") {
    locs_out <- left_join(
      locs_out,
      meta |> select(DeploymentID, common_name),
      by = "DeploymentID")
  }



  return(locs_out)
}



##' @title WC Locations datafile write to .csv
##'
##' @description Write WC Locations datafile to .csv - format specific to ATN program
##'
##' @param wc_ssm SSM-appended WC tag datafile - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, only `atn`.
##' @param path path to write .csv files
##' @param dropIDs individual ids to be dropped
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

wc_write_locations <- function(wc_ssm,
                               meta,
                               program = "atn",
                               path = NULL,
                               dropIDs = NULL,
                               suffix = "_nrt") {
  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))


  ## Argos data
  locs <- wc_ssm$Locations |>
    filter(!DeploymentID %in% dropIDs) |>
    filter(!is.na(DeploymentID))

  if(program == "atn") {
    locs <- left_join(locs,
                      meta |> select(DeploymentID, AnimalAphiaID, ADRProjectID),
                      by = "DeploymentID")

  } else if(program != "atn") {
    locs <- left_join(locs,
                      meta |> select(DeploymentID, common_name),
                      by = "DeploymentID")

  }


  return(locs)
}



