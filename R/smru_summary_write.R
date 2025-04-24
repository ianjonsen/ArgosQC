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

smru_summary_write <- function(smru_ssm,
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
