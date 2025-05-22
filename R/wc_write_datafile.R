##' @title WC generic datafile write to .csv
##'
##' @description Write WC generic datafile to .csv - format specific to ATN program
##'
##' @param smru_ssm SSM-appended WC tag datafile - output of \code{append_ssm}
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

  out <- inner_join(tmp,
                    meta |> select(DeploymentID, TagID, TagModel, AnimalAphiaID, ADRProjectID),
                    by = "DeploymentID")

  return(out)
}
