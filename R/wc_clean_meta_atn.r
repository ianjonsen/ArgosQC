##' @title read & handle ATN metadata from .CSV file
##'
##' @description utility fn for handling fully specified ATN WC tag metadata
##'
##' @param file filepath to metadata .csv file - supplied by `get_metadata()`
##' @param col.types.string string of single character col_types for `col_types`
##' arg in `read_csv`
##' @param ids subset of WC UUID's to consider - supplied by
##' `conf$harvest$tag.list` in `get_metadata()`
##' @param dropIDs SMRU refs or WC ids to be dropped from QC - supplied by
##' `get_metadata()` & provided by `conf$harvest$dropIDs`
##'
##' @importFrom dplyr filter
##' @importFrom readr read_csv
##'
##' @keywords internal
##'


wc_clean_meta_atn <- function(file,
                              col.types.string = c("iiccccccTccicccccTTcddcccicdccdccdcccdcTc"),
                              ids,
                              dropIDs)

{
  atn.meta <- read_csv(file,
                   col_types = col.types.string) |>
    suppressMessages()

  if (!is.null(ids)) {
    atn.meta <- atn.meta |>
      filter(DeploymentID %in% ids$uuid) |>
      filter(!DeploymentID %in% dropIDs)

  } else {
    ## read metadata & subset to ID's in current data to be QC'd
    atn.meta <- atn.meta |>
      filter(!DeploymentID %in% dropIDs)
  }

  return(atn.meta)
}
