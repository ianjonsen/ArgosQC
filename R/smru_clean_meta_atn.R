##' @title read & handle ATN metadata from .CSV file
##'
##' @description utility fn for handling fully specified ATN SMRU tag metadata
##'
##' @param file filepath to metadata .csv file - supplied by `get_metadata()`
##' @param col.types.string string of single character col_types for `col_types`
##' arg in `read_csv`
##' @param cid the SMRU campaign ID - supplied by `conf$harvest$cid`
##' @param dropIDs SMRU refs or WC ids to be dropped from QC - supplied by
##' `conf$harvest$dropIDs` in `get_metadata()`
##'
##' @importFrom dplyr select mutate filter
##' @importFrom readr read_csv
##'
##' @keywords internal
##'


smru_clean_meta_atn <- function(file,
                              col.types.string = c("iiccccccTccicccccTTcddcccicdccdccdcccdcTc"),
                              cid,
                              dropIDs)
{

  atn.meta <- read_csv(file,
                 col_types = col.types.string) |>
  suppressMessages()

## subset to current campaigns & apply drop.refs
atn.meta <- atn.meta |>
  mutate(SMRUCampaignID = str_split(meta$DeploymentID, "\\-", simplify = TRUE)[,1]) |>
  filter(SMRUCampaignID %in% cid) |>
  filter(!DeploymentID %in% dropIDs) |>
  select(-SMRUCampaignID)

return(atn.meta)
}
