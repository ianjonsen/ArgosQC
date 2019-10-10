##' @title Pull tables from SMRU .mdb files
##'
##' @description extracts specified tables from SMRU .mdb files, using Hmisc::mdb.get
##'
##' @param cids SMRU campaign ids
##' @param path2mdb path to SMRU .mdb file(s)
##' @param tables specify which tables to extract, default is to extract all tables
##'
##' @examples
##'
##' @importFrom dplyr select mutate "%>%"
##' @importFrom future plan
##' @importFrom assertthat assert_that
##' @importFrom Hmisc mdb.get
##' @importFrom furrr future_map
##' @importFrom purrr pmap
##'
##' @export

pull_smru_tables <- function(cids, path2mdb, tables = c("diag","haulout","ctd","dive","summary")) {

  plan("multisession")
  smru_t <- cids %>%
    future_map( ~ try(Hmisc::mdb.get(paste0(file.path(path2mdb, .x), ".mdb"),
                                            tables = tables,
                                            lowernames = TRUE),
                             silent = TRUE), .progress = TRUE
    )
  smru <- smru_t %>% pmap(., rbind)

  return(smru)

}
