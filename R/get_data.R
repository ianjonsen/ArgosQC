##' @title Generic wrapper to download data from arbitrary sources
##'
##' @description SMRU SRDL-CTD data are downloaded from a user-specified URL,
##' or accessed from the SMRU data server by calling `get_smru_mdb()`
##'
##' @param url url for data
##' @param dest destination path for saving data files
##' @param smru (logical) should data be download from SMRU server. Default is
##' TRUE, in which case any user-specified `url` is ignored & `get_smru_mdb()`
##' is called.
##' @param ... additional arguments passed to `get_smru_mdb`
##'
##' @examples
##'
##' @importFrom dplyr %>% pull
##' @importFrom stringr str_split
##' @importFrom tibble as_tibble
##' @importFrom utils download.file unzip
##' @importFrom assertthat assert_that
##' @importFrom purrr walk
##'
##' @export

get_data <- function(url,
                     dest,
                     smru = TRUE,
                     method = "curl",
                     quiet = TRUE,
                     ...) {

  if(!smru) download.file(url, destfile=dest, method = method, quiet = quiet)
  else get_smru_mdb(dest = dest, ...)

}
