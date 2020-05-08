##' @title push to AODN
##'
##' @description zip .csv files by campaign id & push to AODN incoming server
##'
##' @param cids campaign ids to zip and push
##' @param path path to write .csv files
##' @param user AODN incoming server username as a string
##' @param pwd AODN incoming server pwd as a string
##' @param nopush zip files but don't push to AODN incoming server (for testing)
##'
##' @examples
##'
##' @importFrom dplyr "%>%"
##' @importFrom RCurl ftpUpload
##' @importFrom purrr walk
##' @importFrom assertthat assert_that
##'
##' @export

push_2_aodn <- function(cids, path = NULL, user = NULL, pwd = NULL, nopush = TRUE) {

  assert_that(!is.null(path))
  assert_that(!is.null(user))
  assert_that(!is.null(pwd))

  ## zip files by cid
  cids %>% walk( ~ system(paste0("zip -j ", file.path(path, .x), "_nrt.zip ",
                               file.path(path, "*_"), .x, "_nrt.csv")))


  ## push zipfiles

  if(!nopush) {
  AODNfiles <- system(paste0("ls ", file.path(path, "*.zip | xargs -n 1 basename")), intern = TRUE)

  AODNfiles %>%
    walk( ~ ftpUpload(what = file.path(path, .x),
                      to = paste0("ftp://", user, ":", pwd, "@incoming.aodn.org.au/AATAMS_SATTAG_DM/",
                                  .x)))
  ## clean up
  system(paste0("rm ", file.path(path, "*"), "_nrt.csv"))
  }
}
