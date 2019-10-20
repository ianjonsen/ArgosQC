##' @title push to AODN
##'
##' @description zip .csv files by campaign id & push to AODN incoming server
##'
##' @param cids campaign ids to zip and push
##' @param path path to write .csv files
##' @param user AODN incoming server username
##' @param pwd AODN incoming server pwd
##'
##' @examples
##'
##' @importFrom dplyr "%>%"
##' @importFrom RCurl ftpUpload
##' @importFrom purrr walk
##'
##' @export

push_2_aodn <- function(cids, path = "~/Dropbox/collab/imos/imos_qc/aodn") {

  ## zip files by cid
  cids %>% walk( ~ system(paste0("zip -j ", file.path(path, .x), ".zip ",
                               file.path(path, "*_"), .x, ".csv")))


  ## push zipfiles

  AODNfiles <- system(paste0("ls ", file.path(path, "*.zip | xargs -n 1 basename")), intern = TRUE)

  AODNfiles %>%
    walk( ~ ftpUpload(what = file.path(path, .x),
                      to = paste0("ftp://", user, ":", pwd, "@incoming.aodn.org.au/AATAMS_SATTAG_DM/",
                                  .x)))
  ## clean up
  system(paste0("rm ", file.path(path, "*"), ".csv"))
}
