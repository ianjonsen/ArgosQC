##' @title sync to AODN
##'
##' @description zip .csv files by campaign id & sync with AODN incoming server via rsync
##'
##' @param cids campaign ids to zip and sync
##' @param path path to write .csv files
##' @param user AODN incoming server username as a string
##' @param pwd AODN incoming server pwd as a string
##' @param nopush zip files but don't push to AODN incoming server (for testing)
##' @param suffix suffix to add to zip files (_nrt or _dm)
##'
##' @examples
##'
##' @importFrom dplyr "%>%"
##' @importFrom purrr walk
##' @importFrom assertthat assert_that
##'
##' @export

push_2_aodn <- function(cids, path = NULL, user = NULL, host = NULL, dest = NULL, pwd = NULL, nopush = TRUE, suffix = "_nrt") {

  assert_that(!is.null(path))
##  assert_that(!is.null(user))
  assert_that(!is.null(pwd))

  write(pwd, file=paste0(path, "/rsync_pass"))
  system(paste0("chmod 600 ", path, "/rsync_pass"))

  rsync.fn <- function(user, host, dest, file, pwd_file) {
    system(paste0("rsync -auv --password-file=", pwd_file, " ", file, user, "@", host, "::", dest))
  }

  ## zip files by cid
  cids %>% walk( ~ system(paste0("zip -j ", file.path(path, .x), suffix, ".zip ",
                               file.path(path, "*_"), .x, suffix, ".csv")))


  ## push zipfiles

  if(!nopush) {
    stop("rsync not currently supported within this function")
  AODNfiles <- system(paste0("ls ", file.path(path, "*.zip | xargs -n 1 basename")), intern = TRUE)

  AODNfiles %>%
    walk( ~ rsync.fn(user = user, host = host, dest = dest, file = .x, pwd_file = paste0(path, "/", pwd.txt)))
  }

  ## clean up
  system(paste0("rm ", file.path(path, "*"), suffix, ".csv"))

}
