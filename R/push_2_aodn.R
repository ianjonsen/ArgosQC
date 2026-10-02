##' @title Zip QC'd .csv files for transfer to AODN
##'
##' @description Zips the QC'd .csv files of each campaign into
##' `<cid><suffix>.zip` in `path`, then deletes the .csv files. The zip files are
##' transferred to AODN by sftp outside this function.
##'
##' @param cids campaign ids to zip
##' @param path path to the QC'd .csv files
##' @param user no longer used (formerly the rsync username); ignored
##' @param host no longer used (formerly the rsync server); ignored
##' @param dest no longer used (formerly the rsync destination); ignored
##' @param pwd no longer used (formerly the rsync password); ignored
##' @param nopush must be `TRUE`; transfer to AODN is done by sftp outside this
##' function
##' @param suffix suffix to add to zip files (_nrt or _dm)
##'
##' @importFrom dplyr %>%
##' @importFrom purrr walk
##' @importFrom assertthat assert_that
##'
##' @keywords internal

push_2_aodn <- function(cids,
                        path = NULL,
                        user = NULL,
                        host = NULL,
                        dest = NULL,
                        pwd = NULL,
                        nopush = TRUE,
                        suffix = "_nrt") {

  assert_that(!is.null(path))

  if (!nopush) {
    stop("push_2_aodn() only zips files; transfer them to AODN by sftp", call. = FALSE)
  }

  ## zip files by cid
  cids %>% walk( ~ system(paste0("zip -j ", file.path(path, .x), suffix, ".zip ",
                               file.path(path, "*_"), .x, suffix, ".csv")))

  ## clean up
  system(paste0("rm ", file.path(path, "*"), suffix, ".csv"))

}
