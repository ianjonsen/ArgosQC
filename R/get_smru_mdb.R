##' @title Download .mdb files from the SMRU server
##'
##' @description fetches .mdb files from SMRU server and saves to a \code{dest}-ination directory
##'
##' @param cids SMRU campaign ids to be downloaded from SMRU data server
##' @param dest destination path for saving .mdb files
##' @param user SMRU data server username
##' @param pwd  SMRU data server password
##'
##' @examples
##'
##' @importFrom dplyr "%>%" pull
##' @importFrom stringr str_split
##' @importFrom tibble as_tibble
##' @importFrom utils download.file unzip
##' @importFrom assertthat assert_that
##' @importFrom purrr walk
##'
##' @export

get_smru_mdb <-
  function(cids,
           dest = "~/Dropbox/collab/imos/imos_qc/mdb",
           user = "imos",
           pwd = "imos")
  {
    assert_that(dir.exists(dest))

    ## define download fn
    fn <- function(cid,
                   dest = dest,
                   user = user,
                   pwd = pwd) {
      download.file(
        paste0(
          "http://",
          user,
          ":",
          pwd,
          "@www.smru.st-and.ac.uk/protected/",
          cid,
          "/db/",
          cid,
          ".zip"
        ),
        destfile = file.path(dest, paste0(cid, ".zip")),
        method = "libcurl",
        quiet = FALSE,
        mode = "w"
      )

      try(unzip(file.path(dest, paste0(cid, ".zip")), exdir = file.path(dest, ".")), silent = TRUE)
      system(paste0("rm ", file.path(dest, paste0(cid, ".zip"))))
    }

        cids %>% walk(~ fn(
          .x,
          dest = dest,
          user = user,
          pwd = pwd
        ))

  }
