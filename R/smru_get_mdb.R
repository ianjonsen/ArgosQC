##' @title Download .mdb files from the SMRU server
##'
##' @description fetches .mdb files from SMRU server and saves to a \code{dest}-ination directory
##'
##' @param cids SMRU campaign ids to be downloaded from SMRU data server
##' @param dest destination path for saving .mdb files
##' @param user SMRU data server username as a string
##' @param pwd  SMRU data server password as a string
##' @param timeout timeout duration in s (default 120 s); sets
##' options(timeout = timeout) to avoid mdb.zip download error from SMRU server
##' @param verbose turn on/off progress indicator
##'
##' @importFrom stringr str_split
##' @importFrom tibble as_tibble
##' @importFrom utils download.file unzip
##' @importFrom assertthat assert_that
##' @importFrom purrr walk
##'
##' @keywords internal

smru_get_mdb <-
  function(cids,
           dest = NULL,
           user = NULL,
           pwd = NULL,
           timeout = 120,
           verbose = FALSE)
  {
    assert_that(!is.null(dest))
    assert_that(dir.exists(dest))
    assert_that(!is.null(user))
    assert_that(!is.null(pwd))

    ## to help overcome timeouts when downloading from SMRU server
    options(timeout = timeout)
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
          "@www.smru.st-andrews.ac.uk/protected/",
          cid,
          "/db/",
          cid,
          ".zip"
        ),
        destfile = file.path(dest, paste0(cid, ".zip")),
        method = "libcurl",
        quiet = !verbose,
        mode = "w",
        cacheOK = FALSE
      )

      suppressWarnings(unzip(file.path(dest, paste0(cid, ".zip")), exdir = file.path(dest, ".")))
      system(paste0("rm ", file.path(dest, paste0(cid, ".zip"))))
    }

      out <- cids %>% walk(~ fn(
          .x,
          dest = dest,
          user = user,
          pwd = pwd
        ),
        .progress = verbose)

      options(timeout = 60)

      return(out)

  }
