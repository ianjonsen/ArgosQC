##' @title Download SMRU or Wildlife Computers tag data from arbitrary sources
##'
##' @description Satellite tracking data, in a zipfile, are downloaded from a
##' user-specified URL (for accessing zipfiles on GoogleDrive or Dropbox),
##' accessed from the SMRU data server, or accessed from the Wildlife Computers
##' Portal API via the `source` argument.
##'
##' @param path a url or local filepath pointing to a zipfile of tag datafiles
##' @param dest destination path to save download
##' @param source source type of data to be downloaded. Can be one of:
##' * `smru` - SMRU Data Server;
##' * `wc` - Wildlife Computers Data Portal API;
##' * `googledrive` - a url link to a zipfile shared on a GoogleDrive;
##' * `dropbox` - a url link to a zipfile shared on Dropbox;
##' * `local` - a local filepath to a zipfile containing SMRU or WC tag datafiles.
##' @param unzip (logical) should the downloaded zipfile be unzipped.
##' @param cid SMRU tag deployment campaign id(s) to download, eg. "ct180"
##' @param user SMRU data server username as a quoted string
##' @param pwd SMRU data server password as a quoted string
##' @param wc.akey an Access Key issued by Wildlife Computers for their API
##' @param wc.skey a Secret Key issued by Wildlife Computers for their API
##' @param ... additional arguments passed to `smru_get_mdb` or `wc_get_files()`
##'
##' @importFrom utils unzip
##' @importFrom usethis create_download_url
##'
##' @export

download_data <- function(path = NULL,
                     dest = NULL,
                     source = "smru",
                     unzip = FALSE,
                     cid = NULL,
                     user = NULL,
                     pwd = NULL,
                     wc.akey = "VVeerW+G6YUe7olzlrOr6q5o2Nkjx5PTEwuwElcPyKc=",
                     wc.skey = "7k9MupziDacYNur/3IPMDjn7wum6oQk5eV2LRj86iDw=",
                     ...) {

  source <- match.arg(source, choices = c("smru", "wc", "googledrive", "dropbox", "local"))
  if(all(source == "local", (is.null(path) | is.null(dest)))) stop("A filepath containing a zipfile name must be provided")


  if(is.null(dest)) {

    dest <- tempdir()
    message(paste0("No 'dest' directory specified, so downloading data to ", dest))

  } else {

    if(!dir.exists(dest))  {
      message(paste0("'dest' file path: ", dest, " doesn't exist, creating now"))
      dir.create(dest)
    }
  }

  if(!source %in% c("smru", "wc", "local")) {
    url <- create_download_url(path)

    tidy_download(url, destdir = dest)

    if(unzip)
      unzip(dest, exdir = dest)

  } else if(source == "smru") {

    smru_get_mdb(dest = dest, cid = cid, user = user, pwd = pwd, ...)

  } else if(source == "wc") {

    if(any(is.null(wc.akey), is.null(wc.skey)))
      stop("Valid 'wc.akey' and 'wc.skey' must be supplied to download from the Wildlife Computers Portal")
    meta <- wc_get_files(dest = dest,
                 a.key = wc.akey,
                 s.key = wc.skey,
                 ...)

  } else if(source == "local") {

    unzip(path, exdir = dest, setTimes = TRUE)
  }

  if(exists("meta")) return(meta)
}


##' @title download helper fn, borrowed from usethis:::tidy_download - without the interactivity
##'
##' @description helper fn
##'
##' @param url url for data
##' @param destdir destination path to save download
##'
##' @importFrom fs file_temp file_move path path_file
##'
##' @keywords internal
tidy_download <- function (url, destdir = getwd())
{
  options(usethis.quiet = TRUE)
  usethis:::check_path_is_directory(destdir)
  tmp <- file_temp("tidy-download-")
  h <- usethis:::download_url(url, destfile = tmp)
  #  cli::cat_line()
  cd <- usethis:::content_disposition(h)
  base_name <- usethis:::make_filename(cd, fallback = path_file(url))
  full_path <- path(destdir, base_name)

  attr(full_path, "content-type") <- usethis:::content_type(h)
  attr(full_path, "content-disposition") <- cd
  file_move(tmp, full_path)
  invisible(full_path)
}
