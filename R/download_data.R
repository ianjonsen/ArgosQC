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
##' @param cids SMRU tag deployment campaign id(s) to download, eg. "ct180"
##' @param user SMRU data server username as a quoted string
##' @param pwd SMRU data server password as a quoted string
##' @param WC_AccessKey an Access Key issued by Wildlife Computers for their API
##' @param WC_SecretKey a Secret Key issued by Wildlife Computers for their API
##' @param ... additional arguments passed to `get_smru_mdb` or `get_wc_files()`
##'
##' @md
##' @examples
##'# download a SMRU .mdb file from GitHub
##'download_data(url = "https://github.com/ocean-tracking-network/rt-sat-to-obis/tree/c1d93742d3d996a0436315563fae9b51d7a6e3fd/input/ATN/ct169-594-21",
##'dest = tempdir(),
##'source = "github")
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom tidyr drop_na
##' @importFrom utils unzip
##' @importFrom usethis create_download_url
##' @importFrom openssl sha256
##' @importFrom httr2 request req_headers req_body_raw req_perform req_body_form
##' @importFrom httr2 resp_body_xml
##' @importFrom XML xmlParse xmlRoot xmlToDataFrame getNodeSet
##'
##' @export

download_data <- function(path = NULL,
                     dest = NULL,
                     source = "smru",
                     unzip = FALSE,
                     cids = NULL,
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

    tidy_download(url, dest = dest)

    if(unzip)
      unzip(dest, exdir = dest)

  } else if(source == "smru") {

    smru_get_mdb(dest = dest, cids = cids, user = user, pwd = pwd, ...)

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
##' @importFrom fs file_temp file_move path
##'
##' @internal
tidy_download <- function (url, destdir = getwd())
{
  options(usethis.quiet = TRUE)
  usethis:::check_path_is_directory(destdir)
  tmp <- file_temp("tidy-download-")
  h <- usethis:::download_url(url, destfile = tmp)
  #  cli::cat_line()
  cd <- usethis:::content_disposition(h)
  base_name <- usethis:::make_filename(cd, fallback = fs::path_file(url))
  full_path <- path(destdir, base_name)

  attr(full_path, "content-type") <- usethis:::content_type(h)
  attr(full_path, "content-disposition") <- cd
  file_move(tmp, full_path)
  invisible(full_path)
}
