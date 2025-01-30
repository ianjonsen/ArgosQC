##' @title Download SMRU or Wildlife Computers tag data from arbitrary sources
##'
##' @description Satellite tracking data are downloaded from a user-specified URL
##' (which can include files on GoogleDrive or Dropbox), accessed from the SMRU
##' data server, or accessed from the Wildlife Computers Portal API via the
##' `source` argument.
##'
##' @param url url for data
##' @param dest destination path to save download
##' @param source source type of data to be downloaded. Can be one of:
##' * `smru` - SMRU Data Server;
##' * `wc` - Wildlife Computers Data Portal API;
##' * `googledrive` - a url link to a zipfile shared on a GoogleDrive;
##' * `dropbox` - a url link to a zipfile shared on Dropbox;
##' * `url` - a url link to a zipfile on a generic server.
##' @param cids SMRU tag deployment campaign id(s) to download, eg. "ct180"
##' @param user SMRU data server username as a quoted string
##' @param pwd SMRU data server password as a quoted string
##' @param WC_AccessKey an Access Key issued by Wildlife Computers for their API
##' @param WC_SecretKey a Secret Key issued by Wildlife Computers for their API
##' @param ... additional arguments passed to `get_smru_mdb` or `get_wc_files()`
##'
##' @md
##' @examples
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

get_data <- function(url = NULL,
                     dest = NULL,
                     source = "smru",
                     cids = NULL,
                     user = NULL,
                     pwd = NULL,
                     wc.akey = "VVeerW+G6YUe7olzlrOr6q5o2Nkjx5PTEwuwElcPyKc=",
                     wc.skey = "7k9MupziDacYNur/3IPMDjn7wum6oQk5eV2LRj86iDw=",
                     ...) {

  source <- match.arg(source, choices = c("smru","wc", "googledrive","dropbox","url"))

  if(is.null(dest)) {
    dest <- tempdir()
    message(paste0("No 'dest' directory specified, so downloading data to ", dest))
  }

  if(!source %in% c("smru","wc")) {
    if(source != "url") url <- create_download_url(url)

    dwnld <- tidy_download(url, dest = tempdir())

    dd <- file.path(getwd(), "dwnld")
    if(!dir.exists(dd)) dir.create(dd)
    out <- unzip(dwnld, exdir = dd)

  } else if(source == "smru") {

    get_smru_mdb(dest = dest, ...)

  } else if(source == "wc") {
    if(any(is.null(wc.akey), is.null(wc.skey)))
      stop("Valid 'wc.akey' and 'wc.skey' must be supplied to download from the Wildlife Computers Portal")
    get_wc_files(dest = dest,
                 a.key = wc.akey,
                 s.key = wc.skey,
                 ...)

  }


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
  base_name <- usethis:::make_filename(cd, fallback = path_file(url))
  full_path <- path(destdir, base_name)

  attr(full_path, "content-type") <- usethis:::content_type(h)
  attr(full_path, "content-disposition") <- cd
  file_move(tmp, full_path)
  invisible(full_path)
}
