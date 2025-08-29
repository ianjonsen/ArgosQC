##' @title Download SMRU or Wildlife Computers tag data from their servers
##'
##' @description Satellite tracking data are accessed from the SMRU data server,
##' or accessed from the Wildlife Computers Portal API via the `source` argument.
##'
##' @param dest destination path to save download
##' @param source source type of data to be downloaded. Can be one of:
##' * `smru` - SMRU Data Server;
##' * `wc` - Wildlife Computers Data Portal API;
##' @param cid SMRU tag deployment campaign id(s) to download, eg. "ct180"
##' @param user SMRU data server username as a quoted string
##' @param pwd SMRU data server password as a quoted string
##' @param wc.akey an Access Key issued by Wildlife Computers for their API
##' @param wc.skey a Secret Key issued by Wildlife Computers for their API
##' @param owner.id the Wildlife Computers uuid associated with the data owner
##' @param subset.ids a single column .CSV file of WC UUID's to be included in
##' the QC, with uuid as the variable name.
##' @param download (logical) indicating if the data is to be downloaded from
##' the tag manufacturer's server. If the source is `wc` and `download = FALSE`
##' then only the Wildlife Computers tag deployment metadata is downloaded.
##' @param ... additional arguments passed to `smru_get_mdb` or `wc_get_files()`
##'
##' @importFrom utils unzip
##' @importFrom usethis create_download_url
##'
##' @export

download_data <- function(dest = NULL,
                     source = "smru",
                     cid = NULL,
                     user = NULL,
                     pwd = NULL,
                     wc.akey = "VVeerW+G6YUe7olzlrOr6q5o2Nkjx5PTEwuwElcPyKc=",
                     wc.skey = "7k9MupziDacYNur/3IPMDjn7wum6oQk5eV2LRj86iDw=",
                     owner.id = NULL,
                     subset.ids = NULL,
                     download = TRUE,
                     ...) {

  source <- match.arg(source, choices = c("smru", "wc"))

  if(is.null(dest)) {

    dest <- tempdir()
    message(paste0("No 'dest' directory specified, so downloading data to ", dest))

  } else {

    if(!dir.exists(dest))  {
      message(paste0("'dest' file path: ", dest, " doesn't exist, creating now"))
      dir.create(dest)
    }
  }

  if(source == "smru") {

    smru_get_mdb(dest = dest, cid = cid, user = user, pwd = pwd, ...)

  } else if(source == "wc") {

    if(any(is.null(wc.akey), is.null(wc.skey)))
      stop("Valid 'wc.akey' and 'wc.skey' must be supplied to access the Wildlife Computers Portal API")

    meta <- wc_get_files(dest = dest,
                 a.key = wc.akey,
                 s.key = wc.skey,
                 owner.id = owner.id,
                 subset.ids,
                 ...,
                 download = download,
                 return.tag.meta = TRUE)

  }

  if(exists("meta")) return(meta)
}
