##' @title Download SMRU or Wildlife Computers tag data from their servers
##'
##' @description Satellite tracking data are accessed from the SMRU data server,
##' or accessed from the Wildlife Computers Portal API via the `source` argument.
##' Data files are saved to the `data.dir` specified in the JSON config file.
##' SMRU tag data are currently downloaded as a single `.mdb` (Microsoft Access
##' Database) file. Wildlife Computers tag data are downloaded as a series of `.CSV`
##' files saved in tag-specific directories (uniquely named with WC UUID's).
##' Wildlife Computers data, partial deployment metadata are output as an R object.
##'
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
##' @param ... additional arguments passed to `smru_get_mdb()` or `wc_get_files()`
##'
##' @importFrom utils unzip
##' @importFrom usethis create_download_url
##'
##' @examples
##' \dontrun{
##' ## SMRU data download
##'   download_data(
##'     dest = file.path(wd, config$setup$data.dir),
##'     source = "smru",
##'     cid = config$harvest$cid,
##'     user = config$harvest$smru.usr,
##'     pwd = config$harvest$smru.pwd,
##'     timeout = config$harvest$timeout
##'     )
##'
##' ## Wildlife Computers data download & deployment metadata acquisition
##' wc.deploy.meta <- download_data(
##'                     dest = file.path(wd, config$setup$data.dir),
##'                     source = "wc",
##'                     unzip = TRUE,
##'                     wc.akey = config$harvest$wc.akey,
##'                     wc.skey = config$harvest$wc.skey,
##'                     subset.ids = config$harvest$tag.list,
##'                     download = TRUE,
##'                     owner.id = config$harvest$owner.id
##'                     )
##' }
##'
##'
##' @export

download_data <- function(dest = NULL,
                     source = "smru",
                     cid = NULL,
                     user = NULL,
                     pwd = NULL,
                     wc.akey = NULL,
                     wc.skey = NULL,
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
                 subset.ids = subset.ids,
                 ...,
                 download = download,
                 return.tag.meta = TRUE)

  }

  if(exists("meta")) return(meta)
}
