##' @title Generic wrapper to download data from arbitrary sources
##'
##' @description SMRU SRDL-CTD data are downloaded from a user-specified URL
##' (which can include files on GoogleDrive or Dropbox), accessed from the SMRU
##' data server by calling `get_smru_mdb()`, or accessed from the Wildlife
##' Computers Portal API.
##'
##' @param url url for data
##' @param dest destination path to save download
##' @param source source type of data to be downloaded. Can be one of:
##' `smru` - SMRU Data Server; `wc` - Wildlife Computers Data Portal API;
##' `googledrive` - a url link to a zipfile shared on a GoogleDrive;
##' `dropbox` - a url link to a zipfile shared on Dropbox;
##' `url` - a url link to a zipfile on a generic server.
##' @param WC_AccessKey an Access Key issued by Wildlife Computers for their API
##' @param WC_SecretKey a Secret Key issued by Wildlife Computers for their API
##' @param ... additional arguments passed to `get_smru_mdb`
##'
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
                     WC_AccessKey = "VVeerW+G6YUe7olzlrOr6q5o2Nkjx5PTEwuwElcPyKc=",
                     WC_SecretKey = "7k9MupziDacYNur/3IPMDjn7wum6oQk5eV2LRj86iDw=",
                     ...) {

  source <- match.arg(source, choices = c("smru","wc", "googledrive","dropbox","url"))

  if(is.null(dest)) dest <- tempdir()

  if(!source %in% c("smru","wc")) {
    if(source != "url") url <- create_download_url(url)

    dwnld <- tidy_download(url, dest = tempdir())

    dd <- file.path(getwd(), "dwnld")
    if(!dir.exists(dd)) dir.create(dd)
    out <- unzip(dwnld, exdir = dd)

  } else if(source == "smru") {

    get_smru_mdb(dest = dest, ...)

  } else if(source == "wc") {

    if(any(is.null(WC_AccessKey), is.null(WC_SecretKey))) stop("A Wildlife Computers API Access Key & Secret Key are required to download data")

    req <- request('https://my.wildlifecomputers.com/services/')

    ## get My collaborators as a data.frame
    hash <- sha256("action=get_collaborators", key = WC_SecretKey)

    xml <- req |> req_headers(`X-Access` = WC_AccessKey,
                              `X-Hash` = hash) |>
      req_body_form(action = "get_collaborators") |>
      req_perform() |>
      resp_body_xml() |>
      xmlParse() |>
      xmlRoot()

    collabs <- xmlToDataFrame(nodes = getNodeSet(xml, "//collaborator"))

    # use collaborator ID(s) to get list of deployments as a data.frame
    deps <- lapply(1:nrow(collabs), function(i) {
      hash <- sha256(paste0("action=get_deployments&owner_id=", collabs$id[i]),
                     key = WC_SecretKey)
      xml <- req |>
        req_headers(`X-Access` = WC_AccessKey,
                    `X-Hash` = hash) |>
        req_body_raw(paste0("action=get_deployments&owner_id=", collabs$id[i])) |>
        req_perform() |>
        resp_body_xml() |>
        xmlParse() |>
        xmlRoot()

      xmlToDataFrame(nodes = getNodeSet(xml, "//deployment")) |>
        dplyr::select(id, owner, status, tag, last_update_date)
    }) |>
      dplyr::bind_rows() |>
      tidyr::drop_na() |>
      dplyr::mutate(last_update_date = as.POSIXct(as.numeric(last_update_date,
                                                             origin = "1970-01-01",
                                                             tz = "GMT")))

    ## Download data for all deployments as zipfiles
    lapply(1:nrow(deps), function(i) {
      hash <- sha256(paste0("action=download_deployment&id=", deps$id[i]),
                     key = WC_SecretKey)

      req |>
        req_headers(`X-Access` = WC_AccessKey,
                    `X-Hash` = hash) |>
        req_body_raw(paste0("action=download_deployment&id=", deps$id[i])) |>
        req_perform(path = tempfile(tmpdir = "data", fileext = ".zip"))

      system(paste0("unzip -o ",
                    file.path("data", list.files("data", pattern = "*.zip")),
                    paste0(" -d data/", deps$tag[i], "_", i)))
      system(paste0("rm ", file.path("data", list.files("data", pattern = "*.zip"))))
    })

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
