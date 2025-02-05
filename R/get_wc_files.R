##' @title WC API access
##'
##' @description Download satellite tracking data and associated tag files via
##' the Wildlife Computers Portal API. User-supplied WC Access Key, WC Secret Key,
##'
##' Satellite tracking data are downloaded from a user-specified URL
##' (which can include files on GoogleDrive or Dropbox), accessed from the SMRU
##' data server by calling `get_smru_mdb()`, or accessed from the Wildlife
##' Computers Portal API by calling `get_wc_files()`.
##'
##' @param a.key an Access Key issued by Wildlife Computers for their API
##' @param s.key a Secret Key issued by Wildlife Computers for their API
##' @param owner.id a WC data owner ID
##' @param collaborator (logical) should data owned by collaborators be
##' downloaded. Ignored if `owner.id` provided.
##' @param unzip (logical) should deployment zipfile be unzipped into destination
##' directory.
##'
##' @examples
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom assertthat assert_that
##' @importFrom tidyr drop_na
##' @importFrom utils unzip
##' @importFrom openssl sha256
##' @importFrom httr2 request req_headers req_body_raw req_perform req_body_form
##' @importFrom httr2 resp_body_xml
##' @importFrom XML xmlParse xmlRoot xmlToDataFrame getNodeSet
##'
##' @internal

get_wc_files <- function(dest = NULL,
                         a.key = NULL,
                         s.key = NULL,
                         owner.id = NULL,
                         collaborator = TRUE,
                         unzip = TRUE,
                         verbose = FALSE) {

  assert_that(!is.null(a.key), msg = "A valid wc.akey (Access Key) must be provided when downloading data from Wildlife Computers")
  assert_that(!is.null(s.key), msg = "A valid wc.skey (Secret Key) must be provided when downloading data from Wildlife Computers")

  if (any(is.null(a.key), is.null(s.key)))
    stop("A Wildlife Computers API Access Key & Secret Key are required to download data")

  if (all(is.null(owner.id), !collaborator))
    stop("Either an WC owner id must be specified or the collaborator argument must be set to TRUE")

  req <- request('https://my.wildlifecomputers.com/services/')


  if (is.null(owner.id) & collaborator) {
    ## get My collaborators as a data.frame
    hash <- sha256("action=get_collaborators", key = s.key)

    xml <- req |> req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
      req_body_form(action = "get_collaborators") |>
      req_perform() |>
      resp_body_xml() |>
      xmlParse() |>
      xmlRoot()

    ids <- xmlToDataFrame(nodes = getNodeSet(xml, "//collaborator"))

    # use collaborator ID(s) to get list of deployments as a data.frame
    deps <- lapply(1:nrow(ids), function(i) {
      hash <- sha256(paste0("action=get_deployments&owner_id=", ids$id[i]),
                     key = s.key)
      xml <- req |>
        req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
        req_body_raw(paste0("action=get_deployments&owner_id=", ids$id[i])) |>
        req_perform() |>
        resp_body_xml() |>
        xmlParse() |>
        xmlRoot()

      xmlToDataFrame(nodes = getNodeSet(xml, "//deployment")) |>
        dplyr::select(id, owner, status, tag, last_update_date)
    }) |>
      bind_rows() |>
      drop_na() |>
      mutate(last_update_date = as.POSIXct(
        as.numeric(last_update_date, origin = "1970-01-01", tz = "GMT")
      ))

  } else if(!is.null(owner.id)) {

#    stop("Sorry, download via supplied WC data owner.id is not set up yet!")
    hash <- sha256(paste0("action=get_deployments&owner_id=", owner.id),
                   key = s.key)
    xml <- req |>
      req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
      req_body_raw(paste0("action=get_deployments&owner_id=", owner.id)) |>
      req_perform() |>
      resp_body_xml() |>
      xmlParse() |>
      xmlRoot()

    deps <- xmlToDataFrame(nodes = getNodeSet(xml, "//deployment")) |>
      dplyr::select(id, owner, status, tag, last_update_date)

  }

  ## Download data for all deployments as zipfiles
  lapply(1:nrow(deps), function(i) {
    hash <- sha256(paste0("action=download_deployment&id=", deps$id[i]), key = s.key)

    req |>
      req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
      req_body_raw(paste0("action=download_deployment&id=", deps$id[i])) |>
      req_perform(path = file.path(dest, paste0(deps$id[i], "_", deps$tag[i], ".zip")))

    if (unzip) {
      fs <- file.path(dest, list.files(dest, pattern = "*.zip"))
      unzip(zipfile = fs,
            exdir = str_split(list.files(dest,
                                         pattern = "*.zip",
                                         full.names = TRUE),
                              "\\.",
                              simplify = TRUE)[,1])

      system(paste0("rm ", file.path(
        dest, list.files(dest, pattern = "*.zip")
      )))
    }

  })

}
