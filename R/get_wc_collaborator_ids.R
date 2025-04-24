##' @title WC API access to collaborator ids
##'
##' @description Download collaborator ids via the Wildlife Computers Portal API.
##' User-supplied WC Access Key, WC Secret Key.
##'
##' @param a.key an Access Key issued by Wildlife Computers for their API
##' @param s.key a Secret Key issued by Wildlife Computers for their API
##'
##' @details Returns a data.frame with WC ids and email addresses of collaborators
##'
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

get_wc_collab_ids <- function(a.key = NULL,
                              s.key = NULL,
                              verbose = FALSE) {

  assert_that(!is.null(a.key), msg = "A valid wc.akey (Access Key) must be provided when downloading data from Wildlife Computers")
  assert_that(!is.null(s.key), msg = "A valid wc.skey (Secret Key) must be provided when downloading data from Wildlife Computers")

  if (any(is.null(a.key), is.null(s.key)))
    stop("A Wildlife Computers API Access Key & Secret Key are required to download data")

  req <- request('https://my.wildlifecomputers.com/services/')


  ## get My collaborators as a data.frame
  hash <- sha256("action=get_collaborators", key = s.key)

  xml <- req |> req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
    req_body_form(action = "get_collaborators") |>
    req_perform() |>
    resp_body_xml() |>
    xmlParse() |>
    xmlRoot()

  ids <- xmlToDataFrame(nodes = getNodeSet(xml, "//collaborator"))

  return(ids)

}
