##' @title WC API access to tag dataset UUID's
##'
##' @description Download tag dataset UUID's via the Wildlife Computers Portal API.
##' User-supplied WC Access Key, WC Secret Key.
##'
##' @param a.key an Access Key issued by Wildlife Computers for their API
##' @param s.key a Secret Key issued by Wildlife Computers for their API
##' @param owner.id a WC data owner ID
##'
##' @details Returns a data.frame with WC tag dataset UUID's (id), data owner's
##' email address (owner), tag data status (status), tag type (tag),
##' & last update date of tag data (last_update_date)
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
##' @keywords internal

wc_get_uuids <- function(a.key = NULL,
                              s.key = NULL,
                              owner.id = NULL,
                              verbose = FALSE) {

  assert_that(!is.null(a.key), msg = "A valid wc.akey (Access Key) must be provided when downloading data from Wildlife Computers")
  assert_that(!is.null(s.key), msg = "A valid wc.skey (Secret Key) must be provided when downloading data from Wildlife Computers")

  if (any(is.null(a.key), is.null(s.key)))
    stop("A Wildlife Computers API Access Key & Secret Key are required to download data")

  req <- request('https://my.wildlifecomputers.com/services/')


  if (is.null(owner.id)) {
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
      mutate(last_update_date = as.POSIXct(as.numeric(last_update_date, origin = "1970-01-01", tz = "GMT")
      ))

  } else if(!is.null(owner.id)) {

    hash <- sha256(paste0("action=get_deployments&owner_id=", owner.id),
                   key = s.key)
    xml <- req |>
      req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
      req_body_raw(paste0("action=get_deployments&owner_id=", owner.id)) |>
      req_perform() |>
      resp_body_xml() |>
      xmlParse() |>
      xmlRoot()

    ## Parse XML and extract required metadata
    deps <- xmlToDataFrame(nodes = getNodeSet(xml, "//deployment")) |>
      drop_na(id, argos) |>
      select(id, owner, status, tag, argos, deployment, last_update_date, last_location)

  }

  return(deps)
}
