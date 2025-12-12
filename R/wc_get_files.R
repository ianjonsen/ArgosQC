##' @title WC API access to tag data files
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
##' @param subset.ids a single column .CSV file of WC UUID's to be included in
##' the QC, with uuid as the variable name.
##' @param collaborator (logical) should data owned by collaborators be
##' downloaded. Ignored if `owner.id` provided.
##' @param unzip (logical) should deployment zipfile be unzipped into destination
##' directory.
##' @param download (logical) should the tag data files be download from the WC Data portal.
##' If FALSE then only the WC tag metadata is downloaded.
##' @param return.tag.meta (logical) should tag metadata constructed from WC dataset
##' information be returned
##'
##' @importFrom dplyr select mutate bind_rows bind_cols
##' @importFrom assertthat assert_that
##' @importFrom testthat is_testing
##' @importFrom tidyr drop_na
##' @importFrom utils unzip
##' @importFrom openssl sha256
##' @importFrom httr2 request req_headers req_body_raw req_perform req_body_form
##' @importFrom httr2 resp_body_xml
##' @importFrom XML xmlParse xmlRoot xmlToDataFrame getNodeSet
##'
##' @keywords internal

wc_get_files <- function(dest = NULL,
                         a.key = NULL,
                         s.key = NULL,
                         owner.id = NULL,
                         subset.ids = NULL,
                         collaborator = TRUE,
                         unzip = TRUE,
                         verbose = FALSE,
                         download = TRUE,
                         return.tag.meta = FALSE) {

  assert_that(!is.null(a.key), msg = "A valid wc.akey (Access Key) must be provided when downloading data from Wildlife Computers")
  assert_that(!is.null(s.key), msg = "A valid wc.skey (Secret Key) must be provided when downloading data from Wildlife Computers")

  if (all(is.null(owner.id), !collaborator))
    stop("Either a WC owner id must be specified or the collaborator argument must be set to TRUE")

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

    argos <- xmlToDataFrame(nodes = getNodeSet(xml, "//argos")) |>
      rename(sattag_program = program_number,
             ptt = ptt_decimal)

    deps <- bind_cols(deps, argos) |>
      drop_na(last_location)

    last_loc <- xmlToDataFrame(nodes = getNodeSet(xml, "//last_location")) |>
      rename(last_loc_date = location_date,
             last_loc_lon = longitude,
             last_loc_lat = latitude)

    deps <- bind_cols(deps, last_loc) |>
      mutate(deploy.na = is.na(deployment)) |>
      mutate(tag.na = is.na(tag))

    deploy <- xmlToDataFrame(nodes = getNodeSet(xml, "//start"), homogeneous = TRUE)
    names(deploy) <- c("deploy_date", "deploy_lat", "deploy_lon")
    deploy <- deploy |>
      mutate(id = deps$id[!deps$deploy.na])

    ## combine & transform dates to POSIXt
    deps <- left_join(deps, deploy, by = "id") |>
      mutate(last_update_date = as.POSIXct(as.numeric(last_update_date), origin = "1970-01-01", tz = "UTC")) |>
      mutate(first_uplink_date = as.POSIXct(as.numeric(first_uplink_date), origin = "1970-01-01", tz = "UTC")) |>
      mutate(last_uplink_date = as.POSIXct(as.numeric(last_uplink_date), origin = "1970-01-01", tz = "UTC")) |>
      mutate(last_loc_date = as.POSIXct(as.numeric(last_loc_date), origin = "1970-01-01", tz = "UTC")) |>
      mutate(deploy_date = as.POSIXct(as.numeric(deploy_date), origin = "1970-01-01", tz = "UTC"))

    ## select final variables
    deps <- deps |>
      select(
        id,
        owner,
        sattag_program,
        ptt,
        tag,
        deploy_date,
        deploy_lon,
        deploy_lat,
        first_uplink_date,
        last_uplink_date,
        last_loc_date,
        last_loc_lon,
        last_loc_lat,
        last_update_date
      )

      if(!is.null(subset.ids)) {
        ids <- read_csv(subset.ids, col_types = "c") |>
          suppressMessages()
        if(names(ids) != "uuid" | length(names(ids)) != 1) stop("Variable name for the WC ID's to QC'd must be 'uuid'")

        deps <- deps |>
          filter(id %in% ids$uuid)

      } else {
        ids <- NULL
        message("tag.list not provided in config file, downloading all accessible data...")
      }

    ## check for duplicate ptt id's & return tag metadata and stop process with
    ##    message
    if(any(duplicated(deps$ptt))) {
      message("Duplicate PTT ids detected. WC tag metadata has been written to a logfile.\n Review and add UUIDs to be QC'd in the config file 'tag.list'")
      write_csv(deps, "QC_logfile.csv")
      stop("Stopping QC process", call. = FALSE)
    }
    ## drop datasets missing tag serial number
    # deps <- deps |>
    #   filter(!is.na(tag))

    ## keep datasets with deployments > 5 days
    # deps <- deps |>
    #   filter(as.numeric(difftime(last_loc_date, deploy_date, units = "days")) > 5)

  }

  ## Download data for all deployments as zipfiles
  if (download) {
    lapply(1:nrow(deps), function(i) {
      hash <- sha256(paste0("action=download_deployment&id=", deps$id[i]), key = s.key)

      req |>
        req_headers(`X-Access` = a.key, `X-Hash` = hash) |>
        req_body_raw(paste0("action=download_deployment&id=", deps$id[i])) |>
        req_perform(path = file.path(dest, paste0(deps$id[i], "_", deps$tag[i], ".zip")))

      if (unzip) {
        ## sub-setting needed here to remove NA_NA.zip downloads when they exist
        fs <- file.path(dest, list.files(dest, pattern = "*.zip"))
        idx.drop <- grep("NA|\\_NA.zip", fs)
        if(length(idx.drop) > 0) fs.d <- fs[-idx.drop]

        unzip(zipfile = ifelse(exists("fs.d"), fs.d, fs),
              exdir = str_split(fs, "\\.z", simplify = TRUE)[, 1])

        if(exists("fs.d")) system(paste0("rm ", fs.d))
        system(paste0("rm ", fs))
      }

    })
  }

  if(return.tag.meta) return(deps)
}
