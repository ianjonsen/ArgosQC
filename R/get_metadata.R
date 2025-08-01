##' @title download deployment metadata
##'
##' @description downloads, restructures & formats metadata, appends
##' dive/CTD start and end datetimes (for QC), & fills in missing required
##' metadata - eg. release_datetime, release_longitude/latitude's with data from
##' the GPS (if present) or Argos location file.
##'
##' @param source the source of the metadata, current options are `imos`, `smru`,
##' `atn`.
##' @param tag_mfr the tag manufacturer, current options are `smru` or `wc`
##' cid SMRU campaign ids
##' @param tag_data a list of either `smru` data tables or `wc` files as output by
##' `pull_data`.
##' @param cid SMRU campaign id must be provided when the tag_mfr is `smru`
##' @param dropIDs SMRU refs or WC ids to be dropped
##' @param file path to metadata .csv file, if provided then metadata will be
##' read from the provided `source`
##' @param meta.args optional metadata fields to be passed from config file when
##' downloading tag metadata from SMRU server.
##' @param subset.ids a character vector of comma-separated (no spaces) WC UUID's
##' to be included in the QC. Ignored if `tag_mfr != "wc"`
##'
##'
##'
##' @importFrom dplyr select rename mutate filter bind_rows
##' @importFrom rvest read_html html_nodes html_table
##' @importFrom stringr str_to_lower str_replace_all
##' @importFrom lubridate mdy_hms round_date
##' @importFrom assertthat assert_that
##' @importFrom readr read_csv cols locale
##' @importFrom vctrs list_drop_empty
##'
##' @export
##'

get_metadata <- function(source = "smru",
                       tag_mfr = "smru",
                       tag_data = NULL,
                       cid = NULL,
                       dropIDs = NULL,
                       file = NULL,
                       meta.args,
                       subset.ids = NULL,
                       wc.meta = NULL) {

  if(is.null(tag_data)) stop("a tag_data object must be supplied")
#  if(all(source == "imos", is.null(file))) stop("an IMOS-ATF .csv metadata file must be provided")
  if(all((tag_mfr == "smru" | source == "smru"), is.null(cid))) stop("'cid' argument is empty, SMRU campaign id(s) must be specified")
  if(all(source == "atn", tag_mfr %in% c("smru", "wc"), is.null(file))) stop("an ATN .csv metadata file must be provided")


  if(tag_mfr == "wc") {
    if(!is.null(subset.ids)) {
      ids <- read_csv(subset.ids) |>
        suppressMessages()
      if(names(ids) != "uuid" | length(names(ids)) != 1) stop("Variable name for the WC ID's to QC'd must be 'uuid'")

    } else {
      ids <- NULL
    }

      switch(source,
             irap = {
               meta <- read_csv(file) |>
                 suppressMessages()

               if(!is.null(ids)) {
                 meta <- meta |>
                   filter(DeploymentID %in% ids$uuid) |>
                   filter(!DeploymentID %in% dropIDs)
               }

               if(!is.null(wc.meta)) {
                 meta <- left_join(meta, wc.meta, by = c("DeploymentID"="id")) |>
                   select(DeploymentID, sattag_program = sattag_program.y,
                          owner, tag_model, tag_serial_number, tag_ptt,
                          deploy_date, deploy_location = release_location,
                          deploy_lon = longitude, deploy_lat = latitude,
                          common_name, curved_carapace_length,
                          curved_carapace_length_unit)
               }

                meta <- meta |>
                  mutate(irapID = paste(sattag_program,
                                              tag_serial_number,
                                              tag_ptt, sep = "_")) |>
                  select(DeploymentID, irapID, everything())

             },
             atn = {
               meta <- read_csv(file,
                                col_types = c("iiccccccTccicccccTTcddcccicdccdccdcccdcTc"))
               if (!is.null(ids)) {
                 meta <- meta |>
                   filter(DeploymentID %in% ids$uuid) |>
                   filter(!DeploymentID %in% dropIDs)

               } else {
                 ## read metadata & subset to ID's in current data to be QC'd
                 meta <- meta |>
                   filter(!DeploymentID %in% dropIDs)
               }
             })

  } else if(tag_mfr == "smru") {

    switch(source,
           atn = {
             meta <- read_csv(file) |> suppressMessages()

             ## subset to current campaigns & apply drop.refs
             meta <- meta |>
               mutate(SMRUCampaignID = str_split(meta$DeploymentID, "\\-", simplify = TRUE)[,1]) |>
               filter(SMRUCampaignID %in% cid) |>
               filter(!DeploymentID %in% dropIDs) |>
               select(-SMRUCampaignID)
           },
           smru = {
             ## download SMRU metadata & generate QC metadata for IMOS-AODN
             tag_meta <- lapply(1:length(cid), function(i) {
               url <- paste0("https://imos:imos@www.smru.st-andrews.ac.uk/protected/", cid[i], "/", cid[i], ".html")
               tm <- url |>
                 read_html() |>
                 html_nodes(xpath = '/html/body/ul[1]/table') |>
                 html_table()
               tm
             }) |> bind_rows()

             names(tag_meta) <- str_to_lower(names(tag_meta)) |>
               str_replace_all(" ", "_") |>
               str_replace_all("-", "_") |>
               str_replace_all("/", "_")

             tag_meta <- tag_meta %>%
               rename(
                 device_id = reference,
                 device_wmo_ref = wmo,
                 tag_type = parameters,
                 body = body
               ) |>
               select(-latest_uplink,
                      -latest_argos_location)

             tmp <- split(tag_data$diag, tag_data$diag$ref)
             meta_loc <- lapply(tmp, function(x) {
               with(subset(x, d_date <= unique(round_date(d_date, unit="day"))[2]),
                    data.frame(ref = ref[1],
                               release_date = d_date[1],
                               release_longitude = mean(lon, na.rm=T),
                               release_latitude = mean(lat, na.rm = T)))
             }) |>
               bind_rows()

             meta <- suppressWarnings(left_join(tag_meta, meta_loc, by = c("device_id"="ref")) |>
                                        mutate(sattag_program = str_split(device_id, "\\-", simplify = TRUE)[,1]) |>
                                        mutate(common_name = meta.args$common_name,
                                               species = meta.args$species) |>
                                        mutate(release_site = meta.args$release_site) |>
                                        mutate(recovery_date = NA,
                                               age_class = NA,
                                               sex = NA,
                                               length = NA,
                                               estimated_mass = NA,
                                               actual_mass = NA) |>
                                        mutate(state_country = meta.args$state_country) |>
                                        select(sattag_program,
                                               device_id,
                                               ptt,
                                               body,
                                               device_wmo_ref,
                                               tag_type,
                                               common_name,
                                               species,
                                               release_longitude,
                                               release_latitude,
                                               release_site,
                                               release_date,
                                               recovery_date,
                                               everything()) |>
                                        mutate(ptt = as.integer(ptt)) |>
                                        mutate(body = as.integer(body)) |>
                                        mutate(recovery_date = as.POSIXct(recovery_date, tz = "UTC")) |>
                                        mutate(age_class = as.character(age_class)) |>
                                        mutate(sex = as.character(sex)) |>
                                        mutate(length = as.numeric(length)) |>
                                        mutate(actual_mass = as.numeric(actual_mass)) |>
                                        mutate(estimated_mass = as.integer(estimated_mass)))

             ## subset to current campaigns & apply drop.refs
             meta <- meta |>
               filter(sattag_program %in% cid) |>
               filter(!device_id %in% dropIDs)
           },
           imos = {
             meta <- smru_clean_imos_meta(cid = cid,
                                          smru = tag_data,
                                          dropIDs = dropIDs,
                                          file = file)
           }
           )


  }

## append dive/ctd start end dates
  if (tag_mfr == "smru") {
    ## append dive start and end dates for (alternate) track truncation
    ##  to be used as alternate on final, delayed-mode (manual) QC
    if ("dive" %in% names(tag_data)) {
      dive_se <- tag_data$dive |>
        mutate(ref = as.character(ref)) |>
        select(ref, de_date, max_dep) |>
        group_by(ref) |>
        summarise(
          dive_start = min(de_date, na.rm = TRUE),
          dive_end = max(de_date, na.rm = TRUE)
        )
    }

    ## append CTD start and end dates for track truncation
    if ("ctd" %in% names(tag_data)) {
      ctd_se <- tag_data$ctd |>
        mutate(ref = as.character(ref)) |>
        select(ref, end_date) |>
        group_by(ref) |>
        summarise(
          ctd_start = min(end_date, na.rm = TRUE),
          ctd_end = max(end_date, na.rm = TRUE)
        )
    }

    meta <- switch(source,
                   atn = {
                     if ("dive" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(dive_se, by = c("DeploymentID" = "ref"))
                     }
                     if ("ctd" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(ctd_se, by = c("DeploymentID" = "ref"))
                     }
                     meta
                   },
                   smru = {
                     if ("dive" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(dive_se, by = c("device_id" = "ref"))
                     }
                     if ("ctd" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(ctd_se, by = c("device_id" = "ref"))
                     }
                     meta
                   },
                   imos = {
                     meta
                   })

    # if(any(meta$common_name %in% "olive ridley turtle")) {
    #   meta <- meta |>
    #     select(-latest_gps) |>
    #     filter(!is.na(release_date), !is.na(ctd_start))
    # }

  } else if(tag_mfr == "wc") {
    tag_data <- vctrs::list_drop_empty(tag_data)

    dive_se <- vector(mode = "list", length = 4)

    if("Histos" %in% names(tag_data)) {
      dive_se[[1]] <- tag_data$Histos |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = min(Date, na.rm = TRUE)
        )
    }

    if("ECDHistos_SCOUT_TEMP_361A" %in% names(tag_data)) {
      dive_se[[2]] <- tag_data$ECDHistos_SCOUT_TEMP_361A |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = min(Date, na.rm = TRUE)
        )
    }

    if("ECDHistos_SCOUT_DSA" %in% names(tag_data)) {
      dive_se[[3]] <- tag_data$ECDHistos_SCOUT_DSA |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Start, na.rm = TRUE),
          dive_end = min(End, na.rm = TRUE)
        )
    }

    if("MinMaxDepth" %in% names(tag_data)) {
      dive_se[[4]] <- tag_data$MinMaxDepth |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = max(Date, na.rm = TRUE)
        )
    }

    dive_se <- vctrs::list_drop_empty(dive_se)

    dive_se <- dive_se |>
      bind_rows() |>
      group_by(DeploymentID) |>
      summarise(dive_start = min(dive_start),
                dive_end = max(dive_end))

    meta <- meta |>
      left_join(dive_se, by = "DeploymentID") #c("TagID" = "Ptt"))
  }

return(meta)

}
