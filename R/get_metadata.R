##' @title download deployment metadata
##'
##' @description downloads, restructures & formats metadata, appends
##' dive/CTD start and end datetimes (for QC), & fills in missing required
##' metadata - eg. release_datetime, release_longitude/latitude's with data from
##' the GPS (if present) or Argos location file.
##'
##' @param source the source of the deployment metadata, current options are
##' `imos`, `smru`, or `atn`. If `source = 'imos'` or `source = 'atn'` then
##' metadata are obtained from user-provided .CSV file via the config.json file.
##' If `sourcce = 'smru'` then metadata are built from a combination of SMRU
##' server details & deployment details in the config.json file.
##' @param tag_mfr the tag manufacturer, current options are `smru` or `wc`
##' cid SMRU campaign ids
##' @param tag_data a list of either `smru` data tables or `wc` files as output by
##' `pull_data`.
##' @param cid SMRU campaign id must be provided when the tag_mfr is `smru`
##' @param user SMRU data server username as a quoted string - to be used only if
##' metadata are to be built from SMRU server details (`source = 'smru'`).
##' @param pwd SMRU data server password as a quoted string - to be used only if
##' metadata are to be built from SMRU server details (`source = 'smru'`).
##' @param dropIDs SMRU refs or WC ids to be dropped from QC
##' @param file path to metadata .csv file, if provided then metadata will be
##' read from the provided `source`
##' @param meta.args optional metadata fields to be passed from config file when
##' downloading tag metadata from SMRU server.
##' @param subset.ids a character vector of comma-separated (no spaces) WC UUID's
##' to be included in the QC. Ignored if `tag_mfr != "wc"`.
##' @param wc.meta an R data.frame of Wildlife Computers tag deployment metadata
##' obtained via `download_data()`.
##'
##'
##' @importFrom dplyr select rename mutate filter bind_rows starts_with
##' @importFrom lubridate mdy_hms round_date
##' @importFrom readr read_csv cols locale
##' @importFrom vctrs list_drop_empty
##'
##' @export
##'

get_metadata <- function(source = "smru",
                         tag_mfr = "smru",
                         tag_data = NULL,
                         cid = NULL,
                         user = NULL,
                         pwd = NULL,
                         dropIDs = NULL,
                         file = NULL,
                         meta.args,
                         subset.ids = NULL,
                         wc.meta = NULL) {

  if (is.null(tag_data))
    stop("a tag_data object must be supplied")
  #  if(all(source == "imos", is.null(file))) stop("an IMOS-ATF .csv metadata file must be provided")
  if (all((tag_mfr == "smru" |
           source == "smru"), is.null(cid)))
    stop("'cid' argument is empty, SMRU campaign id(s) must be specified")
  if (all(source == "atn", tag_mfr %in% c("smru", "wc"), is.null(file)))
    stop("an ATN .csv metadata file must be provided")


  if (tag_mfr == "wc") {
    if (!is.null(subset.ids)) {
      ids <- read_csv(subset.ids) |>
        suppressMessages()
      if (names(ids) != "uuid" |
          length(names(ids)) != 1)
        stop("Variable name for the WC ID's to QC'd must be 'uuid'")

    } else {
      ids <- NULL
    }

    ## get diving start/end dates - to automatically determine when animal
    ##  goes to sea
    tag_data <- vctrs::list_drop_empty(tag_data)
    dive_se <- vector(mode = "list", length = 4)

    if ("Histos" %in% names(tag_data)) {
      dive_se[[1]] <- tag_data$Histos |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = min(Date, na.rm = TRUE)
        )
    }

    if ("ECDHistos_SCOUT_TEMP_361A" %in% names(tag_data)) {
      dive_se[[2]] <- tag_data$ECDHistos_SCOUT_TEMP_361A |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = min(Date, na.rm = TRUE)
        )
    }

    if ("ECDHistos_SCOUT_DSA" %in% names(tag_data)) {
      dive_se[[3]] <- tag_data$ECDHistos_SCOUT_DSA |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Start, na.rm = TRUE),
          dive_end = min(End, na.rm = TRUE)
        )
    }

    if ("MinMaxDepth" %in% names(tag_data)) {
      dive_se[[4]] <- tag_data$MinMaxDepth |>
        group_by(DeploymentID) |>
        summarise(
          dive_start = min(Date, na.rm = TRUE),
          dive_end = max(Date, na.rm = TRUE)
        )
    }

    dive_se <- list_drop_empty(dive_se)

    dive_se <- dive_se |>
      bind_rows() |>
      group_by(DeploymentID) |>
      summarise(dive_start = min(dive_start),
                dive_end = max(dive_end))


    ## read & clean metadata
    meta <- switch(source,
                   irap = {
                     wc_clean_meta_irap(file = file,
                                        ids = ids,
                                        dropIDs = dropIDs,
                                        wc.meta = wc.meta
                                        )
                     },
                   atn = {
                     wc_clean_meta_atn(file = file,
                                         ids = ids,
                                         dropIDs = dropIDs)
    })


    ## join metadata & dive start/end dates
    meta <- meta |>
      left_join(dive_se, by = "DeploymentID")

    ## revise QC start date (based on dive_start) if QC_start_datetime variable exists in metadata
    if ("QC_start_datetime" %in% names(meta)) {
      meta <- meta |>
        mutate(
          dive_start = ifelse(
            !is.na(QC_start_datetime) & dive_start < QC_start_datetime,
            QC_start_datetime,
            dive_start
          )
        ) |>
        mutate(dive_start = as.POSIXct(dive_start, origin = "1970-01-01", tz = "UTC")) |>
        select(-QC_start_datetime)
    }


  } else if (tag_mfr == "smru") {
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
                     meta <- smru_clean_meta_atn(file = file,
                                                 cid = cid,
                                                 dropIDs = dropIDs)

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
                     ## download SMRU metadata & generate QC metadata for IMOS-AODN
                     meta <- smru_build_meta_imos(cid = cid,
                                                  user = user,
                                                  pwd = pwd,
                                                  dropIDs = dropIDs,
                                                  meta.args = meta.args,
                                                  tag_data = tag_data)

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
                     smru_clean_meta_imos(
                       cid = cid,
                       smru = tag_data,
                       dropIDs = dropIDs,
                       file = file
                     )
                   })
  }


  return(meta)

}
