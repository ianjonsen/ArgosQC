##' @title download deployment metadata
##'
##' @description downloads, restructures & formats metadata, appends
##' dive/CTD start and end datetimes (for QC), & fills in missing required
##' metadata - eg. release_datetime, release_longitude/latitude's with data from
##' the GPS (if present) or Argos location file.
##'
##' @param source the source of the deployment metadata, current options are
##' `imos`, `otn`, `smru`, or `atn`. If `source = 'imos'` or `source = 'atn'` then
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
##' downloading tag metadata from SMRU server. Typically used only when no
##' metadata filepath is provided in the config file.
##' @param subset.ids a character vector of comma-separated (no spaces) WC UUID's
##' to be included in the QC. Ignored if `tag_mfr != "wc"`.
##' @param wc.meta an R data.frame of Wildlife Computers tag deployment metadata
##' obtained via `download_data()`. Typically used only when no metadata filepath
##' is provided in the config file.
##' @param QCmode `"nrt"` or `"dm"`. For IMOS metadata in delayed-mode QC, the
##' tag identifiers are checked against the SMRU data server (see
##' `smru_clean_meta_imos()`)
##' @param build.meta logical; delayed-mode QC only. Build the metadata of
##' deployments missing from the metadata file from the SMRU data server, the
##' .mdb deployments table, the Argos locations and `meta.args`
##' @param server.check logical; delayed-mode QC of IMOS metadata only. Check
##' ptt, body, WMO ID and tag type against the SMRU data server (default
##' `TRUE`). `FALSE` takes them from the metadata file unchecked; `build.meta`
##' must then be `FALSE`. Set by `"server.check"` in the config setup block
##' @param path2mdb,p2mdbtools the .mdb folder and the mdbtools path (used when
##' `build.meta = TRUE`)
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
                         wc.meta = NULL,
                         QCmode = NULL,
                         build.meta = FALSE,
                         server.check = TRUE,
                         path2mdb = NULL,
                         p2mdbtools = NULL) {

  if (is.null(tag_data))
    stop("a tag_data object must be supplied")
  #  if(all(source == "imos", is.null(file))) stop("an IMOS-ATF .csv metadata file must be provided")
  if (all((tag_mfr == "smru" |
           source == "smru"), is.null(cid)))
    stop("'cid' argument is empty, SMRU campaign id(s) must be specified")
  if (all(source == "atn", tag_mfr %in% c("smru", "wc"), is.null(file)))
    stop("an ATN .csv metadata file must be provided")


  if (tag_mfr == "wc") {
    if (!is.null(subset.ids) & !is.data.frame(subset.ids)) {
      ids <- read_csv(subset.ids) |>
        suppressMessages()
      if (names(ids) != "uuid" |
          length(names(ids)) != 1)
        stop("Variable name for the WC ID's to QC'd must be 'uuid'")

      } else {
      ids <- NULL
    }

    tag_data <- vctrs::list_drop_empty(tag_data)

    ## read & clean metadata
    meta <- switch(source,
                   atn = {
                     wc_clean_meta_atn(file = file,
                                       ids = ids,
                                       dropIDs = dropIDs)
                   },
                   irap = {
                     wc_clean_meta_irap(file = file,
                                        ids = ids,
                                        dropIDs = dropIDs,
                                        wc.meta = wc.meta
                                        )
                     },
                   wc = {
                    wc_build_meta_generic(ids = ids,
                                          dropIDs = dropIDs,
                                          meta.args = meta.args,
                                          wc.meta = wc.meta,
                                          tag_data = tag_data)
                   }
                   )

    ## QC start and end dates. The QC start is the deployment start (T0), set
    ##  by wc_deployment_start() (not the first dive record); a QC_start_datetime
    ##  in the metadata file is a later, manual QC start. A QC_end_datetime in
    ##  the metadata file is a manual QC end
    meta$QC_start_date <- if ("QC_start_datetime" %in% names(meta)) {
      as.POSIXct(meta$QC_start_datetime, tz = "UTC")
    } else {
      as.POSIXct(rep(NA_real_, nrow(meta)), tz = "UTC")
    }
    meta$QC_end_date <- if ("QC_end_datetime" %in% names(meta)) {
      as.POSIXct(meta$QC_end_datetime, tz = "UTC")
    } else {
      as.POSIXct(rep(NA_real_, nrow(meta)), tz = "UTC")
    }
    meta <- meta |>
      select(-dplyr::any_of(c("QC_start_datetime", "QC_end_datetime")))
    for (v in intersect(c("deploy_longitude", "deploy_latitude"), names(meta))) {
      meta[[v]] <- suppressWarnings(as.double(meta[[v]]))
    }

  } else if (tag_mfr == "smru") {
    ## append dive start and end dates for (alternate) track truncation
    ##  to be used as alternate on final, delayed-mode (manual) QC
    if ("dive" %in% names(tag_data)) {
      dive_se <- tag_data$dive |>
        mutate(ref = as.character(ref)) |>
#        filter(!is.na(lon), !is.na(lat)) |> ## should drop any dives associated w tag testing
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
#        filter(!is.na(lon), !is.na(lat)) |> ## should drop any profiles associated w tag testing
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
                     dm <- identical(as.character(QCmode), "dm")
                     if (dm && !isTRUE(server.check) && isTRUE(as.logical(build.meta))) {
                       stop("build.meta = TRUE needs the SMRU server check; set server.check ",
                            "to true or build.meta to false in the config file.", call. = FALSE)
                     }
                     smru_clean_meta_imos(
                       cid = cid,
                       smru = tag_data,
                       dropIDs = dropIDs,
                       file = file,
                       server = dm && isTRUE(server.check),
                       user = user,
                       pwd = pwd,
                       build.meta = isTRUE(as.logical(build.meta)),
                       meta.args = meta.args,
                       path2mdb = path2mdb,
                       p2mdbtools = p2mdbtools
                     )
                   },
                   otn = {
                     meta <- read_csv(file, col_types = "cciiccccddcTTccdddc")
                     if(any(!names(meta) %in% c("sattag_program",
                     "device_id",
                     "ptt",
                     "body",
                     "device_wmo_ref",
                     "tag_type",
                     "common_name",
                     "species",
                     "release_longitude",
                     "release_latitude",
                     "release_site",
                     "release_date",
                     "recovery_date",
                     "age_class",
                     "sex",
                     "length",
                     "estimated_mass",
                     "actual_mass",
                     "state_country"))) stop("incorrectly structured metadata file, ensure the .CSV file contains only these fields:\n
                            sattag_program,device_id,ptt,body,device_wmo_ref,tag_type,common_name,species,release_longitude,release_latitude,release_site,release_date,recovery_date,age_class,sex,length,estimated_mass,actual_mass,state_country")

                     if ("dive" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(dive_se, by = c("device_id" = "ref"))
                     }
                     if ("ctd" %in% names(tag_data)) {
                       meta <- meta |>
                         left_join(ctd_se, by = c("device_id" = "ref"))
                     }
                     meta

                   })
  }

  return(meta)

}
