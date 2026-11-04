## Provisional deployment metadata for Wildlife Computers near real-time QC
##  (internal)
##
## The Wildlife Computers equivalent of smru_meta_provisional(), for near
## real-time QC without a metadata file (metadata built from the Wildlife
## Computers Portal). The workflow is the same:
##  1) no file: the first run. The metadata are built from the Portal, T0 is
##     calculated, and <name>_metadata_provisional.csv is written in the working
##     directory (<name> is the config file name without "config_" and ".json",
##     e.g. config_lhtu_wc.json gives lhtu_wc_metadata_provisional.csv);
##  2) a deployment in the tag data but not in the file: its row is built from
##     the Portal metadata, its T0 calculated, and the row appended;
##  3) t0_status "provisional": T0 is calculated again on each run, and becomes
##     "final" once the tag has transmitted for 7 days after it;
##  4) t0_status "final": T0 and its position are used as stored. To set a
##     deployment start by hand, edit t0_date, t0_latitude and t0_longitude and
##     set t0_status to "final";
##  5) for the first 8 weeks after T0, the Portal fields (owner, ptt, tag model
##     and serial number, deployment date and position) are refreshed weekly;
##     Portal values replace those in the file;
##  6) rows are never deleted; rows of tags in dropIDs stay in the file but are
##     left out of the QC.
## The first columns are the Wildlife Computers output metadata columns, in the
## output order. The SMRU file keeps T0 in release_date and the release
## position (as in the AODN metadata). The Wildlife Computers metadata have no
## single release column (deploy_date is the Portal entry, the embark columns
## are calculated), so here T0 is in t0_date, t0_latitude and t0_longitude. The
## other t0_ and prov_ columns are as in the SMRU file. None of the t0_ or prov_
## columns reach the output files.

.prov_wc_t0_cols <- c("t0_date", "t0_latitude", "t0_longitude")
.prov_wc_server_cols <- c("owner", "tag_ptt", "tag_model", "tag_serial_number", "deploy_date",
                          "deploy_longitude", "deploy_latitude")
.prov_wc_types <- c(deploy_date = "time", embark_date = "time", recovery_date = "time",
                    deploy_longitude = "dbl", deploy_latitude = "dbl", embark_longitude = "dbl",
                    embark_latitude = "dbl", length = "dbl", mass = "dbl",
                    t0_date = "time", t0_latitude = "dbl", t0_longitude = "dbl")

wc_prov_name <- function(config) sub("^config_", "", tools::file_path_sans_ext(basename(config)))
wc_prov_file <- function(dir, name) file.path(dir, paste0(name, "_metadata_provisional.csv"))

## read the provisional metadata file; NULL if there is none
wc_prov_read <- function(dir, name) {
  f <- wc_prov_file(dir, name)
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(suppressMessages(readr::read_csv(f, col_types = readr::cols(.default = "c"),
                                                 na = character(0))),
                error = function(e) stop("Provisional metadata file ", f, " cannot be read: ",
                                         conditionMessage(e), ". Correct it, or delete it to ",
                                         "rebuild it from the Wildlife Computers Portal.",
                                         call. = FALSE))
  x <- as.data.frame(x)
  miss <- setdiff(c("DeploymentID", .prov_wc_t0_cols, .prov_extra_cols), names(x))
  if (length(miss) > 0) {
    stop("Provisional metadata file ", f, " is missing column(s): ", paste(miss, collapse = ", "),
         ". Correct it, or delete it to rebuild it from the Wildlife Computers Portal.",
         call. = FALSE)
  }
  types <- c(.prov_wc_types, .prov_types[.prov_extra_cols])
  for (v in names(x)) {
    x[[v]] <- prov_convert(x[[v]], if (v %in% names(types)) types[[v]] else "chr", f, v)
  }
  if (anyDuplicated(x$DeploymentID)) {
    stop("Provisional metadata file ", f, ": duplicated DeploymentID ",
         paste(unique(x$DeploymentID[duplicated(x$DeploymentID)]), collapse = ", "), ".",
         call. = FALSE)
  }
  bad <- !x$t0_status %in% c("provisional", "final")
  if (any(bad)) {
    stop("Provisional metadata file ", f, ": t0_status must be \"provisional\" or \"final\" ",
         "(row(s) ", paste(which(bad), collapse = ", "), ").", call. = FALSE)
  }
  fin <- x$t0_status == "final" & (is.na(x$t0_date) | is.na(x$t0_latitude) | is.na(x$t0_longitude))
  if (any(fin)) {
    stop("Provisional metadata file ", f, ": a final deployment start needs t0_date, ",
         "t0_latitude and t0_longitude (row(s) ", paste(which(fin), collapse = ", "), ").",
         call. = FALSE)
  }
  x
}

## write the provisional metadata file: temporary file first, then renamed
wc_prov_write <- function(rows, dir, name) {
  out_cols <- setdiff(names(rows), c(.prov_wc_t0_cols, .prov_extra_cols))
  rows <- rows[order(rows$DeploymentID), c(out_cols, .prov_wc_t0_cols, .prov_extra_cols)]
  for (v in names(rows)) {
    if (inherits(rows[[v]], "POSIXct")) {
      rows[[v]] <- ifelse(is.na(rows[[v]]), NA_character_,
                          format(rows[[v]], "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
  }
  f <- wc_prov_file(dir, name)
  tmp <- paste0(f, ".tmp")
  readr::write_csv(rows, tmp, na = "")
  if (!file.rename(tmp, f)) stop("Could not write ", f, call. = FALSE)
  invisible(f)
}

## the metadata for near real-time QC from the provisional file and the Portal
##  metadata. Returns the metadata, in the form returned by
##  wc_build_meta_generic(), with the provisional rows in attribute "prov" and
##  notes in attribute "notes"
wc_meta_provisional <- function(name, dir, ids, dropIDs, meta.args, wc.meta, tag_data,
                                now = Sys.time()) {
  now <- as.POSIXct(now, tz = "UTC")
  prov <- wc_prov_read(dir, name)
  refs <- unique(as.character(tag_data$Locations$DeploymentID))
  if (!is.null(ids)) refs <- intersect(refs, as.character(ids$uuid))
  new_refs <- if (is.null(prov)) refs else setdiff(refs, prov$DeploymentID)
  due <- if (is.null(prov)) logical(0) else
    !is.na(prov$t0_date) &
    as.numeric(difftime(now, prov$t0_date, units = "days")) <= .prov_refresh_window_days &
    (is.na(prov$prov_refreshed) |
       as.numeric(difftime(now, prov$prov_refreshed, units = "days")) >= .prov_refresh_days)

  build <- function() as.data.frame(wc_build_meta_generic(ids = ids, dropIDs = character(0),
                                                          meta.args = meta.args,
                                                          wc.meta = wc.meta,
                                                          tag_data = tag_data))
  notes <- character(0)
  srv <- NULL
  if (length(new_refs) > 0) {
    srv <- build()
  } else if (any(due)) {
    srv <- tryCatch(build(), error = function(e) {
      notes <<- c(notes, stats::setNames(paste0(
        "Wildlife Computers Portal metadata not available for the weekly refresh of the ",
        "provisional metadata (", conditionMessage(e), "); the provisional metadata file is ",
        "used as it is"), "server"))
      NULL
    })
  }

  ## weekly refresh of the Portal fields
  if (!is.null(prov) && !is.null(srv) && any(due)) {
    for (k in which(due)) {
      j <- match(prov$DeploymentID[k], srv$DeploymentID)
      if (is.na(j)) next
      for (v in intersect(.prov_wc_server_cols, intersect(names(prov), names(srv)))) {
        sv <- srv[[v]][j]
        if (!is.na(sv) && !identical(as.character(sv), as.character(prov[[v]][k]))) {
          notes <- c(notes, stats::setNames(paste0(
            prov$DeploymentID[k], ": ", v, " updated from the Wildlife Computers Portal (",
            ifelse(is.na(prov[[v]][k]), "empty", as.character(prov[[v]][k])), " to ",
            as.character(sv), ")"), "server"))
          prov[[v]][k] <- if (inherits(prov[[v]], "POSIXct")) as.POSIXct(sv, tz = "UTC") else
            if (is.numeric(prov[[v]])) as.numeric(sv) else as.character(sv)
        }
      }
      prov$prov_refreshed[k] <- now
    }
  }

  ## new rows from the Portal metadata
  if (!is.null(srv) && length(new_refs) > 0) {
    nw <- srv[srv$DeploymentID %in% new_refs, , drop = FALSE]
    nw$t0_date <- as.POSIXct(NA, tz = "UTC")
    nw$t0_latitude <- NA_real_
    nw$t0_longitude <- NA_real_
    nw$t0_status <- "provisional"
    nw$t0_source <- NA_character_
    nw$t0_n_tests <- NA_integer_
    nw$t0_note <- NA_character_
    nw$t0_days_after <- NA_real_
    nw$prov_source <- "Wildlife Computers Portal"
    nw$prov_refreshed <- now
    nw$prov_written <- as.POSIXct(NA, tz = "UTC")
    nw$prov_argosqc_version <- NA_character_
    types <- c(.prov_wc_types, .prov_types[.prov_extra_cols])
    for (v in names(nw)) {
      nw[[v]] <- prov_convert(nw[[v]], if (v %in% names(types)) types[[v]] else "chr",
                              "(Wildlife Computers Portal)", v)
    }
    if (nrow(nw) > 0) {
      notes <- c(notes, stats::setNames(paste0(
        if (is.null(prov)) "First run: metadata built from the Wildlife Computers Portal for "
        else "Deployments added to the provisional metadata from the Wildlife Computers Portal: ",
        paste(nw$DeploymentID, collapse = ", ")), "built"))
      if (is.null(prov)) {
        prov <- nw
      } else {
        for (v in setdiff(names(prov), names(nw))) nw[[v]] <- NA
        prov <- rbind(prov, nw[, names(prov)])
      }
    }
  }
  if (is.null(prov)) stop("No deployment metadata from the Wildlife Computers Portal.",
                          call. = FALSE)

  out_cols <- setdiff(names(prov), c(.prov_wc_t0_cols, .prov_extra_cols))
  meta <- prov[!prov$DeploymentID %in% dropIDs, out_cols]
  rownames(meta) <- NULL
  attr(meta, "prov") <- prov
  attr(meta, "notes") <- notes
  meta
}

## deployment starts stored as final in the provisional metadata, in the form
##  used by wc_deployment_start(fixed = )
wc_prov_fixed_starts <- function(prov) {
  if (is.null(prov)) return(NULL)
  f <- prov[prov$t0_status == "final", ]
  if (nrow(f) == 0) return(NULL)
  data.frame(ref = f$DeploymentID, T0 = f$t0_date, lat0 = f$t0_latitude,
             lon0 = f$t0_longitude,
             source = paste0("final in the provisional metadata file",
                             ifelse(is.na(f$t0_source), "", paste0("; ", f$t0_source))))
}

## update and write the provisional metadata after the deployment start step
##  (as prov_update() for SMRU): provisional rows take the new T0, its position
##  and the embark columns; every row gets the days of transmissions after T0
##  (from the Locations table before the deployment start is applied), and
##  becomes final at 7 days
wc_prov_update <- function(prov, ds, locations, dir, name, now = Sys.time()) {
  now <- as.POSIXct(now, tz = "UTC")
  st <- ds$starts
  m <- ds$meta
  last <- c(tapply(as.numeric(as.POSIXct(locations$Date, tz = "UTC")),
                   as.character(locations$DeploymentID), max, na.rm = TRUE))
  emb <- intersect(c("embark_date", "embark_longitude", "embark_latitude"), names(prov))
  became_final <- character(0)
  for (k in seq_len(nrow(prov))) {
    r <- prov$DeploymentID[k]
    i <- match(r, st$ref)
    if (prov$t0_status[k] == "provisional" && !is.na(i) && !is.na(st$T0[i])) {
      prov$t0_date[k] <- st$T0[i]
      prov$t0_latitude[k] <- st$lat0[i]
      prov$t0_longitude[k] <- st$lon0[i]
      prov$t0_source[k] <- st$source[i]
      prov$t0_n_tests[k] <- as.integer(st$n_tests[i])
      prov$t0_note[k] <- st$note[i]
      j <- match(r, m$DeploymentID)
      if (!is.na(j)) for (v in emb) prov[[v]][k] <- m[[v]][j]
    }
    if (!is.na(prov$t0_date[k]) && !is.na(last[r])) {
      prov$t0_days_after[k] <- round((last[r] - as.numeric(prov$t0_date[k])) / 86400, 2)
      if (prov$t0_status[k] == "provisional" && prov$t0_days_after[k] >= .prov_final_days) {
        prov$t0_status[k] <- "final"
        became_final <- c(became_final, r)
      }
    }
  }
  prov$prov_written <- now
  prov$prov_argosqc_version <- as.character(utils::packageVersion("ArgosQC"))
  f <- wc_prov_write(prov, dir, name)
  notes <- character(0)
  if (length(became_final) > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Deployment start now final (7 days of transmissions after it): ",
      paste(became_final, collapse = ", ")), "sites"))
  }
  notes <- c(notes, stats::setNames(paste0("Provisional metadata written: ", f), "progress"))
  invisible(list(prov = prov, notes = notes))
}
