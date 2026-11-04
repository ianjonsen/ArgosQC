## Provisional deployment metadata for near real-time QC (internal)
##
## Near real-time QC without a metadata file builds the metadata from the SMRU
## data server and calculates the deployment start (T0) from the locations. The
## first run on a campaign writes them to <cid>_metadata_provisional.csv in the
## working directory, and later runs read that file:
##  1) no file: the first run on the campaign. The metadata are built from the
##     SMRU server, T0 is calculated, and the file is written;
##  2) a tag in the tag data but not in the file (e.g. deployed later): its row
##     is built from the SMRU server, its T0 calculated, and the row appended;
##  3) t0_status "provisional": T0 is calculated again on each run. It becomes
##     "final" once the tag has transmitted for 7 days after T0 (enough for a
##     pre-deployment test burst, followed by a gap of more than 5 days, to be
##     recognised and dropped);
##  4) t0_status "final": T0 and the release position are used as stored. To set
##     a deployment start by hand, edit release_date, release_latitude and
##     release_longitude and set t0_status to "final";
##  5) for the first 8 weeks after T0, the SMRU server identifiers (ptt, body,
##     device_wmo_ref, tag_type) are refreshed weekly, as SMRU staff may add
##     them (e.g. WMO IDs) later. Server values replace those in the file. If
##     the server cannot be reached for a refresh, the file is used as it is;
##  6) rows are never deleted. Rows of tags in dropIDs stay in the file but are
##     left out of the QC.
## The first 19 columns are the AODN metadata columns, with the same names,
## order and types as in the AODN metadata file (release_date and the release
## position are T0 and its position). The other columns, prefixed t0_ or prov_,
## are for ArgosQC only and never reach the AODN files. Delete the file to start
## the campaign again from the SMRU server.

.prov_aodn_cols <- c("sattag_program", "device_id", "ptt", "body", "device_wmo_ref",
                     "tag_type", "common_name", "species", "release_longitude",
                     "release_latitude", "release_site", "release_date", "recovery_date",
                     "age_class", "sex", "length", "estimated_mass", "actual_mass",
                     "state_country")
.prov_extra_cols <- c("t0_status", "t0_source", "t0_n_tests", "t0_note", "t0_days_after",
                      "prov_source", "prov_refreshed", "prov_written",
                      "prov_argosqc_version")
.prov_types <- c(sattag_program = "chr", device_id = "chr", ptt = "int", body = "int",
                 device_wmo_ref = "chr", tag_type = "chr", common_name = "chr",
                 species = "chr", release_longitude = "dbl", release_latitude = "dbl",
                 release_site = "chr", release_date = "time", recovery_date = "time",
                 age_class = "chr", sex = "chr", length = "dbl", estimated_mass = "int",
                 actual_mass = "dbl", state_country = "chr", t0_status = "chr",
                 t0_source = "chr", t0_n_tests = "int", t0_note = "chr",
                 t0_days_after = "dbl", prov_source = "chr", prov_refreshed = "time",
                 prov_written = "time", prov_argosqc_version = "chr")
.prov_final_days <- 7
.prov_refresh_days <- 7
.prov_refresh_window_days <- 56
.prov_server_cols <- c("ptt", "body", "device_wmo_ref", "tag_type")

prov_file <- function(dir, cid) file.path(dir, paste0(cid, "_metadata_provisional.csv"))

## convert one column to its type; values present in the file but not
##  convertible stop with an error naming the file, row and column
prov_convert <- function(x, type, file, col) {
  x <- as.character(x)
  x[!is.na(x) & trimws(x) %in% c("", "NA")] <- NA
  out <- switch(type,
                chr = x,
                int = suppressWarnings(as.integer(x)),
                dbl = suppressWarnings(as.numeric(x)),
                time = suppressWarnings(lubridate::parse_date_time(
                  x, orders = c("Ymd HMS", "Ymd HM", "Ymd"), tz = "UTC", quiet = TRUE)))
  bad <- which(!is.na(x) & is.na(out))
  if (length(bad) > 0) {
    stop("Provisional metadata file ", file, ": column ", col, " has values that are not ",
         switch(type, int = "integers", dbl = "numbers", time = "date-times (UTC)", type),
         " in row(s) ", paste(bad, collapse = ", "), " (", paste(unique(x[bad]), collapse = ", "),
         "). Correct the file, or delete it to rebuild it from the SMRU server.", call. = FALSE)
  }
  if (type == "time") out <- as.POSIXct(out, tz = "UTC")
  out
}

## read the provisional metadata files of campaigns cid; NULL if there are none
prov_read <- function(dir, cid) {
  out <- lapply(cid, function(id) {
    f <- prov_file(dir, id)
    if (!file.exists(f)) return(NULL)
    x <- tryCatch(suppressMessages(readr::read_csv(f, col_types = readr::cols(.default = "c"),
                                                   na = character(0))),
                  error = function(e) stop("Provisional metadata file ", f, " cannot be read: ",
                                           conditionMessage(e), ". Correct it, or delete it to ",
                                           "rebuild it from the SMRU server.", call. = FALSE))
    miss <- setdiff(c(.prov_aodn_cols, .prov_extra_cols), names(x))
    if (length(miss) > 0) {
      stop("Provisional metadata file ", f, " is missing column(s): ", paste(miss, collapse = ", "),
           ". Correct it, or delete it to rebuild it from the SMRU server.", call. = FALSE)
    }
    x <- as.data.frame(x)[, c(.prov_aodn_cols, .prov_extra_cols)]
    for (v in names(x)) x[[v]] <- prov_convert(x[[v]], .prov_types[[v]], f, v)
    other <- unique(x$sattag_program[!is.na(x$sattag_program) & x$sattag_program != id])
    if (length(other) > 0 || any(smru_cid(x$device_id) != id)) {
      stop("Provisional metadata file ", f, " holds deployments of another campaign (",
           paste(unique(c(other, setdiff(smru_cid(x$device_id), id))), collapse = ", "),
           "). Check the file.", call. = FALSE)
    }
    if (anyDuplicated(x$device_id)) {
      stop("Provisional metadata file ", f, ": duplicated device_id ",
           paste(unique(x$device_id[duplicated(x$device_id)]), collapse = ", "), ".",
           call. = FALSE)
    }
    bad <- !x$t0_status %in% c("provisional", "final")
    if (any(bad)) {
      stop("Provisional metadata file ", f, ": t0_status must be \"provisional\" or \"final\" ",
           "(row(s) ", paste(which(bad), collapse = ", "), ").", call. = FALSE)
    }
    fin <- x$t0_status == "final" & (is.na(x$release_date) | is.na(x$release_latitude) |
                                       is.na(x$release_longitude))
    if (any(fin)) {
      stop("Provisional metadata file ", f, ": a final deployment start needs release_date, ",
           "release_latitude and release_longitude (row(s) ", paste(which(fin), collapse = ", "),
           ").", call. = FALSE)
    }
    x
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) NULL else do.call(rbind, out)
}

## write the provisional metadata files, one per campaign; written to a
##  temporary file first, then renamed, so an interrupted run cannot leave a
##  partly written file
prov_write <- function(rows, dir) {
  rows <- rows[, c(.prov_aodn_cols, .prov_extra_cols)]
  for (v in names(rows)) {
    if (inherits(rows[[v]], "POSIXct")) {
      rows[[v]] <- ifelse(is.na(rows[[v]]), NA_character_,
                          format(rows[[v]], "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
  }
  files <- character(0)
  for (id in unique(smru_cid(rows$device_id))) {
    x <- rows[smru_cid(rows$device_id) == id, ]
    x <- x[order(x$device_id), ]
    f <- prov_file(dir, id)
    tmp <- paste0(f, ".tmp")
    readr::write_csv(x, tmp, na = "")
    if (!file.rename(tmp, f)) stop("Could not write ", f, call. = FALSE)
    files <- c(files, f)
  }
  invisible(files)
}

## the metadata for near real-time QC from the provisional files and the SMRU
##  server (see above). Returns the metadata, in the form returned by
##  smru_build_meta_imos(), with the provisional rows in attribute "prov" and
##  notes in attribute "notes"
smru_meta_provisional <- function(cid, dir, user, pwd, dropIDs, meta.args, tag_data,
                                  now = Sys.time()) {
  now <- as.POSIXct(now, tz = "UTC")
  prov <- prov_read(dir, cid)
  refs <- qc_tag_refs(tag_data)
  refs <- refs[smru_cid(refs) %in% cid]
  new_refs <- if (is.null(prov)) refs else setdiff(refs, prov$device_id)
  due <- if (is.null(prov)) logical(0) else
    !is.na(prov$release_date) &
    as.numeric(difftime(now, prov$release_date, units = "days")) <= .prov_refresh_window_days &
    (is.na(prov$prov_refreshed) |
       as.numeric(difftime(now, prov$prov_refreshed, units = "days")) >= .prov_refresh_days)

  build <- function() smru_build_meta_imos(cid = cid, user = user, pwd = pwd,
                                           dropIDs = character(0), meta.args = meta.args,
                                           tag_data = tag_data)
  notes <- character(0)
  srv <- NULL
  if (length(new_refs) > 0) {
    srv <- build()
  } else if (any(due)) {
    srv <- tryCatch(build(), error = function(e) {
      notes <<- c(notes, stats::setNames(paste0(
        "SMRU server not reached for the weekly refresh of the provisional metadata (",
        conditionMessage(e), "); the provisional metadata file is used as it is"), "server"))
      NULL
    })
  }

  ## weekly refresh of the SMRU server identifiers
  if (!is.null(prov) && !is.null(srv) && any(due)) {
    for (k in which(due)) {
      j <- match(prov$device_id[k], srv$device_id)
      if (is.na(j)) next
      for (v in .prov_server_cols) {
        sv <- srv[[v]][j]
        if (!is.na(sv) && !identical(as.character(sv), as.character(prov[[v]][k]))) {
          notes <- c(notes, stats::setNames(paste0(
            prov$device_id[k], ": ", v, " updated from the SMRU server (",
            ifelse(is.na(prov[[v]][k]), "empty", prov[[v]][k]), " to ", sv, ")"), "server"))
          prov[[v]][k] <- if (is.integer(prov[[v]])) as.integer(sv) else as.character(sv)
        }
      }
      prov$prov_refreshed[k] <- now
    }
  }

  ## new rows from the SMRU server
  if (!is.null(srv) && length(new_refs) > 0) {
    nw <- srv[srv$device_id %in% new_refs, , drop = FALSE]
    nw <- as.data.frame(nw)
    for (v in setdiff(.prov_aodn_cols, names(nw))) nw[[v]] <- NA
    nw <- nw[, .prov_aodn_cols]
    nw$t0_status <- "provisional"
    nw$t0_source <- NA_character_
    nw$t0_n_tests <- NA_integer_
    nw$t0_note <- NA_character_
    nw$t0_days_after <- NA_real_
    nw$prov_source <- "SMRU server"
    nw$prov_refreshed <- now
    nw$prov_written <- as.POSIXct(NA, tz = "UTC")
    nw$prov_argosqc_version <- NA_character_
    for (v in names(nw)) nw[[v]] <- prov_convert(nw[[v]], .prov_types[[v]], "(SMRU server)", v)
    if (nrow(nw) > 0) {
      notes <- c(notes, stats::setNames(paste0(
        if (is.null(prov)) "First run on the campaign: metadata built from the SMRU server for "
        else "Deployments added to the provisional metadata from the SMRU server: ",
        paste(nw$device_id, collapse = ", ")), "built"))
    }
    prov <- if (is.null(prov)) nw else rbind(prov, nw)
  }
  if (is.null(prov)) stop("No deployment metadata for ", paste(cid, collapse = ", "),
                          " from the SMRU server.", call. = FALSE)

  meta <- prov[!qc_ref_key(prov$device_id) %in% qc_ref_key(dropIDs), .prov_aodn_cols]
  rownames(meta) <- NULL
  attr(meta, "prov") <- prov
  attr(meta, "notes") <- notes
  meta
}

## deployment starts stored as final in the provisional metadata, in the form
##  used by smru_deployment_start(fixed = )
prov_fixed_starts <- function(prov) {
  if (is.null(prov)) return(NULL)
  f <- prov[prov$t0_status == "final", ]
  if (nrow(f) == 0) return(NULL)
  data.frame(ref = f$device_id, T0 = f$release_date, lat0 = f$release_latitude,
             lon0 = f$release_longitude,
             source = paste0("final in the provisional metadata file",
                             ifelse(is.na(f$t0_source), "", paste0("; ", f$t0_source))))
}

## update and write the provisional metadata after the deployment start step:
##  provisional rows take the new T0 and release position; every row gets the
##  days of transmissions after T0, and becomes final at 7 days
prov_update <- function(prov, ds, smru, dir, now = Sys.time()) {
  now <- as.POSIXct(now, tz = "UTC")
  st <- ds$starts
  m <- ds$meta
  last <- c(tapply(as.numeric(smru$diag$d_date), as.character(smru$diag$ref), max, na.rm = TRUE))
  if ("gps" %in% names(smru) && is.data.frame(smru$gps) && nrow(smru$gps) > 0) {
    lg <- c(tapply(as.numeric(smru$gps$d_date), as.character(smru$gps$ref), max, na.rm = TRUE))
    for (r in names(lg)) last[r] <- max(last[r], lg[r], na.rm = TRUE)
  }
  notes <- character(0)
  became_final <- character(0)
  for (k in seq_len(nrow(prov))) {
    r <- prov$device_id[k]
    i <- match(r, st$ref)
    if (prov$t0_status[k] == "provisional" && !is.na(i) && !is.na(st$T0[i])) {
      j <- match(r, m$device_id)
      prov$release_date[k] <- st$T0[i]
      if (!is.na(j)) {
        prov$release_latitude[k] <- m$release_latitude[j]
        prov$release_longitude[k] <- m$release_longitude[j]
      }
      prov$t0_source[k] <- st$source[i]
      prov$t0_n_tests[k] <- as.integer(st$n_tests[i])
      prov$t0_note[k] <- st$note[i]
    }
    if (!is.na(prov$release_date[k]) && !is.na(last[r])) {
      prov$t0_days_after[k] <- round((last[r] - as.numeric(prov$release_date[k])) / 86400, 2)
      if (prov$t0_status[k] == "provisional" && prov$t0_days_after[k] >= .prov_final_days) {
        prov$t0_status[k] <- "final"
        became_final <- c(became_final, r)
      }
    }
  }
  prov$prov_written <- now
  prov$prov_argosqc_version <- as.character(utils::packageVersion("ArgosQC"))
  files <- prov_write(prov, dir)
  if (length(became_final) > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Deployment start now final (7 days of transmissions after it): ",
      paste(became_final, collapse = ", ")), "sites"))
  }
  notes <- c(notes, stats::setNames(paste0("Provisional metadata written: ",
                                           paste(files, collapse = ", ")), "progress"))
  invisible(list(prov = prov, notes = notes))
}
