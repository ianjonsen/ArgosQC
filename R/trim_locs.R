##' @title Truncate the location data of individual deployments (delayed-mode QC only)
##'
##' @description Right- and/or left-truncates the prepared location data of
##' individual deployments, using dates supplied in a .csv file. This step runs
##' after `smru_prep_loc()` or `wc_prep_loc()` and before the SSM is fitted.
##' Track truncation is a supervised decision, so the function stops unless the
##' QC mode is delayed-mode (`QCmode = "dm"`). It is never applied in near
##' real-time QC.
##'
##' @param locs_sf the prepared location data returned by `smru_prep_loc()` or
##' `wc_prep_loc()`
##' @param file path to a .csv file with a `ref` column (the deployment ID: the
##' SMRU ref, or the Wildlife Computers DeploymentID) and an `end_date` and/or
##' `start_date` column. Dates are UTC and may be given as `YYYY-MM-DD` (midnight)
##' or `YYYY-MM-DD HH:MM:SS`. Locations at or after `end_date` and before
##' `start_date` are removed. Rows with no dates, and any other columns (such
##' as those written by `smru_suggest_trim()`), are ignored.
##' @param QCmode the QC mode from the config file; must be `"dm"`
##'
##' @return `locs_sf` with the location data of the listed deployments truncated
##'
##' @importFrom readr read_csv cols col_character
##' @importFrom lubridate ymd_hms
##'
##' @export

trim_locs <- function(locs_sf, file, QCmode) {

  if (is.null(QCmode) || !identical(as.character(QCmode), "dm")) {
    stop("Track truncation can only be used in delayed-mode QC (QCmode = \"dm\"). ",
         "Near real-time QC is unsupervised, so no tracks are truncated.",
         call. = FALSE)
  }

  id_var <- intersect(c("ref", "DeploymentID", "irapID"), names(locs_sf))[1]
  if (is.na(id_var)) {
    stop("No deployment ID column (ref, DeploymentID or irapID) found in the location data.",
         call. = FALSE)
  }

  if (!file.exists(file)) stop("Track truncation file not found: ", file, call. = FALSE)

  trims <- read_csv(file, col_types = cols(.default = col_character()), progress = FALSE)

  if (!"ref" %in% names(trims)) {
    stop("Track truncation file ", file, " must have a 'ref' column.", call. = FALSE)
  }
  if (!any(c("start_date", "end_date") %in% names(trims))) {
    stop("Track truncation file ", file, " must have an 'end_date' and/or 'start_date' column.",
         call. = FALSE)
  }
  if (!"start_date" %in% names(trims)) trims$start_date <- NA_character_
  if (!"end_date" %in% names(trims)) trims$end_date <- NA_character_

  ## ignore rows with no dates
  trims <- trims[!(is.na(trims$start_date) & is.na(trims$end_date)), ]
  if (nrow(trims) == 0) {
    message("Track truncation file ", file, " lists no dates; no tracks truncated.")
    return(locs_sf)
  }

  dup <- unique(trims$ref[duplicated(trims$ref)])
  if (length(dup) > 0) {
    stop("Deployments listed more than once in ", file, ": ",
         paste(dup, collapse = ", "), call. = FALSE)
  }

  unknown <- setdiff(trims$ref, as.character(locs_sf[[id_var]]))
  if (length(unknown) > 0) {
    stop("Deployments in ", file, " are not in the location data ",
         "(check the IDs, and that they are not in dropIDs): ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }

  parse_utc <- function(x) suppressWarnings(ymd_hms(x, truncated = 3, tz = "UTC", quiet = TRUE))
  start <- parse_utc(trims$start_date)
  end <- parse_utc(trims$end_date)

  bad <- c(trims$ref[!is.na(trims$start_date) & is.na(start)],
           trims$ref[!is.na(trims$end_date) & is.na(end)])
  if (length(bad) > 0) {
    stop("Unreadable dates in ", file, " for: ", paste(unique(bad), collapse = ", "),
         ". Use YYYY-MM-DD or YYYY-MM-DD HH:MM:SS (UTC).", call. = FALSE)
  }

  inverted <- trims$ref[!is.na(start) & !is.na(end) & start >= end]
  if (length(inverted) > 0) {
    stop("start_date is not before end_date in ", file, " for: ",
         paste(inverted, collapse = ", "), call. = FALSE)
  }

  for (i in seq_len(nrow(trims))) {
    j <- which(as.character(locs_sf[[id_var]]) == trims$ref[i])
    d <- locs_sf$d_sf[[j]]
    keep_rows <- rep(TRUE, nrow(d))
    if (!is.na(end[i])) keep_rows <- keep_rows & d$date < end[i]
    if (!is.na(start[i])) keep_rows <- keep_rows & d$date >= start[i]

    if (sum(keep_rows) == 0) {
      stop("Truncation removes every location for ", trims$ref[i], ". Check its dates in ",
           file, ".", call. = FALSE)
    }

    locs_sf$d_sf[[j]] <- d[keep_rows, ]

    message(sprintf("  %s: %d of %d locations removed (%s%s%s)",
                    trims$ref[i], sum(!keep_rows), length(keep_rows),
                    ifelse(is.na(start[i]), "", paste0("before ", format(start[i], "%Y-%m-%d %H:%M:%S"))),
                    ifelse(!is.na(start[i]) & !is.na(end[i]), "; ", ""),
                    ifelse(is.na(end[i]), "", paste0("at or after ", format(end[i], "%Y-%m-%d %H:%M:%S")))))
  }

  return(locs_sf)
}
