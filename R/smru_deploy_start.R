## Apply the deployment start (T0) to SMRU tag data and metadata (internal)
##
## Works out T0 for each deployment (see qc_deployment_start()), removes every
## record before T0 from the diag, gps, dive, ctd, haulout, summary and cruise
## tables, and sets the metadata release_date to T0 and, where the deployment
## position comes from the locations, the release position to that position.
## Adds column deploy_start to the metadata. Used in
## near real-time and delayed-mode QC by smru_qc() and smru_suggest_trim().
##
##  from_file: logical, one per metadata row: TRUE where the row comes from a
##   metadata file (its release date, time and position are used if consistent
##   with the data); FALSE where it was built from the SMRU server or the data
##   (T0 is calculated)
##  fixed: NULL, or deployment starts to use as given (ref, T0, lat0, lon0,
##   source), e.g. final starts from the provisional metadata file
## Returns list(smru, meta, starts, notes); notes are named by message type
smru_deployment_start <- function(smru, meta, from_file = NULL, site_km = 10,
                                  meta_km = 100, fixed = NULL) {

  id_col <- intersect(c("device_id", "DeploymentID"), names(meta))[1]
  if (is.na(id_col)) stop("Unrecognised metadata ID variable", call. = FALSE)
  ids <- as.character(meta[[id_col]])
  if (is.null(from_file)) from_file <- rep(FALSE, length(ids))

  ## locations: Argos (diag) and GPS, without headquarters or 0, 0 locations
  lc_chr <- function(lq) {
    lq <- as.character(lq)
    ifelse(lq == "-1", "A", ifelse(lq == "-2", "B", ifelse(lq == "-9", "Z", lq)))
  }
  locs <- data.frame(ref = as.character(smru$diag$ref), date = smru$diag$d_date,
                     lc = lc_chr(smru$diag$lq), lat = as.numeric(smru$diag$lat),
                     lon = as.numeric(smru$diag$lon))
  if ("gps" %in% names(smru) && is.data.frame(smru$gps) && nrow(smru$gps) > 0) {
    locs <- rbind(locs, data.frame(ref = as.character(smru$gps$ref), date = smru$gps$d_date,
                                   lc = "G", lat = as.numeric(smru$gps$lat),
                                   lon = as.numeric(smru$gps$lon)))
  }
  locs <- locs[locs$ref %in% ids, ]
  locs <- qc_drop_null_island(locs, quiet = TRUE)
  locs <- qc_drop_hq(locs, "smru")

  ## metadata deployment start, where the row comes from a metadata file:
  ##  release date, release date and time (IMOS: TDeploy; ATN:
  ##  DeploymentStartDateTime; OTN: release_date, when it has a time), and
  ##  release position
  col <- function(v) {
    v <- intersect(v, names(meta))
    if (length(v) == 0) NULL else meta[[v[1]]]
  }
  num <- function(v) if (is.null(v)) rep(NA_real_, length(ids)) else
    suppressWarnings(as.numeric(v))
  tm <- function(v) if (is.null(v)) as.POSIXct(rep(NA_real_, length(ids)), tz = "UTC") else
    as.POSIXct(v, tz = "UTC")
  rdate <- tm(col(c("release_date", "DeploymentStartDateTime")))
  rtime <- tm(col(c("release_time", "DeploymentStartDateTime")))
  if (!any(c("release_time", "DeploymentStartDateTime") %in% names(meta))) {
    has_clock <- !is.na(rdate) & format(rdate, "%H:%M:%S", tz = "UTC") != "00:00:00"
    rtime[has_clock] <- rdate[has_clock]
  }
  ms <- data.frame(ref = ids, date = rdate, time = rtime,
                   lat = num(col(c("release_latitude", "DeploymentLatitude", "deploy_latitude"))),
                   lon = num(col(c("release_longitude", "DeploymentLongitude", "deploy_longitude"))))
  ## a release position filled in from the data (not in the metadata file) is
  ##  not used to check the metadata release date and time
  if ("release_pos_file" %in% names(meta)) {
    filled <- !meta$release_pos_file %in% TRUE
    ms$lat[filled] <- NA_real_
    ms$lon[filled] <- NA_real_
  }
  ms <- ms[from_file, , drop = FALSE]

  if (!is.null(fixed) && nrow(fixed) > 0) {
    hq_dropped <- attr(locs, "dropped")
    locs <- locs[!locs$ref %in% fixed$ref, ]
    attr(locs, "dropped") <- hq_dropped
    ms <- ms[!ms$ref %in% fixed$ref, , drop = FALSE]
  }
  starts <- qc_deployment_start(locs, meta_start = if (nrow(ms) > 0) ms else NULL,
                                site_km = site_km, meta_km = meta_km)
  if (!is.null(fixed) && nrow(fixed) > 0) {
    fx <- data.frame(ref = as.character(fixed$ref), T0 = as.POSIXct(fixed$T0, tz = "UTC"),
                     lat0 = as.numeric(fixed$lat0), lon0 = as.numeric(fixed$lon0),
                     source = as.character(fixed$source), pos_from_data = FALSE,
                     n_tests = NA_integer_, note = NA_character_)
    fx <- fx[fx$ref %in% ids, ]
    starts <- rbind(starts, fx)
  }

  ## remove records before T0 from every table
  n_removed <- list()
  cut <- function(tab, col) {
    if (!tab %in% names(smru) || !is.data.frame(smru[[tab]]) || nrow(smru[[tab]]) == 0 ||
        !col %in% names(smru[[tab]])) return(invisible(NULL))
    x <- smru[[tab]]
    T0 <- starts$T0[match(as.character(x$ref), starts$ref)]
    t <- as.POSIXct(x[[col]], tz = "UTC")
    drop <- !is.na(T0) & !is.na(t) & t < T0
    if (any(drop)) n_removed[[tab]] <<- table(as.character(x$ref[drop]))
    smru[[tab]] <<- x[!drop, , drop = FALSE]
  }
  cut("diag", "d_date")
  cut("gps", "d_date")
  cut("dive", "de_date")
  cut("ctd", "end_date")
  cut("haulout", "e_date")
  cut("summary", "e_date")
  cut("cruise", "e_date")

  ## metadata: release date = T0; release position = the deployment position
  ##  where it comes from the locations
  i <- match(ids, starts$ref)
  ok <- !is.na(i) & !is.na(starts$T0[i])
  meta$deploy_start <- as.POSIXct(starts$T0[i], tz = "UTC")
  if ("release_date" %in% names(meta)) {
    rd <- as.POSIXct(meta$release_date, tz = "UTC")
    rd[ok] <- starts$T0[i][ok]
    meta$release_date <- rd
  }
  calc <- ok & starts$pos_from_data[i] %in% TRUE
  if (all(c("release_latitude", "release_longitude") %in% names(meta))) {
    meta$release_latitude[calc] <- starts$lat0[i][calc]
    meta$release_longitude[calc] <- starts$lon0[i][calc]
  }

  ## notes
  notes <- character(0)
  hq <- attr(locs, "dropped")
  if (length(hq) > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Locations within 15 km of the SMRU headquarters (St Andrews) dropped: ",
      paste0(names(hq), " (", as.integer(hq), ")", collapse = ", ")), "excluded"))
  }
  for (k in seq_len(nrow(starts))) {
    s <- starts[k, ]
    txt <- paste0(s$ref, ": deployment start ", format(s$T0, "%Y-%m-%d %H:%M:%S"), " UTC at ",
                  round(s$lat0, 3), ", ", round(s$lon0, 3), " (", s$source,
                  if (!is.na(s$n_tests) && s$n_tests > 0) paste0("; ", s$n_tests,
                                                                 " test burst(s) dropped"),
                  ")")
    notes <- c(notes, stats::setNames(txt, "sites"))
    if (!is.na(s$note)) {
      notes <- c(notes, stats::setNames(paste0(s$ref, ": metadata deployment start not used: ",
                                               s$note, "; deployment start calculated from ",
                                               "the locations instead"), "review"))
    }
  }
  if (length(n_removed) > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Records before the deployment start removed: ",
      paste(vapply(names(n_removed), function(tb) paste0(tb, " ", sum(n_removed[[tb]])),
                   character(1)), collapse = ", ")), "excluded"))
  }

  list(smru = smru, meta = meta, starts = starts, notes = notes)
}

## which metadata rows come from a metadata file (TRUE), rather than being built
##  from the SMRU server or the tag data (FALSE: T0 is calculated)
qc_meta_from_file <- function(meta, meta.source) {
  id_col <- intersect(c("device_id", "DeploymentID"), names(meta))[1]
  n <- nrow(meta)
  if (identical(meta.source, "smru")) return(rep(FALSE, n))
  out <- rep(TRUE, n)
  built <- attr(meta, "built")
  if (!is.null(built) && nrow(built) > 0) {
    out[as.character(meta[[id_col]]) %in% as.character(built$device_id)] <- FALSE
  }
  out
}
