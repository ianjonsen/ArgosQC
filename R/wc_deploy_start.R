## Apply the deployment start (T0) to Wildlife Computers tag data and metadata
##  (internal)
##
## The Wildlife Computers equivalent of smru_deployment_start(). Works out T0 for
## each deployment (see qc_deployment_start()) and sets the metadata column
## QC_start_date to T0, or to a later manual QC start (QC_start_datetime in the
## metadata file). Every Wildlife Computers table is cut at QC_start_date when
## the SSM-estimated locations are appended (wc_append_ssm()), and the
## Locations and FastGPS tables are cut here. Locations within 15 km of the
## Wildlife Computers headquarters (Redmond, WA, USA) are dropped.
##
##  from_file: logical; TRUE where the metadata come from a metadata file (ATN,
##   IRAP: their deployment date, time and position are used if consistent with
##   the data); FALSE where they were built from the Wildlife Computers Portal
##   (T0 is calculated)
## Returns list(wc, meta, starts, notes); notes are named by message type
wc_deployment_start <- function(wc, meta, from_file = TRUE, site_km = 10, meta_km = 100) {

  ids <- as.character(meta$DeploymentID)

  ## Argos locations and FastGPS fixes
  locs <- data.frame(ref = as.character(wc$Locations$DeploymentID),
                     date = as.POSIXct(wc$Locations$Date, tz = "UTC"),
                     lc = as.character(wc$Locations$Quality),
                     lat = suppressWarnings(as.numeric(wc$Locations$Latitude)),
                     lon = suppressWarnings(as.numeric(wc$Locations$Longitude)))
  gps <- wc$FastGPS
  if (is.data.frame(gps) && nrow(gps) > 0 &&
      all(c("Day", "Time", "Latitude", "Longitude") %in% names(gps))) {
    locs <- rbind(locs, data.frame(
      ref = as.character(gps$DeploymentID),
      date = suppressWarnings(lubridate::dmy_hms(paste(gps$Day, gps$Time), tz = "UTC")),
      lc = "G",
      lat = suppressWarnings(as.numeric(gps$Latitude)),
      lon = suppressWarnings(as.numeric(gps$Longitude))))
  }
  locs$lc[is.na(locs$lc)] <- "Z"
  locs <- locs[locs$ref %in% ids, ]
  locs <- qc_drop_null_island(locs, quiet = TRUE)
  locs <- qc_drop_hq(locs, "wc")
  hq <- attr(locs, "dropped")

  ## metadata deployment start: ATN DeploymentStartDateTime; IRAP and Portal
  ##  deploy_date (a date and time, where its time is not midnight)
  col <- function(v) {
    v <- intersect(v, names(meta))
    if (length(v) == 0) NULL else meta[[v[1]]]
  }
  num <- function(v) if (is.null(v)) rep(NA_real_, length(ids)) else
    suppressWarnings(as.numeric(v))
  tm <- function(v) if (is.null(v)) as.POSIXct(rep(NA_real_, length(ids)), tz = "UTC") else
    as.POSIXct(v, tz = "UTC")
  rdate <- tm(col(c("DeploymentStartDateTime", "deploy_date")))
  rtime <- as.POSIXct(rep(NA_real_, length(ids)), tz = "UTC")
  has_clock <- !is.na(rdate) & format(rdate, "%H:%M:%S", tz = "UTC") != "00:00:00"
  rtime[has_clock] <- rdate[has_clock]
  ms <- data.frame(ref = ids, date = rdate, time = rtime,
                   lat = num(col(c("deploy_latitude", "deploy_lat", "DeploymentLatitude",
                                   "DeployLatitude", "release_latitude"))),
                   lon = num(col(c("deploy_longitude", "deploy_lon", "DeploymentLongitude",
                                   "DeployLongitude", "release_longitude"))))
  if (length(from_file) == 1) from_file <- rep(from_file, length(ids))
  ms <- ms[from_file, , drop = FALSE]

  starts <- qc_deployment_start(locs, meta_start = if (nrow(ms) > 0) ms else NULL,
                                site_km = site_km, meta_km = meta_km)

  ## QC start = T0, or a later manual QC start
  i <- match(ids, starts$ref)
  T0 <- as.POSIXct(starts$T0[i], tz = "UTC")
  manual <- as.POSIXct(meta$QC_start_date, tz = "UTC")
  qs <- T0
  later <- !is.na(manual) & (is.na(qs) | manual > qs)
  qs[later] <- manual[later]
  meta$QC_start_date <- qs

  ## Portal-built metadata: the calculated deployment start and position go in
  ##  the embark columns
  if (all(c("embark_date", "embark_longitude", "embark_latitude") %in% names(meta))) {
    ok <- !is.na(i) & starts$pos_from_data[i] %in% TRUE & !from_file
    ## only where the Portal has no deployment date or position (where the
    ##  embark columns were calculated from the locations)
    if ("deploy_date" %in% names(meta)) {
      lon_col <- intersect(c("deploy_longitude", "deploy_lon"), names(meta))[1]
      no_pos <- if (!is.na(lon_col)) is.na(meta[[lon_col]]) else rep(TRUE, nrow(meta))
      ok <- ok & (is.na(meta$deploy_date) | no_pos)
    }
    meta$embark_date <- as.POSIXct(meta$embark_date, tz = "UTC")
    meta$embark_date[ok] <- T0[ok]
    meta$embark_longitude[ok] <- starts$lon0[i][ok]
    meta$embark_latitude[ok] <- starts$lat0[i][ok]
  }

  ## remove locations before the QC start, and headquarters locations
  cut_tab <- function(x, t) {
    q <- meta$QC_start_date[match(as.character(x$DeploymentID), ids)]
    keep <- is.na(q) | is.na(t) | t >= q
    x[keep, , drop = FALSE]
  }
  n0 <- nrow(wc$Locations)
  lat <- suppressWarnings(as.numeric(wc$Locations$Latitude))
  lon <- suppressWarnings(as.numeric(wc$Locations$Longitude))
  hq0 <- .qc_manufacturer_hq[.qc_manufacturer_hq$maker == "wc", ]
  at_hq <- !is.na(lat) & !is.na(lon) & dm_dist_km(lat, lon, hq0$lat, hq0$lon) <= 15
  wc$Locations <- wc$Locations[!at_hq, , drop = FALSE]
  wc$Locations <- cut_tab(wc$Locations, as.POSIXct(wc$Locations$Date, tz = "UTC"))
  n_loc <- n0 - nrow(wc$Locations)
  n_gps <- 0
  if (is.data.frame(gps) && nrow(gps) > 0 && all(c("Day", "Time") %in% names(gps))) {
    g0 <- nrow(wc$FastGPS)
    wc$FastGPS <- cut_tab(wc$FastGPS, suppressWarnings(
      lubridate::dmy_hms(paste(wc$FastGPS$Day, wc$FastGPS$Time), tz = "UTC")))
    n_gps <- g0 - nrow(wc$FastGPS)
  }

  ## notes
  notes <- character(0)
  if (length(hq) > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Locations within 15 km of the Wildlife Computers headquarters (Redmond) dropped: ",
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
  if (any(later)) {
    notes <- c(notes, stats::setNames(paste0(
      "Manual QC start (QC_start_datetime) after the deployment start used: ",
      paste(ids[later], collapse = ", ")), "progress"))
  }
  if (n_loc + n_gps > 0) {
    notes <- c(notes, stats::setNames(paste0(
      "Records before the deployment start (or at the headquarters) removed: Locations ",
      n_loc, ", FastGPS ", n_gps, "; other tables are cut when QC'd locations are appended"),
      "excluded"))
  }

  list(wc = wc, meta = meta, starts = starts, notes = notes)
}
