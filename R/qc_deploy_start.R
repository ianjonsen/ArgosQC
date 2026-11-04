## Deployment start (T0): time and position (internal)
##
## Every QC track starts at the deployment start, T0, with no end truncation in
## near real-time QC (delayed-mode track ends come from the trimIDs file).
## Records before T0 are removed from the SSM input and from every output table.
##
## T0 from the metadata (release date, time and position) is used when it is
## consistent with the data. Otherwise T0 is calculated from the locations:
##  1) locations within 15 km of the tag manufacturer's headquarters are dropped
##     (SMRU: St Andrews, UK; Wildlife Computers: Redmond, WA, USA), as are
##     Argos placeholder locations at 0, 0;
##  2) pre-deployment test bursts are dropped: leading segments of locations
##     (split at gaps of more than gap_days) that span less than test_days;
##  3) the deployment position is the median of the class 1-3 (or GPS)
##     locations in the first full 24 h of the remaining locations, taken
##     around the class 1-3 location with the most locations within site_km
##     (so an outlying location cannot pull the position away from the site);
##  4) T0 is the first location within site_km of that position.
## With a metadata release date and time (IMOS: TDeploy, UTC), T0 is that time.
## The deployment position is the median of the class 1-3 (or GPS) locations in
## the 12 h after T0, or the 24 h after T0 if there are none in 12 h (metadata
## positions are often approximate, e.g. an island centroid), or the metadata
## position if there are none in 24 h. Lower-quality locations are not used:
## for an animal that leaves the site quickly they place the release at sea. The metadata time is rejected, and T0
## calculated, only if the data contradict it: no locations after it, or the
## deployment position more than meta_km (default 100 km) from the metadata
## release position.
## With a metadata release date but no time, T0 is the first class 1-3 (or GPS)
## location within site_km of the metadata release position on the release date;
## if there is none, T0 is calculated.

## tag manufacturers' headquarters
.qc_manufacturer_hq <- data.frame(maker = c("smru", "wc"),
                                  place = c("St Andrews, UK", "Redmond, WA, USA"),
                                  lat = c(56.3398, 47.6740),
                                  lon = c(-2.7967, -122.1215))

## drop locations within km of a manufacturer's headquarters; returns the kept
##  rows, with the number dropped per reference in attribute "dropped"
qc_drop_hq <- function(d, maker, km = 15) {
  hq <- .qc_manufacturer_hq[.qc_manufacturer_hq$maker == maker, ]
  if (nrow(hq) == 0 || nrow(d) == 0) return(d)
  dd <- dm_dist_km(d$lat, d$lon, hq$lat, hq$lon)
  at_hq <- !is.na(dd) & dd <= km
  out <- d[!at_hq, , drop = FALSE]
  attr(out, "dropped") <- table(as.character(d$ref[at_hq]))
  out
}

## T0 for each deployment.
##  locs: data frame with ref, date (POSIXct UTC), lc (character: "3", "2", "1",
##   "0", "A", "B", "Z" or "G" for GPS), lat, lon; headquarters and 0, 0
##   locations already removed
##  meta_start: NULL or a data frame with ref, date (release date), time (release
##   date and time, or NA), lat, lon (release position, or NA)
##  meta_km: a metadata release date and time is rejected if the deployment
##   position from the locations is more than meta_km from the metadata position
##  pos_hours: windows (h) after a metadata release time for the deployment
##   position: the first, or the second if the first has no class 1-3 location
## Returns one row per reference: ref, T0, lat0, lon0, source, pos_from_data
##  (TRUE where lat0, lon0 come from the locations), n_tests, note
qc_deployment_start <- function(locs,
                                meta_start = NULL,
                                site_km = 10,
                                meta_km = 100,
                                pos_hours = c(12, 24),
                                gap_days = 5,
                                test_days = 3) {

  good <- function(lc) as.character(lc) %in% c("1", "2", "3", "G")
  dist_to <- function(d, lat, lon) dm_dist_km(d$lat, d$lon, lat, lon)

  ## deployment position from the candidate locations `cand` (class 1-3 or GPS
  ##  where available): the candidate with the most locations of `support`
  ##  (any class except Z) within site_km, then the median of the candidates
  ##  within site_km of it. A plain median fails when an outlying location is
  ##  among few candidates (e.g. one class 1 location 1,000 km away and one at
  ##  the release site gives a median halfway between them)
  centre <- function(cand, support) {
    if (nrow(support) == 0) support <- cand
    n <- vapply(seq_len(nrow(cand)), function(k)
      sum(dist_to(support, cand$lat[k], cand$lon[k]) <= site_km), numeric(1))
    k <- which.max(n)
    cl <- cand[dist_to(cand, cand$lat[k], cand$lon[k]) <= site_km, ]
    c(lat = stats::median(cl$lat), lon = stats::median(cl$lon))
  }

  calculate <- function(d) {
    d <- d[d$lc != "Z", ]
    if (nrow(d) == 0) return(NULL)
    t <- as.numeric(d$date)
    seg <- cumsum(c(1, diff(t) > gap_days * 86400))
    n_tests <- 0L
    while (length(unique(seg)) > 1) {
      s1 <- seg == min(seg)
      if (diff(range(t[s1])) / 86400 >= test_days) break
      d <- d[!s1, ]; t <- t[!s1]; seg <- seg[!s1]
      n_tests <- n_tests + 1L
    }
    w <- d[t < t[1] + 86400, ]
    g <- w[good(w$lc), ]
    if (nrow(g) == 0) g <- w
    p0 <- centre(g, w)
    lat0 <- unname(p0["lat"])
    lon0 <- unname(p0["lon"])
    near <- d[dist_to(d, lat0, lon0) <= site_km, ]
    T0 <- if (nrow(near) > 0) near$date[1] else d$date[1]
    list(T0 = T0, lat0 = lat0, lon0 = lon0, n_tests = n_tests)
  }

  refs <- unique(as.character(locs$ref))
  out <- lapply(refs, function(r) {
    d <- locs[as.character(locs$ref) == r & !is.na(locs$date) & !is.na(locs$lat) &
                !is.na(locs$lon), ]
    d <- d[order(d$date), ]
    note <- NA_character_
    m <- if (!is.null(meta_start)) meta_start[match(r, meta_start$ref), ] else NULL

    if (!is.null(m) && !is.na(m$ref) && (!is.na(m$time) || !is.na(m$date))) {
      g <- d[good(d$lc), ]
      has_pos <- !is.na(m$lat) && !is.na(m$lon)

      if (!is.na(m$time)) {
        ## metadata release date and time: T0 is the metadata time. The
        ##  position is the median of the class 1-3 (or GPS) locations in the
        ##  pos_hours[1] h after T0 (pos_hours[2] h if there are none; metadata
        ##  release positions are often approximate, e.g. an island centroid),
        ##  or the metadata position if there are none. The metadata time is
        ##  rejected only if the data contradict it
        T0m <- m$time
        nxt <- g[g$date >= T0m & g$date < T0m + pos_hours[1] * 3600, ]
        if (nrow(nxt) == 0 && length(pos_hours) > 1) {
          nxt <- g[g$date >= T0m & g$date < T0m + pos_hours[2] * 3600, ]
        }
        if (nrow(d[d$date >= T0m, ]) == 0) {
          note <- "there are no locations after the metadata release date and time"
        } else {
          if (nrow(nxt) > 0) {
            p0 <- centre(nxt, d[d$lc != "Z" & d$date >= T0m &
                                  d$date < T0m + max(pos_hours) * 3600, ])
            lat0 <- unname(p0["lat"])
            lon0 <- unname(p0["lon"])
            how <- "metadata release date and time; position from the locations"
            from_data <- TRUE
          } else if (has_pos) {
            lat0 <- m$lat
            lon0 <- m$lon
            how <- "metadata release date and time and position"
            from_data <- FALSE
          } else {
            a1 <- d[d$date >= T0m, ][1, ]
            lat0 <- a1$lat
            lon0 <- a1$lon
            how <- "metadata release date and time; position from the first location"
            from_data <- TRUE
          }
          d_meta <- if (has_pos && from_data) dm_dist_km(lat0, lon0, m$lat, m$lon) else NA_real_
          if (is.na(d_meta) || d_meta <= meta_km) {
            return(data.frame(ref = r, T0 = T0m, lat0 = lat0, lon0 = lon0, source = how,
                              pos_from_data = from_data, n_tests = NA_integer_,
                              note = NA_character_))
          }
          note <- sprintf(paste0("the class 1-3 locations after the metadata release date ",
                                 "and time are %.0f km from the metadata release position ",
                                 "(more than %g km)"), d_meta, meta_km)
        }

      } else if (has_pos) {
        ## metadata release date only: T0 is the first class 1-3 (or GPS)
        ##  location within site_km of the metadata release position on the
        ##  release date
        day <- as.Date(m$date, tz = "UTC")
        at_site <- g[dist_to(g, m$lat, m$lon) <= site_km, ]
        on_day <- at_site[as.Date(at_site$date, tz = "UTC") == day, ]
        if (nrow(on_day) > 0) {
          return(data.frame(ref = r, T0 = on_day$date[1], lat0 = m$lat, lon0 = m$lon,
                            source = paste0("first class 1-3 location at the metadata ",
                                            "release site on the release date"),
                            pos_from_data = FALSE, n_tests = NA_integer_,
                            note = NA_character_))
        }
        note <- paste0("no class 1-3 location within ", site_km, " km of the metadata release ",
                       "position on the release date")
      }
    }

    cc <- calculate(d)
    if (is.null(cc)) {
      return(data.frame(ref = r, T0 = as.POSIXct(NA, tz = "UTC"), lat0 = NA_real_,
                        lon0 = NA_real_, source = "none: no locations",
                        pos_from_data = FALSE, n_tests = 0L, note = note))
    }
    data.frame(ref = r, T0 = cc$T0, lat0 = cc$lat0, lon0 = cc$lon0,
               source = "calculated from the locations", pos_from_data = TRUE,
               n_tests = cc$n_tests, note = note)
  })
  if (length(out) == 0) {
    return(data.frame(ref = character(0), T0 = as.POSIXct(character(0), tz = "UTC"),
                      lat0 = numeric(0), lon0 = numeric(0), source = character(0),
                      pos_from_data = logical(0), n_tests = integer(0),
                      note = character(0)))
  }
  out <- do.call(rbind, out)
  out$T0 <- as.POSIXct(out$T0, tz = "UTC")
  out
}

## seconds after midnight from a clock time ("15:06", "10:35:00", "3:21:00pm",
##  "9:31:00 AM"); NA where empty or not a valid clock time
qc_parse_clock <- function(x) {
  x <- toupper(gsub("\\s+", "", as.character(x)))
  m <- regmatches(x, regexec("^([0-9]{1,2}):([0-9]{2})(:([0-9]{2}))?(AM|PM)?$", x))
  vapply(m, function(z) {
    if (length(z) == 0) return(NA_real_)
    h <- as.numeric(z[2]); mi <- as.numeric(z[3])
    s <- if (z[5] == "") 0 else as.numeric(z[5])
    if (z[6] != "") {
      if (h < 1 || h > 12) return(NA_real_)
      if (z[6] == "PM" && h < 12) h <- h + 12
      if (z[6] == "AM" && h == 12) h <- 0
    }
    if (h > 23 || mi > 59 || s > 59) return(NA_real_)
    h * 3600 + mi * 60 + s
  }, numeric(1))
}
