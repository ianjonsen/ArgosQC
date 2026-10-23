## Delayed-mode deployment metadata (internal)
##
## In delayed-mode QC the IMOS metadata file is read first. Then:
##  1) every deployment's ptt, body, device_wmo_ref and tag_type are checked
##     against the campaign page on the SMRU data server; where they differ,
##     the SMRU server value is used and the difference is reported. The QC
##     stops if the SMRU server cannot be reached;
##  2) with build.meta = TRUE, deployments in the tag data but not in the
##     metadata file are built from the SMRU server (identifiers), the .mdb
##     deployments table (LOCATION), the Argos locations (release date and
##     position) and the config meta block (species; fallback release site).
##     A LOCATION is used only if its site lies within max_km of the release
##     position estimated from the Argos locations.
## Called by smru_clean_meta_imos(); used by smru_qc(), smru_suggest_trim()
##  and imos_smru_dm_qc().

## fixed coordinates for release sites with none in the metadata file
.imos_site_coords_fixed <- data.frame(release_site = "Casey",
                                      lat = -66.2825,
                                      lon = 110.5257)

## great-circle distance (km)
dm_dist_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}

## tag identifiers from the campaign page(s) on the SMRU data server. Columns
##  are read by their header names, which differ between campaign pages
smru_server_meta <- function(cid, user, pwd) {
  out <- lapply(cid, function(x) {
    url <- paste0("https://www.smru.st-andrews.ac.uk/protected/", x, "/", x, ".html")
    resp <- tryCatch(
      httr2::request(url) |>
        httr2::req_auth_basic(user, pwd) |>
        httr2::req_timeout(60) |>
        httr2::req_perform(),
      error = function(e) e)
    if (inherits(resp, "error")) {
      stop("Delayed-mode QC stopped: the SMRU data server page for ", x,
           " could not be read with the login in the config file (",
           conditionMessage(resp), ").", call. = FALSE)
    }
    tab <- httr2::resp_body_string(resp) |>
      rvest::read_html() |>
      rvest::html_element(xpath = "/html/body/ul[1]/table") |>
      rvest::html_table(convert = FALSE)
    names(tab) <- tolower(trimws(names(tab)))
    if (!all(c("reference", "ptt") %in% names(tab))) {
      stop("Delayed-mode QC stopped: no tag table found on the SMRU data server page for ",
           x, ".", call. = FALSE)
    }
    col <- function(n) if (n %in% names(tab)) trimws(as.character(tab[[n]])) else
      rep(NA_character_, nrow(tab))
    clean <- function(v) ifelse(is.na(v) | v %in% c("", "-", "NA"), NA_character_, v)
    data.frame(device_id = col("reference"),
               ptt = suppressWarnings(as.integer(clean(col("ptt")))),
               body = suppressWarnings(as.integer(clean(col("body")))),
               device_wmo_ref = clean(col("wmo")),
               tag_type = clean(col("parameters")),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

## LOCATION and SPECIES of each tag from the .mdb deployments table; NULL if
##  the table cannot be read
smru_mdb_deployments <- function(cid, path2mdb, p2mdbtools = NULL) {
  exe <- if (is.null(p2mdbtools)) "mdb-export" else file.path(p2mdbtools, "mdb-export")
  out <- lapply(cid, function(x) {
    f <- file.path(path2mdb, paste0(x, ".mdb"))
    txt <- tryCatch(suppressWarnings(system2(exe, c(shQuote(f), "deployments"),
                                             stdout = TRUE, stderr = FALSE)),
                    error = function(e) character(0))
    if (length(txt) < 2) return(NULL)
    d <- utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE)
    names(d) <- tolower(names(d))
    if (!"ref" %in% names(d)) return(NULL)
    data.frame(device_id = as.character(d$ref),
               location = if ("location" %in% names(d)) as.character(d$location) else NA_character_,
               mdb_species = if ("species" %in% names(d)) as.character(d$species) else NA_character_,
               stringsAsFactors = FALSE)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) NULL else do.call(rbind, out)
}

## standard release site from a free-text .mdb LOCATION, e.g.
##  "Australia, West Island" -> "West Island", "Montague Is,Australia" ->
##  "Montague Island". NA if no specific known site remains
imos_resolve_location <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
    y <- tolower(s)
    y <- gsub("south australia|western australia|new south wales|australia|new zealand",
              " ", y)
    y <- gsub("[,;]", " ", y)
    y <- gsub("\\bis\\b\\.?", "island", y)
    y <- gsub("^\\s*s\\s+", "south ", y)
    y <- imos_norm(y)
    if (!nzchar(y)) return(NA_character_)
    known <- .imos_sites$release_site
    kn <- imos_norm(known)
    hit <- imos_site(y)
    if (!is.na(hit) && imos_norm(hit) %in% kn) return(known[match(imos_norm(hit), kn)])
    y2 <- paste(y, "island")
    if (y2 %in% kn) return(known[match(y2, kn)])
    part <- which(grepl(paste0("\\b", y, "\\b"), kn))
    if (length(part) == 1) return(known[part])
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

## release site coordinates: the median Lat and Long of each standard site in
##  the metadata file, plus fixed coordinates for sites with none
imos_site_coords <- function(file) {
  md <- suppressWarnings(suppressMessages(
    readr::read_csv(file, col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE)))
  names(md) <- trimws(names(md))
  d <- data.frame(release_site = imos_site(md$Location),
                  lat = suppressWarnings(as.numeric(md$Lat)),
                  lon = suppressWarnings(as.numeric(md$Long)))
  d <- d[!is.na(d$release_site) & !is.na(d$lat) & !is.na(d$lon), ]
  s <- if (nrow(d) > 0) {
    stats::aggregate(cbind(lat, lon) ~ release_site, data = d, FUN = stats::median)
  } else {
    data.frame(release_site = character(0), lat = numeric(0), lon = numeric(0))
  }
  rbind(s, .imos_site_coords_fixed[!.imos_site_coords_fixed$release_site %in% s$release_site, ])
}

## release date and position estimated from the Argos locations. Locations
##  (class Z excluded) are split at gaps of more than gap_days; leading
##  segments spanning less than test_days and followed by such a gap are
##  pre-deployment tests (e.g. at SMRU, or where the tags were prepared) and are
##  dropped. The release position is the median of the class 1-3 locations
##  (any class if none) on the first day of what remains
smru_release_estimate <- function(diag, refs, gap_days = 5, test_days = 3) {
  out <- lapply(refs, function(r) {
    d <- diag[as.character(diag$ref) == r & !is.na(diag$d_date) & !is.na(diag$lat) &
                !is.na(diag$lon) & !(diag$lq %in% -9), ]
    if (nrow(d) == 0) {
      return(data.frame(device_id = r, release_date = as.POSIXct(NA, tz = "UTC"),
                        start_time = as.POSIXct(NA, tz = "UTC"),
                        est_lat = NA_real_, est_lon = NA_real_, n_tests = 0L))
    }
    d <- d[order(d$d_date), ]
    t <- as.numeric(d$d_date)
    seg <- cumsum(c(1, diff(t) > gap_days * 86400))
    n_tests <- 0L
    while (length(unique(seg)) > 1) {
      s1 <- seg == min(seg)
      if (diff(range(t[s1])) / 86400 >= test_days) break
      d <- d[!s1, ]; t <- t[!s1]; seg <- seg[!s1]
      n_tests <- n_tests + 1L
    }
    first <- d[t < t[1] + 86400, ]
    good <- first[first$lq %in% 1:3, ]
    if (nrow(good) == 0) good <- first
    data.frame(device_id = r,
               release_date = as.POSIXct(format(d$d_date[1], "%Y-%m-%d", tz = "UTC"), tz = "UTC"),
               start_time = d$d_date[1],
               est_lat = stats::median(good$lat),
               est_lon = stats::median(good$lon),
               n_tests = n_tests)
  })
  do.call(rbind, out)
}

## known release sites visited early in the track: the daily median positions
##  (class 1-3 locations) in the first early_days after start_time, matched to
##  the nearest known site within early_km. Returns the first such site and
##  day, or NA. Seals haul out at or near their release site soon after
##  release, so this finds the site when the first locations are at sea or at a
##  pre-deployment test site
smru_early_site <- function(diag, ref, start_time, coords, early_days = 7, early_km = 10) {
  none <- list(site = NA_character_, km = NA_real_, day = as.Date(NA))
  if (is.na(start_time) || nrow(coords) == 0) return(none)
  d <- diag[as.character(diag$ref) == ref & !is.na(diag$d_date) & !is.na(diag$lat) &
              !is.na(diag$lon) & diag$lq %in% 1:3 & diag$d_date >= start_time &
              diag$d_date < start_time + early_days * 86400, ]
  if (nrow(d) == 0) return(none)
  day <- as.Date(d$d_date, tz = "UTC")
  for (dy in sort(unique(day))) {
    lat <- stats::median(d$lat[day == dy])
    lon <- stats::median(d$lon[day == dy])
    dd <- dm_dist_km(lat, lon, coords$lat, coords$lon)
    if (min(dd) <= early_km) {
      return(list(site = coords$release_site[which.min(dd)], km = min(dd),
                  day = as.Date(dy, origin = "1970-01-01")))
    }
  }
  none
}

## apply the SMRU server identifiers to the metadata file rows, and build the
##  deployments missing from the metadata file (build.meta = TRUE). Returns the
##  metadata with attribute "built", a table describing each built deployment
smru_meta_dm <- function(meta,
                         cid,
                         smru,
                         dropIDs = NULL,
                         file,
                         user,
                         pwd,
                         build.meta = FALSE,
                         meta.args = NULL,
                         path2mdb = NULL,
                         p2mdbtools = NULL,
                         max_km = 50,
                         early_days = 7,
                         early_km = 10) {

  server <- smru_server_meta(cid, user, pwd)
  notes <- character(0)
  note <- function(...) notes <<- c(notes, paste0(...))

  ## 1) the SMRU server is correct where it differs from the metadata file
  meta$ptt <- as.integer(meta$ptt)
  meta$body <- as.integer(meta$body)
  meta$device_wmo_ref <- as.character(meta$device_wmo_ref)
  meta$tag_type <- as.character(meta$tag_type)
  m <- match(meta$device_id, server$device_id)
  for (v in c("ptt", "body", "device_wmo_ref", "tag_type")) {
    sv <- server[[v]][m]
    fv <- meta[[v]]
    diff <- !is.na(sv) & (is.na(fv) | as.character(fv) != as.character(sv))
    if (any(diff)) {
      note("SMRU server value used for ", v, ": ",
              paste0(meta$device_id[diff], " (metadata file: ", fv[diff], "; SMRU server: ",
                     sv[diff], ")", collapse = "; "))
      meta[[v]][diff] <- sv[diff]
    }
    keep <- is.na(sv) & !is.na(fv) & !is.na(m)
    if (v == "device_wmo_ref" && any(keep)) {
      note("No WMO ID on the SMRU server for ", paste(meta$device_id[keep], collapse = ", "),
              "; the metadata file value is kept.")
    }
  }
  notfound <- meta$device_id[is.na(m)]
  if (length(notfound) > 0) {
    note("In the metadata file but not on the SMRU server page: ",
            paste(notfound, collapse = ", "))
  }

  ## 2) deployments with tag data but no metadata file entry
  in_data <- unique(as.character(smru$diag$ref[!is.na(smru$diag$lat)]))
  in_data <- in_data[smru_cid(in_data) %in% cid]
  missing <- setdiff(in_data, c(meta$device_id, dropIDs))
  attr(meta, "built") <- NULL
  attr(meta, "notes") <- NULL
  if (length(missing) == 0) {
    attr(meta, "notes") <- notes
    return(meta)
  }
  if (!isTRUE(build.meta)) {
    note("Deployments in the tag data but not in the metadata file (excluded from the QC; ",
            "set \"build.meta\": true in the config setup block to build their metadata): ",
            paste(sort(missing), collapse = ", "))
    attr(meta, "notes") <- notes
    return(meta)
  }

  rel <- smru_release_estimate(smru$diag, missing)
  dep <- if (!is.null(path2mdb)) smru_mdb_deployments(cid, path2mdb, p2mdbtools) else NULL
  coords <- imos_site_coords(file)
  cfg_site <- if (!is.null(meta.args$release_site) && !grepl(";", meta.args$release_site)) {
    imos_site(meta.args$release_site)
  } else NA_character_

  site_dist <- function(site, lat, lon) {
    i <- match(site, coords$release_site)
    if (is.na(site) || is.na(i) || is.na(lat)) return(NA_real_)
    dm_dist_km(lat, lon, coords$lat[i], coords$lon[i])
  }

  review <- lapply(seq_len(nrow(rel)), function(k) {
    r <- rel[k, ]
    loc <- if (!is.null(dep)) dep$location[match(r$device_id, dep$device_id)] else NA_character_
    mdb_sp <- if (!is.null(dep)) dep$mdb_species[match(r$device_id, dep$device_id)] else NA_character_
    loc_site <- imos_resolve_location(loc)
    d_loc <- site_dist(loc_site, r$est_lat, r$est_lon)
    near <- NA_character_; d_near <- NA_real_
    if (!is.na(r$est_lat) && nrow(coords) > 0) {
      dd <- dm_dist_km(r$est_lat, r$est_lon, coords$lat, coords$lon)
      near <- coords$release_site[which.min(dd)]
      d_near <- min(dd)
    }
    d_cfg <- site_dist(cfg_site, r$est_lat, r$est_lon)
    early <- smru_early_site(smru$diag, r$device_id, r$start_time, coords,
                             early_days = early_days, early_km = early_km)
    if (!is.na(d_loc) && d_loc <= max_km) {
      site <- loc_site; src <- ".mdb LOCATION"
    } else if (!is.na(early$site) && identical(early$site, loc_site)) {
      site <- loc_site
      src <- paste0(".mdb LOCATION, confirmed by locations on ", early$day)
    } else if (!is.na(early$site)) {
      site <- early$site
      src <- paste0("known site within ", round(early$km), " km of the locations on ", early$day)
    } else if (!is.na(d_near) && d_near <= max_km) {
      site <- near; src <- "nearest known site"
    } else if (!is.na(d_cfg) && d_cfg <= max_km) {
      site <- cfg_site; src <- "config meta block"
    } else {
      site <- NA_character_; src <- "REVIEW: no known site within the distance limit"
    }
    data.frame(device_id = r$device_id,
               mdb_location = loc, mdb_species = mdb_sp,
               location_site = loc_site, location_km = round(d_loc),
               release_date = r$release_date, start_time = r$start_time,
               est_lat = round(r$est_lat, 3),
               est_lon = round(r$est_lon, 3), tests_dropped = r$n_tests,
               nearest_site = near, nearest_km = round(d_near),
               release_site = site, site_source = src,
               stringsAsFactors = FALSE)
  })
  review <- do.call(rbind, review)

  ## new rows with the same columns and classes as the metadata file rows
  built <- meta[rep(NA_integer_, nrow(review)), , drop = FALSE]
  sv <- server[match(review$device_id, server$device_id), ]
  built$sattag_program <- smru_cid(review$device_id)
  built$device_id <- review$device_id
  built$ptt <- as.integer(sv$ptt)
  built$body <- as.integer(sv$body)
  built$device_wmo_ref <- as.character(sv$device_wmo_ref)
  built$tag_type <- as.character(sv$tag_type)
  sp <- if (!is.null(meta.args$species)) meta.args$species else NA_character_
  built$species <- sp
  built$common_name <- imos_common_name(sp, if (!is.null(meta.args$common_name))
    meta.args$common_name else NA_character_)
  built$release_site <- review$release_site
  sc <- imos_state_country(review$release_site)
  if (!is.null(meta.args$state_country) && !grepl(";", meta.args$state_country)) {
    sc[is.na(sc)] <- meta.args$state_country
  }
  built$state_country <- sc
  built$release_latitude <- review$est_lat
  built$release_longitude <- review$est_lon
  built$release_date <- if (inherits(meta$release_date, "Date")) {
    as.Date(review$release_date)
  } else review$release_date

  review$species <- sp
  note("Metadata built for ", nrow(review), " deployment(s) not in the metadata file: ",
          paste0(review$device_id, " (", ifelse(is.na(review$release_site), "site unknown",
                                                review$release_site), ")", collapse = ", "))
  out <- dplyr::bind_rows(meta, built)
  attr(out, "built") <- review
  attr(out, "notes") <- notes
  out
}
