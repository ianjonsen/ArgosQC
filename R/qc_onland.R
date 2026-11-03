## On-land intervals from haul-out and dive records (internal)
##
## SSM-estimated locations within an on-land interval are not re-routed off land
## (see aniMotum::route_path(), argument `onland`). The tag is on land:
##  1) during a haul-out record;
##  2) between two haul-out records less than ho.gap hours apart, with no dive
##     record between them (e.g. an animal lying in the surf on the beach, where
##     the wet-dry sensor ends and restarts haul-outs).
## A dive record always means the animal is at sea.
##
## Which haul-out records are used depends on the species (or common name), from
## the config file meta block or, if absent there, the deployment metadata:
##  1) seals, sea lions and fur seals: every haul-out record;
##  2) turtles: only haul-out records that end after the deployment start (T0),
##     start within turtle.ho.days (21) of T0, and lie within turtle.ho.km (5 km) of the T0
##     position. IMOS turtles are mostly females tagged on nesting beaches, and
##     these records are returns to the nesting beach. Other turtle "haul-outs"
##     are dry periods at the sea surface;
##  3) other species: no haul-out records.
## Locations on land at the start and end of a track are always kept on land by
## route_path().

## taxon of each deployment from its species or common name: "pinniped",
##  "turtle" or "other"
qc_taxon <- function(species = NULL, common_name = NULL) {
  txt <- tolower(paste(if (is.null(species)) "" else species,
                       if (is.null(common_name)) "" else common_name))
  pinn <- paste0("seal|sea lion|sealion|walrus|mirounga|leptonychotes|hydrurga|lobodon|",
                 "ommatophoca|arctocephalus|neophoca|zalophus|otaria|eumetopias|",
                 "callorhinus|phocarctos|phoca|pusa|halichoerus|histriophoca|pagophilus|",
                 "cystophora|erignathus|monachus|neomonachus|odobenus")
  turt <- paste0("turtle|chelonia|caretta|lepidochelys|eretmochelys|natator|dermochelys")
  ifelse(grepl(pinn, txt), "pinniped", ifelse(grepl(turt, txt), "turtle", "other"))
}

## taxon per deployment: config meta block first, then the metadata columns
##  species and common_name (or the ATN equivalents)
qc_meta_taxon <- function(meta, id_col, conf_meta = NULL) {
  ids <- as.character(meta[[id_col]])
  sp <- conf_meta$species
  cn <- conf_meta$common_name
  if (!is.null(sp) && all(!is.na(sp)) || !is.null(cn) && all(!is.na(cn))) {
    tx <- rep(qc_taxon(if (is.null(sp) || all(is.na(sp))) NULL else sp[1],
                       if (is.null(cn) || all(is.na(cn))) NULL else cn[1]), length(ids))
  } else {
    col <- function(v) {
      v <- intersect(v, names(meta))
      if (length(v) == 0) NULL else as.character(meta[[v[1]]])
    }
    tx <- qc_taxon(col(c("species", "Species", "ScientificName", "scientific_name")),
                   col(c("common_name", "CommonName", "Common name")))
    if (length(tx) != length(ids)) tx <- rep("other", length(ids))
  }
  stats::setNames(tx, ids)
}

## keep the haul-out records to be used, by taxon (see above)
##  ho: id, start, end, lat, lon (position of the haul-out record, or NA)
##  taxon: named by id; starts: qc_deployment_start() output (ref, T0, lat0,
##   lon0); locs: ref, date, lat, lon, used for haul-out records without a
##   position (the nearest location in time, within 12 h)
qc_haulout_taxon <- function(ho, taxon, starts = NULL, locs = NULL,
                             turtle.ho.days = 21, turtle.ho.km = 5) {
  if (is.null(ho) || nrow(ho) == 0) return(ho)
  tx <- unname(taxon[as.character(ho$id)])
  tx[is.na(tx)] <- "other"
  keep <- tx == "pinniped"
  tt <- which(tx == "turtle")
  if (length(tt) > 0 && !is.null(starts) && nrow(starts) > 0) {
    i <- match(as.character(ho$id[tt]), starts$ref)
    T0 <- as.POSIXct(starts$T0[i], tz = "UTC")
    lat <- ho$lat[tt]
    lon <- ho$lon[tt]
    ## haul-out records without a position: the nearest location in time
    miss <- which(is.na(lat) | is.na(lon))
    if (length(miss) > 0 && !is.null(locs) && nrow(locs) > 0) {
      for (k in miss) {
        l <- locs[locs$ref == ho$id[tt[k]], ]
        if (nrow(l) == 0) next
        dt <- abs(as.numeric(difftime(l$date, ho$start[tt[k]], units = "hours")))
        j <- which.min(dt)
        if (length(j) == 1 && dt[j] <= 12) {
          lat[k] <- l$lat[j]
          lon[k] <- l$lon[j]
        }
      }
    }
    ## records that end after T0 and start within turtle.ho.days of T0
    days <- as.numeric(difftime(ho$start[tt], T0, units = "days"))
    after <- ho$end[tt] >= T0
    km <- dm_dist_km(lat, lon, starts$lat0[i], starts$lon0[i])
    keep[tt] <- !is.na(days) & after %in% TRUE & days <= turtle.ho.days &
      !is.na(km) & km <= turtle.ho.km
  }
  ho[keep, , drop = FALSE]
}

## merge haul-out records into on-land intervals
##  ho: data frame with id, start, end (POSIXct UTC)
##  dives: data frame with id, start, end (POSIXct UTC) of dive records
## Returns a data frame with id, start, end
qc_onland_intervals <- function(ho, dives = NULL, ho.gap = 6) {

  empty <- data.frame(id = character(0), start = as.POSIXct(character(0), tz = "UTC"),
                      end = as.POSIXct(character(0), tz = "UTC"))
  if (is.null(ho) || nrow(ho) == 0) return(empty)
  ho <- ho[!is.na(ho$start) & !is.na(ho$end) & ho$end >= ho$start, ]
  if (nrow(ho) == 0) return(empty)

  out <- lapply(split(ho, ho$id), function(h) {
    h <- h[order(h$start), ]
    dv <- if (!is.null(dives)) dives[dives$id == h$id[1], ] else NULL
    s <- h$start[1]
    e <- h$end[1]
    res <- list()
    for (k in seq_len(nrow(h))[-1]) {
      gap_h <- as.numeric(difftime(h$start[k], e, units = "hours"))
      dive_in_gap <- !is.null(dv) && nrow(dv) > 0 &&
        any(dv$end > e & dv$start < h$start[k])
      if (gap_h <= ho.gap && !dive_in_gap) {
        e <- max(e, h$end[k])
      } else {
        res[[length(res) + 1]] <- data.frame(id = h$id[1], start = s, end = e)
        s <- h$start[k]
        e <- h$end[k]
      }
    }
    res[[length(res) + 1]] <- data.frame(id = h$id[1], start = s, end = e)
    do.call(rbind, res)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out$id <- as.character(out$id)
  out$start <- as.POSIXct(out$start, tz = "UTC")
  out$end <- as.POSIXct(out$end, tz = "UTC")
  out
}

## SMRU: haul-out table (s_date, e_date, lat, lon) and dive table (de_date =
##  dive end, dive_dur in seconds)
##  taxon: named by reference (see qc_meta_taxon()); NULL uses every haul-out
##   record; starts: qc_deployment_start() output (for turtles)
smru_onland <- function(smru, ho.gap = 6, taxon = NULL, starts = NULL) {
  if (!"haulout" %in% names(smru) || !is.data.frame(smru$haulout) ||
      nrow(smru$haulout) == 0) {
    return(qc_onland_intervals(NULL))
  }
  h <- smru$haulout
  ho <- data.frame(id = as.character(h$ref),
                   start = as.POSIXct(h$s_date, tz = "UTC"),
                   end = as.POSIXct(h$e_date, tz = "UTC"),
                   lat = if ("lat" %in% names(h)) suppressWarnings(as.numeric(h$lat)) else NA_real_,
                   lon = if ("lon" %in% names(h)) suppressWarnings(as.numeric(h$lon)) else NA_real_)
  if (!is.null(taxon)) {
    locs <- data.frame(ref = as.character(smru$diag$ref), date = smru$diag$d_date,
                       lat = suppressWarnings(as.numeric(smru$diag$lat)),
                       lon = suppressWarnings(as.numeric(smru$diag$lon)))
    ho <- qc_haulout_taxon(ho, taxon, starts = starts, locs = locs)
  }
  dives <- NULL
  if ("dive" %in% names(smru) && is.data.frame(smru$dive) && nrow(smru$dive) > 0) {
    de <- as.POSIXct(smru$dive$de_date, tz = "UTC")
    dur <- if ("dive_dur" %in% names(smru$dive)) suppressWarnings(as.numeric(smru$dive$dive_dur)) else 0
    dur[is.na(dur)] <- 0
    dives <- data.frame(id = as.character(smru$dive$ref), start = de - dur, end = de)
    dives <- dives[!is.na(dives$end), ]
  }
  qc_onland_intervals(ho, dives, ho.gap = ho.gap)
}

## Wildlife Computers: HaulOut table (StartMax to EndMin where present, the
##  interval the tag was certainly hauled out; otherwise StartMin or Start to
##  EndMax or End) and dive records (DSA: DiveStart, plus duration where
##  given; ECDHistos_SCOUT_DSA: Start, End)
wc_onland <- function(wc, ho.gap = 6, taxon = NULL, starts = NULL) {
  ## haul-out records read by wc_pull_data() (attribute "haulout")
  ho <- attr(wc, "haulout")
  if (is.null(ho) || !is.data.frame(ho) || nrow(ho) == 0) ho <- wc$Haulout
  if (is.null(ho) || !is.data.frame(ho) || nrow(ho) == 0) return(qc_onland_intervals(NULL))
  st_col <- intersect(c("StartMax", "StartMin", "Start"), names(ho))[1]
  en_col <- intersect(c("EndMin", "EndMax", "End"), names(ho))[1]
  if (is.na(st_col) || is.na(en_col)) return(qc_onland_intervals(NULL))
  ho <- data.frame(id = as.character(ho$DeploymentID),
                   start = wc_parse_time(ho[[st_col]]),
                   end = wc_parse_time(ho[[en_col]]),
                   lat = if ("Latitude" %in% names(ho)) suppressWarnings(as.numeric(ho$Latitude)) else NA_real_,
                   lon = if ("Longitude" %in% names(ho)) suppressWarnings(as.numeric(ho$Longitude)) else NA_real_)
  if (!is.null(taxon)) {
    locs <- data.frame(ref = as.character(wc$Locations$DeploymentID),
                       date = as.POSIXct(wc$Locations$Date, tz = "UTC"),
                       lat = suppressWarnings(as.numeric(wc$Locations$Latitude)),
                       lon = suppressWarnings(as.numeric(wc$Locations$Longitude)))
    ho <- qc_haulout_taxon(ho, taxon, starts = starts, locs = locs)
  }

  dives <- list()
  if (is.data.frame(wc$DSA) && nrow(wc$DSA) > 0 && "DiveStart" %in% names(wc$DSA)) {
    s <- wc_parse_time(wc$DSA$DiveStart)
    dur_col <- intersect(c("DiveDuration", "Duration"), names(wc$DSA))[1]
    dur <- if (!is.na(dur_col)) suppressWarnings(as.numeric(wc$DSA[[dur_col]])) else 0
    dur[is.na(dur)] <- 0
    dives[[1]] <- data.frame(id = as.character(wc$DSA$DeploymentID), start = s, end = s + dur)
  }
  ecd <- wc$ECDHistos_SCOUT_DSA
  if (is.data.frame(ecd) && nrow(ecd) > 0 && all(c("Start", "End") %in% names(ecd))) {
    dives[[2]] <- data.frame(id = as.character(ecd$DeploymentID),
                             start = wc_parse_time(ecd$Start), end = wc_parse_time(ecd$End))
  }
  dives <- if (length(dives) > 0) do.call(rbind, dives) else NULL
  if (!is.null(dives)) dives <- dives[!is.na(dives$start) & !is.na(dives$end), ]
  qc_onland_intervals(ho, dives, ho.gap = ho.gap)
}

## WC date-times: POSIXct as is; text as "HH:MM:SS dd-Mon-YYYY" (WC portal
##  files) or a standard date-time
wc_parse_time <- function(x) {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  x <- as.character(x)
  out <- suppressWarnings(lubridate::parse_date_time(x, orders = c("HMS dbY", "Ymd HMS", "dmY HMS"),
                                                     tz = "UTC", quiet = TRUE))
  as.POSIXct(out, tz = "UTC")
}
