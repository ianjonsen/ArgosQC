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
## Tags without haul-out records (e.g. most turtle tags) have no on-land
## intervals; only their locations on land at the start and end of a track are
## kept on land by route_path().

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

## SMRU: haul-out table (s_date, e_date) and dive table (de_date = dive end,
##  dive_dur in seconds)
smru_onland <- function(smru, ho.gap = 6) {
  if (!"haulout" %in% names(smru) || !is.data.frame(smru$haulout) ||
      nrow(smru$haulout) == 0) {
    return(qc_onland_intervals(NULL))
  }
  ho <- data.frame(id = as.character(smru$haulout$ref),
                   start = as.POSIXct(smru$haulout$s_date, tz = "UTC"),
                   end = as.POSIXct(smru$haulout$e_date, tz = "UTC"))
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
wc_onland <- function(wc, ho.gap = 6) {
  ho <- wc$Haulout
  if (is.null(ho) || !is.data.frame(ho) || nrow(ho) == 0) return(qc_onland_intervals(NULL))
  st_col <- intersect(c("StartMax", "StartMin", "Start"), names(ho))[1]
  en_col <- intersect(c("EndMin", "EndMax", "End"), names(ho))[1]
  if (is.na(st_col) || is.na(en_col)) return(qc_onland_intervals(NULL))
  ho <- data.frame(id = as.character(ho$DeploymentID),
                   start = wc_parse_time(ho[[st_col]]),
                   end = wc_parse_time(ho[[en_col]]))

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
