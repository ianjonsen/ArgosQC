##' @title Suggest track end dates for delayed-mode QC of SMRU tag data
##'
##' @description Reads a campaign's SMRU tag data and deployment metadata, as
##' `smru_qc()` does, but fits no SSM. For each deployment it finds the end of
##' diving (the last dive or CTD profile) and tests whether the animal stayed in
##' one place afterwards. When it did, the function suggests an `end_date` for
##' truncating the track. Suggestions are written to a draft .csv file for
##' review, with one review plot per deployment. After review, the file is
##' supplied to `smru_qc()` through the `trimIDs` entry of the config file.
##'
##' A suggested `end_date` never removes diving. It always keeps at least one
##' location more than one SSM time step (`time.step` in the config file) after
##' the last dive or CTD profile, so the SSM-predicted track covers every dive and
##' CTD profile.
##'
##' The function runs only for delayed-mode QC (`QCmode = "dm"`). Near real-time
##' QC is unsupervised, so no tracks are truncated there.
##'
##' @details The rules, applied to each deployment:
##'   1. No dive or CTD records, or fewer than `min_post` days of locations after
##'   the last dive or CTD profile: no suggestion.
##'   2. Two tests use the locations after the last dive or CTD profile: class
##'   1-3 locations if at least 3 exist, otherwise classes 0-3 and A, otherwise
##'   all classes. The spread is the 90th percentile of their distances from their
##'   median position (rounded up, so with few locations it is the largest
##'   distance). The drift is the distance between the median positions of the
##'   first and second halves of the locations, split by number in time order. The
##'   animal is stationary if the spread is at most `max_spread` km and the drift
##'   is at most `max_drift` km. The drift test separates a slowly travelling
##'   animal from a stationary one with noisy Argos locations. It is skipped when
##'   haul-out records cover at least half the period after the last dive or CTD
##'   profile, because an animal ashore may move a short distance along the coast.
##'   3. Stationary: the suggested `end_date` is the start of the first haul-out
##'   beginning within `haulout_window` days after the last dive or CTD profile,
##'   or otherwise the last dive or CTD profile. It is then moved later, if needed,
##'   to keep the first location at least one time step after the last dive or
##'   CTD profile, and moved later by `keep_days`.
##'   4. Not stationary: no `end_date`. The deployment is flagged for review, and
##'   the date rule 3 would give is reported as `candidate_end_date`.
##'
##' @param wd the working directory, as in `smru_qc()`
##' @param config the JSON config file, as in `smru_qc()`. `QCmode` must be `"dm"`
##' @param draft_file path for the draft truncation file. Default:
##' `<cid>_trimIDs_draft.csv` in `wd`
##' @param plot_dir directory for the review plots. Default: `trim_review` inside
##' the config's `diag.dir`. Use `plots = FALSE` to skip the plots
##' @param max_spread largest spread (km) of locations counted as stationary
##' @param max_drift largest drift (km) between the median positions of the first
##' and second halves of the locations counted as stationary
##' @param haulout_window days after the last dive or CTD profile within which a
##' haul-out must begin to set the suggested `end_date`
##' @param min_post minimum days of locations after the last dive or CTD profile
##' needed for a suggestion
##' @param keep_days days added to each suggested `end_date`
##' @param plots logical; write one review plot per deployment
##'
##' @return invisibly, a data frame with one row per deployment: `ref`,
##' `start_date` (empty), `end_date` (the suggestion, or empty), `rule`,
##' `candidate_end_date`, `last_dive`, `last_ctd`, `last_location`,
##' `days_after_last_dive_or_ctd`, `spread_km`, `drift_km`, `spread_location_classes`,
##' `haulout_cover`, `n_locations_removed`, `n_dives_after_end`,
##' `n_ctd_after_end`. The same table is written to `draft_file`.
##'
##' @importFrom jsonlite read_json
##' @importFrom readr read_csv write_csv
##' @importFrom stats median quantile
##' @importFrom sf st_as_sf st_transform st_coordinates
##' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_line geom_vline
##' @importFrom ggplot2 facet_wrap labs theme_minimal theme scale_colour_manual ggsave
##'
##' @export

smru_suggest_trim <- function(wd,
                              config,
                              draft_file = NULL,
                              plot_dir = NULL,
                              max_spread = 50,
                              max_drift = 20,
                              haulout_window = 2,
                              min_post = 2,
                              keep_days = 0,
                              plots = TRUE) {

  if (!file.exists(wd)) stop("Working directory `wd` does not exist", call. = FALSE)
  old_wd <- setwd(wd)
  on.exit(setwd(old_wd), add = TRUE)

  conf <- read_json(config, simplifyVector = TRUE)

  if (!identical(as.character(conf$model$QCmode), "dm")) {
    stop("smru_suggest_trim() is for delayed-mode QC only (QCmode = \"dm\"); ",
         config, " has QCmode = \"", conf$model$QCmode, "\".", call. = FALSE)
  }

  cid <- conf$harvest$cid
  sec_day <- 86400
  ts_sec <- as.numeric(conf$model$time.step) * 3600

  ## settings handled as in smru_qc()
  if (is.na(conf$setup$meta.file)) {
    conf$setup$meta.file <- NULL
    meta.source <- "smru"
  } else {
    meta.source <- conf$setup$program
  }
  if (is.null(conf$harvest$dropIDs) || all(is.na(conf$harvest$dropIDs))) {
    dropIDs <- c("")
  } else {
    dropIDs <- suppressMessages(read_csv(conf$harvest$dropIDs)$ref)
  }
  if (any(!"p2mdbtools" %in% names(conf$harvest), is.na(conf$harvest$p2mdbtools))) {
    conf$harvest$p2mdbtools <- NULL
  }

  ## use the local .mdb files; download only those that are missing
  mdb <- file.path(conf$setup$data.dir, paste0(cid, ".mdb"))
  if (any(!file.exists(mdb))) {
    if (isTRUE(as.logical(conf$harvest$download))) {
      message("Downloading tag data from SMRU server...")
      dir.create(conf$setup$data.dir, showWarnings = FALSE, recursive = TRUE)
      download_data(dest = file.path(wd, conf$setup$data.dir),
                    source = "smru",
                    cid = cid[!file.exists(mdb)],
                    user = conf$harvest$smru.usr,
                    pwd = conf$harvest$smru.pwd,
                    timeout = conf$harvest$timeout)
    } else {
      stop("Missing .mdb file(s): ", paste(mdb[!file.exists(mdb)], collapse = ", "),
           call. = FALSE)
    }
  }

  message("Reading tag data from .mdb file...")
  smru <- smru_pull_tables(cids = cid,
                           path2mdb = conf$setup$data.dir,
                           p2mdbtools = conf$harvest$p2mdbtools)

  message("Reading deployment metadata...")
  meta <- get_metadata(source = meta.source,
                       tag_data = smru,
                       cid = cid,
                       user = conf$harvest$smru.usr,
                       pwd = conf$harvest$smru.pwd,
                       dropIDs = dropIDs,
                       file = conf$setup$meta.file,
                       meta.args = conf$meta) |>
    suppressMessages()

  obs <- smru_clean_diag(smru, dropIDs = dropIDs)
  obs <- obs[obs$ref %in% meta$device_id & !is.na(obs$date) &
               !is.na(obs$lon) & !is.na(obs$lat), ]
  obs$lc <- as.character(obs$lc)

  get_times <- function(tab, var) {
    if (!tab %in% names(smru) || !var %in% names(smru[[tab]])) {
      return(data.frame(ref = character(0), t = as.POSIXct(character(0), tz = "UTC")))
    }
    out <- data.frame(ref = as.character(smru[[tab]]$ref),
                      t = as.POSIXct(smru[[tab]][[var]], tz = "UTC"))
    out[!is.na(out$t), ]
  }
  dives <- get_times("dive", "de_date")
  ctds <- get_times("ctd", "end_date")
  ho_s <- get_times("haulout", "s_date")
  ho <- if (nrow(ho_s) > 0) {
    data.frame(ref = as.character(smru$haulout$ref),
               s_date = as.POSIXct(smru$haulout$s_date, tz = "UTC"),
               e_date = as.POSIXct(smru$haulout$e_date, tz = "UTC"))
  } else {
    data.frame(ref = character(0), s_date = as.POSIXct(character(0), tz = "UTC"),
               e_date = as.POSIXct(character(0), tz = "UTC"))
  }
  ho <- ho[!is.na(ho$s_date) & !is.na(ho$e_date), ]

  ## helpers
  fmt <- function(x) if (length(x) == 0 || is.na(x)) NA_character_ else
    format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")

  covered_seconds <- function(s, e, lo, hi) {
    s <- pmax(as.numeric(s), lo); e <- pmin(as.numeric(e), hi)
    k <- e > s
    if (!any(k)) return(0)
    s <- s[k]; e <- e[k]; o <- order(s); s <- s[o]; e <- e[o]
    total <- 0; cs <- s[1]; ce <- e[1]
    for (i in seq_along(s)[-1]) {
      if (s[i] > ce) { total <- total + ce - cs; cs <- s[i]; ce <- e[i] }
      else ce <- max(ce, e[i])
    }
    total + ce - cs
  }

  ## spread and drift (km) of locations p, which are in time order
  spread_drift_km <- function(p) {
    pts <- st_as_sf(p, coords = c("lon", "lat"), crs = 4326)
    aeqd <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=km +datum=WGS84",
                    p$lat[1], p$lon[1])
    xy <- st_coordinates(st_transform(pts, aeqd))
    d <- sort(sqrt((xy[, 1] - median(xy[, 1]))^2 + (xy[, 2] - median(xy[, 2]))^2))
    spread <- d[ceiling(0.9 * (length(d) - 1)) + 1]
    half <- seq_len(nrow(xy)) <= nrow(xy) %/% 2
    drift <- sqrt((median(xy[half, 1]) - median(xy[!half, 1]))^2 +
                    (median(xy[half, 2]) - median(xy[!half, 2]))^2)
    c(spread = spread, drift = drift)
  }

  refs <- sort(unique(obs$ref))
  rows <- vector("list", length(refs))
  plot_data <- vector("list", length(refs))

  for (k in seq_along(refs)) {
    ref <- refs[k]
    f <- obs[obs$ref == ref, ]
    f <- f[order(f$date), ]
    d_t <- sort(dives$t[dives$ref == ref])
    c_t <- sort(ctds$t[ctds$ref == ref])
    h <- ho[ho$ref == ref, ]
    h <- h[order(h$s_date), ]

    last_dive <- if (length(d_t)) max(d_t) else NA
    last_ctd <- if (length(c_t)) max(c_t) else NA
    last_act <- if (length(d_t) + length(c_t) > 0) max(c(d_t, c_t)) else NA
    last_fix <- max(f$date)

    r <- list(ref = ref, start_date = NA_character_, end_date = NA_character_,
              rule = NA_character_,
              candidate_end_date = NA_character_,
              last_dive = fmt(last_dive), last_ctd = fmt(last_ctd),
              last_location = fmt(last_fix),
              days_after_last_dive_or_ctd = NA_real_, spread_km = NA_real_,
              drift_km = NA_real_,
              spread_location_classes = NA_character_, haulout_cover = NA_real_,
              n_locations_removed = NA_integer_, n_dives_after_end = NA_integer_,
              n_ctd_after_end = NA_integer_)
    end <- NA

    if (length(d_t) + length(c_t) == 0) {
      r$rule <- "No dive or CTD records; no suggestion"
    } else {
      post_days <- as.numeric(difftime(last_fix, last_act, units = "days"))
      r$days_after_last_dive_or_ctd <- round(post_days, 2)

      if (post_days < min_post) {
        r$rule <- sprintf("Less than %g days of locations after the last dive or CTD profile; no suggestion",
                          min_post)
      } else {
        post <- f[f$date > last_act, ]
        tiers <- list(c("1", "2", "3"), c("0", "1", "2", "3", "A"), unique(post$lc))
        tier_names <- c("1-3", "0-3 and A", "all")
        sp <- NA_real_
        dr <- NA_real_
        for (i in seq_along(tiers)) {
          p <- post[post$lc %in% tiers[[i]], c("lon", "lat")]
          if (nrow(p) >= 3) {
            sd_km <- spread_drift_km(p)
            sp <- sd_km[["spread"]]
            dr <- sd_km[["drift"]]
            r$spread_location_classes <- tier_names[i]
            break
          }
        }
        r$spread_km <- round(sp, 1)
        r$drift_km <- round(dr, 1)
        cover <- covered_seconds(h$s_date, h$e_date, as.numeric(last_act), as.numeric(last_fix)) /
          (as.numeric(last_fix) - as.numeric(last_act))
        r$haulout_cover <- round(cover, 2)

        ## candidate end: haul-out start or end of diving, then moved later
        ## so the SSM track covers every dive and CTD profile
        first_ho <- h$s_date[h$s_date >= last_act]
        use_ho <- length(first_ho) > 0 &&
          as.numeric(difftime(first_ho[1], last_act, units = "days")) <= haulout_window
        candidate <- if (use_ho) first_ho[1] else last_act
        need <- which(f$date >= last_act + ts_sec)

        if (length(need) == 0) {
          r$rule <- "No location more than one time step after the last dive or CTD profile; truncation would leave dives without locations, so no suggestion"
        } else {
          cand_end <- max(candidate, f$date[need[1]] + 1) + keep_days * sec_day
          r$candidate_end_date <- fmt(cand_end)

          if (is.na(sp)) {
            r$rule <- "Too few locations after the last dive or CTD profile to test for movement; review the plot"
          } else if (sp <= max_spread && (dr <= max_drift || cover >= 0.5)) {
            end <- cand_end
            r$end_date <- fmt(end)
            r$rule <- if (use_ho) {
              paste0("Stationary after the last dive or CTD profile; haul-out began ", fmt(first_ho[1]))
            } else {
              "Stationary after the last dive or CTD profile; no haul-out record began within the window"
            }
          } else if (cover >= 0.5) {
            r$rule <- "Haul-out records after the last dive or CTD profile, but the locations moved; review the plot"
          } else {
            r$rule <- "Locations moved after the last dive or CTD profile, with no dives; review the plot"
          }
        }
      }
    }

    if (!is.na(end)) {
      r$n_locations_removed <- sum(f$date >= end)
      r$n_dives_after_end <- sum(d_t >= end)
      r$n_ctd_after_end <- sum(c_t >= end)
      if (r$n_dives_after_end > 0 || r$n_ctd_after_end > 0) {
        stop("Internal error: suggested end_date for ", ref, " would remove dives or CTD profiles.",
             call. = FALSE)
      }
    }

    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE)
    plot_data[[k]] <- list(f = f, d_t = d_t, c_t = c_t, h = h, last_act = last_act,
                           end = end, r = r)
  }

  out <- do.call(rbind, rows)
  ord <- order(is.na(out$end_date), is.na(out$candidate_end_date), out$ref)
  out <- out[ord, ]
  rownames(out) <- NULL

  if (is.null(draft_file)) draft_file <- paste0(paste(cid, collapse = "_"), "_trimIDs_draft.csv")
  write_csv(out, draft_file, na = "")

  if (plots) {
    if (is.null(plot_dir)) plot_dir <- file.path(conf$setup$diag.dir, "trim_review")
    dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
    for (pd in plot_data) trim_review_plot(pd, plot_dir)
  }

  message(sprintf("%d deployments: %d with a suggested end_date, %d flagged for review. Draft written to %s",
                  nrow(out), sum(!is.na(out$end_date)),
                  sum(is.na(out$end_date) & grepl("review the plot", out$rule)),
                  file.path(wd, draft_file)))
  if (plots) message("Review plots written to ", file.path(wd, plot_dir))

  invisible(out)
}


## one review plot per deployment (internal)
trim_review_plot <- function(pd, plot_dir) {
  f <- pd$f
  grp <- ifelse(f$lc %in% c("1", "2", "3"), "Location class 1-3",
                ifelse(f$lc %in% c("0", "A"), "Location class 0 or A",
                       "Location class B or Z"))

  ## latitude and longitude, plotted within their 0.5th to 99.5th percentiles
  ##  so a few extreme Argos locations do not compress the axis
  within_range <- function(x) {
    q <- quantile(x, c(0.005, 0.995), names = FALSE, na.rm = TRUE)
    x >= q[1] & x <= q[2]
  }
  keep_lat <- within_range(f$lat)
  keep_lon <- within_range(f$lon)
  n_hidden <- sum(!(keep_lat & keep_lon))
  pts <- rbind(data.frame(date = f$date[keep_lat], value = f$lat[keep_lat],
                          panel = "Latitude", series = grp[keep_lat]),
               data.frame(date = f$date[keep_lon], value = f$lon[keep_lon],
                          panel = "Longitude", series = grp[keep_lon]))

  ## daily counts over every day from the first to the last location. Days with
  ##  no Argos location (nothing received) are left empty, so lines break there;
  ##  on days with locations, days with no dives or CTD profiles count as 0
  days <- seq(as.Date(min(f$date), tz = "UTC"), as.Date(max(f$date), tz = "UTC"), by = "day")
  count_days <- function(t) {
    n <- table(factor(as.character(as.Date(t, tz = "UTC")), levels = as.character(days)))
    as.numeric(n)
  }
  n_fix <- count_days(f$date)
  received <- n_fix > 0
  mid_day <- as.POSIXct(as.character(days), tz = "UTC") + 43200
  daily <- function(n, label) {
    data.frame(date = mid_day, value = ifelse(received, n, NA_real_),
               panel = "Daily counts", series = label)
  }
  cnts <- rbind(daily(n_fix, "Argos locations per day"),
                daily(count_days(pd$d_t), "Dives per day"),
                daily(count_days(pd$c_t), "CTD profiles per day"))
  if (length(pd$c_t) == 0) cnts <- cnts[cnts$series != "CTD profiles per day", ]
  if (length(pd$d_t) == 0) cnts <- cnts[cnts$series != "Dives per day", ]

  panels <- c("Latitude", "Longitude", "Daily counts")
  pts$panel <- factor(pts$panel, levels = panels)
  cnts$panel <- factor(cnts$panel, levels = panels)

  cols <- c("Location class 1-3" = "#1b7837",
            "Location class 0 or A" = "#6a51a3",
            "Location class B or Z" = "#a6a6a6",
            "Argos locations per day" = "#4d4d4d",
            "Dives per day" = "#2166ac",
            "CTD profiles per day" = "#b2182b")

  p <- ggplot()
  if (nrow(pd$h) > 0) {
    p <- p + geom_rect(data = pd$h,
                       aes(xmin = s_date, xmax = e_date, ymin = -Inf, ymax = Inf),
                       fill = "#f4a582", alpha = 0.35)
  }
  p <- p + geom_point(data = pts, aes(x = date, y = value, colour = series), size = 0.6) +
    geom_line(data = cnts, aes(x = date, y = value, colour = series), linewidth = 0.4,
              na.rm = TRUE)
  if (!is.na(pd$last_act)) p <- p + geom_vline(xintercept = pd$last_act, colour = "black")
  if (!is.na(pd$end)) {
    p <- p + geom_vline(xintercept = pd$end, colour = "red", linetype = 2)
  } else if (!is.na(pd$r$candidate_end_date)) {
    cand <- as.POSIXct(pd$r$candidate_end_date, tz = "UTC")
    p <- p + geom_vline(xintercept = cand, colour = "grey40", linetype = 3)
  }

  sub <- paste0("Shaded: haul-out records. Solid line: last dive or CTD profile. ",
                "Dashed red line: suggested end_date. ",
                "Dotted grey line: candidate end_date of a deployment flagged for review.")
  if (n_hidden > 0) {
    sub <- paste0(sub, "\n", n_hidden, " extreme Argos locations outside the 0.5th to ",
                  "99.5th percentiles of latitude or longitude are not shown.")
  }

  p <- p + facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = cols) +
    labs(title = paste0(pd$r$ref, ": ", pd$r$rule), subtitle = sub,
         x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(file.path(plot_dir, paste0("trim_review_", pd$r$ref, ".png")),
         plot = p, width = 12, height = 8, dpi = 150, bg = "white")
}
