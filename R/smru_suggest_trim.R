##' @title Suggest track end dates for delayed-mode QC of SMRU tag data
##'
##' @description Reads a campaign's SMRU tag data and deployment metadata, as
##' `smru_qc()` does, but fits no SSM. For each deployment it suggests an
##' `end_date` for truncating the track, writes the suggestions to a draft .csv
##' file for review, and writes a review PDF with one page per deployment: the
##' whole track beside a zoom on the end of the track. After review, the file is
##' supplied to `smru_qc()` through the `trimIDs` entry of the config file.
##'
##' The function runs only for delayed-mode QC (`QCmode = "dm"`). Near real-time
##' QC is unsupervised, so no tracks are truncated there.
##'
##' @details The rules, applied to each deployment:
##'   1. The anchor is the last CTD profile, or the last dive for a tag with no
##'   CTD profiles. A suggested `end_date` never removes a CTD profile (except
##'   under rule 5): it always keeps at least one location more than one SSM time
##'   step (`time.step` in the config file) after the anchor, so the
##'   SSM-predicted track covers every CTD profile. Dives after the last CTD
##'   profile are not protected; they are often records from a failing sensor.
##'   2. Stationary end: the final run of days whose daily median positions (all
##'   location classes except Z) after the anchor lie within
##'   `stationary_radius_km` of the run's median position, lasting at least
##'   `min_stationary_days`. This is a haul-out, or a likely haul-out with
##'   missing haul-out records. Candidate: 00:00 UTC on the run's first day.
##'   3. Sparse end: the final run of days whose centred 3-day mean number of
##'   locations is below `sparse_mean`, provided that run includes a day with
##'   fewer than `very_sparse_daily` locations. This is the location data
##'   petering out at the end of the track (battery, antenna or other tag
##'   failure). Candidate: 00:00 UTC on the run's first day.
##'   4. Rules 2 and 3 apply only when there are at least `min_post_days` of
##'   locations after the anchor. The suggested `end_date` is the earlier of
##'   their candidates, moved later if needed to satisfy rule 1, then moved later
##'   by `keep_days`. With neither candidate, the track is kept to its end.
##'   5. Final haul-out: a haul-out that begins within the last
##'   `final_haulout_days` of the track and lasts at least
##'   `min_final_haulout_hours` sets a candidate at its start, even if CTD
##'   profiles follow it. Haul-out records separated by less than 1 hour, with no
##'   dive ending in between, are merged into one haul-out first. If the anchor
##'   (the last CTD profile) comes after the start of that haul-out, and no more
##'   than `haulout_reversal_days` after it, the haul-out was not final: the
##'   candidate is replaced by the point just after the anchor (rule 1), so the
##'   last CTD profile is kept. The suggested `end_date` is the earliest
##'   candidate.
##'
##' @param wd the working directory, as in `smru_qc()`
##' @param config the JSON config file, as in `smru_qc()`. `QCmode` must be `"dm"`
##' @param draft_file path for the draft truncation file. Default:
##' `<cid>_trimIDs_draft.csv` in `wd`
##' @param plot_file path for the review PDF. Default: `trim_review_<cid>.pdf` in
##' the config's `diag.dir`. Use `plots = FALSE` to skip the plots
##' @param min_post_days minimum days of locations after the anchor for rules 2
##' and 3
##' @param stationary_radius_km largest distance (km) of a daily median position
##' from the median position of the final run of days (rule 2)
##' @param min_stationary_days minimum length (days) of the final stationary run
##' (rule 2)
##' @param sparse_mean centred 3-day mean number of locations per day below which
##' a day counts as sparse (rule 3)
##' @param very_sparse_daily a sparse final run must include a day with fewer
##' locations than this (rule 3)
##' @param final_haulout_days a haul-out beginning within this many days of the
##' last location can set a candidate at its start (rule 5)
##' @param min_final_haulout_hours minimum length (hours) of that haul-out
##' (rule 5); shorter dry periods are not treated as a final haul-out
##' @param haulout_reversal_days if the anchor follows the start of the final
##' haul-out by no more than this many days, the haul-out candidate is replaced by
##' the point just after the anchor (rule 5)
##' @param keep_days days added to each suggested `end_date` from rules 2 and 3
##' @param plots logical; write the review PDF
##'
##' @return invisibly, a data frame with one row per deployment: `ref`,
##' `start_date` (empty), `end_date` (the suggestion, or empty), `rule`,
##' `anchor`, `anchor_type`, `last_dive`, `last_ctd`, `last_location`,
##' `days_after_anchor`, `stationary_end`, `sparse_end`, `final_haulout_start`,
##' `n_locations_removed`, `n_dives_removed`, `n_ctd_removed`. The same table is
##' written to `draft_file`.
##'
##' @importFrom jsonlite read_json
##' @importFrom readr read_csv write_csv
##' @importFrom stats median quantile
##' @importFrom ggplot2 ggplot aes geom_rect geom_point geom_line geom_vline
##' @importFrom ggplot2 facet_wrap labs theme_minimal theme scale_colour_manual
##' @importFrom ggplot2 scale_x_datetime element_text
##' @importFrom grDevices pdf dev.off
##'
##' @export

smru_suggest_trim <- function(wd,
                              config,
                              draft_file = NULL,
                              plot_file = NULL,
                              min_post_days = 2,
                              stationary_radius_km = 30,
                              min_stationary_days = 2,
                              sparse_mean = 10,
                              very_sparse_daily = 5,
                              final_haulout_days = 2,
                              min_final_haulout_hours = 6,
                              haulout_reversal_days = 5,
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
  sec_day <- 86400
  fmt <- function(x) if (length(x) == 0 || is.na(x)) NA_character_ else
    format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  midnight <- function(d) as.POSIXct(format(d, "%Y-%m-%d"), tz = "UTC")
  dist_km <- function(lat1, lon1, lat2, lon2) {
    r <- pi / 180
    a <- sin((lat2 - lat1) * r / 2)^2 +
      cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
    2 * 6371 * asin(pmin(1, sqrt(a)))
  }

  refs <- sort(unique(obs$ref))
  rows <- vector("list", length(refs))
  plot_data <- vector("list", length(refs))

  for (k in seq_along(refs)) {
    ref <- refs[k]
    f <- obs[obs$ref == ref & obs$lc != "Z", ]
    f <- f[order(f$date), ]
    d_t <- sort(dives$t[dives$ref == ref])
    c_t <- sort(ctds$t[ctds$ref == ref])
    h <- ho[ho$ref == ref, ]
    h <- h[order(h$s_date), ]

    last_dive <- if (length(d_t)) max(d_t) else NA
    last_ctd <- if (length(c_t)) max(c_t) else NA
    anchor <- if (length(c_t)) last_ctd else last_dive
    anchor_type <- if (length(c_t)) "last CTD profile" else if (length(d_t)) "last dive" else NA_character_
    last_fix <- if (nrow(f)) max(f$date) else NA

    r <- list(ref = ref, start_date = NA_character_, end_date = NA_character_,
              rule = NA_character_, anchor = fmt(anchor), anchor_type = anchor_type,
              last_dive = fmt(last_dive), last_ctd = fmt(last_ctd),
              last_location = fmt(last_fix), days_after_anchor = NA_real_,
              stationary_end = NA_character_, sparse_end = NA_character_,
              final_haulout_start = NA_character_,
              n_locations_removed = NA_integer_, n_dives_removed = NA_integer_,
              n_ctd_removed = NA_integer_)
    na_time <- .POSIXct(NA_real_, tz = "UTC")
    end <- na_time
    stat_end <- na_time
    sparse_end <- na_time
    ho_start <- na_time
    ho_reversed <- FALSE

    if (nrow(f) == 0) {
      r$rule <- "No locations; no suggestion"
    } else if (is.na(anchor)) {
      r$rule <- "No dive or CTD records; no suggestion"
    } else {
      post_days <- as.numeric(difftime(last_fix, anchor, units = "days"))
      r$days_after_anchor <- round(post_days, 2)

      ## rule 1: lower bound that keeps every CTD profile (or dive) located
      need <- which(f$date >= anchor + as.numeric(conf$model$time.step) * 3600)
      lower <- if (length(need)) f$date[need[1]] + 1 else NA

      ## rule 5: haul-out of at least min_final_haulout_hours beginning in the
      ##  last days of the track. Merge records separated by less than 1 hour
      ##  with no dive ending in between
      if (nrow(h) > 0) {
        b_s <- h$s_date[1]
        b_e <- h$e_date[1]
        bouts <- NULL
        if (nrow(h) > 1) {
          for (j in 2:nrow(h)) {
            gap_ok <- as.numeric(difftime(h$s_date[j], b_e, units = "hours")) < 1 &&
              !any(d_t > b_e & d_t < h$s_date[j])
            if (gap_ok) {
              b_e <- max(b_e, h$e_date[j])
            } else {
              bouts <- rbind(bouts, data.frame(s = b_s, e = b_e))
              b_s <- h$s_date[j]
              b_e <- h$e_date[j]
            }
          }
        }
        bouts <- rbind(bouts, data.frame(s = b_s, e = b_e))
        final <- bouts$s >= last_fix - final_haulout_days * sec_day & bouts$s <= last_fix &
          as.numeric(difftime(bouts$e, bouts$s, units = "hours")) >= min_final_haulout_hours
        if (any(final)) {
          ho_start <- min(bouts$s[final])
          r$final_haulout_start <- fmt(ho_start)
          ## CTD profiles (or dives) after the haul-out began: it was not final.
          ##  Replace the candidate by the point just after the anchor
          lag_days <- as.numeric(difftime(anchor, ho_start, units = "days"))
          if (lag_days > 0 && lag_days <= haulout_reversal_days) {
            ho_reversed <- TRUE
            ho_start <- if (!is.na(lower)) .POSIXct(as.numeric(lower), tz = "UTC") else na_time
          }
        }
      }

      post <- f[f$date > anchor, ]
      if (nrow(post) > 0 && post_days >= min_post_days && !is.na(lower)) {
        day <- as.Date(post$date, tz = "UTC")
        days <- sort(unique(day))
        mlat <- as.numeric(tapply(post$lat, as.character(day), median))
        mlon <- as.numeric(tapply(post$lon, as.character(day), median))

        ## rule 2: stationary end
        run <- integer(0)
        for (j in rev(seq_along(days))) {
          cand <- c(run, j)
          clat <- median(mlat[cand])
          clon <- median(mlon[cand])
          if (all(dist_km(clat, clon, mlat[cand], mlon[cand]) <= stationary_radius_km)) {
            run <- cand
          } else {
            break
          }
        }
        if (length(run) &&
            as.numeric(days[max(run)] - days[min(run)]) + 1 >= min_stationary_days) {
          stat_end <- max(midnight(days[min(run)]), lower) + keep_days * sec_day
          r$stationary_end <- fmt(stat_end)
        }

        ## rule 3: sparse end
        all_days <- seq(min(days), max(days), by = "day")
        n_day <- as.numeric(table(factor(as.character(day), levels = as.character(all_days))))
        nd <- length(n_day)
        mean3 <- vapply(seq_len(nd), function(i) mean(n_day[max(1, i - 1):min(nd, i + 1)]),
                        numeric(1))
        i <- nd
        while (i >= 1 && mean3[i] < sparse_mean) i <- i - 1
        if (i < nd && min(n_day[(i + 1):nd]) < very_sparse_daily) {
          sparse_end <- max(midnight(all_days[i + 1]), lower) + keep_days * sec_day
          r$sparse_end <- fmt(sparse_end)
        }
      }

      cands <- c(stationary = as.numeric(stat_end), sparse = as.numeric(sparse_end),
                 haulout = as.numeric(ho_start))
      cands <- cands[!is.na(cands)]
      if (length(cands)) {
        end <- .POSIXct(min(cands), tz = "UTC")
        which_end <- names(cands)[cands == min(cands)]
        r$end_date <- fmt(end)
        r$rule <- if (identical(which_end, "haulout") && ho_reversed) {
          paste0("Haul-out began ", r$final_haulout_start, " but was followed by the ",
                 anchor_type, "; trimmed just after the ", anchor_type)
        } else if (identical(which_end, "haulout")) {
          paste0("Final haul-out of at least ", min_final_haulout_hours, " hours began ",
                 fmt(ho_start), ", within the last ", final_haulout_days, " days of the track")
        } else if (all(c("stationary", "sparse") %in% which_end)) {
          "Stationary and sparse to the end of the track"
        } else if ("stationary" %in% which_end) {
          "Stationary to the end of the track (haul-out, or likely haul-out)"
        } else if ("sparse" %in% which_end) {
          "Locations sparse to the end of the track"
        } else {
          paste0("Final haul-out began ", fmt(ho_start), ", within the last ",
                 final_haulout_days, " days of the track")
        }
      } else if (post_days < min_post_days) {
        r$rule <- sprintf("Less than %g days of locations after the %s; no suggestion",
                          min_post_days, anchor_type)
      } else {
        r$rule <- paste0("Locations continue after the ", anchor_type,
                         " without becoming stationary or sparse; track kept to its end")
      }
    }

    if (!is.na(end)) {
      r$n_locations_removed <- sum(f$date >= end)
      r$n_dives_removed <- sum(d_t >= end)
      r$n_ctd_removed <- sum(c_t >= end)
      if (r$n_ctd_removed > 0 && !(!is.na(ho_start) && end == ho_start)) {
        stop("Internal error: suggested end_date for ", ref, " would remove CTD profiles.",
             call. = FALSE)
      }
    }

    rows[[k]] <- as.data.frame(r, stringsAsFactors = FALSE)
    plot_data[[k]] <- list(f = f, d_t = d_t, c_t = c_t, h = h, anchor = anchor,
                           end = end, others = c(stat_end, sparse_end, ho_start), r = r)
  }

  out <- do.call(rbind, rows)
  out <- out[order(is.na(out$end_date), out$ref), ]
  rownames(out) <- NULL

  if (is.null(draft_file)) draft_file <- paste0(paste(cid, collapse = "_"), "_trimIDs_draft.csv")
  write_csv(out, draft_file, na = "")

  if (plots) {
    if (is.null(plot_file)) {
      plot_file <- file.path(conf$setup$diag.dir,
                             paste0("trim_review_", paste(cid, collapse = "_"), ".pdf"))
    }
    dir.create(dirname(plot_file), showWarnings = FALSE, recursive = TRUE)
    names(plot_data) <- refs
    pdf(plot_file, width = 17, height = 9)
    on.exit(dev.off(), add = TRUE)
    for (ref in out$ref) {
      suppressWarnings(trim_review_page(trim_review_plot(plot_data[[ref]])))
    }
  }

  message(sprintf("%d deployments: %d with a suggested end_date. Draft written to %s",
                  nrow(out), sum(!is.na(out$end_date)), file.path(wd, draft_file)))
  if (plots) message("Review PDF written to ", file.path(wd, plot_file))

  invisible(out)
}

## review plots for one deployment (internal): the whole track, and a zoom on
##  the end of the track, from 7 days before the anchor.
##  Returns the two plots as a list.
trim_review_plot <- function(pd) {
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

  others <- pd$others[!is.na(pd$others)]
  if (!is.na(pd$end)) others <- others[others != pd$end]

  sub <- paste0("Shaded: haul-out records. Solid line: ",
                ifelse(is.na(pd$r$anchor_type), "anchor", pd$r$anchor_type),
                " (anchor). Dashed red line: suggested end_date. ",
                "Dotted grey lines: other candidate end dates.")
  if (n_hidden > 0) {
    sub <- paste0(sub, "\n", n_hidden, " extreme Argos locations outside the 0.5th to ",
                  "99.5th percentiles of latitude or longitude are not shown.")
  }

  cols <- c("Location class 1-3" = "#1b7837",
            "Location class 0 or A" = "#6a51a3",
            "Location class B or Z" = "#a6a6a6",
            "Argos locations per day" = "#4d4d4d",
            "Dives per day" = "#2166ac",
            "CTD profiles per day" = "#b2182b")

  draw <- function(from, to, title, n_labels, legend_position) {
    in_window <- function(t) t >= from & t <= to
    p_pts <- pts[in_window(pts$date), ]
    p_cnts <- cnts[cnts$date >= from - 43200 & cnts$date <= to + 43200, ]
    p_h <- pd$h[pd$h$e_date >= from & pd$h$s_date <= to, ]
    p_h$s_date <- pmax(p_h$s_date, from)
    p_h$e_date <- pmin(p_h$e_date, to)

    ## date labels: about 25 per plot, with a minor grid line for every day on
    ##  plots shorter than about 3 months
    span <- as.numeric(difftime(to, from, units = "days"))
    step <- c(1, 2, 3, 7, 14, 28)
    step <- step[which(span / step <= n_labels)[1]]
    if (is.na(step)) step <- 28
    minor <- if (span <= 90) "1 day" else "7 days"

    p <- ggplot()
    if (nrow(p_h) > 0) {
      p <- p + geom_rect(data = p_h,
                         aes(xmin = s_date, xmax = e_date, ymin = -Inf, ymax = Inf),
                         fill = "#f4a582", alpha = 0.35)
    }
    p <- p + geom_point(data = p_pts, aes(x = date, y = value, colour = series), size = 0.6) +
      geom_line(data = p_cnts, aes(x = date, y = value, colour = series), linewidth = 0.4,
                na.rm = TRUE)
    if (!is.na(pd$anchor)) p <- p + geom_vline(xintercept = pd$anchor, colour = "black")
    if (!is.na(pd$end)) p <- p + geom_vline(xintercept = pd$end, colour = "red", linetype = 2)
    for (o in others) {
      p <- p + geom_vline(xintercept = as.POSIXct(o, origin = "1970-01-01", tz = "UTC"),
                          colour = "grey40", linetype = 3)
    }

    p <- p + facet_wrap(~panel, ncol = 1, scales = "free_y") +
      scale_x_datetime(date_breaks = paste(step, "days"), date_minor_breaks = minor,
                       date_labels = "%d %b", limits = c(from, to)) +
      scale_colour_manual(values = cols) +
      labs(title = title, x = NULL, y = NULL, colour = NULL) +
      theme_minimal() +
      theme(legend.position = legend_position,
            axis.text.x = element_text(angle = 45, hjust = 1))

    p
  }

  first_fix <- min(f$date)
  last_fix <- max(f$date)
  whole <- draw(first_fix, last_fix, "Whole track", 20, "none")

  zoom_from <- if (!is.na(pd$anchor)) pd$anchor - 7 * 86400 else last_fix - 30 * 86400
  zoom_from <- max(zoom_from, first_fix)
  end_zoom <- draw(zoom_from, last_fix,
                   "End of track: from 7 days before the anchor", 12, "right")

  list(title = paste0(pd$r$ref, ": ", pd$r$rule), subtitle = sub,
       whole = whole, end_zoom = end_zoom)
}

## draw one review page: title, then the whole track beside the end of the track
trim_review_page <- function(pl) {
  grid::grid.newpage()
  lay <- grid::grid.layout(2, 2,
                           heights = grid::unit(c(0.11, 0.89), "npc"),
                           widths = grid::unit(c(0.52, 0.48), "npc"))
  grid::pushViewport(grid::viewport(layout = lay))
  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid::grid.text(pl$title, x = 0.01, y = 0.72, just = "left",
                  gp = grid::gpar(fontsize = 14, fontface = "bold"))
  grid::grid.text(pl$subtitle, x = 0.01, y = 0.28, just = "left",
                  gp = grid::gpar(fontsize = 9))
  grid::popViewport()
  print(pl$whole, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(pl$end_zoom, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  grid::popViewport()
}
