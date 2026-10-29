##' @title Zoom in on part of a deployment's time series to choose track
##' start and end dates
##'
##' @description Draws any time window of one deployment from the
##' [smru_suggest_trim()] review data: a map of the locations in the window,
##' coloured by time, beside their latitude and longitude against time and the
##' counts of locations, dives and CTD profiles. All location classes are shown,
##' including Z. The suggested end date, the anchor and the dates in the draft
##' track truncation file are drawn as vertical lines, so a start or end date can
##' be placed at an exact time, clear of a messy stretch of track. Locations
##' more than `outlier_km` from the running median position (7 locations,
##' centred) are ringed in red: a cluster of rings marks a messy stretch. A table
##' of the last `n` locations in the window is printed, with the distance, time
##' and speed from the previous location and the distance from the running
##' median; outlying locations are marked `*`.
##'
##' In [imos_smru_dm_qc()] it is offered at the track end date review menu.
##'
##' @param trim the data frame returned by [smru_suggest_trim()]
##' @param ref the deployment reference (`-` and `_` are treated as the same)
##' @param from,to the window, as `"YYYY-MM-DD"` or `"YYYY-MM-DD HH:MM"` (UTC).
##'   Default: the start and the end of the track
##' @param draft optional path to the draft track truncation file
##'   (`<cid>_trimIDs_draft.csv`); its `start_date` and `end_date` for `ref` are
##'   drawn as blue dashed lines
##' @param outlier_km distance (km) from the running median position beyond
##'   which a location is marked as outlying
##' @param file optional PDF file to write the plot to. Default: the current
##'   graphics device (the RStudio Plots pane, which can be enlarged)
##' @param n the number of locations listed: the last `n` in the window
##'   (`0` lists none)
##'
##' @return invisibly, a data frame of the locations in the window
##'
##' @examples
##' \dontrun{
##' trim <- smru_suggest_trim(wd = dm, config = "pending/config_ct23.json")
##' trim_zoom(trim, "ct23-C12-06", from = "2007-08-01", to = "2007-08-20 12:00",
##'           draft = file.path(dm, "ct23_trimIDs_draft.csv"))
##' }
##'
##' @md
##' @export

trim_zoom <- function(trim,
                      ref,
                      from = NULL,
                      to = NULL,
                      draft = NULL,
                      outlier_km = 100,
                      file = NULL,
                      n = 30) {

  pdl <- attr(trim, "plot_data")
  if (is.null(pdl)) {
    stop("`trim` must be the data frame returned by smru_suggest_trim().", call. = FALSE)
  }
  ref <- qc_match_refs(ref, names(pdl))
  if (!ref %in% names(pdl)) {
    stop("Unknown deployment: ", ref, ". Deployments: ", paste(names(pdl), collapse = ", "),
         call. = FALSE)
  }
  pd <- pdl[[ref]]

  fa <- if (!is.null(pd$f_all)) pd$f_all else pd$f
  fa <- fa[order(fa$date), ]
  if (nrow(fa) == 0) stop("No locations for ", ref, call. = FALSE)

  parse_time <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(default)
    t <- lubridate::ymd_hms(trimws(x), truncated = 3, tz = "UTC", quiet = TRUE)
    if (is.na(t)) stop("Unreadable date: ", x, ". Use YYYY-MM-DD or YYYY-MM-DD HH:MM (UTC).",
                       call. = FALSE)
    t
  }
  from <- parse_time(from, min(fa$date))
  to <- parse_time(to, max(fa$date))
  if (to <= from) stop("`to` must be later than `from`.", call. = FALSE)

  ## steps between consecutive locations (all classes)
  km <- c(NA_real_, dm_dist_km(utils::head(fa$lat, -1), utils::head(fa$lon, -1),
                               fa$lat[-1], fa$lon[-1]))
  hrs <- c(NA_real_, diff(as.numeric(fa$date)) / 3600)
  speed <- ifelse(!is.na(hrs) & hrs > 0, km * 1000 / (hrs * 3600), NA_real_)
  ## distance from the running median position of 7 locations (centred)
  k <- if (nrow(fa) >= 7) 7 else if (nrow(fa) >= 3) 3 else 1
  rm_lat <- if (k > 1) stats::runmed(fa$lat, k, endrule = "median") else fa$lat
  rm_lon <- if (k > 1) stats::runmed(fa$lon, k, endrule = "median") else fa$lon
  km_rm <- dm_dist_km(fa$lat, fa$lon, rm_lat, rm_lon)
  tab <- data.frame(date = fa$date, lc = as.character(fa$lc),
                    lat = round(fa$lat, 4), lon = round(fa$lon, 4),
                    km_from_previous = round(km, 1), hours_from_previous = round(hrs, 2),
                    speed_m_s = round(speed, 2), km_from_running_median = round(km_rm, 1),
                    outlier = ifelse(km_rm > outlier_km, "*", ""),
                    stringsAsFactors = FALSE)
  w <- tab[tab$date >= from & tab$date <= to, ]
  if (nrow(w) == 0) stop("No locations for ", ref, " between ", format(from), " and ",
                         format(to), call. = FALSE)

  cls <- function(lc) ifelse(lc %in% c("1", "2", "3"), "Class 1-3",
                             ifelse(lc %in% c("0", "A"), "Class 0 or A",
                                    ifelse(lc == "B", "Class B", "Class Z")))
  w$class <- factor(cls(w$lc), levels = c("Class 1-3", "Class 0 or A", "Class B", "Class Z"))

  ## plotted range: the locations that are not outlying, padded. Outlying
  ##  locations beyond it are left off the map and the latitude and longitude
  ##  panels (they are listed in the table), so the track itself stays visible
  core <- w[w$outlier != "*", ]
  if (nrow(core) < 3) core <- w
  rng <- function(x) {
    r <- range(x, na.rm = TRUE)
    r + c(-1, 1) * max(0.1, diff(r) * 0.15)
  }
  lat_r <- rng(core$lat)
  lon_r <- rng(core$lon)
  w$in_range <- w$lat >= lat_r[1] & w$lat <= lat_r[2] & w$lon >= lon_r[1] & w$lon <= lon_r[2]
  n_off <- sum(!w$in_range)
  wi <- w[w$in_range, ]

  ## dates drawn as lines
  lines <- data.frame(t = as.POSIXct(character(0), tz = "UTC"), what = character(0))
  add_line <- function(t, what) {
    if (length(t) == 0 || is.na(t)) return(invisible(NULL))
    lines <<- rbind(lines, data.frame(t = as.POSIXct(t, tz = "UTC"), what = what))
  }
  add_line(pd$anchor, "anchor (last CTD profile or dive)")
  add_line(pd$end, "suggested end date")
  if (!is.null(draft) && file.exists(draft)) {
    d <- readr::read_csv(draft, col_types = readr::cols(.default = readr::col_character()),
                         progress = FALSE)
    d$ref <- qc_match_refs(d$ref, names(pdl))
    if (ref %in% d$ref) {
      dr <- d[d$ref == ref, ][1, ]
      if ("start_date" %in% names(dr)) add_line(parse_time(dr$start_date, NA), "draft start_date")
      if ("end_date" %in% names(dr)) add_line(parse_time(dr$end_date, NA), "draft end_date")
    }
  }
  lines <- lines[lines$t >= from & lines$t <= to, ]
  line_cols <- c("anchor (last CTD profile or dive)" = "black",
                 "suggested end date" = "red",
                 "draft start_date" = "#2166ac",
                 "draft end_date" = "#2166ac")
  line_types <- c("anchor (last CTD profile or dive)" = 1, "suggested end date" = 2,
                  "draft start_date" = 2, "draft end_date" = 2)

  ## map: the whole track in grey, the window coloured by time; outliers ringed
  tnum <- as.numeric(wi$date)
  lab_time <- function(b) format(as.POSIXct(b, origin = "1970-01-01", tz = "UTC"), "%d %b %H:%M")
  pm <- ggplot2::ggplot() +
    ggplot2::geom_path(data = tab[tab$outlier != "*", ], ggplot2::aes(x = lon, y = lat),
                       colour = "grey88", linewidth = 0.3) +
    ggplot2::geom_path(data = wi, ggplot2::aes(x = lon, y = lat, colour = tnum),
                       linewidth = 0.4) +
    ggplot2::geom_point(data = wi, ggplot2::aes(x = lon, y = lat, colour = tnum, shape = class),
                        size = 1.6) +
    ggplot2::geom_point(data = wi[wi$outlier == "*", ], ggplot2::aes(x = lon, y = lat),
                        shape = 1, size = 3.2, colour = "red", stroke = 0.6) +
    ggplot2::scale_colour_viridis_c(name = "Time (UTC)", labels = lab_time) +
    ggplot2::scale_shape_manual(values = c("Class 1-3" = 16, "Class 0 or A" = 17,
                                           "Class B" = 15, "Class Z" = 4),
                                drop = FALSE, name = NULL) +
    ggplot2::coord_quickmap(xlim = lon_r, ylim = lat_r) +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = paste0("Red rings: locations more than ", outlier_km,
                                   " km from the running median position.")) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                   legend.key.width = grid::unit(1.6, "cm"))

  ## time series: latitude, longitude, and counts per hour, 6 hours or day
  span_d <- as.numeric(difftime(to, from, units = "days"))
  bin <- if (span_d <= 3) 3600 else if (span_d <= 14) 6 * 3600 else 86400
  bin_lab <- if (bin == 3600) "per hour" else if (bin == 21600) "per 6 hours" else "per day"
  count <- function(t, what) {
    t <- t[t >= from & t <= to]
    if (length(t) == 0) return(NULL)
    b <- as.POSIXct(floor(as.numeric(t) / bin) * bin + bin / 2, origin = "1970-01-01", tz = "UTC")
    tb <- table(b)
    data.frame(date = as.POSIXct(names(tb), tz = "UTC"), value = as.numeric(tb),
               panel = paste("Counts", bin_lab), series = what)
  }
  cnt <- rbind(count(w$date, "Argos locations"), count(pd$d_t, "Dives"),
               count(pd$c_t, "CTD profiles"))
  pts <- rbind(data.frame(date = wi$date, value = wi$lat, panel = "Latitude", class = wi$class,
                          outlier = wi$outlier),
               data.frame(date = wi$date, value = wi$lon, panel = "Longitude", class = wi$class,
                          outlier = wi$outlier))
  panels <- c("Latitude", "Longitude", paste("Counts", bin_lab))
  pts$panel <- factor(pts$panel, levels = panels)
  if (!is.null(cnt)) cnt$panel <- factor(cnt$panel, levels = panels)
  h <- pd$h
  if (!is.null(h) && nrow(h) > 0) {
    h <- h[h$e_date >= from & h$s_date <= to, ]
    h$s_date <- pmax(h$s_date, from)
    h$e_date <- pmin(h$e_date, to)
  }

  pt <- ggplot2::ggplot()
  if (!is.null(h) && nrow(h) > 0) {
    pt <- pt + ggplot2::geom_rect(data = h, ggplot2::aes(xmin = s_date, xmax = e_date,
                                                         ymin = -Inf, ymax = Inf),
                                  fill = "#f4a582", alpha = 0.35)
  }
  pt <- pt +
    ggplot2::geom_blank(data = data.frame(date = from, value = NA_real_,
                                          panel = factor(panels, levels = panels)),
                        ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(data = pts, ggplot2::aes(x = date, y = value), colour = "grey75",
                       linewidth = 0.25) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = date, y = value, shape = class,
                                                 colour = class), size = 1.1) +
    ggplot2::geom_point(data = pts[pts$outlier == "*", ], ggplot2::aes(x = date, y = value),
                        shape = 1, size = 2.6, colour = "red", stroke = 0.5) +
    ggplot2::scale_shape_manual(values = c("Class 1-3" = 16, "Class 0 or A" = 17,
                                           "Class B" = 15, "Class Z" = 4),
                                drop = FALSE, name = NULL, guide = "none") +
    ggplot2::scale_colour_manual(values = c("Class 1-3" = "#1b7837", "Class 0 or A" = "#6a51a3",
                                            "Class B" = "#8c8c8c", "Class Z" = "#bdbdbd",
                                            "Argos locations" = "#4d4d4d",
                                            "Dives" = "#2166ac", "CTD profiles" = "#b2182b"),
                                 name = NULL)
  if (!is.null(cnt)) {
    pt <- pt + ggplot2::geom_step(data = cnt, ggplot2::aes(x = date, y = value, colour = series),
                                  direction = "mid", linewidth = 0.4)
  }
  if (nrow(lines) > 0) {
    pt <- pt + ggplot2::geom_vline(data = lines, ggplot2::aes(xintercept = t, linetype = what),
                                   colour = line_cols[lines$what], linewidth = 0.5) +
      ggplot2::scale_linetype_manual(values = line_types, name = NULL)
  }
  pt <- pt +
    ggplot2::facet_wrap(~panel, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_datetime(limits = c(from, to), breaks = scales::breaks_pretty(n = 10),
                              labels = function(b) format(b, "%d %b\n%H:%M", tz = "UTC")) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")

  title <- paste0(ref, ": ", format(from, "%Y-%m-%d %H:%M"), " to ", format(to, "%Y-%m-%d %H:%M"),
                  " UTC (", nrow(w), " locations)")
  sub <- paste0("Shaded: haul-out records. Lines: anchor (solid black), suggested end date ",
                "(red dashed), draft start_date and end_date (blue dashed).\nRed rings: more than ",
                outlier_km, " km from the running median position.",
                if (n_off > 0) paste0(" ", n_off, " outlying locations beyond the plotted range are ",
                                      "not shown (see the table)."))

  draw <- function() {
    grid::grid.newpage()
    lay <- grid::grid.layout(2, 2, heights = grid::unit(c(0.1, 0.9), "npc"),
                             widths = grid::unit(c(0.42, 0.58), "npc"))
    grid::pushViewport(grid::viewport(layout = lay))
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2))
    grid::grid.text(title, x = 0.01, y = 0.78, just = "left",
                    gp = grid::gpar(fontsize = 13, fontface = "bold"))
    grid::grid.text(sub, x = 0.01, y = 0.3, just = "left", gp = grid::gpar(fontsize = 9))
    grid::popViewport()
    print(pm, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(pt, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
    grid::popViewport()
  }
  if (!is.null(file)) {
    grDevices::pdf(file, width = 16, height = 9)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  suppressWarnings(draw())

  if (n > 0) {
    show <- utils::tail(w[, c("date", "lc", "lat", "lon", "km_from_previous",
                              "hours_from_previous", "speed_m_s", "km_from_running_median",
                              "outlier")], n)
    show$date <- format(show$date, "%Y-%m-%d %H:%M:%S")
    cat("\nLast ", nrow(show), " of ", nrow(w), " locations in the window (", ref, "):\n", sep = "")
    print(show, row.names = FALSE)
  }
  invisible(w)
}
