## Latitude and longitude coverage plots (internal), written by diagnostics().
##
## One panel per deployment, in the same hue as its track on the map (see
##  qc_track_map()): the SSM-estimated locations as a dark line with points,
##  drawn over the Argos locations used in the fit in a light tint of the same
##  hue. Argos locations removed by the prefilter are semi-transparent points
##  in the dark shade. Tag locations not used in the QC (before the deployment
##  start, or outside the QC'd period) are small grey points, and the time
##  outside the QC'd period is shaded grey. Each panel's y range is the SSM
##  range plus 15%, so outlying Argos locations do not flatten the track; the
##  number of Argos locations outside that range is given in the panel title.

## one hue per deployment, stepped by the golden angle (shared with the map)
qc_track_hues <- function(ids) {
  ids <- sort(unique(as.character(ids)))
  hues <- ((seq_along(ids) - 1) * 137.508 + 15) %% 360
  list(dark = stats::setNames(grDevices::hcl(hues, c = 85, l = 45), ids),
       light = stats::setNames(grDevices::hcl(hues, c = 40, l = 80), ids))
}

## longitude in 0-360 for deployments whose locations span the antimeridian
qc_unwrap_lon <- function(lon, id, ssm_lon, ssm_id) {
  wrap <- vapply(split(ssm_lon, ssm_id), function(x) diff(range(x, na.rm = TRUE)) > 180,
                 logical(1))
  w <- wrap[as.character(id)] %in% TRUE
  lon[w & lon < 0] <- lon[w & lon < 0] + 360
  lon
}

## draw and save one coverage plot
##  locs: SSM-estimated locations (sf); obs: Argos locations used in the fit
##   (sf, with keep); raw: data frame of all tag locations with id, date, lat,
##   lon; coord: "lat" or "lon"
qc_coverage_draw <- function(locs, obs, raw, coord = c("lat", "lon"), file, title = NULL) {
  coord <- match.arg(coord)
  ll <- function(x) {
    xy <- sf::st_coordinates(sf::st_transform(x, 4326))
    data.frame(id = as.character(x$id), date = x$date, lon = xy[, 1], lat = xy[, 2])
  }
  ld <- ll(locs)
  od <- ll(obs)
  od$keep <- obs$keep
  rd <- data.frame(id = as.character(raw$id), date = as.POSIXct(raw$date, tz = "UTC"),
                   lon = as.numeric(raw$lon), lat = as.numeric(raw$lat))
  rd <- rd[rd$id %in% ld$id & !is.na(rd$date) & !is.na(rd$lat) & !is.na(rd$lon), ]

  if (coord == "lon") {
    od$lon <- qc_unwrap_lon(od$lon, od$id, ld$lon, ld$id)
    rd$lon <- qc_unwrap_lon(rd$lon, rd$id, ld$lon, ld$id)
    ld$lon <- qc_unwrap_lon(ld$lon, ld$id, ld$lon, ld$id)
  }
  ld$v <- ld[[coord]]
  od$v <- od[[coord]]
  rd$v <- rd[[coord]]

  ids <- sort(unique(ld$id))
  n <- length(ids)
  cols <- qc_track_hues(ids)

  ## panel ranges: time from the SSM track (plus 3%), values from the SSM
  ##  track (plus 15%)
  rng <- do.call(rbind, lapply(split(ld, ld$id), function(d) {
    tr <- range(as.numeric(d$date))
    tr <- tr + c(-1, 1) * max(0.03 * diff(tr), 86400)
    vr <- range(d$v)
    vr <- vr + c(-1, 1) * max(0.15 * diff(vr), 0.05)
    data.frame(id = d$id[1], t0 = tr[1], t1 = tr[2], v0 = vr[1], v1 = vr[2],
               start = min(d$date), end = max(d$date))
  }))
  inside <- function(d) {
    r <- rng[match(d$id, rng$id), ]
    as.numeric(d$date) >= r$t0 & as.numeric(d$date) <= r$t1
  }
  in_range <- function(d) {
    r <- rng[match(d$id, rng$id), ]
    d$v >= r$v0 & d$v <= r$v1
  }
  od <- od[inside(od), ]
  rd <- rd[inside(rd), ]
  ## tag locations not in the fit (before the deployment start, or dropped)
  key <- function(d) paste(d$id, as.numeric(d$date))
  rd <- rd[!key(rd) %in% key(od), ]

  ## Argos locations outside the y range, counted in the panel title
  n_out <- tapply(!in_range(od), factor(od$id, levels = ids), sum)
  n_out[is.na(n_out)] <- 0
  od <- od[in_range(od), ]
  rd <- rd[in_range(rd), ]
  lab <- stats::setNames(ifelse(n_out > 0, paste0(ids, " (", n_out, " Argos outside y range)"),
                                ids), ids)

  ## time outside the QC'd track, shaded
  sh <- rbind(data.frame(id = rng$id, xmin = .POSIXct(rng$t0, tz = "UTC"), xmax = rng$start),
              data.frame(id = rng$id, xmin = rng$end, xmax = .POSIXct(rng$t1, tz = "UTC")))
  ## invisible corners fix each panel's range
  bl <- rbind(data.frame(id = rng$id, date = .POSIXct(rng$t0, tz = "UTC"), v = rng$v0),
              data.frame(id = rng$id, date = .POSIXct(rng$t1, tz = "UTC"), v = rng$v1))

  ylab <- if (coord == "lat") "Latitude" else "Longitude"
  cap <- paste0("Dark line and points: SSM-estimated ", tolower(ylab), ". Light points: Argos ",
                "locations used in the fit, in the same hue. Semi-transparent dark points: Argos ",
                "locations removed by the prefilter.\nGrey points: tag locations not used in the QC ",
                "(e.g. before the deployment start). Grey shading: outside the QC'd track.")
  if (is.null(title)) title <- "QC'd tracks"
  title <- paste0(title, ": ", tolower(ylab), " coverage, ", n,
                  ifelse(n == 1, " deployment", " deployments"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_blank(data = bl, ggplot2::aes(x = date, y = v)) +
    ggplot2::geom_rect(data = sh, ggplot2::aes(xmin = xmin, xmax = xmax),
                       ymin = -Inf, ymax = Inf, fill = "grey90", colour = NA) +
    ggplot2::geom_point(data = rd, ggplot2::aes(x = date, y = v), colour = "grey55",
                        size = 0.7) +
    ggplot2::geom_point(data = od[!od$keep, ], ggplot2::aes(x = date, y = v, colour = id),
                        shape = 16, size = 1.2, alpha = 0.5) +
    ggplot2::geom_point(data = od[od$keep, ], ggplot2::aes(x = date, y = v, fill = id),
                        shape = 21, stroke = 0, size = 1.9, alpha = 0.9) +
    ggplot2::geom_line(data = ld, ggplot2::aes(x = date, y = v, colour = id), linewidth = 0.35) +
    ggplot2::geom_point(data = ld, ggplot2::aes(x = date, y = v, colour = id), size = 0.3) +
    ggplot2::scale_colour_manual(values = cols$dark, guide = "none") +
    ggplot2::scale_fill_manual(values = cols$light, guide = "none") +
    ggplot2::facet_wrap(ggplot2::vars(id), scales = "free", ncol = min(4, n),
                        labeller = ggplot2::as_labeller(lab)) +
    ggplot2::labs(title = title, caption = cap, x = NULL, y = ylab) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
                   panel.border = ggplot2::element_rect(fill = NA, colour = "grey70",
                                                        linewidth = 0.3),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = 8),
                   plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30"),
                   plot.title = ggplot2::element_text(face = "bold"),
                   axis.text = ggplot2::element_text(size = 7))

  rows <- ceiling(n / min(4, n))
  ggplot2::ggsave(file, p, width = 14, height = min(48, 1.2 + 2.3 * rows), units = "in",
                  dpi = 150, bg = "white", limitsize = FALSE)
  invisible(p)
}

## build and save the latitude and longitude coverage plots for diagnostics()
##  data: the tag locations (SMRU diag: ref, date, lat, lon; WC Locations:
##   DeploymentID, Date, Latitude, Longitude)
qc_coverage_plot <- function(fit, what = "predicted", cut = FALSE, data, dpath, cid = NULL) {
  d <- qc_track_map_data(fit, what = what, cut = cut)
  if ("DeploymentID" %in% names(data)) {
    raw <- data.frame(id = data$DeploymentID, date = data$Date,
                      lat = data$Latitude, lon = data$Longitude)
  } else {
    raw <- data.frame(id = data$ref, date = data$date, lat = data$lat, lon = data$lon)
  }
  tag <- if (!is.null(cid)) paste(cid, collapse = "_") else ""
  title <- if (!is.null(cid)) paste(cid, collapse = ", ") else NULL
  for (coord in c("lat", "lon")) {
    qc_coverage_draw(d$locs, d$obs, raw, coord = coord, title = title,
                     file = file.path(dpath, paste0(coord, "_coverage_", tag, ".jpg")))
  }
  invisible(NULL)
}
