## Map of QC'd tracks (internal), written by diagnostics().
##
## Each deployment has its own hue: the SSM-estimated track (line and points)
##  in a dark shade, and the Argos locations used in the fit in a light tint of
##  the same hue. Argos locations removed by the prefilter (keep = FALSE) are
##  semi-transparent filled circles in the track's dark shade. Circles mark the start of each SSM track, squares the end.
##  The map extent is the SSM track extent, so a track pulled away by an
##  outlying location is shown in full.

## extract the SSM-estimated and observed locations from one or more fits, in
##  the projection of the first fit
qc_track_map_data <- function(fit, what = "predicted", cut = FALSE) {
  fits <- if (inherits(fit, "ssm_df")) list(fit) else fit
  locs <- lapply(fits, function(f) grab_QC(f, what = what, cut = cut, as_sf = TRUE))
  prj <- sf::st_crs(locs[[1]])
  locs <- do.call(rbind, lapply(locs, function(l) {
    sf::st_transform(l, prj)[, c("id", "date")]
  }))
  obs <- do.call(rbind, lapply(fits, function(f) {
    o <- grab_QC(f, what = "data", as_sf = TRUE)
    if (!"keep" %in% names(o)) o$keep <- TRUE
    sf::st_transform(o, prj)[, c("id", "date", "keep")]
  }))
  list(locs = locs, obs = obs)
}

## draw and save the map. `locs` and `obs` are sf point objects in the same
##  projected crs, with columns id and date (and keep, for obs)
qc_track_map_draw <- function(locs, obs, file, title = NULL) {
  prj <- sf::st_crs(locs)
  ids <- sort(unique(as.character(locs$id)))
  n <- length(ids)

  ## one hue per deployment, stepped by the golden angle so neighbouring ids
  ##  differ; dark shade for the SSM track, light tint for its Argos locations
  hues <- ((seq_len(n) - 1) * 137.508 + 15) %% 360
  ssm_col <- stats::setNames(grDevices::hcl(hues, c = 85, l = 45), ids)
  obs_col <- stats::setNames(grDevices::hcl(hues, c = 40, l = 80), ids)

  xy <- sf::st_coordinates(locs)
  ld <- data.frame(id = as.character(locs$id), date = locs$date, x = xy[, 1], y = xy[, 2])
  ld <- ld[order(ld$id, ld$date), ]
  ## break a track line where predicted locations are missing (cut = TRUE)
  ld$seg <- unlist(lapply(split(ld$date, ld$id)[unique(ld$id)], function(t) {
    dt <- diff(as.numeric(t))
    cumsum(c(0, dt > 1.5 * stats::median(dt, na.rm = TRUE)))
  }))
  ld$grp <- paste(ld$id, ld$seg)
  ends <- do.call(rbind, lapply(split(ld, ld$id), function(d) {
    rbind(data.frame(d[1, c("id", "x", "y")], where = "start"),
          data.frame(d[nrow(d), c("id", "x", "y")], where = "end"))
  }))

  oxy <- sf::st_coordinates(obs)
  od <- data.frame(id = as.character(obs$id), keep = obs$keep, x = oxy[, 1], y = oxy[, 2])
  od <- od[od$id %in% ids, ]

  ## map extent: the SSM tracks, plus 5% on each side
  xr <- grDevices::extendrange(ld$x, f = 0.05)
  yr <- grDevices::extendrange(ld$y, f = 0.05)
  ## at least 100 km across, for tracks that barely move
  if (diff(xr) < 100) xr <- mean(xr) + c(-50, 50)
  if (diff(yr) < 100) yr <- mean(yr) + c(-50, 50)
  od <- od[od$x >= xr[1] & od$x <= xr[2] & od$y >= yr[1] & od$y <= yr[2], ]

  ## land, clipped to the map extent
  frame <- sf::st_as_sfc(sf::st_bbox(c(xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2]),
                                     crs = prj))
  land <- suppressWarnings(suppressMessages({
    wm <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
    wm <- sf::st_make_valid(sf::st_transform(sf::st_geometry(wm), prj))
    wm <- wm[!sf::st_is_empty(wm)]
    sf::st_make_valid(sf::st_intersection(wm, frame))
  }))

  cap <- paste0("Dark lines and points: SSM-estimated locations. Light points: Argos locations ",
                "used in the fit, in the same hue.\nSemi-transparent dark points: Argos locations ",
                "removed by the prefilter. Circle: start of track. Square: end of track.")
  if (is.null(title)) title <- "QC'd tracks"
  title <- paste0(title, ": ", n, ifelse(n == 1, " deployment, ", " deployments, "),
                  format(min(ld$date), "%d %b %Y"), " to ", format(max(ld$date), "%d %b %Y"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = land, fill = "grey80", colour = "grey55", linewidth = 0.2) +
    ggplot2::geom_point(data = od[!od$keep, ], ggplot2::aes(x = x, y = y, colour = id),
                        shape = 16, size = 0.9, alpha = 0.5) +
    ggplot2::geom_point(data = od[od$keep, ], ggplot2::aes(x = x, y = y, fill = id),
                        shape = 21, stroke = 0, size = 0.9, alpha = 0.8) +
    ggplot2::geom_path(data = ld, ggplot2::aes(x = x, y = y, colour = id, group = grp),
                       linewidth = 0.45) +
    ggplot2::geom_point(data = ld, ggplot2::aes(x = x, y = y, colour = id), size = 0.35) +
    ggplot2::geom_point(data = ends, ggplot2::aes(x = x, y = y, shape = where),
                        fill = ssm_col[ends$id], colour = "grey10", size = 2.6, stroke = 0.5) +
    ggplot2::scale_colour_manual(values = ssm_col, name = NULL) +
    ggplot2::scale_fill_manual(values = obs_col, guide = "none") +
    ggplot2::scale_shape_manual(values = c(start = 21, end = 22), guide = "none") +
    ggplot2::coord_sf(crs = prj, xlim = xr, ylim = yr, expand = FALSE) +
    ggplot2::labs(title = title, caption = cap, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_line(colour = "grey88", linewidth = 0.3),
                   plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30"),
                   plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = if (n <= 40) "bottom" else "none",
                   legend.text = ggplot2::element_text(size = 7)) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = min(4, n),
                                                   override.aes = list(linewidth = 1.2,
                                                                       size = 1.5)))

  ## page size follows the map's aspect ratio
  w <- 10
  legend_rows <- if (n <= 40) ceiling(n / min(4, n)) else 0
  h <- min(16, max(5, w * diff(yr) / diff(xr))) + 1.2 + 0.18 * legend_rows

  ggplot2::ggsave(file, p, width = w, height = h, units = "in", dpi = 300, bg = "white")
  invisible(p)
}

## build and save the map for diagnostics()
qc_track_map <- function(fit, what = "predicted", cut = FALSE, mpath, cid = NULL) {
  d <- qc_track_map_data(fit, what = what, cut = cut)
  file <- file.path(mpath, paste0("map_", if (!is.null(cid)) paste0(paste(cid, collapse = "_"), "_"),
                                  Sys.Date(), ".png"))
  qc_track_map_draw(d$locs, d$obs, file, title = if (!is.null(cid)) paste(cid, collapse = ", "))
}
