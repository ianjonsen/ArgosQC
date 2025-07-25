##' @title map_QC
##'
##' @description map aniMotum-estimated locations and behavioural indices with
##' coastline and projection options
##'
##' @param x a `aniMotum` ssm fit object with class `ssm_df` or (old) `fG_ssm`
##' @param y optionally, a `aniMotum` mpm fit object with class `mpm_df` or (old)
##' `fG_mpm`
##' @param what specify which location estimates to map: fitted, predicted or
##' rerouted
##' @param aes a list of map controls and aesthetics (shape, size, col, fill, alpha)
##' for each map feature (estimated locations, confidence ellipses, track lines,
##' observed locations, land masses, water bodies). Constructed by `aes_lst()` and
##' can be modified for custom maps (see examples)
##' @param by.id when mapping multiple tracks, should locations be coloured by
##' id (logical; default = TRUE if `nrow(x) > 1` else FALSE; ignored if behavioural
##' index provided)
##' @param by.date when mapping single tracks, should locations be coloured by
##' date (logical; default = FALSE; ignored if behavioural
##' index provided)
##' @param cut logical; should predicted locations be dropped from mapping
##' if keep = FALSE. default = FALSE.
##' @param crs `proj4string` for re-projecting locations, if NULL the
##' default projection (Mercator) for the fitting the SSM will be used
##' @param ext.rng proportion (can exceed 1) to extend the plot range in x and y
##' dimensions
##' @param buffer distance (in km) to buffer locations for subsetting land
##' polygons (default = 10000). If map extents are expanded by many factors then
##' the buffer distance may need to be increased, otherwise this should not be
##' used. Ignored if `map_type != "default"`.
##' @param normalise logical; if output includes a move persistence estimate,
##' should g (the move persistence index) be normalised to have minimum = 0 and
##' maximum = 1 (default = TRUE).
##' @param group logical; should g be normalised among individuals as a group,
##' a 'relative g', or separately to highlight regions of lowest and highest move
##' persistence along a track (default = FALSE).
##' @param silent logical; generate maps silently (default = FALSE).
##'
##' @return a map as a ggplot2 object
##'
##' @importFrom ggplot2 ggplot geom_sf aes ggtitle xlim ylim unit
##' @importFrom ggplot2 element_text theme  scale_fill_gradientn scale_fill_manual
##' @importFrom ggplot2 element_blank scale_colour_manual scale_colour_gradientn
##' @importFrom ggplot2 coord_sf
##' @importFrom sf st_bbox st_transform st_crop st_as_sf st_as_sfc st_buffer st_make_valid
##' @importFrom sf st_crs st_coordinates st_cast st_multipolygon st_polygon st_union
##' @importFrom utils data
##' @importFrom grDevices extendrange grey
##' @importFrom dplyr group_by summarise
##' @importFrom grDevices hcl.colors
##' @importFrom aniMotum aes_lst join
##'
##'
##' @export

map_QC <- function(x,
                y = NULL,
                what = c("fitted", "predicted", "rerouted"),
                aes = aes_lst(),
                by.id = TRUE,
                by.date = FALSE,
                cut = FALSE,
                crs = NULL,
                ext.rng = c(0.05, 0.05),
                buffer = 10000,
                normalise = TRUE,
                group = FALSE,
                silent = FALSE) {

  what <- match.arg(what)
  stopifnot("x must be a aniMotum ssm fit object with class `ssm_df`" =
              any(inherits(x, "ssm_df"), inherits(x, "fG_ssm")))
  stopifnot("y must either be NULL or a aniMotum mpm fit object with class `mpm_df`" =
              any(inherits(y, "mpm_df"), inherits(y, "fG_mpm"), is.null(y)))
  stopifnot("individual `ssm` fit objects with differing projections not currently supported" =
              length(unique(sapply(x$ssm, function(.) st_crs(.$predicted)$epsg))) == 1)
  if(!is.null(crs)) {
    stopifnot("crs must be a proj4string with units=km,
              \n eg. `+proj=stere +lat_0=-90 +lon_0=0 +datum=WGS84 +units=km +no_defs`" =
                is.character(crs))
  }

  ## estimated locations in projected form
  if (all(what == "rerouted", !"rerouted" %in% names(x$ssm[[1]]))){
    stop("what = 'rerouted' can not be used as model fits do not have rerouted paths")
  } else {
    loc_sf <- grab_QC(x, what = what, cut = cut, as_sf = TRUE, normalise = normalise, group = group)
  }

  ## handle mpm fits if present
  if(!is.null(y)) {
    loc_sf <- join(x, y, what.ssm = what, as_sf = TRUE, normalise = normalise, group = group)
  }

  if(!is.null(crs)) {
    if (length(grep("+units=km", crs, fixed = TRUE)) == 0) {
      cat("converting projection units from m to km to match SSM output")
      crs <- paste(crs, "+units=km")
    }
    loc_sf <- st_transform(loc_sf, crs = crs)
  } else {
    crs <- st_crs(loc_sf)
  }

  ## generate track lines & cast to MULTILINESTRING for plot efficiency
  if (aes$line) {
    line_sf <- group_by(loc_sf, id)
    line_sf <- summarise(line_sf, do_union = FALSE)
    line_sf <- st_cast(line_sf, "MULTILINESTRING")
  } else {
    line_sf <- NULL
  }

  ## calc confidence ellipses around estimated locations & dissolve overlapping segments
  if(aes$conf) {
    locs <- st_coordinates(loc_sf)
    locs <- data.frame(id = loc_sf$id, x = locs[,1], y = locs[,2], x.se = loc_sf$x.se, y.se = loc_sf$y.se)
    ## check for NA's in se estimates
    if(any(is.na(locs$x.se),is.na(locs$y.se))) {
      aes$conf <- FALSE
      message("NA's detected in location standard errors, can not render confidence ellipses")
    }
  }
  if(aes$conf) {
    locs.lst <- split(locs, locs$id)
    conf_poly <- lapply(locs.lst, function(x) {
      conf <- lapply(1:nrow(x), function(j)
        with(x, aniMotum:::elps(x[j], y[j], x.se[j], y.se[j], 90)))
      lapply(conf, function(x)
        st_polygon(list(x))) %>%
        st_multipolygon()
    })

    conf_sf <- st_as_sfc(conf_poly)
    conf_sf <- st_as_sf(conf_sf, crs = st_crs(loc_sf))
    conf_sf$id <- unique(loc_sf$id)
    ## dissolve individual polygons where they overlap one another
    conf_sf <- st_union(conf_sf, by_feature = TRUE)
    conf_sf <- st_make_valid(conf_sf)
  } else {
    conf_sf <- NULL
  }


  ## get observations & set map extents
  if (aes$obs) {
    obs_sf <- st_transform(subset(grab_QC(x, "data", as_sf = TRUE), keep), crs = crs)
    extents <- st_bbox(obs_sf)
  } else {
    obs_sf <- NULL
    extents <- st_bbox(loc_sf)
  }

  extents[c("xmin", "xmax")] <- extendrange(extents[c("xmin", "xmax")],
                                           f = ext.rng[1])
  extents[c("ymin", "ymax")] <- extendrange(extents[c("ymin", "ymax")],
                                           f = ext.rng[2])

  ## select appropriate mapping fn based on x, y inputs
  if(all(nrow(x) == 1)) {
    m <- aniMotum:::map_single_track_base(map_type = "default",
                               obs_sf,
                               conf_sf,
                               line_sf,
                               loc_sf,
                               by.date,
                               extents,
                               buffer,
                               aes,
                               silent)
  }
  else if(all(nrow(x) > 1)) {
    m <- aniMotum:::map_multi_track_base(map_type = "default",
                         obs_sf,
                         conf_sf,
                         line_sf,
                         loc_sf,
                         by.id,
                         by.date,
                         extents,
                         buffer,
                         aes,
                         silent)
  }

  return(m)

}
