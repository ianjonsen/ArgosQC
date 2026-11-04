##' @title run QC diagnostics for WC or SMRU tag workflows
##'
##' @description produces a map of all QC'd tracks and latitude and longitude
##' coverage plots to assess the QC run. The map shows each SSM-estimated track
##' in its own hue, with its Argos locations in a light tint of that hue; Argos
##' locations removed by the prefilter are semi-transparent points in the
##' track's dark shade. The coverage plots use the same hues, one panel per
##' deployment, with the SSM-estimated latitude or longitude drawn as a dark
##' line over the Argos locations, and the time outside the QC'd track shaded.
##'
##' @param fit the final aniMotum fit object from QC process
##' @param fit1 ignored (kept for compatibility)
##' @param what the SSM-estimated or rerouted locations to be used
##' @param cut logical; should predicted locations be dropped if keep = FALSE - ie. in a large data gap
##' @param data the standardized WC Locations or SMRU diag file (prior to truncation by metadata CTD start and end dates)
##' @param ssm ignored (kept for compatibility)
##' @param meta ignored (kept for compatibility)
##' @param lines ignored; the map always shows track lines (kept for compatibility)
##' @param obs ignored; the map always shows the Argos locations (kept for compatibility)
##' @param mpath path to write map file
##' @param dpath path to write all other diagnostic files
##' @param QCmode ignored (kept for compatibility)
##' @param tag_mfr ignored (kept for compatibility)
##' @param cid SMRU campaign id (from config file). Ignored if WC data is used.
##'
##'
##' @importFrom dplyr %>% group_by summarise pull
##' @importFrom sf st_as_sf st_transform st_cast st_bbox
##' @importFrom rnaturalearth ne_countries
##' @importFrom ggplot2 ggplot geom_sf geom_point geom_rect facet_wrap aes
##' @importFrom ggplot2 vars theme_minimal xlim ylim ggsave
##' @importFrom lubridate decimal_date
##' @importFrom aniMotum aes_lst
##' @importFrom kableExtra kable kable_styling
##' @importFrom readr write_csv
##'
##' @export
##'

diagnostics <-
  function(fit,
           fit1,
           what = "p",
           cut,
           data,
           ssm,
           meta,
           lines = FALSE,
           obs = FALSE,
           mpath = NULL,
           dpath = NULL,
           QCmode = "nrt",
           tag_mfr = "wc",
           cid = NULL) {

    if(is.null(mpath)) stop("A valid file path for the map must be provided")
    if(is.null(dpath)) stop("A valid file path for the diagnostics must be provided")

    ## map of all QC'd tracks: SSM-estimated tracks, each with its Argos
    ##  locations in a light tint of the track's hue (see qc_track_map())
    qc_track_map(fit, what = what, cut = cut, mpath = mpath, cid = cid)

    ## latitude and longitude coverage plots, in the map's style: each
    ##  deployment in its own hue, the SSM-estimated track over its Argos
    ##  locations (see qc_coverage_plot())
    qc_coverage_plot(fit, what = what, cut = cut, data = data, dpath = dpath, cid = cid)

    invisible(NULL)
  }

