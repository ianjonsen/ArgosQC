##' @title ATN WC tag QC workflow
##'
##' @description Wrapper function that executes the complete workflow from data
##' download to SSM-appended tag data files output as CSV files.
##'
##' @param wd the path to the working directory that contains: 1) the data directory
##' where tag data files are stored (if source = `local`); 2) the metadata directory
##' where all metadata files are stored; and 3) the destination directory for QC output.
##' @param datadir the name of the data directory (to be added to the `wd` path).
##' @param meta.file the metadata filename. Must reside within the `wd`
##' @param outdir the name of the QC output directory where CSV files will be
##' written (to be added to the `wd` path).
##' @param dropIDs the ATN DeploymentID's that are to be ignored during the QC process
##' @param model the aniMotum SSM model to be used for the location QC - typically
##' either `rw` or `crw`.
##' @param vmax for SSM fitting; max travel rate (m/s) to identify implausible locations
##' @param time.step the prediction interval (in hours) to be used by the SSM
##' @param proj the proj4string to be used for the location data & for the SSM-estimated
##' locations. The default (NULL) will result in one of 3 projections being used,
##' depending on whether the centroid of the observed latitudes lie in N or S polar
##' regions, temperate or equatorial regions, or if tracks straddle (or lie close to)
##' -180,180 longitude.
##' @param reroute whether QC'd tracks should be re-routed off of land
##' (default is FALSE). Note, in some circumstances this can substantially increase
##' processing time. Default land polygon data are sourced from the
##' `ropensci/rnaturalearthhires` R package.
##' @param dist the distance in km from outside the convex hull of observed
##' locations from which to select land polygon data for re-routing. Ignored if
##' `reroute = FALSE`.
##' @param buffer the distance in km to buffer rerouted locations from the
##' coastline. Ignored if `reroute = FALSE`.
##' @param centroids whether centroids are to be included in the visibility graph
##' mesh used by the rerouting algorithm. See `?pathroutr::prt_visgraph` for
##' details. Ignored if `reroute = FALSE`.
##' @param cut logical; should predicted locations be dropped if they lie within
##' in a large data gap (default is FALSE).
##' @param min.gap the minimum data gap duration (h) to be used for cutting
##' predicted locations (default is 72 h)
##' @param QCmode one of either `nrt` for Near Real-Time QC or `dm` for Delayed
##' Mode QC.
##' @param p.int prediction interval to use for sub-sampling predicted locations
##'  (default = 6 h)
##' @param output logical; should fn return a list of QC-generated objects. This
##' results in a single large object return that can be useful for troubleshooting
##' QC errors or undesirable results.
##' @param ... additional arguments to be passed to SSM model fitting, see
##' `aniMotum::fit_ssm` & `aniMotum::route_path` for details.
##'
##' @importFrom stringr str_split str_length
##'
##' @md
##' @export

atn_wc_qc <- function(wd = NULL,
                      datadir = NULL,
                      meta.file = NULL,
                      outdir = NULL,
                      collab.id = NULL,
                      wc.akey = NULL,
                      wc.skey = NULL,
                      dropIDs = NULL,
                      model = "rw",
                      vmax = 3,
                      time.step = 6,
                      proj = NULL,
                      reroute = FALSE,
                      dist = 1000,
                      buffer = 0.5,
                      centroids = TRUE,
                      cut = FALSE,
                      min.gap = 72,
                      QCmode = "nrt",
                      p.int = 6,
                      output = FALSE,
                      ...) {

  if(!file.exists(wd)) stop("Working directory `wd` does not exist")
  else setwd(wd)
  if(!file.exists(file.path(wd, outdir))) {
    dir.create(file.path(outdir), recursive = TRUE, showWarnings = FALSE)
    message(paste(outdir, "directory created"))
  }

  what <- "p"
  if(reroute) what <- "r"

  # mdbs <- list.dirs(file.path(wd, datadir), full.names = FALSE, recursive = FALSE)
  # mdbs <- str_split(mdbs, pattern = "\\-", simplify = TRUE)[,1]
  # mdbs <- unique(mdbs)
  # ## filter out non-SMRU campaign ids
  # mdbs <- mdbs[str_length(mdbs) <= 5]

  ## Download WC tag data from WC Portal API & extract partial deployment metadata
  download_data(
    dest = file.path(wd, datadir),
    source = "wc",
    unzip = TRUE,
    wc.akey = wc.akey,
    wc.skey = wc.skey,
    owner.id = collab.id
  )

  ## read SMRU tag file data from .mdb/source files
  wc <- wc_pull_data(datadir)


  ## get metadata
  meta <- get_metadata(source = "atn",
                       tag_data = wc,
                       tag_mfr = "wc",
                       dropIDs = dropIDs,
                       file = meta.file
  )


  ## prepare location data
  locs_sf <- wc_prep_loc(wc,
                         meta,
                         dropIDs = dropIDs,
                         crs = NULL,
                         QCmode = QCmode)


  fit1 <- fit2 <- vector("list", length = length(locs_sf))

  ## First pass SSM-filter - separately by species
  fit1 <- lapply(1:length(fit1), function(i) {
    multi_filter(
      locs_sf[[i]],
      vmax = vmax,
      model = model,
      ts = time.step,
      ...
    )
  })

  ## Second pass SSM-filter - separately by species
  fit2 <- lapply(1:length(fit1), function(i){
    redo_multi_filter(
      fit1[[i]],
      locs_sf[[i]],
      vmax = vmax,
      model = model,
      ts = time.step,
      map = list(psi = factor(NA)),
      reroute = TRUE,
      dist = dist,
      buffer = buffer,
      centroids = centroids,
      ...
    )
  })
  names(fit1) <- names(fit2) <- names(locs_sf)

  ## Mark SSM track segments for removal in data gaps > min.gap hours long
  if (cut) {
    fit2 <- lapply(1:length(fit2), function(i) {
      ssm_mark_gaps(fit2[[i]], min.gap = min.gap)
    })

    names(fit2) <- names(locs_sf)
  }


  ## Append SSM-estimated locations to tag datafiles
  wc_ssm <- wc_append_ssm(wc = wc,
                          fit = fit2,
                          what = what,
                          meta = meta,
                          cut = cut,
                          dropIDs = dropIDs)


  ## Diagnostic plots
  diagnostics(
    fit2,
    fit1,
    what = what,
    cut = cut,
    data = wc$Locations,
    ssm = wc_ssm,
    meta = meta,
    lines = TRUE,
    obs = FALSE,
    mpath = file.path(wd, "qc", "wc", "maps"),
    dpath = file.path(wd, "qc", "wc", "diag"),
    QCmode = "dm"
  )

  ## write SSM-annotated datafiles to .csv
  lapply(fit2, function(x) {
    wc_write_csv(wc_ssm,
                 x,
                 what = what,
                 meta,
                 program = "atn",
                 path = outdir,
                 dropIDs = dropIDs,
                 suffix = "_dm",
                 pred.int = p.int)
  })

  if (output) {
    return(list(dropIDs=dropIDs,
                wc=wc,
                meta=meta,
                locations_sf=locs_sf,
                fit1=fit1,
                fit2=fit2,
                wc_ssm=wc_ssm))
  }
}
