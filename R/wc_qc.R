##' @title WC tag QC workflow
##'
##' @description Wrapper function that executes the complete workflow from data
##' download to SSM-appended tag data files output as CSV files.
##'
##' @param wd the path to the working directory that contains: 1) the data
##' directory where tag data files are stored (if source = `local`); 2) the
##' metadata directory where all metadata files are stored; and 3) the
##' destination directory for QC output.
##' @param config a hierarchical JSON configuration file containing the following
##' blocks, each with a set of block-specific parameters:
##' * `setup` config block specifies paths to required data, metadata & output
##' directories:
##'   * `program` the national (or other) program of which the data is a part.
##'   Current options are: `atn`, or `irap`.
##'   * `data.dir` the name of the data directory. Must reside within the `wd`.
##'   * `meta.file` the metadata filename. Must reside within the `wd`. Can be NULL,
##'   in which case, the `meta` config block (see below) must be present &
##'   tag-specific metadata are scraped from the SMRU data server.
##'   * `maps.dir` the directory path to write diagnostic maps of QC'd tracks.
##'   * `diag.dir` the directory path to write diagnostic time-series plots of
##'   QC'd lon & lat.
##'   * `output.dir` the directory path to write QC output CSV files. Must reside
##'   within the `wd`.
##'   * `return.R` logical; should the function return a list of QC-generated
##'   objects to the R works pace. This results in a single large object
##'   containing the following elements:
##'     * `cid` the SMRU campaign ID
##'     * `dropIDs` the SMRU Reference ID's droppped from the QC process
##'     * `smru` the SMRU tag data tables extracted from the downloaded .mdb file
##'     * `meta` the working metadata
##'     * `locs_sf` the projected location data to be passed as input to the SSM
##'     * `fit1` the initial SSM output fit object
##'     * `fit2` the final SSM output fit object including re-routed locations if
##'     specified.
##'     * `smru_ssm` the SSM-annotated SMRU tag data tables.
##'   This output object can be useful for troubleshooting undesirable results
##'   during supervised or delayed-mode QC workflows.
##'
##' * `harvest` config block specifies data harvesting parameters:
##'   * `download` a logical indicating whether tag data are to be downloaded from
##'   the WC data portal (TRUE) or read from the local `data.dir` (FALSE).
##'   * `collab.id` (optional) the WC data owner ID associated with the data to
##'   be downloaded. Ignored (if provided) when `harvest$download`:FALSE.
##'   * `wc.akey` (optional) the WC access key for API access to the data portal.
##'   Ignored (if provided) when `harvest$download`:FALSE.
##'   * `wc.skey` (optional) the WC secret key for API access to the data portal.
##'   Ignored (if provided) when `harvest$download`:FALSE.
##'   * `dropIDs` the WC UUID(s) for specific tag data set(s) that is/are to be
##'   ignored during the QC process. Can be NULL.
##'
##' * `model` config block specifies model- and data-specific parameters:
##'   * `model` the aniMotum SSM model to be used for the location QC - typically
##' either `rw` or `crw`.
##'   * `vmax` for SSM fitting; max travel rate (m/s) to identify implausible
##'   locations
##'   * `time.step` the prediction interval (in decimal hours) to be used by the
##'   SSM
##'   * `proj` the proj4string to be used for the location data & for the
##'   SSM-estimated locations. Can be NULL, which will result in one of 5
##'   projections being used, depending on whether the centroid of the observed
##'   latitudes lies in N or S polar regions, temperate or equatorial regions, or
##'   if tracks straddle (or lie close to) -180,180 longitude.
##'   * `reroute` a logical; whether QC'd tracks should be re-routed off of land
##'   (default is FALSE). Note, in some circumstances this can substantially
##'   increase processing time. Default land polygon data are sourced from the
##'   `ropensci/rnaturalearthhires` R package.
##'   * `dist` the distance in km from outside the convex hull of observed
##'   locations from which to select land polygon data for re-routing. Ignored
##'   if `reroute = FALSE`.
##'   * `barrier` the file path (must be within the working directory) for a
##'   shapefile to use for the land barrier. If NULL (default) then the default
##'   `rnaturalearth` coastline polygon data is used.
##'   * `buffer` the distance in km to buffer rerouted locations from the
##'   coastline. Ignored if `reroute = FALSE`.
##'   * `centroids` whether centroids are to be included in the visibility graph
##'   mesh used by the rerouting algorithm. See `?pathroutr::prt_visgraph` for
##'   details. Ignored if `reroute = FALSE`.
##'   * `cut` logical; should predicted locations be dropped if they lie within
##'   in a large data gap (default is FALSE).
##'   * `min.gap` the minimum data gap duration (h) to be used for cutting
##'   predicted locations (default is 72 h)
##'   * `QCmode` one of either `nrt` for Near Real-Time QC or `dm` for Delayed
##'   Mode QC.
##'   * `pred.int` the prediction interval (h) to use for sub-sampling predicted
##'   locations prior to interpolation of QC'd locations to tag data file event
##'   times.
##'
##'
##' @importFrom stringr str_split str_length
##' @importFrom jsonlite read_json
##' @importFrom testthat skip_if is_testing
##'
##' @md
##' @export

wc_qc <- function(wd,
                      config) {

  if(!file.exists(wd)) stop("Working directory `wd` does not exist")
  else setwd(wd)

#  skip_if(is_testing(), "Skipping - test in development")

  conf <- read_json(config, simplifyVector = TRUE)

  ## define metadata source - if no metadata file supplied in config file then
  ##  build deployment metadata from WC Data Portal xml dump
  if(is.na(conf$setup$meta.file)) {
    conf$setup$meta.file <- NULL
    meta.source <- "wc"

  } else {
    ## specifies program-specific metadata
    meta.source <- conf$setup$program
  }


  ## check for metadata file, throw error if missing as there's currently no
  ##    alternative for WC data
#  if(is.na(conf$setup$meta.file)) stop("A metadata file must be provided")

  ## Create output dirs if they do not exist
  ##    NOTE: dir names can NOT include spaces or _
  ##    as these will mess up data download &/or file extraction
  dir.create(file.path(wd, conf$setup$data.dir),
             showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(wd, conf$setup$maps.dir),
             showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(wd, conf$setup$diag.dir),
             showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(wd, conf$setup$output.dir),
             showWarnings = FALSE,
             recursive = TRUE)

  ## Set parameters from JSON NA to R NULL
  if(is.na(conf$harvest$wc.akey)) conf$harvest$wc.akey <- NULL
  if(is.na(conf$harvest$wc.skey)) conf$harvest$wc.skey <- NULL
  if(is.na(conf$harvest$tag.list)) conf$harvest$tag.list <- NULL
  if(is.na(conf$harvest$dropIDs)) conf$harvest$dropIDs <- NULL
  if(is.na(conf$model$proj)) conf$model$proj <- NULL
  if(is.na(conf$model$barrier)) conf$model$barrier <- NULL

  if(is.null(conf$harvest$dropIDs)) {
    dropIDs <- c("")
  } else {
    dropIDs <- conf$harvest$dropIDs
  }

  what <- "p"
  if(conf$model$reroute) what <- "r"

  if (all(!is.null(conf$harvest$owner.id),
          !is.null(conf$harvest$wc.akey),
          !is.null(conf$harvest$wc.skey))){
    if(conf$harvest$download)
      message("\nDownloading tag data from Wildlife Computers Data Portal...")
    ## Conditionally download from WC Portal API
    wc.meta <- download_data(
      dest = file.path(wd, conf$setup$data.dir),
      source = "wc",
      unzip = TRUE,
      wc.akey = conf$harvest$wc.akey,
      wc.skey = conf$harvest$wc.skey,
      subset.ids = conf$harvest$tag.list,
      download = conf$harvest$download,
      owner.id = conf$harvest$owner.id
    )
  }

  message("Reading tag data files...")
  ## read SMRU tag file data from .mdb/source files
  wc <- pull_data(path2data = conf$setup$data.dir,
                  source = "wc",
                  subset.ids = conf$harvest$tag.list)

  ## get metadata
  meta <- get_metadata(source = meta.source,
                       tag_data = wc,
                       tag_mfr = "wc",
                       dropIDs = dropIDs,
                       file = file.path(wd, conf$setup$meta.file),
                       subset.ids = conf$harvest$tag.list,
                       wc.meta = wc.meta,
                       meta.args = conf$meta
  )

  ## prepare location data
  locs_sf <- wc_prep_loc(
    wc,
    meta,
    dropIDs = dropIDs,
    crs = conf$model$proj,
    program = conf$setup$program,
    QCmode = conf$model$QCmode
  )


  ## FIT QC SSM in 2 passes
  ## First pass SSM-filter
  message("Fitting QC SSM - first pass...")
  fit1 <- multi_filter(locs_sf,
                       vmax = conf$model$vmax,
                       model = conf$model$model,
                       ts = conf$model$time.step) |>
    suppressWarnings()


  ## Second pass SSM-filter - separately by species
  if(conf$model$reroute) message("Fitting QC SSM - second pass & rerouting locations off land...")
  else message("Fitting QC SSM - second pass...")
  fit2 <- redo_multi_filter(
    fit1,
    locs_sf,
    vmax = conf$model$vmax,
    model = conf$model$model,
    ts = conf$model$time.step,
    ## most common SSM failure point when fitting to Argos locs w error ellipse info
    map = list(psi = factor(NA)),
    reroute = conf$model$reroute,
    dist = conf$model$dist,
    barrier = conf$model$barrier,
    buffer = conf$model$buffer,
    centroids = conf$model$centroids
  ) |>
    suppressWarnings()


  message("Locations estimated & rerouted...")

  ## Mark SSM track segments for removal in data gaps > min.gap hours long
  ##  predicted & rerouted locations are 'marked' with a `keep` column
  ## (TRUE = keep, FALSE = ignore). Executed only if conf$model$cut = TRUE
  if (conf$model$cut) {
    fit2 <- ssm_mark_gaps(fit2[[i]], min.gap = conf$model$min.gap)
  }


  ## Append SSM-estimated locations to tag datafiles
  wc_ssm <- wc_append_ssm(
    wc = wc,
    fit = fit2,
    what = what,
    meta = meta,
    cut = conf$model$cut,
    dropIDs = dropIDs
  )

  message("QC'd locations appended to tag data files...")


  ## Diagnostic plots
  diagnostics(
    fit2,
    fit1,
    what = what,
    cut = conf$model$cut,
    data = wc$Locations,
    ssm = wc_ssm,
    meta = meta,
    lines = TRUE,
    obs = FALSE,
    mpath = file.path(conf$setup$maps.dir),
    dpath = file.path(conf$setup$diag.dir),
    QCmode = conf$model$QCmode,
    tag_mfr = "wc",
    cid = NULL
  )

  message("SSM fit diagnostics generated...")


  ## write SSM-annotated datafiles to .csv
  wc_write_csv(
    wc_ssm = wc_ssm,
    fit = fit2,
    what = what,
    meta,
    program = conf$setup$program,
    path = file.path(wd, conf$setup$output.dir),
    dropIDs = dropIDs,
    suffix = paste0("_", conf$model$QCmode),
    pred.int = conf$model$pred.int
  )

  message("QC workflow completed")

  if (conf$setup$return.R) {
    return(list(dropIDs=dropIDs,
                wc=wc,
                meta=meta,
                locations_sf=locs_sf,
                fit1=fit1,
                fit2=fit2,
                wc_ssm=wc_ssm))
  }
}
