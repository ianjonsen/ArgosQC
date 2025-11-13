##' @title SMRU SRDL QC workflow
##'
##' @description Wrapper function that executes the complete SMRU QC workflow from data
##' download to SSM-appended tag data files output as CSV files. All settings are
##' specified in a JSON config file, including program - currently, IMOS, ATN or OTN.
##' The program field determines the specific ArgosQC workflow functions called
##' within the wrapper fn.
##'
##' @param wd the path to the working directory that contains: 1) the data directory
##' where tag data files are stored (if `harvest$download` = FALSE) or downloaded to
##' (if `harvest$download` = TRUE); 2) the metadata directory where all metadata
##' files are stored; and 3) the destination directory for QC outputs.
##' @param config a hierarchical JSON configuration file containing the following
##' blocks, each with a set of block-specific parameters:
##' * `setup` config block specifies paths to required data, metadata & output
##' directories:
##'   * `program` the national (or other) program of which the data is a part.
##'   Current options are: `imos`, `atn`, or `otn`.
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
##'   the SMRU data server or read from the local `data.dir`.
##'   * `cid` SMRU campaign ID.
##'   * `smru.usr` SMRU data server username as a string.
##'   * `smru.pwd` SMRU data server password as a string.
##'   * `timeout` extends the download timeout period a specified number of
##'   seconds for slower internet connections.
##'   * `dropIDs` the SMRU ref ID's that are to be ignored during the QC process.
##'   SMRU ref ID's must be supplied as a .CSV file `dropIDs.csv` with a single
##'   variable named `ref`. Can be NULL.
##'   * `p2mdbtools` (optional) provides the path to the mdbtools library if it
##'   is installed in a non-standard location (e.g., on Macs when installed via
##'   Homebrew).
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
##'
##' * `meta` config block specifies species and deployment location information.
##' This config block is only necessary when no metadata file is provided in the
##' `setup` config block.
##'   * `common_name` the species common name (e.g., "southern elephant seal")
##'   * `species` the species scientific name (e.g., "Mirounga leonina")
##'   * `release_site` the location where tags were deployed (e.g., "Iles Kerguelen")
##'   * `state_country` the country/territory name (e.g., "French Overseas Territory")
##'
##' @importFrom stringr str_split str_length
##' @importFrom jsonlite read_json
##'
##' @md
##' @export

smru_qc <- function(wd,
                    config) {

  if(!file.exists(wd)) stop("Working directory `wd` does not exist")
  else setwd(wd)

  conf <- read_json(config, simplifyVector = TRUE)

  ## define metadata source
  if(is.na(conf$setup$meta.file)) {
    conf$setup$meta.file <- NULL
    meta.source <- "smru"
  } else {
    meta.source <- conf$setup$program
  }

  ## Create output dirs if they do not exits
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
  if(is.na(conf$harvest$dropIDs)) conf$harvest$dropIDs <- NULL

  if(any(!"p2mdbtools" %in% names(conf$harvest),
     is.na(conf$harvest$p2mdbtools))) {
    conf$harvest$p2mdbtools <- NULL
  }

  if(is.na(conf$model$proj)) conf$model$proj <- NULL
  if(is.na(conf$model$barrier)) conf$model$barrier <- NULL

  if(is.null(conf$harvest$dropIDs)) {
    dropIDs <- c("")
  } else {
    dropIDs <- suppressMessages(readr::read_csv(conf$harvest$dropIDs)$ref)
  }

  ## Define the QC locations - p = predicted; r = rerouted
  what <- "p"
  if(conf$model$reroute) what <- "r"


  ## Conditionally download data from SMRU server
  if(conf$harvest$download) {
    message("\nDownloading tag data from SMRU server...")
    #   system(paste0("rm ", file.path(conf$setup$datadir, "*.mdb")))
    ## download tag data from SMRU server
    download_data(
      dest = file.path(wd, conf$setup$data.dir),
      source = "smru",
      cid = conf$harvest$cid,
      user = conf$harvest$smru.usr,
      pwd = conf$harvest$smru.pwd,
      timeout = conf$harvest$timeout
    )

  }

  message("Reading tag data from .mdb file...")
  ## Read SMRU tag file data from .mdb/source files
  ## Pull tables (diag, gps*, haulout*, ctd, dive*, cruise* & summary) from .mdb files
  ##    * if present
  smru <- pull_data(
    path2data = conf$setup$data.dir,
    source = "smru",
    cid = conf$harvest$cid,
    p2mdbtools = conf$harvest$p2mdbtools
  )

  if(!is.null(conf$setup$meta.file)) message("Pulling metadata from file...")
  else if(is.null(conf$setup$meta.file)) message("Building metadata from SMRU Portal...")
  ## Download or load metadata
  meta <- get_metadata(
      source = meta.source,
      tag_data = smru,
      cid = conf$harvest$cid,
      user = conf$harvest$smru.usr,
      pwd = conf$harvest$smru.pwd,
      dropIDs = dropIDs,
      file = conf$setup$meta.file,
      meta.args = conf$meta
    ) |>
      suppressMessages()

  message("Preparing location data for QC...")
  ## Prepare location data
  locs_sf <- smru_prep_loc(
    smru,
    meta,
    dropIDs = dropIDs,
    crs = conf$model$proj,
    QCmode = conf$model$QCmode
  )


  message("Fitting QC SSM - first pass...")

  ## FIT QC SSM in 2 passes
  ## First pass SSM-filter
  fit1 <- multi_filter(
    locs_sf,
    vmax = conf$model$vmax,
    model = conf$model$model,
    ts = conf$model$time.step
  ) |> suppressWarnings()

  if(conf$model$reroute) message("Fitting QC SSM - second pass & rerouting locations off land...")
  else message("Fitting QC SSM - second pass...")

  ## Second pass SSM-filter
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
  ) |> suppressWarnings()

  if(conf$model$reroute) message("QC'd locations estimated & rerouted...")
  else message("QC'd locations estimated...")


## Mark SSM track segments for removal in data gaps > min.gap hours long
##  predicted & rerouted locations are 'marked' with a `keep` column
## (TRUE = keep, FALSE = ignore). Executed only if conf$model$cut = TRUE
if (conf$model$cut) {
  fit2 <- ssm_mark_gaps(fit2, min.gap = conf$model$min.gap)
}

message("Appending QC'd locations to tag data files...")
## Append SSM locations to SMRU tag data files at observation times
smru_ssm <- smru_append_ssm(
  smru = smru,
  fit = fit2,
  what = what,
  meta = meta,
  cut = conf$model$cut,
  dropIDs = dropIDs
)

  message("QC'd locations appended to tag data files...")

## Generate SSM fit diagnostics & SSM-predicted track map
obs <- smru_clean_diag(smru, dropIDs = dropIDs)

diagnostics(
  fit = fit2,
  fit1 = fit1,
  what = what,
  cut = conf$model$cut,
  data = obs,
  ssm = smru_ssm,
  meta = meta,
  mpath = file.path(conf$setup$maps.dir),
  dpath = file.path(conf$setup$diag.dir),
  QCmode = conf$model$QCmode,
  tag_mfr = "smru",
  cid = conf$harvest$cid
)

message("SSM fit diagnostics generated...")

## write SSM-appended files to CSV
smru_write_csv(
  smru_ssm = smru_ssm,
  fit = fit2,
  what = what,
  meta = meta,
  program = conf$setup$program,
  path = file.path(wd, conf$setup$output.dir),
  dropIDs = dropIDs,
  suffix = paste0("_", conf$model$QCmode)
)

message("QC workflow completed")

if (as.logical(conf$setup$return.R)) {
  return(list(cid=cid,
              dropIDs=dropIDs,
              smru=smru,
              meta=meta,
              locs_sf=locs_sf,
              fit1=fit1,
              fit2=fit2,
              smru_ssm=smru_ssm))
}

}
