##' @title ATN SMRU SRDL QC workflow
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
##' @param dropIDs the SMRU ref ID's that are to be ignored during the QC process
##' @param proj the proj4string to be used for the location data & for the SSM-estimated
##' locations. The default (NULL) will result in one of 3 projections being used,
##' depending on whether the centroid of the observed latitudes lie in N or S polar
##' regions, temperate or equatorial regions, or if tracks straddle (or lie close to)
##' -180,180 longitude.
##' @param model the aniMotum SSM model to be used for the location QC - typically
##' either `rw` or `crw`.
##' @param vmax for SSM fitting; max travel rate (m/s) to identify implausible locations
##' @param time.step the prediction interval (in hours) to be used by the SSM
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
##' @param output logical; should fn return a list of QC-generated objects. This
##' results in a single large object return that can be useful for troubleshooting
##' QC errors or undesirable results.
##' @importFrom stringr str_split str_length
##' @importFrom jsonlite read_json
##'
##' @md
##' @export

atn_smru_qc <- function(wd,
                        config,
                        ...) {

  if(!file.exists(wd)) stop("Working directory `wd` does not exist")
  else setwd(wd)

  conf <- read_json(config, simplifyVector = TRUE)

  if(is.na(conf$setup$meta.file)) {
    conf$setup$meta.file <- NULL
    meta.source <- "smru"
  } else {
    meta.source <- "imos"
  }

  if(!file.exists(file.path(wd, conf$setup$outdir))) {
    stop("Working QC output directory `outdir` does not exist")
  }

  if(is.na(conf$harvest$dropIDs)) conf$harvest$dropIDs <- NULL
  if(is.na(conf$harvest$p2mdbtools)) conf$harvest$p2mdbtools <- NULL
  if(is.na(conf$proj)) conf$proj <- NULL

  if(is.null(conf$harvest$dropIDs)) {
    dropIDs <- c("")
  } else {
    dropIDs <- conf$harvest$dropIDs
  }


  what <- "p"
  if(conf$model$reroute) what <- "r"

  ## read SMRU tag file data from .mdb/source files
  smru <- pull_data(file.path(wd, conf$setup$datadir),
                    source = "local",
                    cid = conf$harvest$cid,
                    tag_mfr = "smru")

  ## get metadata
  meta <- suppressMessages(get_metadata(source = "atn",
                       tag_data = smru,
                       cid = conf$harvest$cid,
                       dropIDs = dropIDs,
                       file = file.path(wd, conf$setup$meta.file)
  ) )

  ## prepare location data
  diag_sf <- smru_prep_loc(smru,
                       meta,
                       dropIDs = dropIDs,
                       crs = conf$proj,
                       QCmode = conf$model$QCmode)


  fit1 <- fit2 <- vector("list", length = length(diag_sf))

  ## First pass SSM-filter - separately by species
  fit1 <- lapply(1:length(fit1), function(i) {
    multi_filter(
      diag_sf[[i]],
      vmax = conf$model$vmax,
      model = conf$model$model,
      ts = conf$model$time.step
    )
  })

  ## Second pass SSM-filter - separately by species
  fit2 <- lapply(1:length(fit1), function(i){
    redo_multi_filter(
      fit1[[i]],
      diag_sf[[i]],
      vmax = conf$model$vmax,
      model = conf$model$model,
      ts = conf$model$time.step,
      map = list(psi = factor(NA)),
      reroute = conf$model$reroute,
      dist = conf$model$dist,
      buffer = conf$model$buffer,
      centroids = conf$model$centroids
    )
  })
  names(fit1) <- names(fit2) <- names(diag_sf)

  ## Mark SSM track segments for removal in data gaps > min.gap hours long
  if (conf$model$cut) {
    fit2 <- lapply(1:length(fit2), function(i) {
      ssm_mark_gaps(fit2[[i]], min.gap = conf$model$min.gap)
    })

    names(fit2) <- names(diag_sf)
  }

  ## append SSM locations to SMRU tag data files
  smru_ssm <- lapply(1:length(fit2), function(i){
    smru_append_ssm(
      smru = smru,
      fit = fit2[[i]],
      what = what,
      meta = meta,
      cut = conf$model$cut,
      dropIDs = dropIDs
    )
  })


  ## generate SSM fit diagnostics & SSM-predicted track map
  obs <- smru_clean_diag(smru, dropIDs = dropIDs)

  dir.create(conf$setup$maps.dir, showWarnings = FALSE)
  dir.create(conf$setup$diag.dir, showWarnings = FALSE)
  lapply(1:length(smru_ssm), function(i) {
    diagnostics(fit = fit2[[i]],
                fit1 = fit1[[i]],
                what = what,
                cut = conf$model$cut,
                data = obs,
                ssm = smru_ssm[[i]],
                meta = meta,
                mpath = file.path(conf$setup$maps.dir),
                dpath = file.path(conf$setup$diag.dir),
                QCmode = conf$model$QCmode,
                cid = conf$harvest$cid
    )
  })

  ## write SSM-appended files to CSV
  lapply(1:length(smru_ssm), function(i) {
    smru_write_csv(smru_ssm = smru_ssm[[i]],
                   fit = fit2[[i]],
                   what = what,
                   meta = meta,
                   program = "atn",
                   path = file.path(wd, conf$setup$outdir),
                   dropIDs = dropIDs,
                   suffix = paste0("_", conf$model$QCmode)
    )
  })

  if (conf$output) {
    return(list(mdbs=mdbs,
                dropIDs=dropIDs,
                smru=smru,
                meta=meta,
                diag_sf=diag_sf,
                fit1=fit1,
                fit2=fit2,
                smru_ssm=smru_ssm,
                obs=obs))
  }
}

