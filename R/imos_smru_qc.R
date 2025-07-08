##' @title IMOS SMRU SRDL QC workflow
##'
##' @description Wrapper function that executes the complete workflow from data
##' download to SSM-appended tag data files output as CSV files.
##'
##' @param `wd` the path to the working directory that contains: 1) the data directory
##' where tag data files are stored (if source = `local`); 2) the metadata directory
##' where all metadata files are stored; and 3) the destination directory for QC output.
##' @param config a JSON configuration file containing the following parameters:
##' * `datadir` the name of the data directory (to be added to the `wd` path).
##' * `meta.file` the metadata filename. Must reside within the `wd`.
##' * `maps.dir` the directory path to write diagnostic maps of QC'd tracks.
##' * `diag.dir` the directory path to write diagnostic time-series plots of QC'd lon & lat.
##' * `outdir` the name of the QC output directory where CSV files will be
##' written (to be added to the `wd` path).
##' * `cid` SMRU campaign (AODN sattag_program) ID.
##' * `sp` the species being QC'd - currently, either "sese" or "ortu".
##' * `smru.usr` SMRU data server username as a string.
##' * `smru.pwd` SMRU data server password as a string.
##' * `dwnld.timeout` extends the download timeout period a specified number of seconds.
##' * `dropIDs` the SMRU ref ID's that are to be ignored during the QC process.
##' * `proj` the proj4string to be used for the location data & for the SSM-estimated
##' locations. The default (NULL) will result in one of 3 projections being used,
##' depending on whether the centroid of the observed latitudes lie in N or S polar
##' regions, temperate or equatorial regions, or if tracks straddle (or lie close to)
##' -180,180 longitude.
##' * `model` the aniMotum SSM model to be used for the location QC - typically
##' either `rw` or `crw`.
##' * `vmax` for SSM fitting; max travel rate (m/s) to identify implausible locations
##' * `time.step` the prediction interval (in hours) to be used by the SSM
##' * `reroute` whether QC'd tracks should be re-routed off of land
##' (default is FALSE). Note, in some circumstances this can substantially increase
##' processing time. Default land polygon data are sourced from the
##' `ropensci/rnaturalearthhires` R package.
##' * `dist` the distance in km from outside the convex hull of observed
##' locations from which to select land polygon data for re-routing. Ignored if
##' `reroute = FALSE`.
##' * `buffer` the distance in km to buffer rerouted locations from the
##' coastline. Ignored if `reroute = FALSE`.
##' * `centroids` whether centroids are to be included in the visibility graph
##' mesh used by the rerouting algorithm. See `?pathroutr::prt_visgraph` for
##' details. Ignored if `reroute = FALSE`.
##' * `cut` logical; should predicted locations be dropped if they lie within
##' in a large data gap (default is FALSE).
##' * `min.gap` the minimum data gap duration (h) to be used for cutting
##' predicted locations (default is 72 h)
##' * `QCmode` one of either `nrt` for Near Real-Time QC or `dm` for Delayed
##' Mode QC.
##' * `output` logical; should fn return a list of QC-generated objects. This
##' results in a single large object return that can be useful for troubleshooting
##' QC errors or undesirable results.
##' * `dwnld` logical; should the tag data be downloaded from the SMRU server
##' (default TRUE). Useful when testing so unnecessary load is not put on the
##' SMRU server.
##'
##'
##' @importFrom stringr str_split str_length
##' @importFrom jsonlite fromJSON
##'
##' @md
##' @export

imos_smru_qc <- function(wd, config) {


  if(!file.exists(wd)) stop("Working directory `wd` does not exist")
  else setwd(wd)

  conf <- fromJSON(txt = config)
  if(conf$meta.file == "NULL") {
    conf$meta.file <- NULL
    meta.source <- "smru"
  } else {
    meta.source <- "imos"
  }
  if(conf$dropIDs == "NULL") conf$dropIDs <- NULL
  if(conf$p2mdbtools == "NULL") conf$p2mdbtools <- NULL

  if(!file.exists(file.path(wd, conf$outdir))) stop("Working QC output directory `outdir` does not exist")
  if(is.null(conf$dropIDs)) {
    dropIDs <- c("")
  } else {
    dropIDs <- conf$dropIDs
  }

  what <- "p"
  if(conf$reroute) what <- "r"

  if(conf$dwnld) {
    system(paste0("rm ", file.path(conf$datadir, "*.mdb")))
    ## download tag data from SMRU server
    download_data(
      dest = conf$datadir,
      source = "smru",
      cids = conf$cid,
      user = conf$smru.usr,
      pwd = conf$smru.pwd,
      timeout = conf$dwnld.timeout
    )
  }

  ## find which campaigns successfully downloaded from SMRU server
  mdbs <- list.files(conf$datadir)
  if(length(mdbs) > 0) {
    mdbs <- mdbs |> str_split("\\.", simplify = TRUE)
    mdbs <- mdbs[, 1]
  }

  ## read SMRU tag file data from .mdb/source files
  ## pull tables (diag, haulout, ctd, dive, & summary) from .mdb files
  smru <- pull_data(
    path2data = conf$datadir,
    source = "smru",
    cids = mdbs,
    p2mdbtools = conf$p2mdbtools
  )

  if (!"dive" %in% names(smru))
    smru$dive <- NULL

  ## download SMRU metadata & generate QC metadata for IMOS-AODN
  meta <- get_metadata(
    source = meta.source,
    tag_data = smru,
    cids = mdbs,
    dropIDs = dropIDs,
    file = conf$meta.file,
    meta.args = conf
  ) |>
    suppressMessages()


  ## prepare location data
  diag_sf <- smru_prep_loc(smru,
                           meta,
                           dropIDs = dropIDs,
                           crs = conf$proj,
                           QCmode = conf$QCmode)

  fit1 <- fit2 <- vector("list", length = length(diag_sf))

  ## First pass SSM-filter - separately by species
  fit1 <- lapply(1:length(fit1), function(i) {
    multi_filter(
      diag_sf[[i]],
      vmax = as.numeric(conf$vmax),
      model = conf$model,
      ts = as.numeric(conf$time.step)
    ) |> suppressWarnings()
  })

  ## Second pass SSM-filter - separately by species
  fit2 <- lapply(1:length(fit1), function(i){
    redo_multi_filter(
      fit1[[i]],
      diag_sf[[i]],
      vmax = as.numeric(conf$vmax),
      model = conf$model,
      ts = as.numeric(conf$time.step),
      map = list(psi = factor(NA)),
      reroute = as.logical(conf$reroute),
      dist = as.numeric(conf$dist),
      buffer = as.numeric(conf$buffer),
      centroids = as.logical(conf$centroids)
    ) |> suppressWarnings()
  })
  names(fit1) <- names(fit2) <- names(diag_sf)

  ## Mark SSM track segments for removal in data gaps > min.gap hours long
  if (as.logical(conf$cut)) {
    fit2 <- lapply(1:length(fit2), function(i) {
      ssm_mark_gaps(fit2[[i]], min.gap = as.numeric(conf$min.gap))
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
      cut = as.logical(conf$cut),
      dropIDs = dropIDs
    )
  })


  ## generate SSM fit diagnostics & SSM-predicted track map
  obs <- smru_clean_diag(smru, dropIDs = dropIDs)

  dir.create(conf$maps.dir, showWarnings = FALSE)
  dir.create(conf$diag.dir, showWarnings = FALSE)
  lapply(1:length(smru_ssm), function(i) {
    diagnostics(fit = fit2[[i]],
                fit1 = fit1[[i]],
                what = what,
                cut = as.logical(conf$cut),
                data = obs,
                ssm = smru_ssm[[i]],
                meta = meta,
                mpath = file.path(conf$maps.dir),
                dpath = file.path(conf$diag.dir),
                QCmode = conf$QCmode,
                cid = conf$cid
    )
  })

  ## write SSM-appended files to CSV
  lapply(1:length(smru_ssm), function(i) {
    smru_write_csv(smru_ssm = smru_ssm[[i]],
                   fit = fit2[[i]],
                   what = what,
                   meta = meta,
                   program = "imos",
                   path = file.path(wd, conf$outdir),
                   dropIDs = dropIDs,
                   suffix = paste0("_", conf$QCmode)
    )
  })

  if (as.logical(conf$output)) {
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

