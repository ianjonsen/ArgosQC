##' @title Pull SMRU or WC data from downloaded data files
##'
##' @description extracts specified data files from SMRU .mdb files or WC zipfiles.
##' For WC data files, extracts data from `-Locations.csv`, `-FastGPS.csv`,
##' `Histos.csv`, `MinMaxDepth.csv`, `HaulOut.csv`, and `SST.csv` files.
##' Extracted data are aggregated across individual tags and returned in a
##' single named list with the following data.frames:
##' * argos
##' * fastgps
##' * histos
##' * depth
##' * haulout
##' * sst
##' WC tag data files downloaded via `download_data` will be stored in separate,
##' tag-specific subdirectories. `path2data` should point to the outer directory.
##' The function always chooses the latest WC-processed `-Locations.csv` &
##' `FastGPS.csv` files.
##'
##' @param path2data path to data file(s)
##' @param source either "smru" or "wc"
##' @param cids SMRU campaign ids. If not specified then the cids are built from
##' the `.mdb` filenames present in the `path2data` directory.
##' @param datafiles specify which WC data files to extract, default is to
##' extract all files: Locations, FastGPS, Histos, MinMaxDepth, Haulout, and
##' SST (when present).
##' @param ... additional arguments passed to `pull_smru_tables`
##'
##'
##'
##' @examples
##'
##' @md
##' @export

pull_data <- function(path2data,
                      source = "smru",
                      cids = NULL,
                      datafiles = NULL,
                      ...) {

  source <- match.arg(source, choices = c("smru", "wc"))

  if(source == "smru") {
    ## if cids is NULL then build cids from .mdb filenames in path2data
    if(is.null(cids)) {
      fs <- list.files(path2data)
      fs <- fs[grep("\\.mdb", fs)]
      cids <- str_split(fs, "\\.", simplify = TRUE)[,1]
    }

    out <- pull_smru_tables(cids,
                    path2mdb = path2data,
                    ...)

  } else if (source == "wc") {

    out <- pull_wc_data(path2data,
                 datafiles)
  }

  return(out)
}
