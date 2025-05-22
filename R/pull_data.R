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
##' @param source one of "smru", "wc", or "local"
##' @param cids SMRU campaign ids. If not specified then the cids are built from
##' the directory or filenames present in the `path2data` directory.
##' @param tag_mfr either "smru or "wc", ignored if `source` != "local"
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
                      tag_mfr = "smru",
                      ...) {

  source <- match.arg(source, choices = c("smru", "wc", "local"))

  if (source == "smru") {
    if (is.null(cids)) {
      fs <- list.files(path2data)
      fs <- fs[grep("\\.mdb", fs)]
      cids <- str_split(fs, "\\.", simplify = TRUE)[, 1]
    }

    out <- smru_pull_tables(cids,
                    path2mdb = path2data,
                    ...)

  } else if (source == "wc") {

    out <- wc_pull_data(path2data)

  } else if(source == "local") {

    out <- pull_local_data(path2data,
                           tag_mfr)
  }

  return(out)
}
