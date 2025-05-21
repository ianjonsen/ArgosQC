##' @title write annotated WC tag datafiles & annotated metadata
##'
##' @description subsample SSM-predicted locations to 6-h intervals, write annotated files to .csv
##'
##' @param wc_ssm SSM-appended WC tag datafiles - output of \code{append_wc_ssm}
##' @param fit final SSM fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual WC DeploymentID's to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##' @param pred.int prediction interval to use for sub-sampling predicted locations
##' (default = 6 h)
##'
##' @examples
##'
##' @importFrom dplyr filter rename mutate select group_by group_split
##' @importFrom stringr str_replace
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @export

wc_write_csv <- function(wc_ssm,
                        fit,
                        what,
                        meta,
                        program = "atn",
                        path = NULL,
                        dropIDs = NULL,
                        suffix = "_nrt",
                        pred.int = 6) {


  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## SSM predictions
  ssm_out <- wc_ssm_outputs(fit = fit,
                            what = what,
                            dropIDs = dropIDs,
                            suffix = suffix,
                            pred.int = pred.int)

browser()
}
