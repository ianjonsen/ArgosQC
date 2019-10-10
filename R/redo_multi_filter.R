##' @title redo failed multi-filter cases
##'
##' @description re-apply SSM filter to diag data for id's that failed to converge. parallelized
##'
##' @param fit foieGras fit object from first round of filtering
##' @param diag_sf \code{sf}-projected diag to be used
##' @param meta metadata to split diag by species
##'
##' @examples
##'
##' @importFrom dplyr filter "%>%" bind_rows
##' @importFrom future plan
##' @importFrom furrr future_map
##' @importFrom foieGras fit_ssm
##'
##' @export
##'

redo_multi_filter <- function(fit) {


}
