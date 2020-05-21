##' @title multi-filter
##'
##' @description apply SSM filter to diag data across multiple processors
##'
##' @param x \code{sf}-projected diag file of locations to be filtered
##' @param vmax for prefilter
##' @param ang for prefilter
##' @param distlim for prefilter
##' @param min.dt for prefilter
##' @param model \code{foieGras} model ("rw" orr "crw)
##' @param ts \code{foieGrsa} time.step
##' @param map params to fix
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

multi_filter <- function(x, vmax = 4, ang = c(15, 25), distlim = c(2500,5000), min.dt = 60, model = "crw", ts = 2, map = NULL) {

  plan("multisession")
  fit <-
    x$d_sf %>% future_map(~ try(fit_ssm(
      d = .x,
      model = model,
      time.step = ts,
      verbose = 0,
      vmax = vmax,
      ang = ang,
      distlim = distlim,
      min.dt = min.dt,
      map = map
    ), silent = TRUE), .progress = TRUE) %>%
    do.call(rbind, .)

  return(fit)
}
