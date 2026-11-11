##' @title multi-filter
##'
##' @description apply SSM filter to diag data across multiple processors
##'
##' @param x \code{sf}-projected diag file of locations to be filtered
##' @param vmax for prefilter
##' @param model \code{aniMotum} model ("rw" orr "crw)
##' @param ts \code{foieGrsa} time.step
##' @param verbose turn on/off furrr::future_map progress indicator
##' @param fix.psi optional logical vector, one element per track (row of
##' \code{x}): \code{TRUE} fixes the \code{aniMotum} error ellipse scaling
##' parameter \code{psi} at 1 for that track (see \code{qc_fix_psi()}).
##' \code{NULL} (default) estimates \code{psi} for all tracks.
##'
##' @importFrom dplyr %>% filter
##' @importFrom future plan
##' @importFrom furrr future_map future_map2 furrr_options
##' @importFrom aniMotum fit_ssm ssm_control
##'
##' @export
##'

multi_filter <- function(x,
                         vmax = 4,
                         model = "rw",
                         ts = 6,
                         verbose = FALSE,
                         fix.psi = NULL) {

  plan("multisession")
  if (is.null(fix.psi)) fix.psi <- rep(FALSE, nrow(x))
  if (length(fix.psi) != nrow(x)) stop("fix.psi must have one element per track")

  fit <-
    future_map2(x$d_sf, fix.psi, ~ try(fit_ssm(
      x = .x,
      model = model,
      time.step = ts,
      vmax = vmax,
      map = if (isTRUE(.y)) list(psi = factor(NA)) else NULL,
      control = ssm_control(verbose = ifelse(verbose, 1, 0))
    ), silent = TRUE),
    .progress = verbose,
    .options = furrr_options(seed = TRUE)
    ) %>%
    do.call(rbind, .)

  return(fit)
}
