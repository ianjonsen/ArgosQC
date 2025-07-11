##' @title multi-filter
##'
##' @description apply SSM filter to diag data across multiple processors
##'
##' @param x \code{sf}-projected diag file of locations to be filtered
##' @param vmax for prefilter
##' @param model \code{aniMotum} model ("rw" orr "crw)
##' @param ts \code{foieGrsa} time.step
##' @param verbose turn on/off furrr::future_map progress indicator
##'
##' @importFrom dplyr %>% filter
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom aniMotum fit_ssm ssm_control
##'
##' @export
##'

multi_filter <- function(x,
                         vmax = 4,
                         model = "rw",
                         ts = 6,
                         verbose = FALSE) {

  plan("multisession")
  fit <-
    x$d_sf |> future_map(~ try(fit_ssm(
      x = .x,
      model = model,
      time.step = ts,
      vmax = vmax,
      control = ssm_control(verbose = ifelse(verbose, 1, 0))
    ), silent = TRUE),
    .progress = verbose,
    .options = furrr_options(seed = TRUE)
    ) %>%
    do.call(rbind, .)

  return(fit)
}
