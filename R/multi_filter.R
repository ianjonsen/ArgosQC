##' @title multi-filter
##'
##' @description apply SSM filter to diag data across multiple processors
##'
##' @param diag_sf \code{sf}-projected diag to be used
##' @param vmax for prefilter
##' @param ang for prefilter
##' @param min.dt for prefilter
##' @param model \code{foieGras} model ("rw" orr "crw)
##' @param ts \code{foieGrsa} time.step
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

multi_filter <- function(diag_sf, vmax = 4, ang = c(15, 25), min.dt = 60, model = "crw", ts = 2) {

  ## split data by WESE vs SESE campaigns
  wese.m <- meta %>% filter(common_name == "Weddell seal")
  sese.m <- meta %>% filter(common_name == "southern elephant seal")
  diag_wese <- diag_sf %>%
    filter(cid %in% wese.m$sattag_program)
  diag_sese <- diag_sf %>%
    filter(cid %in% sese.m$sattag_program)

  ## fit crw model to SESE
  plan("multisession")
  fit_sese <-
    diag_sese$d_sf %>% future_map(~ try(fit_ssm(
      d = .x,
      model = model,
      time.step = ts,
      verbose = 0,
      vmax = vmax,
      ang = ang,
      min.dt = min.dt
    ), silent = TRUE), .progress = TRUE) %>%
    do.call(rbind, .)

  ## rw model to WESE
  plan("multisession")
  fit_wese <-
    diag_wese$d_sf %>% future_map(~ try(fit_ssm(
      d = .x,
      model = "rw",
      time.step = 6,
      verbose = 0,
      vmax = 5,
      ang = c(15,25),
      min.dt = 300
    )), .progress = TRUE) %>%
    do.call(rbind, .)

  fit1 <- bind_rows(fit_sese, fit_wese)

  return(fit1)
}
