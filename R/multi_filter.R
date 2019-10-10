##' @title multi-filter
##'
##' @description apply SSM filter to diag data across multiple processors
##'
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

multi_filter <- function(diag_sf, meta) {

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
      model = "crw",
      time.step = 2,
      verbose = 0,
      vmax = 10,
      ang = c(15,25),
      min.dt = 60
    )), .progress = TRUE) %>%
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
