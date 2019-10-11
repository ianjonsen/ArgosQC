##' @title redo failed multi-filter cases
##'
##' @description re-apply SSM filter to diag data for id's that failed to converge. parallelized
##'
##' @param fit foieGras fit object from first round of filtering
##' @param diag_sf \code{sf}-projected diag to be used
##' @param model model argument ("rw" or "crw) for \code{foieGras::fit_ssm}
##' @param ts time.step argument for \code{foieGras::fit_ssm}
##' @param min.dt min.dt argument for \code{foieGras::fit_ssm}
##'
##'
##' @examples
##'
##' @importFrom dplyr filter "%>%" bind_rows slice ungroup left_join select mutate
##' @importFrom tidyr nest
##' @importFrom future plan
##' @importFrom furrr future_map
##' @importFrom foieGras fit_ssm
##'
##' @export
##'

redo_multi_filter <- function(fit, diag_sf, model = "crw", ts = 2, min.dt = 180) {

  oc <- which(sapply(fit$ssm, inherits, "try-error"))
  sprintf("%d optimiser crashes", length(oc))
  nc <- which(!fit$converged)
  sprintf("%d failed to converge", length(nc))

  if(length(nc) == 0 & length(oc) == 0) {
    stop("\n no fit failures")
  } else {
    d <- sort(unique(c(oc,nc)))
    fit.f <- fit %>% ungroup() %>% slice(d)
    fit.s <- fit %>% ungroup() %>% slice(-d)

    ## get data for fit failures
    fail_dat <- diag_sf %>%
      filter(ref %in% fit.f$id)

    ## Refit Stage 1 - refit with a bigger min.dt
    fit_fail <- fail_dat$d_sf %>%
      future_map( ~ try(fit_ssm(
        d = .x,
        model = model,
        time.step = ts,
        min.dt = min.dt,
        vmax = 4,
        ang = c(15, 25),
        verbose = 0
      ), silent = TRUE), .progress = TRUE) %>%
      do.call(rbind, .)

    ## add successes onto original successful fits
    fit.s <- bind_rows(fit.s, fit_fail[fit_fail$converged, ])

    ## Refit Stage 2 - attempt to fix by truncating with end of ctd data & and use rw model
    if(nrow(fit.s) < nrow(fit)) {
      fit.f <- which(!fit_fail$converged)
      if(length(fit.f) > 0){
        fail_dat <- fail_dat %>% slice(fit.f)
        fail_dat <- lapply(fail_dat$d_sf, function(x) {
          left_join(x, ctd_end, by = c("id" = "ref")) %>%
            filter(date <= ctd_end) %>%
            select(-ctd_end)
        }) %>% do.call(rbind, .) %>%
          mutate(ref = id) %>%
          nest(-ref, .key = "d_sf") %>%
          mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))

        fit_fail <- fail_dat$d_sf %>%
          future_map(~ try(fit_ssm(
            d = .x,
            model = model,
            time.step = ts,
            min.dt = min.dt,
            vmax = 4,
            ang = c(15, 25),
            verbose = 0
          ), silent = TRUE), .progress = TRUE
          ) %>%
          do.call(rbind, .)

        fit.s <- bind_rows(fit.s, fit_fail[fit_fail$converged, ])
      }
    }
    sprintf("%d convergence failures remain", sum(!fit_fail$converged))

    return(fit.s)
  }


}
