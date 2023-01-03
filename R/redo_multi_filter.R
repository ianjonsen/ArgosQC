##' @title redo failed multi-filter cases
##'
##' @description re-apply SSM filter to diag data for id's that failed to converge. parallelized
##'
##' @param fit aniMotum fit object from first round of filtering
##' @param diag_sf \code{sf}-projected diag to be used
##' @param model model argument ("rw" or "crw) for \code{aniMotum::fit_ssm}
##' @param ts time.step argument for \code{aniMotum::fit_ssm}
##' @param vmax threshold travel speed (m/s) to apply during track pre-filtering
##' @param ang sdafilter argument
##' @param distlim sdafilter argument
##' @param min.dt min.dt argument for \code{aniMotum::fit_ssm}
##' @param map params to fix
##' @param verbose turn on/off furrr::future_map progress indicator
##'
##'
##' @examples
##'
##' @importFrom dplyr filter "%>%" bind_rows slice ungroup left_join select mutate
##' @importFrom tidyr nest
##' @importFrom future plan
##' @importFrom furrr future_map furrr_options
##' @importFrom aniMotum fit_ssm ssm_control
##'
##' @export
##'

redo_multi_filter <-
  function(fit,
           diag_sf,
           model = "crw",
           ts = 3,
           vmax = 2,
           ang = c(15, 25),
           distlim = c(1500, 5000),
           min.dt = 180,
           map = NULL,
           verbose = TRUE) {

  oc <- which(sapply(fit$ssm, inherits, "try-error"))
  sprintf("%d optimiser crashes", length(oc))
  nc <- which(!fit$converged)
  sprintf("%d failed to converge", length(nc))

  if(length(nc) > 0 | length(oc) > 0) {

    d <- sort(unique(c(oc,nc)))
    fit.f <- fit %>% ungroup() %>% slice(d)
    fit.s <- fit %>% ungroup() %>% slice(-d)

    ## get data for fit failures
    fail_dat <- diag_sf %>%
      filter(ref %in% fit.f$id)

    ## Refit Stage 1 - refit with a bigger min.dt
    fit_fail <- fail_dat$d_sf %>%
      future_map( ~ try(fit_ssm(
        x = .x,
        model = model,
        time.step = ts,
        min.dt = min.dt,
        vmax = vmax,
        ang = ang,
        distlim = distlim,
        map = map,
        control = ssm_control(verbose = 0)
      ), silent = TRUE),
      .progress = verbose,
      .options = furrr_options(seed = TRUE)
      ) %>%
      do.call(rbind, .)

    ## add successes onto original successful fits
    fit.s <- bind_rows(fit.s, fit_fail[fit_fail$converged, ])

    ## Refit Stage 2 - attempt to fix by truncating with end of ctd data & and use rw model
    if(nrow(fit.s) < nrow(fit)) {
      fit.f <- which(!fit_fail$converged)
      if(length(fit.f) > 0){
        fail_dat <- fail_dat %>%
          slice(fit.f)

        fit_fail <- fail_dat$d_sf %>%
          future_map(~ try(fit_ssm(
            x = .x,
            model = model,
            time.step = ifelse(ts==3, ts * 2, ts),
            min.dt = min.dt * 2,
            vmax = vmax,
            ang = ang,
            distlim = distlim,
            map = map,
            control = ssm_control(verbose = 0)
          ), silent = TRUE),
          .progress = verbose,
          .options = furrr_options(seed = TRUE)
          ) %>%
          do.call(rbind, .)

        fit.s <- bind_rows(fit.s, fit_fail[fit_fail$converged, ])
      }
    }
    sprintf("%d convergence failures remain", sum(!fit_fail$converged))

    return(fit.s)
  } else {
    return(fit)
  }


}
