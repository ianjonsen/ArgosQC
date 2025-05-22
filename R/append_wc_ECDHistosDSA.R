##' @title Interpolate & append SSM locations to WC ECDHistos_SCOUT_DSA datafiles
##'
##' @description interpolate SSM locations to WC ECDHistos_SCOUT_DSA datafiles event times &
##' append to WC ECDHistos_SCOUT_DSA datafiles
##'
##' @param wc the WC ECDHistos_SCOUT_DSA datafiles list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

append_wc_ECDHistosDSA <- function(wc,
                               locs,
                               deploy_meta,
                               dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(End >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(End, .by_group = TRUE) |>
    ungroup()

  out <- locs |>
    group_by(DeploymentID) |>
    do(locs = approx_ssm(., wc = wc)) |>
    unnest(cols = c(locs))

  out <- left_join(wc, out, by = c("DeploymentID", "End" = "Date")) |>
    distinct()

  out

}
