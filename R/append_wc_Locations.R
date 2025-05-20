##' @title Interpolate & append SSM locations to WC Locations file
##'
##' @description interpolate SSM locations to Locations event times &
##' append to Locations WC tag datafile
##'
##' @param wc the wc tag datafile list
##' @param locs SSM location estimates
##' @param deploy_meta the deployment metadata object created in `append_wc()`
##' @param dropIDs DeploymentID's to be ignored
##'
##' @importFrom dplyr filter group_by ungroup arrange right_join
##' @importFrom stats approx
##'
##' @keywords internal

append_wc_Locations <- function(wc,
                                locs,
                                deploy_meta,
                                dropIDs) {

  wc <- wc |>
    left_join(deploy_meta, by = "DeploymentID") |>
    filter(Date >= dive_start) |>
    filter(!DeploymentID %in% dropIDs) |>
    group_by(DeploymentID) |>
    arrange(Date, .by_group = TRUE) |>
    ungroup()

  wc.out <- left_join(wc, locs, by = c("DeploymentID", "Date" = "date")) |>
    group_by(DeploymentID) |>
    arrange(.by_group = TRUE) |>
    distinct() |>
    rename(
      ssm_lon = lon,
      ssm_lat = lat,
      ssm_x = x,
      ssm_y = y,
      ssm_x_se = x.se,
      ssm_y_se = y.se
    )


    return(wc.out)
}

