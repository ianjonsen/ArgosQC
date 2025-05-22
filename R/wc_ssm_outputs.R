##' @title create ssmoutputs data.frame
##'
##' @description build ssmoutputs data.frame from SSM predicted/rerouted locations
##'
##' @param fit final \code{aniMotum} fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param dropIDs individual WC DeploymentIDs to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##' @param pred.int prediction interval to use for sub-sampling predicted locations
##' (default = 6 h)
##'
##' @importFrom dplyr filter rename mutate select any_of bind_rows group_by
##' @importFrom dplyr group_split
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @keywords internal

wc_ssm_outputs <- function(fit,
                           what,
                           dropIDs,
                           suffix,
                           pred.int = 6
) {

  ## get predicted locations from fits
  ## can't cut using keelocshere as it messes ulocstrack sub-sampling
  locs <- grab_QC(fit,
               what = what,
               cut = FALSE,
               as_sf = FALSE) |>
    rename(DeploymentID = id) |>
    filter(!DeploymentID %in% dropIDs)
  names(locs)[-1] <- to_snake_case(names(locs)[-1])

  if (all(!c("u", "v", "u_se", "v_se", "s", "s_se") %in% names(locs))) {
    if (suffix != "_nrt") {
      if ("keep" %in% names(locs)) {
        locs <- locs |>
          mutate(
            u = NA,
            v = NA,
            u_se = NA,
            v_se = NA,
            s = NA,
            s_se = NA
          ) |>
          select(DeploymentID,
                 date,
                 lon,
                 lat,
                 x,
                 y,
                 x_se,
                 y_se,
                 u,
                 v,
                 u_se,
                 v_se,
                 s,
                 s_se,
                 keelocs)

      } else if (!"keep" %in% names(locs)){
        locs <- locs |>
          mutate(
            u = NA,
            v = NA,
            u_se = NA,
            v_se = NA,
            s = NA,
            s_se = NA
          ) |>
          select(DeploymentID,
                 date,
                 lon,
                 lat,
                 x,
                 y,
                 x_se,
                 y_se,
                 u,
                 v,
                 u_se,
                 v_se,
                 s,
                 s_se)
      }

    } else if (suffix == "_nrt") {
      locs <- locs |>
        mutate(
          u = NA,
          v = NA,
          u_se = NA,
          v_se = NA,
          s = NA,
          s_se = NA
        ) |>
        select(DeploymentID,
               date,
               lon,
               lat,
               x,
               y,
               x_se,
               y_se,
               u,
               v,
               u_se,
               v_se,
               s,
               s_se)
    }
  }


  locs.lst <- split(locs, locs$DeploymentID)
  p4s <- lapply(fit$ssm, function(x) {
    data.frame(id = x$fitted$id[1], proj4string = st_crs(x$fitted)[[1]])
  }) |>
    bind_rows()

  ## sub-sample predicted locs to 6-h resolution
  locs_out <- lapply(locs.lst, function(x) {
    ts <- subset(fit, id == x$DeploymentID[1])$ssm[[1]]$ts
    if (ts <= pred.int)
      x[seq(1, nrow(x), by = ceiling(pred.int / ts)), ]
    else
      stop(paste0("time stelocsis > ", pred.int, " h, can't subsample to ", pred.int, " h"))
  }) |> bind_rows()

  ## calc QC start and end dates for each deployment - to be appended to metadata
  qc_se <- locs_out |>
    group_by(DeploymentID) |>
    summarise(qc_start_date = min(date),
              qc_end_date = max(date))
  out <- left_join(qc_se, p4s, by = c("DeploymentID" = "id"))

  return(list(locs_out = locs_out, qc_se = out))

}
