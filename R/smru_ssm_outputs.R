##' @title create ssmoutputs data.frame
##'
##' @description build ssmoutputs data.frame from SSM predicted location
##'
##' @param fit final \code{aniMotum} fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param dropIDs individual SMRU ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
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

smru_ssm_outputs <- function(fit,
                        what,
                        dropIDs,
                        suffix
                        ) {

  ## get predicted locations from fits
  ## can't cut using keep here as it messes up track sub-sampling
  locs <- grab_QC(fit,
               what = what,
               cut = FALSE,
               as_sf = FALSE) |>
    rename(ref = id) |>
    filter(!ref %in% dropIDs) |>
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))
  names(locs) <- to_snake_case(names(locs))

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
          select(ref,
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
                 cid,
                 keep)

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
          select(ref,
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
                 cid)
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
        select(ref,
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
               cid)
    }
  }


  locs.lst <- split(locs, locs$ref)

  p4s <- lapply(fit$ssm, function(x) {
    data.frame(id = x$fitted$id[1], proj4string = st_crs(x$fitted)[[1]])
  }) |>
    bind_rows()

  ## sub-sample predicted locs to 6-h resolution
  locs_out <- lapply(locs.lst, function(x) {
    ts <- subset(fit, id == x$ref[1])$ssm[[1]]$ts
    if (ts <= 6)
      x[seq(1, nrow(x), by = ceiling(6 / ts)), ]
    else
      stop("time step is > 6 h, can't subsample to 6 h")
  }) |> bind_rows()

  ## calc QC start and end dates for each deployment - to be appended to metadata
  qc_se <- locs_out |>
    group_by(ref) |>
    summarise(qc_start_date = min(date),
              qc_end_date = max(date))
  out <- left_join(qc_se, p4s, by = c("ref" = "id"))

  return(list(locs_out = locs_out, qc_se = out))

}
