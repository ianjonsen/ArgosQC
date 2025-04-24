##' @title create ssmoutputs data.frame
##'
##' @description build ssmoutputs data.frame from SSM predicted location
##'
##' @param fit final \code{aniMotum} fit object
##' @param what specify whether predicted or rerouted locations are to be used
##' @param drop.refs individual SMRU ids to be dropped
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

ssm_outputs <- function(fit,
                        what,
                        drop.refs
                        ) {

  ## get predicted locations from fits
  ## can't cut using keep here as it messes up track sub-sampling
  p <- grab_QC(fit,
               what = what,
               cut = FALSE,
               as_sf = FALSE) |>
    rename(ref = id) |>
    filter(!ref %in% drop.refs) |>
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))
  names(p) <- to_snake_case(names(p))

  if (all(!c("u", "v", "u_se", "v_se", "s", "s_se") %in% names(p))) {
    if (suffix != "_nrt") {
      p <- p |>
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
    } else {
      p <- p |>
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

  p.lst <- split(p, p$ref)

  ## sub-sample predicted locs to 6-h resolution
  p_out <- lapply(p.lst, function(x) {
    ts <- subset(fit, id == x$ref[1])$ssm[[1]]$ts
    if (ts <= 6)
      x[seq(1, nrow(x), by = ceiling(6 / ts)), ]
    else
      stop("time step is > 6 h, can't subsample to 6 h")
  }) |> bind_rows()

  ## calc QC start and end dates for each deployment - to be appended to metadata
  qc_se <- p_out |>
    group_by(ref) |>
    summarise(qc_start_date = min(date),
              qc_end_date = max(date))

  return(list(p_out = p_out, qc_se = qc_se))

}
