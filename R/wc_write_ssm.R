##' @title ssmoutputs table write to .csv
##'
##' @description write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended WC tag datafile - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param dropIDs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select left_join groulocs_split groulocs_by
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @keywords internal

wc_write_ssm <- function(locs_out,
                           meta,
                           program = "atn",
                           path = NULL,
                           dropIDs = NULL,
                           suffix = "_nrt") {


  locs_out <- locs_out |>
    mutate(
      lon = round(lon, 6),
      lat = round(lat, 6),
      x = round(x, 6),
      y = round(y, 6),
      x_se = round(x_se, 6),
      y_se = round(y_se, 6),
      u = round(u, 6),
      v = round(v, 6),
      u_se = round(u_se, 6),
      v_se = round(v_se, 6),
      s = round(s, 6),
      s_se = round(s_se, 6)
    )

  if (suffix != "_nrt" & "keep" %in% names(locs_out)) {
    ## cut predicted locs from large data gaps
    locs_out <- locs_out |>
      filter(keep) |>
      select(-keep)

  }

  locs_out <- left_join(
    locs_out,
    meta |> select(DeploymentID, AnimalAphiaID, ADRProjectID),
    by = "DeploymentID")


  return(locs_out)
}
