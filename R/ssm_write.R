##' @title ssmoutputs table write to .csv
##'
##' @description write to .csv - format depends on program (IMOS, ATN)
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select left_join group_split group_by
##' @importFrom readr write_csv
##' @importFrom purrr walk
##'
##' @internal

ssm_write <- function(p_out,
                      meta,
                      program = "imos",
                      path = NULL,
                      drop.refs = NULL,
                      suffix = "_nrt") {

  p_out <- p_out |>
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

  if (suffix != "_nrt") {
    ## cut predicted locs from large data gaps
    p_out <- p_out |>
      filter(keep) |>
      select(-keep)

  }

  if (program == "atn") {
    p_out <- left_join(
      p_out,
      meta |> select(DeploymentID, AnimalAphiaID, DeploymentLocation),
      by = c("ref" = "DeploymentID")
    ) |>
      select(-cid)

  }

  return(p_out)
}
