##' @title general interpolation function
##'
##' @description interpolate SSM locations to WC tag datafile event times
##'
##' @param sloc data.frame of SSM estimated locations
##' @param wc the wc tag datafile list
##'
##' @importFrom dplyr filter pull
##' @importFrom stats approx
##'
##' @keywords internal

approx_ssm <- function(sloc, wc) {
  if ("Date" %in% names(wc)) {
    if ("HistType" %in% names(wc)) {
      dt <- wc |>
        filter(DeploymentID == sloc$DeploymentID[1]) |>
        mutate(Date = ifelse(HistType == grepl("LIMITS", HistType), NA, Date)) |>
        mutate(Date = as.POSIXct(Date, tz = "UTC")) |>
        pull(Date)
    } else {
      dt <- wc |>
        filter(DeploymentID == sloc$DeploymentID[1]) |>
        pull(Date)
    }
  } else if ("End" %in% names(wc)) {
    dt <- wc |>
      filter(DeploymentID == sloc$DeploymentID[1]) |>
      pull(End)
  } else if ("DiveStart" %in% names(wc)) {
    dt <- wc |>
      filter(DeploymentID == sloc$DeploymentID[1]) |>
      pull(DiveStart)
  }
  lon1 <- approx(x = cbind(sloc$date, sloc$lon), xout = dt)$y
  lat1 <- approx(x = cbind(sloc$date, sloc$lat), xout = dt)$y
  x1 <- approx(x = cbind(sloc$date, sloc$x), xout = dt)$y
  y1 <- approx(x = cbind(sloc$date, sloc$y), xout = dt)$y
  x.se1 <- approx(x = cbind(sloc$date, sloc$x.se), xout = dt)$y
  y.se1 <- approx(x = cbind(sloc$date, sloc$y.se), xout = dt)$y
  data.frame(
    Date = dt,
    ssm_lon = round(lon1,6),
    ssm_lat = round(lat1,6),
    ssm_x = round(x1,6),
    ssm_y = round(y1,6),
    ssm_x_se = round(x.se1,6),
    ssm_y_se = round(y.se1,6)
  )
}
