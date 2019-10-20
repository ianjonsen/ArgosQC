##' @title clean diag files
##'
##' @description restructures diag files, formats dates & lc's in preparation for SSM-filtering
##'
##' @param smru list of SMRU tables
##' @param drop.refs SMRU refs to be dropped (eg. tags were turned on but not deployed)
##'
##' @examples
##'
##' @importFrom dplyr select rename mutate filter "%>%"
##' @importFrom lubridate mdy_hms
##' @importFrom assertthat assert_that
##'
##' @export
##'

clean_diag <- function(smru, drop.refs = NULL) {

  assert_that(is.list(smru))

  diag <- smru$diag %>%
    select(1, 3:4, 6, 5, 23:25) %>%
    rename(
      ref = ref,
      date = d.date,
      lc = lq,
      smaj = semi.major.axis,
      smin = semi.minor.axis,
      eor = ellipse.orientation
    ) %>%
    mutate(ref = as.character(ref)) %>%
    mutate(date = mdy_hms(date, tz = "UTC")) %>%
    mutate(lc = ifelse(lc==-1, "A", lc), lc = ifelse(lc==-2, "B", lc), lc = ifelse(lc==-9, "Z", lc)) %>%
    mutate(lc = factor(
      lc,
      levels = c(3, 2, 1, 0, "A", "B", "Z"),
      labels = c("3", "2", "1", "0", "A", "B", "Z"),
      ordered = TRUE
    )) %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    mutate(smaj = as.numeric(smaj),
           smin = as.numeric(smin),
           eor = as.numeric(eor))

  diag <- diag %>%
    filter(!ref %in% drop.refs)

  return(diag)
}
