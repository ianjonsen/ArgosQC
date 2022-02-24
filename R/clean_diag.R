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
##' @importFrom assertthat assert_that
##'
##' @export
##'

clean_diag <- function(smru, drop.refs = NULL) {

  assert_that(is.list(smru))

  ## clean step
  if("semi_major_axis" %in% names(smru$diag)) {
    diag <- smru$diag %>%
      select(ref,
             d_date,
             lq,
             lon,
             lat,
             smaj = semi_major_axis,
             smin = semi_minor_axis,
             eor = ellipse_orientation)
  } else {
    diag <- smru$diag %>%
      select(ref,
             d_date,
             lq,
             lon,
             lat) %>%
      mutate(smaj = NA,
             smin = NA,
             eor = NA)
  }

  diag <- diag %>%
    rename(
      ref = ref,
      date = d_date,
      lc = lq
    ) %>%
    mutate(ref = as.character(ref)) %>%
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
    filter(!ref %in% drop.refs) %>%
    filter(!is.na(ref))

  return(diag)
}
