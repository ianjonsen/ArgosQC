##' @title clean diag files
##'
##' @description restructures diag files, formats dates & lc's in preparation for SSM-filtering
##'
##' @param smru list of SMRU tables
##' @param dropIDs SMRU refs to be dropped (eg. tags were turned on but not deployed)
##'
##' @importFrom dplyr select rename mutate filter %>%
##' @importFrom assertthat assert_that
##'
##' @export
##'

smru_clean_diag <- function(smru,
                       dropIDs = NULL) {

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
    filter(!ref %in% dropIDs) %>%
    filter(!is.na(ref))

  ## locations at exactly 0, 0 are Argos placeholders, not positions
  diag <- qc_drop_null_island(diag, quiet = TRUE)

  return(diag)
}

## drop locations at exactly latitude 0 and longitude 0 (internal). Argos
##  reports 0, 0 as a placeholder for no position; runs of them pass the speed
##  filter (zero distance between consecutive locations) and pull the SSM fit to
##  the equator. Used in near real-time and delayed-mode QC. Reports the number
##  dropped per deployment unless quiet = TRUE
qc_drop_null_island <- function(d, quiet = FALSE) {
  null <- !is.na(d$lat) & !is.na(d$lon) & d$lat == 0 & d$lon == 0
  if (any(null) && !quiet) {
    tb <- table(as.character(d$ref[null]))
    qc_msg("excluded", "Locations at 0, 0 (Argos placeholder) dropped: ",
           paste0(names(tb), " (", as.integer(tb), ")", collapse = ", "))
  }
  d[!null, , drop = FALSE]
}
