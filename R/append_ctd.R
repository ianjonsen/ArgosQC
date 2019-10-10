##' @title append CTD data to metadata
##'
##' @description calculates CTD end dates for each id and appends to corresponding metadata record. This is used to right-truncate deployments that fail the first 1-2 SSM-filtering rounds - usually due to sporadic tag function toward end of deployment. The tags usually stop recording (or transmitting?) CTD profiles prior to extreme 'sporadicity'.
##'
##' @param cids SMRU campaign id(s)
##' @param smru SMRU table file - output of \code{pull_smru_tables}
##' @param meta clean metadata object - output of \code{clean_meta}
##'
##' @examples
##'
##' @importFrom dplyr select mutate group_by "%>%" summarise left_join
##' @importFrom lubridate mdy_hms
##'
##' @export

append_ctd <- function(cids, smru, meta) {

  ctd_end <- smru$ctd %>%
    mutate(ref = as.character(ref)) %>%
    select(ref, end.date) %>%
    mutate(end.date = mdy_hms(end.date, tz = "UTC")) %>%
    group_by(ref) %>%
    summarise(end = max(end.date))

  meta_ctd <- meta %>%
    left_join(., ctd_end, by = c("device_id" = "ref"))

  return(meta_ctd)
}
