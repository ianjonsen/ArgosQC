##' @title truncate diag data and convert to sf tibble
##'
##' @description truncate start of diag data for each individual, using deployment date-time from metadata
##'
##' @param diag diag to use
##' @param meta metadata used to truncate start of diag data for each individual
##' @param crs a proj4string to re-project diag locations from longlat
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm), in latter case diag is not right-truncated
##'
##' @examples
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do "%>%" ungroup
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract
##'
##' @export
##'

truncate_diag_sf <- function(diag, meta,
                             crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +k=1 +ellps=WGS84 +units=km +no_defs",
                             QCmode = "nrt") {

  deploy_meta <- meta %>%
    select(device_id, ctd_start, ctd_end)

  if(QCmode == "nrt") {
    ## left- and right-truncate tracks
    diag <- diag %>%
      left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
      filter(date >= ctd_start & date <= ctd_end) %>%
      select(-ctd_start, -ctd_end)
  } else {
    ## only left-truncate tracks
    diag <- diag %>%
      left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
      filter(date >= ctd_start) %>%
      select(-ctd_start, -ctd_end)
  }

  diag_sf <- diag %>%
    mutate(id = ref) %>%
    select(ref, id, everything()) %>%
    group_by(ref) %>%
    do(
      d_sf =
        sf::st_as_sf(., coords = c("lon", "lat"),
                     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
        sf::st_transform(., crs = crs) %>%
        select(-ref)
    ) %>%
    ungroup() %>%
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))

  ## add species code
  msp <- meta %>% select(device_id, species)

  diag_sf <- diag_sf %>%
    left_join(., msp, by = c("ref" = "device_id")) %>%
    mutate(species = ifelse(species == "Mirounga leonina", "sese",
                            ifelse(species == "Leptonychotes weddellii", "wese",
                                   ifelse(species == "Arctocephalus forsteri", "nzfs",
                                          ifelse(species == "Arctocephalus pusillus", "aufs",
                                                 ifelse(species == "Neophoca cinerea", "ausl", NA)))))) %>%
    rename(sp = species) %>%
    select(ref, cid, sp, d_sf)

  return(diag_sf)
}
