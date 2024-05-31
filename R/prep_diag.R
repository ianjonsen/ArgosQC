##' @title clean, truncate & convert diag files to sf-tibbles
##'
##' @description restructures diag files, formats dates & lc's; truncates start
##' (and end for "nrt") of individual deployments using ctd dates; converts to
##' sf geometry - all in preparation for SSM-filtering. Splits resulting truncated
##' diag files by species.
##'
##' @param smru list of SMRU tables
##' @param meta metadata used to truncate start of diag data for each individual
##' @param drop.refs SMRU refs to be dropped (eg. tags were turned on but not deployed)
##' @param crs a proj4string to re-project diag locations from longlat
##' @param gps if a GPS table exists in `smru` should location be merged into diag (default is FALSE)
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm),
##' in latter case diag is not right-truncated & date of first dive is used
##' for the track start date
##'
##' @examples
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do
##' @importFrom dplyr %>% ungroup rename select
##' @importFrom assertthat assert_that
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract
##' @importFrom aniMotum format_data
##'
##' @export
##'

prep_diag <- function(smru,
                      meta,
                      drop.refs = NULL,
                      crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=70 +k=1 +ellps=WGS84 +units=km +no_defs",
                      gps = FALSE,
                      QCmode = "nrt") {

  assert_that(is.list(smru))

  ## clean step
  if("semi_major_axis" %in% names(smru$diag)) {
    diag <- smru$diag %>%
      dplyr::select(ref,
             d_date,
             lq,
             lon,
             lat,
             semi_major_axis,
             semi_minor_axis,
             ellipse_orientation)
  } else {
    diag <- smru$diag %>%
      dplyr::select(ref,
             d_date,
             lq,
             lon,
             lat) %>%
      mutate(semi_major_axis = NA,
             semi_minor_axis = NA,
             ellipse_orientation = NA)
  }

  diag <- diag %>%
    rename(
      ref = ref,
      date = d_date,
      lc = lq,
      smaj = semi_major_axis,
      smin = semi_minor_axis,
      eor = ellipse_orientation
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

  if(all(gps, "gps" %in% names(smru))) {
   gps <- smru$gps %>%
      mutate(date = lubridate::mdy_hms(d_date, tz="UTC")) %>%
      aniMotum::format_data(id = "ref",
                  coord = c("lon","lat")) %>%
      select(ref = id,date,lc,lon,lat,smaj,smin,eor) %>%
     filter(!is.na(lon), !is.na(lat))

   diag <- bind_rows(diag, gps) %>%
     group_by(ref) %>%
     arrange(date, .by_group=TRUE) %>%
     ungroup()
  }

  diag <- diag %>%
    filter(!ref %in% drop.refs)

  ## truncate & convert to sf geometry steps
  deploy_meta <- meta %>%
    dplyr::select(device_id, ctd_start, dive_start, ctd_end, dive_end)

  if(QCmode == "nrt") {
    ## left- and right-truncate tracks
    diag <- diag %>%
      left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
      filter(date >= ctd_start & date <= ctd_end) %>%
      dplyr::select(-ctd_start, -ctd_end, -dive_end)
  } else {
    ## only left-truncate tracks with date of first dive
    diag <- diag %>%
      left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
      filter(date >= dive_start) %>%
      dplyr::select(-ctd_start, -dive_start, -ctd_end, -dive_end)
  }

  diag_sf <- diag %>%
    mutate(id = ref) %>%
    dplyr::select(ref, id, everything()) %>%
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
                                                 ifelse(species == "Neophoca cinerea", "ausl",
                                                        ifelse(species == "Chelonia mydas", "grtu",
                                                               ifelse(species == "Lepidochelys olivacea", "ortu",
                                                                      ifelse(species == "Natator depressus", "fbtu", NA))))))))) %>%
    rename(sp = species) %>%
    mutate(sp = factor(sp, levels = c("sese","wese","nzfs","aufs","ausl","grtu","ortu","fbtu"), ordered = TRUE)) %>%
    mutate(sp = droplevels(sp)) %>%
    dplyr::select(ref, cid, sp, d_sf) %>%
    split(., .$sp)

  return(diag_sf)

}
