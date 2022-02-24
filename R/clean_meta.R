##' @title clean metadata
##'
##' @description restructures metadata, formats dates,
##' standardizes variable names, subsets to specified campaign ids, appends
##' dive start and end datetimes (for QC), & fills in missing
##' release_longitude/latitude's with the first best quality (lc 3,2, or 1)
##' Argos location after release_datetime
##'
##' @param cids SMRU campaign ids
##' @param smru SMRU table
##' @param drop.refs SMRU refs to be dropped
##' @param file path to metadata .csv file
##'
##' @examples
##'
##' @importFrom dplyr select rename mutate filter "%>%" bind_rows
##' @importFrom readr read_csv
##' @importFrom stringr str_to_lower str_replace_all str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom assertthat assert_that
##'
##' @export
##'

clean_meta <- function(cids, smru, drop.refs = NULL, file = NULL) {

  assert_that(!is.null(file))

  meta <- suppressWarnings(read_csv(file)) %>%
    select(
      Species,
      "Common name",
      Sex,
      AgeClass,
      Unittype,
      "TDR_CTDID",
      PTT,
      "SMRU_IDBody",
      "SMRU_Ref",
      WMO,
      Day,
      Month,
      Year,
      Location,
      Lat,
      Long,
      TDeploy,
      STD_L,
      CV_L,
      M_est,
      Mass,
      Age,
      "G1-G6",
      "G6-G3",
      "G3-G1",
      G1,G2,G3,G4,G5,G6
    )
  names(meta) <- str_to_lower(names(meta)) %>%
    str_replace_all(., " ", "_") %>%
    str_replace_all(., "-", "_") %>%
    str_replace_all(., "/", "_")

  meta <- meta %>%
    rename(
      device_id = smru_ref,
      device_wmo_ref = wmo,
      tag_type = unittype,
      age_class = ageclass,
      body = tdr_ctdid,
      deploy_time = tdeploy,
      release_latitude = lat,
      release_longitude = long,
      release_site = location,
      length = std_l,
      estimated_mass = m_est,
      actual_mass = mass
    )

  meta <- meta %>%
    mutate(release_date = lubridate::ymd(paste(year, month, day, sep = "-"), tz = "UTC")) %>%
    mutate(sattag_program = str_extract(device_id, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE))) %>%
    mutate(recovery_date = NA) %>%
    mutate(common_name = ifelse((species == "Leptonychotes weddellii" & common_name != "Weddell seal"), "Weddell seal", common_name)) %>%
    select(
      sattag_program,
      device_id,
      ptt,
      body,
      device_wmo_ref,
      tag_type,
      common_name,
      species,
      release_longitude,
      release_latitude,
      release_site,
      #    state_country,
      release_date,
      recovery_date,
      age_class,
      sex,
      length,
      estimated_mass,
      actual_mass
    ) %>%
    mutate(ptt = as.integer(ptt)) %>%
    mutate(body = as.integer(body))

  meta <- suppressWarnings(meta %>% mutate(estimated_mass = as.integer(estimated_mass)))

  ## subset to current campaigns & apply drop.refs
  meta <- meta %>%
    filter(sattag_program %in% cids) %>%
    filter(!device_id %in% drop.refs)

  ## append dive start and end dates for (alternate) track truncation
  ##  to be used as alternate on final, delayed-mode (manual) QC
  dive_se <- smru$dive %>%
    mutate(ref = as.character(ref)) %>%
    select(ref, de_date) %>%
    group_by(ref) %>%
    summarise(dive_start = min(de_date, na.rm = TRUE), dive_end = max(de_date, na.rm = TRUE))

  ## append CTD start and end dates for track truncation
  ctd_se <- smru$ctd %>%
    mutate(ref = as.character(ref)) %>%
    select(ref, end_date) %>%
    group_by(ref) %>%
    summarise(ctd_start = min(end_date, na.rm = TRUE),
              ctd_end = max(end_date, na.rm = TRUE))

  meta <- meta %>%
    left_join(., dive_se, by = c("device_id" = "ref")) %>%
    left_join(., ctd_se, by = c("device_id" = "ref"))

  ## if absent, fill in missing release_longitude/latitude's with the
  ##   most recent lc = 3 | 2 Argos location (after or on release_date)0
  if(any(is.na(meta$release_longitude), is.na(meta$release_longitude))) {
    tmp <- meta %>% select(device_id, release_date)
    tmp1 <- left_join(smru$diag, tmp, by = c("ref" = "device_id")) %>%
      select(device_id = ref, lq, d_date, release_date, lon, lat) %>%
      filter(lq > 1 & d_date >= release_date) %>%
      split(., .$device_id) %>%
      lapply(., function(x) x[1,]) %>%
      bind_rows() %>%
      select(device_id, lon, lat)
    meta <- left_join(meta, tmp1, by = "device_id") %>%
      mutate(release_latitude = ifelse(is.na(release_latitude),
                                       lat,
                                       release_latitude),
             release_longitude = ifelse(is.na(release_longitude),
                                        lon,
                                        release_longitude)
             ) %>%
      select(-lon, -lat)
  }

  return(meta)
}
