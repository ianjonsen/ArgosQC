##' @title clean metadata
##'
##' @description restructures metadata, formats dates, standardizes variable names, subsets to specified campaign ids
##'
##' @param cids SMRU campaign ids
##' @param file path to metadata .csv file
##'
##' @examples
##'
##' @importFrom dplyr select rename mutate filter "%>%"
##' @importFrom readr read_csv
##' @importFrom stringr str_to_lower str_replace_all str_extract regex
##' @importFrom lubridate mdy_hms
##'
##' @export
##'

clean_meta <- function(cids, file = "~/Dropbox/collab/imos/qc/r/meta/IMOS_CTD_metadata_11042019.csv") {

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
    ) %>%
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
    )

  ## subset to current campaigns
  ## currently, the metadata are missing 10 individuals - including all of wd11 (8 seals)
  meta <- meta %>%
    filter(sattag_program %in% cids)

  return(meta)
}