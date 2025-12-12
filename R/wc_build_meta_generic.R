##' @title construct generic deployment metadata from WC data portal xml & config
##' file details
##'
##' @description utility fn for constructing generic metadata when a .CSV metadata
##' file is not available. Required deployment info is pulled from the xml file
##' download from the WC Data Portal API. XML file downloaded via `wc_get_files()`
##'
##' @param ids subset of WC UUID's to consider - supplied by
##' `conf$harvest$tag.list` in `get_metadata()`
##' @param dropIDs WC ids to be dropped from QC - supplied by `conf$harvest$dropIDs`
##' in `get_metadata()`
##' @param meta.args metadata fields to be passed from config file - required when
##' downloading tag metadata from WC Data Portal API.
##' @param wc.meta an R data.frame of Wildlife Computers tag deployment metadata
##' obtained via `download_data()` - supplied by `get_metadata()`
##' @param tag_data a list of SMRU data tables as output by `pull_data`.
##'
##' @importFrom dplyr select mutate filter rename left_join everything case_when
##' @importFrom dplyr lag slice
##' @importFrom rvest read_html html_elements html_table
##' @importFrom stringr str_detect str_sub
##' @importFrom stats weighted.mean median
##' @importFrom assertthat assert_that
##'
##' @keywords internal
##'


wc_build_meta_generic <- function(ids,
                                 dropIDs,
                                 meta.args,
                                 wc.meta,
                                 tag_data)
{

  assert_that(!is.null(meta.args), msg = "A 'meta' block must be included in the config file")
  assert_that(!is.null(wc.meta), msg = "Metadata were not download from the WC Data Portal")

  tag_meta <- wc.meta |>
      filter(id %in% ids$uuid) |>
      filter(!id %in% dropIDs)

  tag_meta <- tag_meta |>
    rename(
      DeploymentID = id,
      device_id = tag
    ) |>
    select(DeploymentID,
           owner,
           sattag_program,
           ptt,
           device_id,
           deploy_date,
           deploy_lon,
           deploy_lat)

  ## split device_id into tag_model and tag_serial_number
  # tag_meta <- tag_meta |>
  #   mutate(tag_model = str)
  tag_meta <- tag_meta |>
    mutate(is_tag_serial = str_detect(device_id, "U")) |>
    mutate(tag_serial_number = case_when(
      is_tag_serial == TRUE ~ str_sub(device_id, -7),
      is_tag_serial == FALSE ~ NA)) |>
    mutate(tag_model = case_when(
      is_tag_serial == TRUE ~ str_sub(device_id, 1, -8),
      is_tag_serial == FALSE ~ device_id)) |>
    select(-is_tag_serial,
           -device_id)

  ## calculate Embark date, lon, lat & join to metadata
  ids <- tag_meta |>
    filter(is.na(deploy_date)) |>
    pull(DeploymentID)

  ## First check that there is no huge time gap between 1st several & subsequent locations
  ##  eg. caused by turning tag on at factory, research facility, etc... prior to
  ##  actual deployment

  first.locs <- tag_data$Locations |>
    filter(DeploymentID %in% ids)
  first.locs <- split(first.locs, first.locs$DeploymentID)
  st.idx <- sapply(first.locs, function(x) {
    x <- slice(x, 1:20)
    dt <- difftime(x$Date, lag(x$Date), units = "hours") |>
      as.numeric()
    if(any(dt > 12)) which.max(dt) + 1
    else 1
  })

  embark_dat <- tag_data$Locations |>
    filter(DeploymentID %in% ids)
  embark_dat <- split(embark_dat, embark_dat$DeploymentID)

  embark_meta <- lapply(1:length(embark_dat), function(i) {
    x <- subset(embark_dat[[i]], Date >= Date[st.idx[i]] & Date <= (Date[st.idx[i]] + 86400)) |>
      arrange(Date)

    with(x, data.frame(DeploymentID = DeploymentID[1],
                       embark_date = median(Date),
                       embark_longitude = weighted.mean(Longitude, w = 1/`Error radius`),
                       embark_latitude = weighted.mean(Latitude, w = 1/`Error radius`)
                       ))
  }) |>
    bind_rows()

  tag_meta <- left_join(tag_meta, embark_meta, by = c("DeploymentID" = "DeploymentID")) |>
    mutate(common_name = meta.args$common_name,
           species = meta.args$species) |>
    mutate(release_site = meta.args$release_site) |>
    mutate(recovery_date = NA,
           age_class = NA,
           sex = NA,
           length = NA,
           length_measurement = NA,
           length_unit = NA,
           mass = NA,
           mass_measurement = NA,
           mass_unit = NA
           ) |>
    mutate(state_country = meta.args$state_country) |>
    select(DeploymentID,
           sattag_program,
           owner,
           tag_model,
           tag_serial_number,
           tag_ptt = ptt,
           deploy_date,
           deploy_location = release_site,
           deploy_longitude = deploy_lon,
           deploy_latitude = deploy_lat,
           embark_date,
           embark_longitude,
           embark_latitude,
           common_name,
           species,
           sex,
           age_class,
           length,
           length_measurement,
           length_unit,
           mass,
           mass_measurement,
           mass_unit)


  return(tag_meta)

}
