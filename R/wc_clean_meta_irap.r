##' @title read & handle IRAP metadata from .CSV file & WC data portal
##'
##' @description utility fn for organising IRAP (NSWOO turtle/seal) metadata
##'
##' @param file filepath to metadata .csv file - supplied by `get_metadata()`
##' @param col.types.string string of single character col_types for `col_types`
##' arg in `read_csv`
##' @param ids subset of WC UUID's to consider - supplied by
##' `conf$harvest$tag.list` in `get_metadata()`
##' @param dropIDs SMRU refs or WC ids to be dropped from QC - supplied by
##' `conf$harvest$dropIDs` in `get_metadata()`
##' @param wc.meta an R data.frame of Wildlife Computers tag deployment metadata
##' obtained via `download_data()` - supplied by `get_metadata()`
##'
##' @importFrom dplyr select mutate filter left_join starts_with case_when everything
##' @importFrom readr read_csv
##'
##' @keywords internal
##'


wc_clean_meta_irap <- function(file,
                               col.types.string = "cTcddcccdccdcccciciT",
                               ids,
                               dropIDs,
                               wc.meta)
  {

  irap.meta <- read_csv(file, col_types = col.types.string) |>
    suppressMessages()

  if(!is.null(ids)) {
    irap.meta <- irap.meta |>
      filter(DeploymentID %in% ids$uuid) |>
      filter(!DeploymentID %in% dropIDs)
  }

  if(!is.null(wc.meta)) {
    irap.meta <- left_join(irap.meta, wc.meta, by = c("DeploymentID" = "id")) |>
      select(
        DeploymentID,
        sattag_program = sattag_program.y,
        owner,
        tag_model,
        tag_serial_number,
        tag_ptt,
        deploy_date,
        release_date,
        deploy_location = release_location,
        deploy_lon = longitude,
        deploy_lat = latitude,
        common_name,
        sex,
        age_class,
        length,
        length_measurement,
        length_unit,
        mass,
        mass_measurement,
        mass_unit,
        starts_with("QC_")
      ) |>
      mutate(deploy_date = case_when(is.na(deploy_date) ~ release_date,
                                     !is.na(deploy_date) ~ deploy_date)) |>
      select(-release_date)

  }

  irap.meta <- irap.meta |>
    mutate(irapID = paste(sattag_program,
                          tag_serial_number,
                          tag_ptt, sep = "_")) |>
    select(DeploymentID, irapID, everything())

  return(irap.meta)
}
