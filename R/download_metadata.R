##' @title download deployment metadata
##'
##' @description downloads, restructures & formats metadata, appends
##' dive/CTD start and end datetimes (for QC), & fills in missing required
##' metadata - eg. release_datetime, release_longitude/latitude's with data from
##' the GPS (if present) or Argos location file.
##'
##' @param source the source of the metadata, current options are `imos`, `smru`,
##' `wc`, `ATN`.
##' @param tag_mfr the tag manufacturer, current options are `smru` or `wc`
##' cids SMRU campaign ids
##' @param tag_data a list of either `smru` data tables or `wc` files as output by
##' `pull_data`.
##' @param dropIDs SMRU refs or WC ids to be dropped
##' @param file path to metadata .csv file, if provided then metadata will be
##' downloaded from the provided `source`
##'
##' @examples
##'
##' @importFrom dplyr select rename mutate filter bind_rows
##' @importFrom rvest read_html html_nodes html_table
##' @importFrom stringr str_to_lower str_replace_all
##' @importFrom lubridate mdy_hms
##' @importFrom assertthat assert_that
##'
##' @export
##'

download_meta <- function(source = "smru",
                       tag_mfr = "smru",
                       tag_data = NULL,
                       cids = NULL,
                       dropIDs = NULL,
                       file = NULL) {

  if(is.null(tag_data)) stop("a tag_data object must be supplied")
  if(all(source == "imos", is.null(file))) stop("an IMOS-ATF .csv metadata file must be provided")

  if(source == "imos") {
    if(tag_mfr == "smru") {
      meta <- clean_meta(cids = cids,
                         smru = tag_data,
                         drop.refs = dropIDs,
                         file = file)

    } else if(tag_mfr != "wc") {
      stop("IMOS WC tags not yet supported")
    }

  } else if(source == "smru") {
    tag_mfr <- "smru"

    ## download SMRU metadata & generate QC metadata for IMOS-AODN
   tag_meta <- lapply(1:length(cids), function(i) {
      url <- paste0("https://imos:imos@www.smru.st-andrews.ac.uk/protected/", cids[i], "/", cids[i], ".html")
      tm <- url |>
        rvest::read_html() |>
        rvest::html_nodes(xpath = '/html/body/ul[1]/table') |>
        rvest::html_table()
      tm
    }) |> bind_rows()

    names(tag_meta) <- str_to_lower(names(tag_meta)) |>
      str_replace_all(" ", "_") |>
      str_replace_all("-", "_") |>
      str_replace_all("/", "_")

    tag_meta <- tag_meta %>%
      rename(
        device_id = reference,
        device_wmo_ref = wmo,
        tag_type = parameters,
        body = body
      ) |>
      select(-latest_uplink,
             -latest_argos_location)

    tmp <- split(tag_data$diag, tag_data$diag$ref)
    meta_loc <- lapply(tmp, function(x) {
      with(subset(x, d_date <= unique(round_date(d_date, unit="day"))[2]),
           data.frame(ref = ref[1],
                      release_date = d_date[1],
                      release_longitude = mean(lon, na.rm=T),
                      release_latitude = mean(lat, na.rm = T)))
    }) |>
      bind_rows()

    meta <- suppressWarnings(left_join(tag_meta, meta_loc, by = c("device_id"="ref")) |>
      mutate(sattag_program = str_split(device_id, "\\-", simplify = TRUE)[,1]) |>
      mutate(common_name = "southern elephant seal",
             species = "Mirounga leonina") |>
      mutate(release_site = "Iles Kerguelen") |>
      mutate(recovery_date = NA,
             age_class = NA,
             sex = NA,
             length = NA,
             estimated_mass = NA,
             actual_mass = NA) |>
      mutate(state_country = "French Overseas Territory") |>
      select(sattag_program,
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
             release_date,
             recovery_date,
             everything()) |>
      mutate(ptt = as.integer(ptt)) |>
      mutate(body = as.integer(body)) |>
      mutate(estimated_mass = as.integer(estimated_mass)))

## subset to current campaigns & apply drop.refs
meta <- meta |>
  filter(sattag_program %in% mdbs) |>
  filter(!device_id %in% dropIDs)

## append dive start and end dates for (alternate) track truncation
##  to be used as alternate on final, delayed-mode (manual) QC
dive_se <- tag_data$dive |>
  mutate(ref = as.character(ref)) |>
  select(ref, de_date, max_dep) |>
  group_by(ref) |>
  summarise(dive_start = min(de_date, na.rm = TRUE),
            dive_end = max(de_date, na.rm = TRUE))

## append CTD start and end dates for track truncation
ctd_se <- tag_data$ctd |>
  mutate(ref = as.character(ref)) |>
  select(ref, end_date) |>
  group_by(ref) |>
  summarise(ctd_start = min(end_date, na.rm = TRUE),
            ctd_end = max(end_date, na.rm = TRUE))

meta <- meta |>
  left_join(dive_se, by = c("device_id" = "ref")) |>
  left_join(ctd_se, by = c("device_id" = "ref"))

  }

return(meta)

}
