##' @title construct IMOS metadata from SMRU data server & config file details
##'
##' @description utility fn for constructing IMOS metadata when a .CSV metadata
##' file is not available. Required deployment info is scraped from the SMRU
##' Data server table associated with the provided `cid` (SMRU campaign id).
##'
##' @param cid the SMRU campaign ID - supplied by `conf$harvest$cid`
##' @param dropIDs SMRU refs or WC ids to be dropped from QC - supplied by
##' `conf$harvest$dropIDs` in `get_metadata()`
##' @param meta.args metadata fields to be passed from config file - required when
##' downloading tag metadata from SMRU server.
##' @param tag_data a list of SMRU data tables as output by `pull_data`.
##'
##' @importFrom dplyr select mutate filter rename left_join everything
##' @importFrom rvest read_html html_elements html_table
##' @importFrom stringr str_replace_all str_to_lower str_split
##'
##' @keywords internal
##'


smru_build_meta_imos <- function(cid,
                                dropIDs,
                                meta.args,
                                tag_data)
{

  ## download SMRU metadata & generate QC metadata for IMOS-AODN
  tag_meta <- lapply(1:length(cid), function(i) {
    url <- paste0("https://imos:imos@www.smru.st-andrews.ac.uk/protected/", cid[i], "/", cid[i], ".html")
    tm <- url |>
      read_html() |>
      html_elements(xpath = '/html/body/ul[1]/table') |>
      html_table()
    tm
  }) |> bind_rows()

  names(tag_meta) <- str_to_lower(names(tag_meta)) |>
    str_replace_all(" ", "_") |>
    str_replace_all("-", "_") |>
    str_replace_all("/", "_")

  tag_meta <- tag_meta |>
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
                             mutate(common_name = meta.args$common_name,
                                    species = meta.args$species) |>
                             mutate(release_site = meta.args$release_site) |>
                             mutate(recovery_date = NA,
                                    age_class = NA,
                                    sex = NA,
                                    length = NA,
                                    estimated_mass = NA,
                                    actual_mass = NA) |>
                             mutate(state_country = meta.args$state_country) |>
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
                             mutate(recovery_date = as.POSIXct(recovery_date, tz = "UTC")) |>
                             mutate(age_class = as.character(age_class)) |>
                             mutate(sex = as.character(sex)) |>
                             mutate(length = as.numeric(length)) |>
                             mutate(actual_mass = as.numeric(actual_mass)) |>
                             mutate(estimated_mass = as.integer(estimated_mass)))

  ## subset to current campaigns & apply drop.refs
  meta <- meta |>
    filter(sattag_program %in% cid) |>
    filter(!device_id %in% dropIDs)


}
