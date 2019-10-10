##' @title truncate diag data and convert to sf tibble
##'
##' @description truncate start of diag data for each individual, using deployment date-time from metadata
##'
##' @param diag diag to use
##' @param meta metadata used to truncate start of diag data for each individual
##' @param crs a proj4string or EPSG code to re-project diag locations from longlat
##'
##' @examples
##'
##' @importFrom dplyr select left_join mutate filter group_by everything do "%>%" ungroup
##' @importFrom sf st_as_sf st_transform
##' @importFrom stringr str_extract
##'
##' @export
##'

truncate_diag_sf <- function(diag, meta, crs = "+init=epsg:3395 +units=km") {

  deploy_meta <- meta %>%
    select(device_id, release_date)

  diag <- diag %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    mutate(release_date = ifelse(is.na(release_date), date, release_date)) %>%
    mutate(release_date = as.POSIXct(release_date, origin = "1970-01-01", tz = "UTC")) %>%
    filter(date >= release_date) %>%
    select(-release_date)

  diag_sf <- diag %>%
    mutate(id = ref) %>%
    select(ref, id, everything()) %>%
    group_by(ref) %>%
    do(
      d_sf =
        sf::st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_transform(., crs = "+init=epsg:3395 +units=km") %>%
        select(-ref)
    ) %>%
    ungroup() %>%
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE))) %>%
    select(ref, cid, d_sf)

  return(diag_sf)
}
