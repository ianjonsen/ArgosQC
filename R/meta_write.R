##' @title metadata table tests & write to .csv
##'
##' @description Apply AODN tests to metadata table, write to .csv - format depends on program (IMOS, ATN)
##'
##' @param meta metadata
##' @param program Determines structure of output metadata. Currently, either `imos` or `atn`.
##' @param test should variables be tested for standards compliance, default is TRUE.
##' Standards compliance is specific to the program. Currently, only program = `imos`
##' has defined variable standard against which output compliance is tested.
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##'
##' @importFrom dplyr filter rename mutate select any_of bind_rows group_by
##' @importFrom dplyr group_split
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @keywords internal

meta_write <- function(meta,
                           program = "imos",
                           test = TRUE,
                           path = NULL,
                           drop.refs = NULL,
                           suffix = "_nrt") {

  stopifnot("A destination directory for .csv files must be provided" = !is.null(path))

  ## remove dive, ctd start/end dates columns, add 'state_country' for AODN (based on deployment location)
  if (program == "imos") {

    meta <- meta |>
      mutate(
        state_country = ifelse(
          release_site == "Dumont d'Urville",
          "French Antarctic Territory",
          NA
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Dumont D'Urville",
          "French Antarctic Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Iles Kerguelen",
          "French Overseas Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Scott Base",
          "New Zealand Antarctic Territory",
          state_country
        )
      ) |>
      mutate(state_country = ifelse(
        release_site == "Campbell Island",
        "New Zealand",
        state_country
      )) |>
      mutate(state_country = ifelse(release_site == "Montague Island", "Australia", state_country)) |>
      mutate(state_country = ifelse(
        release_site == "Macquarie Island",
        "Australia",
        state_country
      )) |>
      mutate(
        state_country = ifelse(
          release_site == "Casey",
          "Australian Antarctic Territory",
          state_country
        )
      ) |>
      mutate(
        state_country = ifelse(
          release_site == "Davis",
          "Australian Antarctic Territory",
          state_country
        )
      ) |>
      mutate(state_country = ifelse(is.na(state_country), "Unknown", state_country))


    ## check metadata schema compliance to AODN standard
    meta <- meta |>
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
        release_date,
        recovery_date,
        age_class,
        sex,
        length,
        estimated_mass,
        actual_mass,
        state_country,
        qc_start_date,
        qc_end_date
      )

    ## return error if unexpected object mode or value
    tests <- with(
      meta,
      c(
        is.character(sattag_program),
        is.character(device_id),
        is.integer(ptt),
        is.integer(body),
        is.character(device_wmo_ref),
        is.character(tag_type),
        is.character(common_name),
        is.character(species),
        all(is.double(release_longitude), ((release_longitude >= -180 &
                                              release_longitude <= 180) |
                                             (release_longitude >= 0 &
                                                release_longitude <= 360) |
                                             is.na(release_longitude)
        )),
        all(is.double(release_latitude), ((release_latitude >= -90 &
                                             release_latitude <= 90) |
                                            is.na(release_latitude)
        )),
        is.character(release_site),
        any(inherits(release_date, "POSIXct"), is.na(release_date)),
        any(inherits(recovery_date, "POSIXct"), is.na(recovery_date)),
        all(
          unique(age_class) %in% c("adult", "subadult", "juvenille", "juvenile", "weaner", NA)
        ),
        all(unique(sex) %in% c("female", "male", "f", "m", NA)),
        all(is.double(length), (length > 0 |
                                  is.na(length))),
        all(is.integer(estimated_mass), (
          estimated_mass > 0 | is.na(estimated_mass)
        )),
        all(is.double(actual_mass), (actual_mass > 0 |
                                       is.na(actual_mass))),
        is.character(state_country),
        any(inherits(qc_start_date, "POSIXct"), is.na(qc_start_date)),
        any(inherits(qc_end_date, "POSIXct"), is.na(qc_end_date))
      )
    )
    if(test) {
      fails <- names(meta)[which(!tests)]
      if (length(fails) > 0)
        stop(paste0("non-compliant metadata records found in: ", fails, "\n"))
    }

    ## If metadata is compliant then write to .csv by sattag_program (SMRU campaign id)
    meta <- meta |>
      mutate(age_class = ifelse(age_class == "juvenile", "juvenille", age_class))

  } else if (program == "atn") {

    meta <- meta |> filter(!DeploymentID %in% drop.refs)

  }

  return(meta)
}
