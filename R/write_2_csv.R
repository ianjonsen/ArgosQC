##' @title reconfigure annotated SMRU tables, test for expected data schema for AODN
##'
##' @description reconfigure annotated tables - subsample predicted locations to 6-h interval, write to .csv and zip by campaign id
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param fit final \code{aniMotum} fit object
##' @param meta metadata
##' @param path path to write .csv files
##' @param drop.refs individual ids to be dropped
##' @param suffix suffix to add to .csv files (_nrt, _dm, or _hist)
##'
##' @examples
##'
##' @importFrom dplyr filter rename mutate %>% select any_of
##' @importFrom stringr str_extract regex
##' @importFrom lubridate mdy_hms
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom snakecase to_snake_case
##'
##' @export

write_2_csv <- function(smru_ssm, fit, meta, path = "~/Dropbox/collab/imos/imos_qc/aodn", drop.refs = NULL, suffix = "_nrt") {

  ## get predicted locations from fits
  ## can't cut using keep here as it messes up track subsampling
  p <- grab_QC(fit, "predicted", cut = FALSE, as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))
  names(p) <- to_snake_case(names(p))

  if(all(!c("u","v","u_se","v_se","s","s_se") %in% names(p))) {
    p <- p %>%
      mutate(u = NA, v = NA, u_se = NA, v_se = NA, s = NA, s_se = NA) %>%
      select(ref, date, lon, lat, x, y, x_se, y_se, u, v, u_se, v_se, s, s_se, keep, cid)
  }

  p.lst <- split(p, p$ref)

  ## subsample predicted locs to 6-h resolution
  p_out <- lapply(p.lst, function(x) {
    ts <- subset(fit, id == x$ref[1])$ssm[[1]]$ts
    if (ts <= 6)
      x[seq(1, nrow(x), by = ceiling(6 / ts)),]
    else
      stop("time step is > 6 h, can't subsample to 6 h")
  }) %>% do.call(rbind, .)

  ## calc QC start and end dates for each deployment - to be appended to metadata
  qc_se <- p_out %>%
    group_by(ref) %>%
    summarise(qc_start_date = min(date), qc_end_date = max(date))

  if(suffix != "_nrt") {
  ## cut predicted locs from large data gaps, split by campaign id & write .csv files
  p_out %>%
    mutate(lon = round(lon,6),
           lat = round(lat,6),
           x = round(x,6),
           y = round(y,6),
           x_se = round(x_se,6),
           y_se = round(y_se,6),
           u = round(u,6),
           v = round(v,6),
           u_se = round(u_se,6),
           v_se = round(v_se,6),
           s = round(s,6),
           s_se = round(s_se,6)) %>%
    filter(keep) %>%
    select(-keep) %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "ssmoutputs"), "_", .x$cid[1], suffix, ".csv"))))
  } else {
    p_out %>%
      mutate(lon = round(lon,6),
             lat = round(lat,6),
             x = round(x,6),
             y = round(y,6),
             x_se = round(x_se,6),
             y_se = round(y_se,6),
             u = round(u,6),
             v = round(v,6),
             u_se = round(u_se,6),
             v_se = round(v_se,6),
             s = round(s,6),
             s_se = round(s_se,6)) %>%
      split(., .$cid) %>%
      walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "ssmoutputs"), "_", .x$cid[1], suffix, ".csv"))))
    }
  diag <- smru_ssm$diag %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}"))

  ## check diag schema compliance to AODN standard
  vars <- c("ref",
                 "ptt",
                 "d_date",
                 "lq",
                 "lat",
                 "lon",
                 "alt_lat",
                 "alt_lon",
                 "n_mess",
                 "n_mess_120",
                 "best_level",
                 "pass_dur",
                 "freq",
                 "v_mask",
                 "alt",
                 "est_speed",
                 "km_from_home",
                 "iq",
                 "nops",
                 "deleted",
                 "actual_ptt",
                 "error_radius",
                 "semi_major_axis",
                 "semi_minor_axis",
                 "ellipse_orientation",
                 "hdop",
                 "satellite",
                 "diag_id",
                 "ssm_lon",
                 "ssm_lat",
                 "ssm_x",
                 "ssm_y",
                 "ssm_x_se",
                 "ssm_y_se",
                 "cid")
  diag <- diag %>%
    select(any_of(vars))

  ## return error if unexpected object mode or value
  tests <- with(diag, c(is.character(ref),
                        is.integer(ptt),
                        inherits(d_date, "POSIXct"),
                        is.integer(lq),
                        all(is.double(lat),
                            ((lat >= -90 & lat <= 90) |
                               is.na(lat))),
                        all(is.double(lon),
                            ((lon >= -180 & lon <= 180) |
                               (lon >= 0 & lon <= 360) |
                               is.na(lon))),
                        all(is.double(alt_lat),
                            ((alt_lat >= -90 & alt_lat <= 90) |
                               is.na(alt_lat))),
                        all(is.double(alt_lon),
                            ((alt_lon >= -180 & alt_lon <= 180) |
                               (alt_lon >= 0 & alt_lon <= 360) |
                               is.na(alt_lon))),
                        all(is.integer(n_mess), n_mess >= 0),
                        all(is.integer(n_mess_120), n_mess_120 >= 0),
                        is.integer(best_level),
                        is.integer(pass_dur),
                        all(is.double(freq), freq > 0),
                        is.integer(v_mask),
                        any(is.numeric(alt), is.na(alt)),
                        all(is.double(est_speed), (est_speed >= -1 | is.na(est_speed))),
                        all(is.double(km_from_home), (km_from_home >= 0 | is.na(km_from_home))),
                        is.integer(iq),
                        is.integer(nops),
                        any(is.logical(deleted), is.character(deleted)),
                        is.integer(actual_ptt),
                        if("error_radius" %in% names(diag)) {
                          all(any(is.integer(error_radius), is.na(error_radius)), (error_radius >= 0 | is.na(error_radius)))
                          },
                        if("semi_major_axis" %in% names(diag)) {
                          all(any(is.integer(semi_major_axis), is.na(semi_major_axis)), (semi_major_axis >= 0 | is.na(semi_major_axis)))
                        },
                        if("semi_minor_axis" %in% names(diag)) {
                          all(any(is.integer(semi_minor_axis), is.na(semi_minor_axis)), (semi_minor_axis >= 0 | is.na(semi_minor_axis)))
                        },
                        if("ellipse_orientation" %in% names(diag)) {
                          all(any(is.integer(ellipse_orientation), is.na(ellipse_orientation)), ((ellipse_orientation >= 0 & ellipse_orientation <= 180) | is.na(ellipse_orientation)))
                        },
                        if("hdop" %in% names(diag)) {
                          any(is.integer(hdop), is.na(hdop))
                        },
                        if("satellite" %in% names(diag)) {
                          any(is.character(satellite), is.na(satellite))
                        },
                        if("diag_id" %in% names(diag)) {
                          is.integer(diag_id)
                        }))
  fails <- names(diag)[which(!tests)]
  if(length(fails) > 0) stop(paste0("non-compliant diag records found in: ", fails, "\n"))

   diag <- diag %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "diag"), "_", .x$cid[1], suffix, ".csv"))))

  smru_ssm$haulout <- smru_ssm$haulout %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}"))

  ## check haulout schema compliance to AODN standard
  vars <- c("ref",
                 "ptt",
                 "s_date",
                 "e_date",
                 "haulout_number",
                 "cnt",
                 "phosi_secs",
                 "wet_n",
                 "wet_min",
                 "wet_max",
                 "wet_mean",
                 "wet_sd",
                 "tagging_id",
                 "s_date_tag",
                 "e_date_tag",
                 "end_number",
                 "lat",
                 "lon",
                 "ssm_lon",
                 "ssm_lat",
                 "ssm_x",
                 "ssm_y",
                 "ssm_x_se",
                 "ssm_y_se",
                 "cid"
  )
  smru_ssm$haulout <- smru_ssm$haulout %>%
    select(any_of(vars))
  if(any(!c("phosi_secs","wet_n","wet_min","wet_max","wet_mean","wet_sd","tagging_id","s_date_tag","e_date_tag") %in%
         names(smru_ssm$haulout))) {
    test <- with(smru_ssm$haulout,
                 c(is.character(ref),
                   is.integer(ptt),
                   inherits(s_date, "POSIXct"),
                   inherits(e_date, "POSIXct"),
                   is.integer(haulout_number),
                   all(is.integer(cnt), cnt >= 0),
                   is.integer(end_number)))
  } else {
  test <- with(smru_ssm$haulout,
               c(is.character(ref),
                 is.integer(ptt),
                 inherits(s_date, "POSIXct"),
                 inherits(e_date, "POSIXct"),
                 is.integer(haulout_number),
                 all(is.integer(cnt), cnt >= 0),
                 all(is.integer(phosi_secs), (phosi_secs >= 0 |
                                                is.na(phosi_secs))),
                 all(is.integer(wet_n), (wet_n >= 0 |
                                                is.na(wet_n))),
                 all(is.double(wet_min), (wet_min >= 0 |
                                           is.na(wet_min))),
                 all(is.double(wet_max), (wet_max >= 0 |
                                           is.na(wet_max))),
                 all(is.double(wet_mean), (wet_mean >= 0 |
                                           is.na(wet_mean))),
                 all(is.double(wet_sd), (wet_sd >= 0 |
                                           is.na(wet_sd))),
                 any(is.integer(tagging_id) | is.na(tagging_id)),
                 any(inherits(s_date_tag, "POSIXct") | is.na(s_date_tag)),
                 any(inherits(e_date_tag, "POSIXct") | is.na(e_date_tag)),
                 is.integer(end_number)))
  }
  ## return error if unexpected object mode or value
  fails <- names(smru_ssm$haulout)[which(!tests)]
  if(length(fails) > 0) stop(paste0("non-compliant haulout records found in: ", fails, "\n"))

  smru_ssm$haulout <- smru_ssm$haulout %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "haulout"), "_", .x$cid[1], suffix, ".csv"))))

  smru_ssm$ctd <- smru_ssm$ctd %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}"))

  ## check ctd schema compliance to AODN standard
  vars <- c("ref",
                 "ptt",
                 "end_date",
                 "max_dbar",
                 "num",
                 "n_temp",
                 "n_cond",
                 "n_sal",
                 "temp_dbar",
                 "temp_vals",
                 "cond_dbar",
                 "cond_vals",
                 "sal_dbar",
                 "sal_vals",
                 "n_fluoro",
                 "fluoro_dbar",
                 "fluoro_vals",
                 "n_oxy",
                 "oxy_dbar",
                 "oxy_vals",
                 "qc_profile",
                 "qc_temp",
                 "qc_sal",
                 "sal_corrected_vals",
                 "created",
                 "modified",
                 "n_photo",
                 "photo_dbar",
                 "photo_vals",
                 "lat",
                 "lon",
                 "ssm_lon",
                 "ssm_lat",
                 "ssm_x",
                 "ssm_y",
                 "ssm_x_se",
                 "ssm_y_se",
                 "cid")
  smru_ssm$ctd <- smru_ssm$ctd %>%
    select(any_of(vars))

  if (any(!c("photo_dbar", "photo_vals", "created", "modified", "n_photo")
          %in% smru_ssm$ctd)) {
    test <- with(
      smru_ssm$ctd,
      c(
        is.character(ref),
        is.integer(ptt),
        inherits(end_date, "POSIXct"),
        all(is.double(max_dbar), max_dbar >= 0),
        is.integer(num),
        all(is.integer(n_temp), (n_temp >= 0 |
                                   is.na(n_temp))),
        all(is.integer(n_cond), (n_cond >= 0 |
                                   is.na(n_cond))),
        all(is.integer(n_sal), (n_sal >= 0 |
                                  is.na(n_sal))),
        any(is.character(temp_dbar), all(is.na(temp_dbar))),
        any(is.character(temp_vals), all(is.na(temp_vals))),
        any(is.character(cond_dbar), all(is.na(cond_dbar))),
        any(is.character(cond_vals), all(is.na(cond_vals))),
        any(is.character(sal_dbar), all(is.na(sal_dbar))),
        any(is.character(sal_vals), all(is.na(sal_vals))),
        any(is.integer(n_fluoro), all(is.na(n_fluoro))),
        any(is.character(fluoro_dbar), all(is.na(fluoro_dbar))),
        any(is.character(fluoro_vals), all(is.na(fluoro_vals))),
        any(is.integer(n_oxy), all(is.na(n_oxy))),
        any(is.character(oxy_dbar), all(is.na(oxy_dbar))),
        any(is.character(oxy_vals), all(is.na(oxy_vals))),
        is.integer(qc_profile),
        any(is.character(qc_temp), all(is.na(qc_temp))),
        any(is.character(qc_sal), all(is.na(qc_sal))),
        any(is.character(sal_corrected_vals),
            all(is.na(
              sal_corrected_vals
            )))
      )
    )
  } else {
    test <- with(
      smru_ssm$ctd,
      c(
        is.character(ref),
        is.integer(ptt),
        inherits(end_date, "POSIXct"),
        all(is.double(max_dbar), max_dbar >= 0),
        is.integer(num),
        all(is.integer(n_temp), (n_temp >= 0 |
                                   is.na(n_temp))),
        all(is.integer(n_cond), (n_cond >= 0 |
                                   is.na(n_cond))),
        all(is.integer(n_sal), (n_sal >= 0 |
                                  is.na(n_sal))),
        any(is.character(temp_dbar), all(is.na(temp_dbar))),
        any(is.character(temp_vals), all(is.na(temp_vals))),
        any(is.character(cond_dbar), all(is.na(cond_dbar))),
        any(is.character(cond_vals), all(is.na(cond_vals))),
        any(is.character(sal_dbar), all(is.na(sal_dbar))),
        any(is.character(sal_vals), all(is.na(sal_vals))),
        any(is.integer(n_fluoro), all(is.na(n_fluoro))),
        any(is.character(fluoro_dbar), all(is.na(fluoro_dbar))),
        any(is.character(fluoro_vals), all(is.na(fluoro_vals))),
        any(is.integer(n_oxy), all(is.na(n_oxy))),
        any(is.character(oxy_dbar), all(is.na(oxy_dbar))),
        any(is.character(oxy_vals), all(is.na(oxy_vals))),
        is.integer(qc_profile),
        any(is.character(qc_temp), all(is.na(qc_temp))),
        any(is.character(qc_sal), all(is.na(qc_sal))),
        any(is.character(sal_corrected_vals),
            all(is.na(
              sal_corrected_vals
            ))),
        any(inherits(created, "POSIXct"), all(is.na(created))),
        any(inherits(modified, "POSIXct"), all(is.na(modified))),
        any(is.integer(n_photo), all(is.na(n_photo))),
        any(is.character(photo_dbar), all(is.na(photo_dbar))),
        any(is.character(photo_vals), all(is.na(photo_vals)))
      )
    )
  }

  ## return error if unexpected object mode or value
  fails <- names(smru_ssm$ctd)[which(!tests)]
  if(length(fails) > 0) stop(paste0("non-compliant ctd records found in: ", fails, "\n"))

  smru_ssm$ctd <- smru_ssm$ctd %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "ctd"), "_", .x$cid[1], suffix, ".csv"))))

  smru_ssm$dive %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "dive"), "_", .x$cid[1], suffix, ".csv"))))

  smru_ssm$ssummary %>%
    filter(!ref %in% drop.refs) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "summary"), "_", .x$cid[1], suffix, ".csv"))))

  ## remove dive, ctd start/end dates columns, add 'state_country' for AODN (based on deployment location)
  meta <- meta %>%
    filter(!device_id %in% drop.refs) %>%
    left_join(., qc_se, by = c("device_id" = "ref")) %>%
    select(-dive_start, -dive_end, -ctd_start, -ctd_end) %>%
    mutate(state_country = ifelse(release_site == "Dumont d'Urville", "French Antarctic Territory", NA)) %>%
    mutate(state_country = ifelse(release_site == "Dumont D'Urville", "French Antarctic Territory", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Iles Kerguelen", "French Overseas Territory", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Scott Base", "New Zealand Antarctic Territory", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Campbell Island", "New Zealand", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Montague Island", "Australia", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Macquarie Island", "Australia", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Casey", "Australian Antarctic Territory", state_country)) %>%
    mutate(state_country = ifelse(release_site == "Davis", "Australian Antarctic Territory", state_country)) %>%
    mutate(state_country = ifelse(is.na(state_country), "Unknown", state_country))

  ## check metadata schema compliance to AODN standard
  meta <- meta %>%
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
           age_class,
           sex,
           length,
           estimated_mass,
           actual_mass,
           state_country,
           qc_start_date,
           qc_end_date)

  ## return error if unexpected object mode or value
  tests <- with(meta, c(is.character(sattag_program),
                    is.character(device_id),
                    is.integer(ptt),
                    is.integer(body),
                    is.character(device_wmo_ref),
                    is.character(tag_type),
                    is.character(common_name),
                    is.character(species),
                    all(is.double(release_longitude),
                        ((release_longitude >= -180 & release_longitude <= 180) |
                        (release_longitude >= 0 & release_longitude <= 360) |
                        is.na(release_longitude))),
                    all(is.double(release_latitude),
                        ((release_latitude >= -90 & release_latitude <= 90) |
                         is.na(release_latitude))),
                    is.character(release_site),
                    any(inherits(release_date, "POSIXct"), is.na(release_date)),
                    any(inherits(recovery_date, "POSIXct"), is.na(recovery_date)),
                    all(unique(age_class) %in% c("adult","subadult","juvenille","juvenile","weaner")),
                    all(unique(sex) %in% c("female","male","f","m")),
                    all(is.double(length), (length > 0 | is.na(length))),
                    all(is.integer(estimated_mass), (estimated_mass > 0 | is.na(estimated_mass))),
                    all(is.double(actual_mass), (actual_mass > 0 | is.na(actual_mass))),
                    is.character(state_country),
                    any(inherits(qc_start_date, "POSIXct"), is.na(qc_start_date)),
                    any(inherits(qc_end_date, "POSIXct"), is.na(qc_end_date))))
  fails <- names(meta)[which(!tests)]
  if(length(fails) > 0) stop(paste0("non-compliant metadata records found in: ", fails))

  ## If metadata is compliant then write to .csv by sattag_program (SMRU campaign id)
  meta %>%
    mutate(age_class = ifelse(age_class == "juvenile", "juvenille", age_class)) %>%
    split(., .$sattag_program) %>%
    walk( ~ suppressMessages(write_csv(.x, file = paste0(file.path(path, "metadata"), "_",
                                               .x$sattag_program[1], suffix, ".csv"))))

  cat("\nwrite to `*.csv` completed\n")

}
