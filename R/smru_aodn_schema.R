## Standard structure of the SMRU tables written for AODN (internal).
##  Column names and order are taken from the latest QC'd IMOS output
##  (ct190, ArgosQC 0.9-18), without the QC variables that ArgosQC appends.
##  Older SMRU tag files, with fewer or different variables, are conformed to
##  this structure by smru_conform_table() before the .csv files are written.

.smru_aodn_cols <- list(
  diag = c(
    "ref", "ptt", "d_date", "lq", "lat", "lon", "alt_lat", "alt_lon", "n_mess",
    "n_mess_120", "best_level", "pass_dur", "freq", "v_mask", "alt",
    "est_speed", "km_from_home", "iq", "nops", "deleted", "actual_ptt",
    "error_radius", "semi_major_axis", "semi_minor_axis",
    "ellipse_orientation", "hdop", "satellite", "diag_id"
  ),
  ctd = c(
    "ref", "ptt", "end_date", "max_dbar", "num", "n_temp", "n_cond", "n_sal",
    "temp_dbar", "temp_vals", "cond_dbar", "cond_vals", "sal_dbar", "sal_vals",
    "n_fluoro", "fluoro_dbar", "fluoro_vals", "n_oxy", "oxy_dbar", "oxy_vals",
    "qc_profile", "qc_temp", "qc_sal", "sal_corrected_vals", "created",
    "modified", "n_photo", "photo_dbar", "photo_vals", "lat", "lon"
  ),
  dive = c(
    "ref", "ptt", "cnt", "de_date", "surf_dur", "dive_dur", "max_dep", "d1",
    "d2", "d3", "d4", "v1", "v2", "v3", "v4", "v5", "travel_r", "homedist",
    "bottom", "t1", "t2", "t3", "t4", "d_speed", "n_depths", "n_speeds",
    "depth_str", "speed_str", "propn_str", "percent_area", "residual",
    "grp_number", "d5", "t5", "degc_str", "illum_str", "pca_desc", "pca_btm",
    "pca_asc", "pca_max_desc", "pca_max_btm", "pca_max_asc", "pca_mean_desc",
    "pca_mean_btm", "pca_mean_asc", "swim_eff_desc", "swim_eff_btm",
    "swim_eff_asc", "swim_eff_whole", "secs_desc", "secs_btm", "secs_asc",
    "pitch_desc", "pitch_btm", "pitch_asc", "pitch_str", "tagging_id",
    "de_date_tag", "qc", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13",
    "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23",
    "d24", "d25", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14",
    "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24",
    "t25", "ds_date", "start_lat", "start_lon", "lat", "lon"
  ),
  haulout = c(
    "ref", "ptt", "s_date", "e_date", "haulout_number", "cnt", "phosi_secs",
    "wet_n", "wet_min", "wet_max", "wet_mean", "wet_sd", "tagging_id",
    "s_date_tag", "e_date_tag", "end_number", "lat", "lon"
  ),
  summary = c(
    "ref", "ptt", "cnt", "s_date", "e_date", "div_dist", "surf_tm", "dive_tm",
    "haul_tm", "n_cycles", "av_depth", "max_depth", "cruise_tm", "avg_sst",
    "avg_speed", "sd_depth", "av_dur", "sd_dur", "max_dur", "dp_n_cycles",
    "dp_av_depth", "dp_max_depth", "dp_avg_speed", "dp_sd_depth", "dp_av_dur",
    "dp_sd_dur", "dp_max_dur", "dp_dive_tm", "av_surf_dur", "sd_surf_dur",
    "max_surf_dur", "dp_av_surf_dur", "dp_sd_surf_dur", "dp_max_surf_dur",
    "pca", "swim_eff_desc", "swim_eff_asc", "swim_eff_whole", "secs_desc",
    "secs_asc", "pitch_desc", "pitch_asc", "av_haulout_dur", "sd_haulout_dur",
    "max_haulout_dur", "av_phosi_dur", "sd_phosi_dur", "max_phosi_dur",
    "tagging_id", "s_date_tag", "e_date_tag"
  )
)

## QC variables appended by ArgosQC, in their output order
.smru_aodn_qc_cols <- c("ssm_lon", "ssm_lat", "ssm_x", "ssm_y", "ssm_x_se", "ssm_y_se", "cid")

## classes for columns added as NA, where the IMOS field tests in
##  smru_write_csv() require a class; other added columns are logical NA
.smru_aodn_int_cols <- c("ptt", "lq", "n_mess", "n_mess_120", "best_level", "pass_dur",
                         "v_mask", "nops", "actual_ptt", "error_radius",
                         "semi_major_axis", "semi_minor_axis", "ellipse_orientation",
                         "hdop", "diag_id", "num", "n_temp", "n_cond", "n_sal",
                         "n_fluoro", "n_oxy", "n_photo", "qc_profile", "cnt",
                         "haulout_number", "end_number", "phosi_secs", "wet_n")
.smru_aodn_dbl_cols <- c("lat", "lon", "alt_lat", "alt_lon", "freq", "est_speed",
                         "km_from_home", "max_dbar", "wet_min", "wet_max",
                         "wet_mean", "wet_sd")
.smru_aodn_chr_cols <- c("ref", "satellite", "temp_dbar", "temp_vals", "cond_dbar",
                         "cond_vals", "sal_dbar", "sal_vals", "fluoro_dbar",
                         "fluoro_vals", "oxy_dbar", "oxy_vals", "photo_dbar",
                         "photo_vals", "qc_temp", "qc_sal", "sal_corrected_vals")
.smru_aodn_time_cols <- c("created", "modified")

smru_typed_na <- function(v, n) {
  if (v %in% .smru_aodn_int_cols) return(rep(NA_integer_, n))
  if (v %in% .smru_aodn_dbl_cols) return(rep(NA_real_, n))
  if (v %in% .smru_aodn_chr_cols) return(rep(NA_character_, n))
  if (v %in% .smru_aodn_time_cols) return(as.POSIXct(rep(NA_real_, n), tz = "UTC"))
  rep(NA, n)
}

## conform one SMRU output table to the standard AODN structure: add missing
##  columns (all NA), drop columns that are not in the standard structure, and
##  put the columns in the standard order, followed by the appended QC variables
smru_conform_table <- function(df, table, quiet = FALSE) {
  std <- .smru_aodn_cols[[table]]
  if (is.null(std) || is.null(df)) return(df)
  miss <- setdiff(std, names(df))
  extra <- setdiff(names(df), c(std, .smru_aodn_qc_cols))
  for (v in miss) df[[v]] <- smru_typed_na(v, nrow(df))
  if (!quiet && (length(miss) > 0 || length(extra) > 0)) {
    message("Conforming the ", table, " table to the AODN structure: ",
            length(miss), " column(s) added as NA",
            if (length(miss) > 0) paste0(" (", paste(miss, collapse = ", "), ")"),
            "; ", length(extra), " column(s) dropped",
            if (length(extra) > 0) paste0(" (", paste(extra, collapse = ", "), ")"))
  }
  dplyr::select(df, dplyr::all_of(c(std, intersect(.smru_aodn_qc_cols, names(df)))))
}

## move new columns to their standard place: after the last column present that
##  precedes them in the standard structure (used when tagging_id and the
##  *_date_tag columns are added to older SMRU tables)
smru_place_after <- function(df, cols, table) {
  std <- .smru_aodn_cols[[table]]
  before <- std[seq_len(match(cols[1], std) - 1)]
  anchor <- utils::tail(intersect(before, names(df)), 1)
  if (length(anchor) == 0) {
    dplyr::relocate(df, dplyr::all_of(cols))
  } else {
    dplyr::relocate(df, dplyr::all_of(cols), .after = dplyr::all_of(anchor))
  }
}
