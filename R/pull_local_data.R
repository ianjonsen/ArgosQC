##' @title Pull tables from local tag datafiles
##'
##' @description reads SMRU or WC tag datafiles & combines in a unified list
##'
##' @param path2data path to local datafile(s)
##' @param tag_mfr either "smru" or "wc"
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom lubridate ymd_hms
##'
##' @export

pull_local_data <- function(path2data,
                             tag_mfr) {

  if(tag_mfr == "smru") {
    cids <- list.dirs(path2data, full.names = FALSE, recursive = FALSE)

    fs <- lapply(1:length(cids), function(i) list.files(file.path(path2data, cids[i]), recursive = TRUE, full.names = TRUE))

    diag <- lapply(fs, function(x) {
      diag.fs <- x[grepl("\\_diag.txt", x)]
      col.fs <- x[grepl("\\_diag_col_types.txt", x)]
      lapply(1:length(diag.fs), function(i) {
        # cols <- read.delim(col.fs[i], sep = "\t") |>
        #   mutate(character = dplyr::case_when(character == "numeric" ~ "n",
        #                                character == "POSIXct" ~ "T",
        #                                character == "integer" ~ "i",
        #                                character == "character" ~ "c"))
        read.delim(diag.fs[i], sep = "\t") #, col_types = paste(cols$character, collapse = ""))
      }) |>
        bind_rows()
    }) |>
      bind_rows()
    names(diag) <- tolower(names(diag))


    gps <- lapply(fs, function(x) {
      gps.fs <- x[grepl("\\_gps.txt", x)]
      if(length(gps.fs) > 0) {
      lapply(1:length(gps.fs), function(i) {
        read.delim(gps.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
      } else {
        NULL
      }
    }) |>
      bind_rows()
    names(gps) <- tolower(names(gps))

    ctd <- lapply(fs, function(x) {
      ctd.fs <- x[grepl("\\_ctd.txt", x)]
      if (length(ctd.fs) > 0) {
        lapply(1:length(ctd.fs), function(i) {
          read.delim(ctd.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
        }) |>
          bind_rows()
      } else {
        NULL
      }
    }) |>
      bind_rows()
    names(ctd) <- tolower(names(ctd))

    dive <- lapply(fs, function(x) {
      dive.fs <- x[grepl("\\_dive.txt", x)]
      if (length(dive.fs) > 0) {
        lapply(1:length(dive.fs), function(i) {
          read.delim(dive.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
        }) |>
          bind_rows()
      } else {
        NULL
      }
    }) |>
      bind_rows()
    names(dive) <- tolower(names(dive))

    cruise <- lapply(fs, function(x) {
      cruise.fs <- x[grepl("\\_cruise.txt", x)]
      lapply(1:length(cruise.fs), function(i) {
        read.delim(cruise.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
    }) |>
      bind_rows()
    names(cruise) <- tolower(names(cruise))

    haulout <- lapply(fs, function(x) {
      haulout.fs <- x[grepl("\\_haulout.txt", x)]
      lapply(1:length(haulout.fs), function(i) {
        read.delim(haulout.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
    }) |>
      bind_rows()
    names(haulout) <- tolower(names(haulout))

    summary <- lapply(fs, function(x) {
      summary.fs <- x[grepl("\\_summary.txt", x)]
      lapply(1:length(summary.fs), function(i) {
        read.delim(summary.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
    }) |>
      bind_rows()
    names(summary) <- tolower(names(summary))

    smru <- list(diag = diag,
                 gps = gps,
                 ctd = ctd,
                 dive = dive,
                 cruise = cruise,
                 haulout = haulout,
                 summary = summary)

  } else if(tag_mfr == "wc") {
    stop("tag_mfr = 'wc' is deprecated. Use 'pull_wd_data()' instead.")
  }

  idx <- as.numeric(which(sapply(smru, nrow) == 0))
  if(length(idx) > 0) smru <- smru[-idx]

  if(any(names(smru) %in% "diag")) {
    smru$diag <- smru$diag |>
      mutate(d_date = lubridate::ymd_hms(d_date, tz = "UTC"))
  }
  if(any(names(smru) %in% "gps")) {
    smru$gps <- smru$gps |>
      mutate(d_date = ymd_hms(d_date, tz = "UTC"))
  }

  if(any(names(smru) %in% "haulout")) {
    smru$haulout <- smru$haulout |>
      mutate(s_date = ymd_hms(s_date, tz = "UTC")) |>
      mutate(e_date = ymd_hms(e_date, tz = "UTC"))
    if("s_date_tag" %in% names(smru$haulout)) {
      smru$haulout <- smru$haulout |>
        mutate(s_date_tag = ifelse(!is.na(s_date_tag),
                                   ymd_hms(s_date_tag, tz = "UTC"),
                                   s_date_tag))
    }
    if("e_date_tag" %in% names(smru$haulout)) {
      smru$haulout <- smru$haulout |>
        mutate(e_date_tag = ifelse(!is.na(e_date_tag),
                                   ymd_hms(e_date_tag, tz = "UTC"),
                                   e_date_tag))
    }
  }

  if(any(names(smru) %in% "ctd")) {
    smru$ctd <- smru$ctd |>
      mutate(end_date = ymd_hms(end_date, tz = "UTC"))

    if("created" %in% names(smru$ctd)) {
      smru$ctd <- smru$ctd |>
        mutate(created = ymd_hms(created, tz = "UTC"))
    }
    if("modified" %in% names(smru$ctd)) {
      smru$ctd <- smru$ctd |>
        mutate(modified = ymd_hms(modified, tz = "UTC"))
    }
  }

  if(any(names(smru) %in% "dive")) {
    smru$dive <- smru$dive |>
      mutate(de_date = ymd_hms(de_date, tz = "UTC"))

    if("ds_date" %in% smru$dive) {
      smru$dive <- smru$dive |>
        mutate(ds_date = ymd_hms(ds_date, tz = "UTC"))
    }
    if("de_date_tag" %in% smru$dive) {
      smru$dive <- smru$dive |>
        mutate(de_date_tag = ifelse(!is.na(de_date_tag),
                                    ymd_hms(de_date_tag, tz = "UTC"),
                                    de_date_tag))
    }
  }

  if(any(names(smru) %in% "summary")) {
    smru$summary <- smru$summary |>
      mutate(s_date = ymd_hms(s_date, tz = "UTC")) |>
      mutate(e_date = ymd_hms(e_date, tz = "UTC"))

    if("s_date_tag" %in% smru$summary) {
      smru$summary <- smru$summary |>
        mutate(s_date_tag = ifelse(!is.na(s_date_tag),
                                   ymd_hms(s_date_tag, tz = "UTC"),
                                   s_date_tag))
    }
    if("e_date_tag" %in% smru$summary) {
      smru$summary <- smru$summary |>
        mutate(e_date_tag = ifelse(!is.na(e_date_tag),
                                   ymd_hms(e_date_tag, tz = "UTC"),
                                   e_date_tag))
    }
    smru$summary <- smru$summary |>
      mutate(surf_tm = ifelse(surf_tm < 0, 0, surf_tm),
             dive_tm = ifelse(dive_tm < 0, 0, dive_tm),
             haul_tm = ifelse(haul_tm < 0, 0, haul_tm))
    ## values are proportion of total time & therefore must be >= 0
  }

  return(smru)

}
