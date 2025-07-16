##' @title Pull tables from local tag datafiles
##'
##' @description reads SMRU or WC tag datafiles & combines in a unified list
##'
##' @param path2data path to local datafile(s)
##' @param cid SMRU campaign id. Ignored if `tag_mfr = "wc"`.
##' @param tag_mfr either "smru" or "wc"
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom lubridate ymd_hms
##' @importFrom vctrs list_drop_empty
##'
##' @export

pull_local_data <- function(path2data,
                            cid = NULL,
                             tag_mfr) {

  if(tag_mfr == "smru") {
    fs <- list.files(file.path(path2data, cid),
                     recursive = TRUE,
                     full.names = TRUE)

    diag.fs <- fs[grepl("\\_diag.txt", fs)]

    diag <- lapply(1:length(diag.fs), function(i) {
        read.delim(diag.fs[i], sep = "\t")
      }) |>
        bind_rows()
    names(diag) <- tolower(names(diag))


    gps.fs <- fs[grepl("\\_gps.txt", fs)]
    if (length(gps.fs) > 0) {
      gps <- lapply(1:length(gps.fs), function(i) {
        read.delim(gps.fs[i], sep = "\t")
      }) |>
        bind_rows()
      names(gps) <- tolower(names(gps))
    } else {
      gps <- NULL
    }


    ctd.fs <- fs[grepl("\\_ctd.txt", fs)]
    if (length(ctd.fs) > 0) {
      ctd <- lapply(1:length(ctd.fs), function(i) {
        read.delim(ctd.fs[i], sep = "\t")
      }) |>
        bind_rows()
      names(ctd) <- tolower(names(ctd))
    } else {
      ctd <- NULL
    }


    dive.fs <- fs[grepl("\\_dive.txt", fs)]
    if (length(dive.fs) > 0) {
      dive <- lapply(1:length(dive.fs), function(i) {
        read.delim(dive.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
      names(dive) <- tolower(names(dive))
    } else {
      dive <- NULL
    }


    cruise.fs <- fs[grepl("\\_cruise.txt", fs)]
    if (length(cruise.fs) > 0) {
      cruise <- lapply(1:length(cruise.fs), function(i) {
        read.delim(cruise.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
      names(cruise) <- tolower(names(cruise))
    } else {
      cruise <- NULL
    }


    haulout.fs <- fs[grepl("\\_haulout.txt", fs)]
    if (length(haulout.fs) > 0) {
      haulout <- lapply(1:length(haulout.fs), function(i) {
        read.delim(haulout.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
      names(haulout) <- tolower(names(haulout))
    } else {
      haulout <- NULL
    }


    summary.fs <- fs[grepl("\\_summary.txt", fs)]
    if (length(summary.fs) > 0) {
      summary <- lapply(1:length(summary.fs), function(i) {
        read.delim(summary.fs[i], sep = "\t")  #, col_types = read.delim(cts[i], header = TRUE, sep = "\t")
      }) |>
        bind_rows()
      names(summary) <- tolower(names(summary))
    } else {
      summary <- NULL
    }


    smru <- list(diag = diag,
                 gps = gps,
                 ctd = ctd,
                 dive = dive,
                 cruise = cruise,
                 haulout = haulout,
                 summary = summary) |>
      list_drop_empty()

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
