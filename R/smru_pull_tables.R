##' @title Pull tables from SMRU .mdb files
##'
##' @description extracts specified tables from SMRU .mdb files, using Hmisc::mdb.get
##'
##' @param cids SMRU campaign ids
##' @param path2mdb path to SMRU .mdb file(s)
##' @param tables specify which tables to extract, default is to extract all tables
##' @param p2mdbtools path to mdbtools binaries. Specifying the path can avoid
##' an error when calling from within RStudio, eg. on MacBook Pro M1 Pro with
##' homebrew-installed mdbtools @ /opt/homebrew/Cellar/mdbtools/1.0.0/bin/
##' @param verbose turn on/off progress indicator
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom future plan
##' @importFrom furrr future_map
##' @importFrom purrr pmap
##' @importFrom lubridate mdy_hms
##' @importFrom stringr str_split
##'
##' @md
##' @export

smru_pull_tables <- function(cids,
                             path2mdb,
                             tables = c("diag", "gps", "haulout", "ctd", "dive", "cruise", "summary"),
                             p2mdbtools = NULL,
                             verbose = FALSE
) {

  ## path for MacBook Pro M1 Pro
  #p2mdbtools <- "/opt/homebrew/Cellar/mdbtools/1.0.1/bin/"
  if(is.null(p2mdbtools)) p2mdbtools <- ""
  ## map data strings to tables strings
  if("argos" %in% tables) {
    idx <- which(tables == "argos")
    tables[idx] <- "diag"
  }

  get.fn <- function(file, tab) {

    f <- tempfile()
    ## read tables present in .mdb file
    tmp <- system(paste0(p2mdbtools, "mdb-tables ", file), intern = TRUE) |>
      str_split("\\ ", simplify = TRUE) |>
      as.vector()

    tab <- tmp[tmp %in% tab]
    D <- vector("list", length(tab))
    names(D) <- tab

    ## read data from tables
    for(i in tab) {
      system(paste0(p2mdbtools, "mdb-export -b strip ", file, " ", shQuote(i), " > ", f))
      d <- read.csv(f)
      names(d) <- casefold(names(d))
      if(length(tab) == 1) {
        D <- d
      }
      else {
        D[[i]] <- d
      }
    }
    D
  }

  if(length(cids) > 1) {
    plan("multisession")

    smru_t <- cids |>
      future_map( ~ try(get.fn(paste0(file.path(path2mdb, .x), ".mdb"),
                               tab = tables),
                        silent = TRUE), .progress = verbose
      )

    smru <- smru_t |>
      pmap(dplyr::bind_rows)

  } else {
    smru <- try(get.fn(paste0(file.path(path2mdb, cids), ".mdb"),
                       tab = tables))

  }

  ## drop empty table(s)
  # idx <- as.numeric(which(sapply(smru, nrow) == 0))
  # if(length(idx) > 0) smru <- smru[-idx]

  if(any(names(smru) %in% "diag")) {
    smru$diag <- smru$diag |>
    mutate(d_date = mdy_hms(d_date, tz = "UTC")) |>
    mutate(iq = as.integer(iq)) |>
    suppressWarnings()

    ## replace ref's with _ with -
    if(min(smru$diag$d_date, na.rm=TRUE) < ISOdate(2006,01,01,tz="UTC")) {
      smru$diag <- smru$diag |>
        mutate(ref = str_replace_all(ref, "\\_", "-"))
    }
  }

  if(any(names(smru) %in% "gps")) {
    smru$gps <- smru$gps |>
      mutate(d_date = mdy_hms(d_date, tz = "UTC"))
  }

  if(any(names(smru) %in% "haulout")) {
    smru$haulout <- smru$haulout |>
      mutate(s_date = mdy_hms(s_date, tz = "UTC")) |>
      mutate(e_date = mdy_hms(e_date, tz = "UTC"))
    if("s_date_tag" %in% names(smru$haulout)) {
      smru$haulout <- smru$haulout |>
        mutate(s_date_tag = ifelse(!is.na(s_date_tag),
                                   mdy_hms(s_date_tag, tz = "UTC"),
                                   s_date_tag))
    }
    if("e_date_tag" %in% names(smru$haulout)) {
      smru$haulout <- smru$haulout |>
        mutate(e_date_tag = ifelse(!is.na(e_date_tag),
                                 mdy_hms(e_date_tag, tz = "UTC"),
                                 e_date_tag))
    }
    ## replace ref's with _ with -
    if(min(smru$haulout$s_date, na.rm=TRUE) < ISOdate(2006,01,01,tz="UTC")) {
      smru$haulout <- smru$haulout |>
        mutate(ref = str_replace_all(ref, "\\_", "-"))
    }
  }

  if(any(names(smru) %in% "ctd")) {
    smru$ctd <- smru$ctd |>
      mutate(end_date = mdy_hms(end_date, tz = "UTC"))

    if("created" %in% names(smru$ctd)) {
      smru$ctd <- smru$ctd |>
        mutate(created = mdy_hms(created, tz = "UTC"))
    }
    if("modified" %in% names(smru$ctd)) {
      smru$ctd <- smru$ctd |>
        mutate(modified = mdy_hms(modified, tz = "UTC"))
    }
    ## replace ref's with _ with -
    if(min(smru$ctd$end_date, na.rm=TRUE) < ISOdate(2006,01,01,tz="UTC")) {
      smru$ctd <- smru$ctd |>
        mutate(ref = str_replace_all(ref, "\\_", "-"))
    }
  }

  if(any(names(smru) %in% "dive")) {
    smru$dive <- smru$dive |>
      mutate(de_date = mdy_hms(de_date, tz = "UTC"))

    if("ds_date" %in% smru$dive) {
      smru$dive <- smru$dive |>
        mutate(ds_date = mdy_hms(ds_date, tz = "UTC"))
    }
    if("de_date_tag" %in% smru$dive) {
      smru$dive <- smru$dive |>
        mutate(de_date_tag = ifelse(!is.na(de_date_tag),
                                    mdy_hms(de_date_tag, tz = "UTC"),
                                    de_date_tag))
    }
    ## replace ref's with _ with -
    if(min(smru$dive$de_date, na.rm=TRUE) < ISOdate(2006,01,01,tz="UTC")) {
      smru$dive <- smru$dive |>
        mutate(ref = str_replace_all(ref, "\\_", "-"))
    }
  }

    if (any(names(smru) %in% "summary")) {
      smru$summary <- smru$summary |>
        mutate(s_date = mdy_hms(s_date, tz = "UTC")) |>
        mutate(e_date = mdy_hms(e_date, tz = "UTC"))

      if ("s_date_tag" %in% smru$summary) {
        smru$summary <- smru$summary |>
          mutate(s_date_tag = ifelse(
            !is.na(s_date_tag),
            mdy_hms(s_date_tag, tz = "UTC"),
            s_date_tag
          ))
      }
      if ("e_date_tag" %in% smru$summary) {
        smru$summary <- smru$summary |>
          mutate(e_date_tag = ifelse(
            !is.na(e_date_tag),
            mdy_hms(e_date_tag, tz = "UTC"),
            e_date_tag
          ))
      }
      ## replace ref's with _ with -
      if (min(smru$summary$s_date, na.rm=TRUE) < ISOdate(2006, 01, 01, tz = "UTC")) {
        smru$summary <- smru$summary |>
          mutate(ref = str_replace_all(ref, "\\_", "-"))
      }
      smru$summary <- smru$summary |>
        mutate(
          surf_tm = ifelse(surf_tm < 0, 0, surf_tm),
          dive_tm = ifelse(dive_tm < 0, 0, dive_tm),
          haul_tm = ifelse(haul_tm < 0, 0, haul_tm)
        )
      ## values are proportion of total time & therefore must be >= 0
    }


    if (any(names(smru) %in% "cruise")) {
      smru$cruise <- smru$cruise |>
        mutate(s_date = mdy_hms(s_date, tz = "UTC")) |>
        mutate(e_date = mdy_hms(e_date, tz = "UTC"))

      if ("s_date_tag" %in% smru$cruise) {
        smru$cruise <- smru$cruise |>
          mutate(s_date_tag = ifelse(
            !is.na(s_date_tag),
            mdy_hms(s_date_tag, tz = "UTC"),
            s_date_tag
          ))
      }
      if ("e_date_tag" %in% smru$cruise) {
        smru$cruise <- smru$cruise |>
          mutate(e_date_tag = ifelse(
            !is.na(e_date_tag),
            mdy_hms(e_date_tag, tz = "UTC"),
            e_date_tag
          ))
      }
      ## replace ref's with _ with -
      if (min(smru$cruise$s_date, na.rm=TRUE) < ISOdate(2006, 01, 01, tz = "UTC")) {
        smru$cruise <- smru$cruise |>
          mutate(ref = str_replace_all(ref, "\\_", "-"))
      }
    }

  return(smru)

}
