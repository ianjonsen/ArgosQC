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
##' @examples
##'
##' @importFrom dplyr select mutate bind_rows
##' @importFrom future plan
##' @importFrom furrr future_map
##' @importFrom purrr pmap
##' @importFrom lubridate mdy_hms
##' @importFrom stringr str_split
##'
##' @export

pull_smru_tables <- function(cids,
                             path2mdb,
                             tables = c("diag", "haulout", "ctd", "dive", "summary"),
                             p2mdbtools = NULL,
                             verbose = FALSE
) {

  ## path for MacBook Pro M1 Pro
  #p2mdbtools <- "/opt/homebrew/Cellar/mdbtools/1.0.0/bin/"
  if(is.null(p2mdbtools)) p2mdbtools <- ""

  ## map data strings to tables strings
  if("argos" %in% tables) {
    idx <- which(tables == "argos")
    tables[idx] <- "diag"
  }

  get.fn <- function(file, tab) {
    f <- tempfile()
    D <- vector("list", length(tab))
    names(D) <- tab

    for(i in tab) {
      if(i == "gps") {
        tmp <- system(paste0(p2mdbtools, "mdb-tables ", file), intern = TRUE)
        if(length(tmp) == 1) {
          tmp <- str_split(tmp, "\\ ", simplify = TRUE) |>
            as.vector()
        }
        if(!"gps" %in% tmp) {
          next
        }
      }

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

    plan("multisession")

    smru_t <- cids |>
      future_map( ~ try(get.fn(paste0(file.path(path2mdb, .x), ".mdb"),
                                       tab = tables),
                        silent = TRUE), .progress = verbose
      )

  smru <- smru_t |>
    pmap(dplyr::bind_rows)

  ## drop empty table(s)
  idx <- as.numeric(which(sapply(smru, nrow) == 0))
  if(length(idx) > 0) smru <- smru[-idx]

  if(any(names(smru) %in% "diag")) {
    smru$diag <- smru$diag |>
    mutate(d_date = mdy_hms(d_date, tz = "UTC"))
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
  }

  if(any(names(smru) %in% "summary")) {
    smru$summary <- smru$summary |>
      mutate(s_date = mdy_hms(s_date, tz = "UTC")) |>
      mutate(e_date = mdy_hms(e_date, tz = "UTC"))

    if("s_date_tag" %in% smru$summary) {
      smru$summary <- smru$summary |>
        mutate(s_date_tag = ifelse(!is.na(s_date_tag),
                                   mdy_hms(s_date_tag, tz = "UTC"),
                                   s_date_tag))
    }
    if("e_date_tag" %in% smru$summary) {
      smru$summary <- smru$summary |>
        mutate(e_date_tag = ifelse(!is.na(e_date_tag),
                                   mdy_hms(e_date_tag, tz = "UTC"),
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
