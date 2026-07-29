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
##' @importFrom dplyr select mutate bind_rows case_when
##' @importFrom future plan
##' @importFrom furrr future_map
##' @importFrom purrr pmap
##' @importFrom lubridate mdy_hms
##' @importFrom stringr str_split
##' @importFrom sf st_as_sf st_buffer st_within
##'
##' @md
##' @export

smru_pull_tables <- function(cids,
                             path2mdb,
                             tables = c("diag", "gps", "haulout", "ctd", "dive", "cruise", "summary"),
                             p2mdbtools = NULL,
                             verbose = FALSE
) {

  ## check for mdbtools install when p2mdbtools is NULL
  if(is.null(p2mdbtools)) {
    ## finds mdbtools location in a platform-independent way
    ## Sys.which returns full path with filename in the correct platform-dependent manner
    ## dirname strips filename from the path
    ## normalizePath ensures result of dirname(p2mdbtools) is platform-independent
    p2mdbtools <- Sys.which("mdb-tables")
    if(nchar(p2mdbtools) > 0 ) p2mdbtools <- normalizePath(dirname(p2mdbtools), mustWork = FALSE)
    else p2mdbtools <- NULL
  }

  ## map data strings to tables strings
  if("argos" %in% tables) {
    idx <- which(tables == "argos")
    tables[idx] <- "diag"
  }

  get.fn <- function(file, tab) {

    # Get table list
    cmd_tables <- paste0(file.path(p2mdbtools, "mdb-tables"), " -1 ", shQuote(file))
    tmp <- system(cmd_tables, intern = TRUE)
    tmp <- trimws(gsub("\r", "", tmp))

    # Find matching tables
    tab <- tmp[tmp %in% tab]

    if(length(tab) == 0) {
      if(verbose) message("No matching tables found in ", basename(file))
      return(NULL)
    }

    D <- vector("list", length(tab))
    names(D) <- tab

    # Loop through each table
    for(table_name in tab) {
      if(verbose) message("  Exporting table: ", table_name)

      # Build command WITHOUT redirection - use intern=TRUE to capture output
      cmd <- paste0(file.path(p2mdbtools, "mdb-export"), " -b strip ", shQuote(file), " ", shQuote(table_name))

      if(verbose) message("    Running: ", cmd)

      # Run command and capture output
      output <- tryCatch({
        system(cmd, intern = TRUE)
      }, error = function(e) {
        if(verbose) message("    Error: ", e$message)
        NULL
      })

      # Process the output
      if(!is.null(output) && length(output) > 0) {
        # Check if it's an error message
        if(length(output) == 1 && grepl("^Wrong|^Usage|^Error", output[1])) {
          if(verbose) message("  Command returned error: ", output[1])
          D[[table_name]] <- NULL
        } else {
          # Write to temp file for CSV parsing
          f <- tempfile(fileext = ".csv")
          f <- gsub("\\\\", "/", f)
          writeLines(output, f)

          # Read the CSV
          d <- read.csv(f, stringsAsFactors = FALSE)
          names(d) <- casefold(names(d))

          # Clean up
          unlink(f)

          if(length(tab) == 1) {
            D <- d
          } else {
            D[[table_name]] <- d
          }
          if(verbose) message("  OK: ", nrow(d), " rows")
        }
      } else {
        if(verbose) message("  Error: No output from command")
        D[[table_name]] <- NULL
      }
    }

    return(D)
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


  ## drop locations within 15km of SMRU HQ
  HQ.ll <- data.frame(lon = -2.7967, lat = 56.3398) |>
    st_as_sf(coords = c("lon","lat"), crs = 4326)

  HQ.buf <- HQ.ll |>
    st_buffer(dist = 15000)

  if(any(names(smru) %in% "diag")) {
    smru$diag <- smru$diag |>
    mutate(d_date = mdy_hms(d_date, tz = "UTC")) |>
    mutate(iq = as.integer(iq)) |>
    suppressWarnings()

    if(all(is.character(smru$diag$lon))) {
      smru$diag <- smru$diag |>
        mutate(lon = str_replace(lon, "\\,", "."),
               lat = str_replace(lat, "\\,", ".")) |>
        mutate(lon = as.numeric(lon),
               lat = as.numeric(lat))
    }

    ## replace ref's with _ with -
    if(min(smru$diag$d_date, na.rm=TRUE) < ISOdate(2006,01,01,tz="UTC")) {
      smru$diag <- smru$diag |>
        mutate(ref = str_replace_all(ref, "\\_", "-"))
    }

    ## If locations remain at SMRU HQ then remove all those within 15km of HQ
    tmp <- smru$diag
    if(all(is.character(tmp$lon))) {
      tmp <- tmp |>
        mutate(lon = str_replace(lon, "\\,", "."),
               lat = str_replace(lat, "\\,", ".")) |>
        mutate(lon = as.numeric(lon),
               lat = as.numeric(lat))
    }
    tmp <- tmp |>
      st_as_sf(coords = c("lon","lat"), crs = 4326)
    tmp.f <- unique(tmp$ref)
    tmp.lst <- split(tmp, tmp$ref)
    diag.lst <- split(smru$diag, smru$diag$ref)

    ## remove by max date within 15km of HQ, in case any but the last loc are
    ##  beyond the 15 km circle due to large Argos errors
    smru$diag <- lapply(1:length(tmp.lst), function(i) {
      win <- st_within(tmp.lst[[i]], HQ.buf) |> as.matrix() |> as.vector()
      if(sum(win) > 0) {
        last.date <- max(tmp.lst[[i]]$d_date[win == TRUE], na.rm = TRUE)
        diag.lst[[i]][tmp.lst[[i]]$d_date > last.date,]
      } else {
        diag.lst[[i]]
      }
    }) |>
      bind_rows()
  }

  if (any(names(smru) %in% "gps")) {
    if (nrow(smru$gps) > 0) {
      smru$gps <- smru$gps |>
        mutate(d_date = mdy_hms(d_date, tz = "UTC")) |>
        mutate(submitted = mdy_hms(submitted, tz = "UTC"))
      if("d_date_tag" %in% names(smru$gps)) {
        smru$gps <- smru$gps |>
          mutate(d_date_tag = case_when(
            is.na(d_date_tag) ~ NA,!is.na(d_date_tag) ~ mdy_hms(d_date_tag, tz = "UTC")
          ))
      }

      ## If locations remain at SMRU HQ then remove all those within 15km of HQ
      tmp <- smru$gps |>
        filter(!is.na(lon), !is.na(lat)) |>
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      tmp.f <- unique(tmp$ref)
      tmp.lst <- split(tmp, tmp$ref)
      gps.lst <- split(smru$gps, smru$gps$ref)

      ## remove by max date within 15km of HQ, in case any but the last loc are
      ##  beyond the 15 km circle
      smru$gps <- lapply(1:length(tmp.lst), function(i) {
        win <- st_within(tmp.lst[[i]], HQ.buf) |> as.matrix() |> as.vector()
        if (sum(win) > 0) {
          last.date <- max(tmp.lst[[i]]$d_date[win == TRUE], na.rm = TRUE)
          gps.lst[[i]][tmp.lst[[i]]$d_date > last.date, ]
        } else {
          gps.lst[[i]]
        }
      }) |>
        bind_rows()

      ## remove gps table if all locations within HQ.buf
      if(nrow(smru$gps) == 0) smru[["gps"]] <- NULL
    }
  }

  first.dates <- smru$diag |> group_by(ref) |> summarise(md = min(d_date, na.rm = TRUE))


  if(any(names(smru) %in% "haulout")) {
    smru$haulout <- smru$haulout |>
      mutate(s_date = mdy_hms(s_date, tz = "UTC")) |>
      mutate(e_date = mdy_hms(e_date, tz = "UTC"))  |>
      left_join(first.dates, by = "ref") |>
      mutate(md = ifelse(is.na(md), s_date[1], md)) |>
      filter(s_date >= md & e_date >= md) |>
      select(-md)


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
      mutate(end_date = mdy_hms(end_date, tz = "UTC")) |>
      left_join(first.dates, by = "ref") |>
      mutate(md = ifelse(is.na(md), end_date[1], md)) |>
      filter(end_date >= md) |>
      select(-md)

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
      mutate(de_date = mdy_hms(de_date, tz = "UTC"))|>
      left_join(first.dates, by = "ref") |>
      mutate(md = ifelse(is.na(md), de_date[1], md)) |>
      filter(de_date >= md) |>
      select(-md)

    if("ds_date" %in% smru$dive) {
      smru$dive <- smru$dive |>
        mutate(ds_date = mdy_hms(ds_date, tz = "UTC")) |>
        left_join(first.dates, by = "ref") |>
        mutate(md = ifelse(is.na(md), ds_date[1], md)) |>
        filter(ds_date >= md) |>
        select(-md)
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
        mutate(e_date = mdy_hms(e_date, tz = "UTC")) |>
        left_join(first.dates, by = "ref") |>
        mutate(md = ifelse(is.na(md), s_date[1], md)) |>
        filter(s_date >= md & e_date >= md) |>
        select(-md)

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
        mutate(e_date = mdy_hms(e_date, tz = "UTC")) |>
        left_join(first.dates, by = "ref") |>
        mutate(md = ifelse(is.na(md), s_date[1], md)) |>
        filter(s_date >= md & e_date >= md) |>
        select(-md)

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
