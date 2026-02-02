##' @title Pull data from Wildlife Computers tag data files
##'
##' @description extracts data from `X-Locations.csv` (Argos), `X-FastGPS.csv`,
##' `ECDHistos.csv`, `Histos.csv`, `MixLayer.csv`, `PDTs.csv`, `DSA.csv`,
##' `MinMaxDepth.csv`, `HaulOut.csv`, and `SST.csv` files.
##' Extracted data are aggregated across individual tags and returned in a
##' single named list with the following data.frames:
##' * Argos
##' * FastGPS
##' * ECDHistos_SCOUT_TEMP_361A
##' * ECDHistos_SCOUT_DSA
##' * Histos
##' * Mixlayer
##' * PDTs
##' * DSA
##' * MinMaxDepth
##' * Haulout
##' * SST
##' WC tag data files downloaded via `download_data` will be stored in separate,
##' tag-specific subdirectories. `path2data` should point to the outer directory.
##'
##' @param path2data path to all WC tag data files.
##' @param subset.ids a single column .CSV file of WC UUID's to be included in
##' the QC, with uuid as the variable name.
##'
##' @importFrom dplyr select mutate bind_rows everything case_when as_tibble
##' @importFrom purrr map
##' @importFrom lubridate dmy_hms
##' @importFrom readr read_csv cols
##' @importFrom stringr str_split str_detect
##' @importFrom vctrs list_drop_empty
##' @importFrom testthat is_testing
##'
##' @md
##' @export

wc_pull_data <- function(path2data,
                         subset.ids = NULL) {

  ## All known WC data files to be QC'd + regex expressions to obtain latest
  ##    WC-post-processed Locations and FastGPS files
    datafiles <- c(
      "[0-9]-Locations",
      "Locations",
      "[0-9]-FastGPS",
      "FastGPS",
      "ECDHistos_SCOUT_TEMP_361A",
      "ECDHistos_SCOUT_DSA",
      "Histos",
      "MixLayer",
      "PDTs",
      "DSA",
      "MinMaxDepth",
      "Haulout",
      "SST"
    )

    if(!is.null(subset.ids)) {
      ids <- read_csv(subset.ids, col_types = "c") |>
        suppressMessages()
      if(names(ids) != "uuid" | length(names(ids)) != 1) stop("Variable name for the WC ID's to QC'd must be 'uuid'")
    } else {
      ids <- NULL
    }


  ## drop tag dirs with *-Locations.csv files < 20 Argos locations &/or without *-Locations.csv
  ##    file(s) b/c these latter dirs likely contain SMRU data
  dirs <- list.dirs(path2data)[-1]

  idx <- sapply(1:length(dirs), function(i) {
    fs <- list.files(dirs[i])
    if (length(fs[grep("[0-9]-Locations.csv", fs)]) > 0) {
      nrow(read_csv(file.path(dirs[i], fs[grep("[0-9]-Locations.csv", fs)][1]), col_types = cols()) |>
             suppressWarnings())
    } else {
      0
    }
  })

  if (inherits(idx, "list")) {
    idx <- unlist(idx)
  }
  dirs.uuid <- str_split(dirs, "\\/", simplify = TRUE)[,2]
  dirs.uuid <- str_split(dirs.uuid, "\\_", simplify = TRUE)[,1]

  ## throw a message if any empty dataset directories found
  if(length(which(idx == 0)) > 0) {
    dir.idx.0 <- dirs.uuid[which(idx == 0)]
    message(paste0("wc_pull_data() removed from the QC workflow ",
                    length(dir.idx.0),
                    " tag dataset(s) with no Locations data: ",
                    dir.idx.0))
  }

  ## throw a message if any ids$uiid not in dataset directory list
  if(any(!ids$uuid %in% dirs.uuid)) {

    message(paste0("wc_pull_data() found a uuid(s): ",
                   ids$uuid[!ids$uuid %in% dirs.uuid],
                   " in the supplied conf$harvest$tag.lst\n ",
                   "that is not found in the dataset directories. ",
                   "Consider updating the tag.list file"))
    ids <- subset(ids, uuid %in% dirs.uuid)
  }

  ## reduce to only the subset in subset.ids (if !is.null)
  if(!is.null(ids)) {
    dd <- dirs[idx > 0]
    sub <- map(ids$uuid, str_detect, string = dd)
    sub <- unlist(sapply(sub, which))
    if(length(sub) > 0) ndirs <- dd[sub]
    else ndirs <- dd

  } else {
    ## ignore any tag datasets with <= 20 observed locations
    idx <- idx > 20
    ndirs <- dirs[idx]
  }

  ## get all required data filenames
  fs <- lapply(1:length(ndirs), function(i) {
    tmp <- grep(paste(datafiles, collapse="|"),
         list.files(ndirs[i]),
         value = TRUE)
    ds <- list.dirs(ndirs[i])
    xx <- c("FastGPS","Locations")
    out <- tmp[c(grep(xx[1], tmp)[1],
          grep(xx[2], tmp)[1],
          grep(paste(xx, collapse="|"), tmp, invert = TRUE))]
    file.path(ds, out[!is.na(out)])
  })

  ## drop the 2 regex expressions from the filenames
  datafiles <- datafiles[-grep("\\[", datafiles)]

  wc <- lapply(fs, function(x) {
    ## get DeploymentID for each x
    xx <- unlist(str_split(x, "\\/"))
    xx <- xx[grep("\\_", xx)] |> unique()
    if(length(xx) > 1) {
      xx <- xx[2]
    }

    xx <- str_split(xx, "\\_", simplify = TRUE)[,1]

    out <- lapply(1:length(x), function(i) {
      xxx <- read_csv(x[[i]],
                     show_col_types = FALSE,
                     name_repair = "unique_quiet") |>
        suppressMessages()

      ## detect & account for any leading blank rows in .csv files
      nn <- rowSums(is.na(xxx)) == ncol(xxx)
      ns <- sum(nn)
      if(ns > 0 & 1 %in% which(nn)) {
        xxx <- read_csv(x[[i]],
                       show_col_types = FALSE,
                       name_repair = "unique_quiet",
                       skip = ns+1) |>
          suppressMessages()
      }
      # print(nrow(xx))
      # if(nrow(xx) > 0) {
      #   ## parse filename strings to isolate UUID & add to datafile as DeploymentID
      #   idx <- grep("\\_", str_split(x[[i]], "\\/", simplify = TRUE))
      #   foo <- unique(str_split(x[[i]], "\\/", simplify = TRUE)[, idx])
      #
      #   if(length(foo > 1)) {
      #     foo <- foo[2]
      #   }
      #
      #   ## removes _suffix (Tag serial number) if present
      #   tmp <- str_split(foo, "\\_", simplify = TRUE)[,1]

        xxx |>
          mutate(DeploymentID = xx) |>
          select(DeploymentID, everything())

      # } else {
      #   NULL
      # }
    })

    nms <- str_split(str_split(x, "\\.", simplify = TRUE)[,1], "-(?=[^-]+$)", simplify = TRUE)[,2]
    names(out) <- nms
    out
  })

  ## Locations df
  ## Need to ensure variable names are exactly the same before binding df rows
  ##  WC doesn't always use identical variable names - Capitalization changes
  nms <- c(
    "DeploymentID",
    "DeployID",
    "Ptt",
    "Instr",
    "Date",
    "Type",
    "Quality",
    "Latitude",
    "Longitude",
    "Error radius",
    "Error Semi-major axis",
    "Error Semi-minor axis",
    "Error Ellipse orientation",
    "Offset",
    "Offset orientation",
    "GPE MSD",
    "GPE U",
    "Count",
    "Comment"
  )

  ## drop locations within 15km of WC HQ
  HQ.ll <- data.frame(lon = -122.1344, lat = 47.6777) |>
    st_as_sf(coords = c("lon","lat"), crs = 4326)

  HQ.buf <- HQ.ll |>
    st_buffer(dist = 15000)


  Locations <- lapply(wc, function(x) {
    if(nrow(x$Locations) > 0) {
      xx <- x$Locations |>
        mutate(DeployID = as.character(DeployID))
      names(xx) <- nms
      xx
    }
  }) |>
    bind_rows() |>
    mutate(Quality = case_when(Type == "User" ~ "G",
              Type == "FastGPS" ~ "G",
              Type == "Mote" ~ "B",
              Quality == "Z" ~ "B",
              .default = as.character(Quality))) |>
    mutate(day = str_split(Date, " ", simplify = TRUE)[, 2],
           time = str_split(Date, " ", simplify = TRUE)[, 1]) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    arrange(Date, by_group = DeploymentID) |>
    suppressMessages()

  ## If locations remain at WC HQ then remove all those within 15km of HQ
  tmp <- Locations |>
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326)
  tmp.f <- unique(tmp$DeployID)
  tmp.lst <- split(tmp, tmp$DeployID)
  locs.lst <- split(Locations, Locations$DeployID)

  ## remove by max date within 15km of HQ, in case any but the last loc are
  ##  beyond the 15 km circle due to large Argos errors
  Locations1 <- lapply(1:length(tmp.lst), function(i) {
    win <- st_within(tmp.lst[[i]], HQ.buf) |> as.matrix() |> as.vector()
    if(sum(win) > 0) {
      last.date <- max(tmp.lst[[i]]$Date[win == TRUE], na.rm = TRUE)
      locs.lst[[i]][tmp.lst[[i]]$Date > last.date,]
    } else {
      locs.lst[[i]]
    }
  }) |>
    bind_rows()

  ## Stop the workflow here if no Locations occur beyond 15km radius of HQ
  if(nrow(Locations1) == 0) stop("All downloaded Locations are at Wildlife Computers HQ, no data to QC...", call. = FALSE)

  tmp <- unique(Locations$DeploymentID)[!unique(Locations$DeploymentID) %in% unique(Locations1$DeploymentID)]
  if(length(tmp) > 1) {
    message(paste0(length(tmp),
                   " tags were removed from the QC workflow because no locations occurred beyond Wildlife Computers HQ:\n ",
                   tmp))

  } else if (length(tmp == 1)) {
    message(paste0(length(tmp),
                   " tag was removed from the QC workflow because no locations occurred beyond Wildlife Computers HQ:\n ",
                   tmp))

  }

  Locations <- Locations1

  first.dates <- Locations |> group_by(DeploymentID) |> summarise(md = min(Date, na.rm = TRUE))

  ## FastGPS df
  FastGPS <- lapply(wc, function(x) {
    if(length(x$FastGPS) > 0) {
      out <- x$FastGPS |>
        mutate(Name = as.character(Name))
      ## Add '50CI' & '90CI' variables if missing so all df's have same vars
      if(!"50CI" %in% names(out)) {
        out <- out |>
          mutate("50CI" = NA,
                 "95CI" = NA) |>
          select(1:17, "50CI", "95CI", 18:ncol(out))
      }
      out
    }
    }) |> list_drop_empty()

  if(length(FastGPS) > 0) FastGPS <- FastGPS |>
    bind_rows() |>
    suppressMessages()

  ## ECDHistos df's - separate for SCOUT_TEMP_361A & SCOUT_DSA b/c df's have different structure
  ECDHistos_SCOUT_TEMP_361A <- lapply(wc, function(x) {
    if (length(x$ECDHistos) > 0 &
        length(unique(x$ECDHistos$Kind)) > 1) {
      x$ECDHistos |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |> list_drop_empty()

  if (length(ECDHistos_SCOUT_TEMP_361A) > 0)
    ECDHistos_SCOUT_TEMP_361A <- ECDHistos_SCOUT_TEMP_361A |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressMessages()

  ECDHistos_SCOUT_DSA <- lapply(wc, function(x) {
    if(length(x$ECDHistos) > 0 & length(unique(x$ECDHistos$Kind)) == 1) {
      x$ECDHistos |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |> list_drop_empty()

  if(length(ECDHistos_SCOUT_DSA) > 0) ECDHistos_SCOUT_DSA <- ECDHistos_SCOUT_DSA |>
    bind_rows() |>
    mutate(day = str_split(Start, " ", simplify = TRUE)[, 2],
           time = str_split(Start, " ", simplify = TRUE)[, 1]) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Start = dmy_hms(paste(day, time), tz = "UTC")) |>
    mutate(day = str_split(End, " ", simplify = TRUE)[, 2],
           time = str_split(End, " ", simplify = TRUE)[, 1]) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(End = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Start[1], md)) |>
    mutate(md = ifelse(is.na(md), End[1], md)) |>
    filter(Start >= md, End >= md) |>
    select(-md) |>
    suppressMessages()


  ## Histos df's
  Histos <- lapply(wc, function(x) {
    if (length(x$Histos) > 0) {
      xx <- x$Histos |>
        mutate(DeployID = as.character(DeployID)) |>
        as.data.frame()
      ## ensure all Bin# are double
      for(i in 1:72) {
        xx[,16+i] <- as.double(xx[,16+i])
      }
      as_tibble(xx)
    }
  }) |>
    list_drop_empty() |>
    suppressWarnings()


  if(length(Histos) > 0) {
    Histos <- Histos |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressWarnings()
  }

  ## MixLayer df's
  MixLayer <- lapply(wc, function(x) {
    if (length(x$MixLayer) > 0) {
      x$MixLayer |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(MixLayer) > 0) {
    MixLayer <- MixLayer |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressMessages()
  }


  ## PDTs df's
  PDTs <- lapply(wc, function(x) {
    if (length(x$PDTs) > 0) {
      x$PDTs |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(PDTs) > 0) PDTs <- PDTs |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressMessages()


  ## DSA df's
  DSA <- lapply(wc, function(x) {
    if (length(x$DSA) > 0) {
      x$DSA |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(DSA) > 0) DSA <- DSA |>
    bind_rows() |>
    mutate(
      day = str_split(DiveStart, " ", simplify = TRUE)[, 2],
      time = str_split(DiveStart, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(DiveStart = dmy_hms(paste(day, time), tz = "UTC")) |>
    mutate(
      day = str_split(SegmentStart, " ", simplify = TRUE)[, 2],
      time = str_split(SegmentStart, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(SegmentStart = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), DiveStart[1], md)) |>
    mutate(md = ifelse(is.na(md), SegmentStart[1], md)) |>
    filter(DiveStart >= md,
           SegmentStart >= md) |>
    select(-md) |>
    suppressWarnings()


  ## MinMaxDepth df's
  MinMaxDepth <- lapply(wc, function(x) {
    if (length(x$MinMaxDepth) > 0) {
      x$MinMaxDepth |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(MinMaxDepth) > 0) MinMaxDepth <- MinMaxDepth |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressWarnings()


  ## Haulout df's
  Haulout <- lapply(wc, function(x) {
    if (length(x$Haulout) > 0) {
      x$Haulout |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(Haulout) > 0) Haulout <- Haulout |>
    bind_rows() |>
    mutate(
      day = str_split(EndMax, " ", simplify = TRUE)[, 2],
      time = str_split(EndMax, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(EndMax = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), EndMax[1], md)) |>
    filter(EndMax >= md) |>
    select(-md) |>
    suppressWarnings()


  ## SST df's
  SST <- lapply(wc, function(x) {
    if (length(x$SST) > 0) {
      x$SST |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(SST) > 0) SST <- SST |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    left_join(first.dates, by = "DeploymentID") |>
    mutate(md = ifelse(is.na(md), Date[1], md)) |>
    filter(Date >= md) |>
    select(-md) |>
    suppressWarnings()

  ## Combine into single wc list
  wc <- list(Locations,
             FastGPS,
             ECDHistos_SCOUT_TEMP_361A,
             ECDHistos_SCOUT_DSA,
             Histos,
             MixLayer,
             PDTs,
             DSA,
             MinMaxDepth,
             Haulout,
             SST)|>
    suppressMessages()

  names(wc) <- datafiles

  return(wc)
}
