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
##' @importFrom lubridate dmy_hms
##' @importFrom readr read_csv cols
##' @importFrom stringr str_split
##' @importFrom vctrs list_drop_empty
##'
##' @md
##' @export

wc_pull_data <- function(path2data,
                         subset.ids) {

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

  if (inherits(idx, "list"))
    idx <- unlist(idx)

  ## reduce to only the subset in subset.ids (if !is.null)
  if(!is.null(ids)) {
    dd <- dirs[idx > 0]
    sub <- purrr::map(ids$uuid, stringr::str_detect, string = dd) |>
      sapply(which) |>
      sort()
    ndirs <- dd[sub]

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
  datafiles <- datafiles[-c(1,3)]

  wc <- suppressWarnings(lapply(fs, function(x) {
    out <- lapply(1:length(x), function(i) {
      xx <- read_csv(x[[i]],
                     show_col_types = FALSE,
                     name_repair = "unique_quiet") |>
        suppressMessages()

      ## detect & account for any leading blank rows in .csv files
      nn <- rowSums(is.na(xx)) == ncol(xx)
      ns <- sum(nn)
      if(ns > 0 & 1 %in% which(nn)) {
        xx <- read_csv(x[[i]],
                       show_col_types = FALSE,
                       name_repair = "unique_quiet",
                       skip = ns+1) |>
          suppressMessages()
      }

      if(nrow(xx) > 0) {
      xx |>
        mutate(DeploymentID = unique(str_split(x[[i]], "\\/", simplify = TRUE)[, grep("\\_", str_split(x[[i]], "\\/", simplify = TRUE))])) |>
        mutate(DeploymentID = str_split(DeploymentID, "\\_", simplify = TRUE)[,1]) |> ## removes _suffix (Tag serial number) if present
        select(DeploymentID, everything())
      } else {
        NULL
      }
    })
    nms <- str_split(str_split(x, "\\.", simplify = TRUE)[,1], "-(?=[^-]+$)", simplify = TRUE)[,2]
    names(out) <- nms
    out
  }))


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

  if(length(Histos) > 0) Histos <- Histos |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    suppressWarnings()

  ## MixLayer df's
  MixLayer <- lapply(wc, function(x) {
    if (length(x$MixLayer) > 0) {
      x$MixLayer |>
        mutate(DeployID = as.character(DeployID))
    }
  }) |>
    list_drop_empty()

  if(length(MixLayer) > 0) MixLayer <- MixLayer |>
    bind_rows() |>
    mutate(
      day = str_split(Date, " ", simplify = TRUE)[, 2],
      time = str_split(Date, " ", simplify = TRUE)[, 1]
    ) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(Date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(-day, -time) |>
    suppressMessages()


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
