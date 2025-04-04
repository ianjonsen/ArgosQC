##' @title Pull data from Wildlife Computers tag data files
##'
##' @description extracts data from `-Locations.csv` (Argos), `-FastGPS.csv`,
##' `Histos.csv`, `MinMaxDepth.csv`, `HaulOut.csv`, and `SST.csv` files.
##' Extracted data are aggregated across individual tags and returned in a
##' single named list with the following data.frames:
##' * argos
##' * fastgps
##' * histos
##' * minmaxdepth
##' * haulout
##' * sst
##' WC tag data files downloaded via `download_data` will be stored in separate,
##' tag-specific subdirectories. `path2data` should point to the outer directory.
##'
##' @param path2data path to all WC tag data files.
##' @param datafiles specify which files to extract, default is to extract data
##' from all of the listed files. At a minimum, data must be extracted from the
##' `-X-Locations.csv` file, where `-X-` denotes the latest WC-processed version.
##'
##' @examples
##'
##' @importFrom dplyr select mutate bind_rows everything
##' @importFrom lubridate dmy_hms
##' @importFrom readr read_csv cols
##' @importFrom stringr str_split
##' @importFrom vctrs list_drop_empty
##'
##' @md
##' @external

pull_wc_data <- function(path2data,
                         datafiles = NULL) {

  if(is.null(datafiles))
    datafiles <- c("argos", "fastgps", "histos", "depth", "haulout", "sst")

  datafiles <- match.arg(datafiles,
                         choices = c("argos",
                                     "fastgps",
                                     "histos",
                                     "depth",
                                     "haulout",
                                     "sst"),
                         several.ok = TRUE)

  if(!"argos" %in% datafiles) stop("At a minimum, the '-Locations.csv' must be specified in 'datafiles'.")

  ## drop tag dirs with *-Locations.csv files < 20 Argos locations
  dirs <- list.dirs(path2data)[-1]

  idx <- sapply(1:length(dirs), function(i) {
    fs <- list.files(dirs[i])
    if (length(fs[grep("[0-9]-Locations.csv", fs)]) > 0) {
      nrow(read_csv(file.path(dirs[i], fs[grep("[0-9]-Locations.csv", fs)][1]), col_types = cols()))
    } else {
      0
    }
  })
  if (inherits(idx, "list"))
    idx <- unlist(idx)
  idx <- idx > 20
  ndirs <- dirs[idx]
  ## get all data files & merge into list
  wc <- vector(mode = "list", length = 6)

  if ("argos" %in% datafiles) {
  ## Argos locs
  wc[[1]] <- lapply(1:length(ndirs), function(i) {
    fs <- list.files(ndirs[i])
    id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]

    if (file.exists(paste0(file.path(ndirs[i], id), "-Locations.csv"))) {
      loc <- list.files(ndirs[i], pattern = "[0-9]-Locations.csv")[1]
      try(read_csv(file.path(ndirs[i], loc), col_types = cols()) |>
            mutate(DeployID = as.character(DeployID),
                   Quality = ifelse(Type == "User", "G", Quality)),
          silent = TRUE)
    }
  }) |>
    bind_rows() |>
    mutate(day = str_split(Date, " ", simplify = TRUE)[, 2],
           time = str_split(Date, " ", simplify = TRUE)[, 1]) |>
    mutate(day = ifelse(day == "", NA, day),
           time = ifelse(time == "", NA, time)) |>
    mutate(date = dmy_hms(paste(day, time), tz = "UTC")) |>
    select(ptt = Ptt, everything(), -day, -time)
}

  if ("gps" %in% datafiles) {
    ## GPS locs
    wc[[2]] <- lapply(1:length(ndirs), function(i) {
      fs <- list.files(ndirs[i])
      id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]

      if (file.exists(paste0(file.path(ndirs[i], id), "-FastGPS.csv"))) {
        loc <- list.files(ndirs[i], pattern = "[0-9]-FastGPS.csv")[1]
        x <- try(read_csv(file.path(ndirs[i], loc),
                          name_repair = "unique_quiet",
                          col_types = cols()), silent = TRUE)
        if (!inherits(x, "try-error"))
          x
      }
    }) |>
      bind_rows() |>
      mutate(date = dmy_hms(paste(Day, Time), tz = "UTC")) |>
      mutate(ptt = Name) |>
      select(ptt, everything())
  }

  if ("histos" %in% datafiles) {
    ## Histos
    wc[[3]] <- suppressWarnings(
      lapply(1:length(ndirs), function(i) {
        fs <- list.files(ndirs[i])
        id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]
        if (file.exists(paste0(file.path(ndirs[i], id), "-Histos.csv"))) {
          x <- try(read_csv(paste0(
            file.path(ndirs[i], id), "-Histos.csv"
          ), col_types = cols()), silent = TRUE)
          x[, c(1, 16:87)] <- apply(x[, c(1, 16:87)], 2, as.character)
          if (!inherits(x, "try-error"))
            x
        }
      }) |>
        bind_rows() |>
        mutate(
          day = str_split(Date, " ", simplify = TRUE)[, 2],
          time = str_split(Date, " ", simplify = TRUE)[, 1]
        ) |>
        mutate(
          day = ifelse(day == "", NA, day),
          time = ifelse(time == "", NA, time)
        ) |>
        mutate(date = dmy_hms(paste(day, time), tz = "UTC")) |>
        mutate(ptt = Ptt) |>
        select(ptt, everything(), -day, -time)
    )
  }

  if ("depth" %in% datafiles) {
    ## MinMaxDepth
    wc[[4]] <- suppressWarnings(
      lapply(1:length(ndirs), function(i) {
        fs <- list.files(ndirs[i])
        id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]
        if (file.exists(paste0(file.path(ndirs[i], id), "-MinMaxDepth.csv"))) {
          x <- try(read_csv(paste0(
            file.path(ndirs[i], id), "-MinMaxDepth.csv"
          ), col_types = cols()), silent = TRUE) |>
            mutate(DeployID = as.character(DeployID))
          if (!inherits(x, "try-error"))
            x
        }
      }) |>
        bind_rows() |>
        mutate(
          day = str_split(Date, " ", simplify = TRUE)[, 2],
          time = str_split(Date, " ", simplify = TRUE)[, 1]
        ) |>
        mutate(
          day = ifelse(day == "", NA, day),
          time = ifelse(time == "", NA, time)
        ) |>
        mutate(date = dmy_hms(paste(day, time), tz = "UTC")) |>
        mutate(ptt = Ptt) |>
        select(ptt, everything(), -day, -time)
    )
  }

  if ("haulout" %in% datafiles) {
    ## Haulout
    wc[[5]] <- suppressWarnings(
      lapply(1:length(ndirs), function(i) {
        fs <- list.files(ndirs[i])
        id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]
        if (file.exists(paste0(file.path(ndirs[i], id), "-HaulOut.csv"))) {
          x <- try(read_csv(paste0(
            file.path(ndirs[i], id), "-HaulOut.csv"
          ), col_types = cols()), silent = TRUE)
          if (!inherits(x, "try-error"))
            x
        }
      }) |>
        bind_rows() |>
        mutate(
          day = str_split(EndMax, " ", simplify = TRUE)[, 2],
          time = str_split(EndMax, " ", simplify = TRUE)[, 1]
        ) |>
        mutate(
          day = ifelse(day == "", NA, day),
          time = ifelse(time == "", NA, time)
        ) |>
        mutate(date = dmy_hms(paste(day, time), tz = "UTC")) |>
        mutate(ptt = Ptt) |>
        select(ptt, everything(), -day, -time)
    )
  }

  if ("sst" %in% datafiles) {
    ## SST
    wc[[6]] <- suppressWarnings(
      lapply(1:length(ndirs), function(i) {
        fs <- list.files(ndirs[i])
        id <- str_split(fs[grep("-All.csv", fs)], "\\-", simplify = TRUE)[, 1]
        if (file.exists(paste0(file.path(ndirs[i], id), "-SST.csv"))) {
          x <- try(read_csv(paste0(file.path(ndirs[i], id), "-SST.csv"), col_types = cols()), silent = TRUE)
          if (!inherits(x, "try-error"))
            x
        }
      }) |>
        bind_rows() |>
        mutate(
          day = str_split(Date, " ", simplify = TRUE)[, 2],
          time = str_split(Date, " ", simplify = TRUE)[, 1]
        ) |>
        mutate(
          day = ifelse(day == "", NA, day),
          time = ifelse(time == "", NA, time)
        ) |>
        mutate(date = dmy_hms(paste(day, time), tz = "UTC")) |>
        mutate(ptt = Ptt) |>
        select(ptt, everything(), -day, -time)
    )
  }

  ## drop empty list elements
  idx <- !sapply(wc, is.null)
  wc <- list_drop_empty(wc)
  names(wc) <- datafiles[idx]

  return(wc)
}
