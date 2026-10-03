##' @title Accept all suggested track end dates for delayed-mode QC
##'
##' @description Accepts the draft truncation file written by
##' `smru_suggest_trim()` as it is: copies `<cid>_trimIDs_draft.csv` to
##' `<cid>_trimIDs.csv` and sets `"trimIDs"` in the config file to that file. By
##' default it also sets `"download"` to `false`, so the delayed-mode QC uses the
##' local .mdb files that the suggestions were made from. Use it after reviewing
##' the review PDF, when every suggestion is accepted unchanged. To change any
##' suggestion, edit the draft and save it as `<cid>_trimIDs.csv` instead.
##'
##' The config file is edited as text: only the `"trimIDs"` value (or a new
##' `"trimIDs"` line after `"dropIDs"`) and the `"download"` value change, so its
##' layout is kept. The edited file is checked to parse, with the new values,
##' before anything is written.
##'
##' The function runs only for delayed-mode QC (`QCmode = "dm"`).
##'
##' @param wd the working directory, as in `smru_qc()`
##' @param config the JSON config file, as in `smru_qc()`
##' @param draft_file the draft truncation file. Default:
##' `<cid>_trimIDs_draft.csv` in `wd`
##' @param trim_file the accepted truncation file to write. Default:
##' `<cid>_trimIDs.csv` in `wd`
##' @param set_download_false logical; set `"download"` to `false` in the config
##' file (default `TRUE`)
##' @param overwrite logical; replace an existing `trim_file` (default `FALSE`,
##' so a hand-edited file is not lost)
##'
##' @return invisibly, the path of the accepted truncation file
##'
##' @importFrom jsonlite read_json
##' @importFrom readr read_csv cols col_character
##' @importFrom lubridate ymd_hms
##'
##' @export

accept_trim_dates <- function(wd,
                              config,
                              draft_file = NULL,
                              trim_file = NULL,
                              set_download_false = TRUE,
                              overwrite = FALSE) {

  if (!file.exists(wd)) stop("Working directory `wd` does not exist", call. = FALSE)
  config_path <- if (file.exists(file.path(wd, config))) file.path(wd, config) else config
  if (!file.exists(config_path)) stop("Config file not found: ", config, call. = FALSE)

  conf <- read_json(config_path, simplifyVector = TRUE)
  if (!identical(as.character(conf$model$QCmode), "dm")) {
    stop("accept_trim_dates() is for delayed-mode QC only (QCmode = \"dm\"); ",
         config, " has QCmode = \"", conf$model$QCmode, "\".", call. = FALSE)
  }

  cid <- paste(conf$harvest$cid, collapse = "_")
  if (is.null(draft_file)) draft_file <- paste0(cid, "_trimIDs_draft.csv")
  if (is.null(trim_file)) trim_file <- paste0(cid, "_trimIDs.csv")
  draft_path <- file.path(wd, draft_file)
  trim_path <- file.path(wd, trim_file)

  if (!file.exists(draft_path)) {
    stop("Draft truncation file not found: ", draft_path,
         ". Run smru_suggest_trim() first.", call. = FALSE)
  }
  if (file.exists(trim_path) && !overwrite) {
    stop(trim_path, " already exists. Use overwrite = TRUE to replace it.", call. = FALSE)
  }

  ## check the draft as trim_locs() will read it
  d <- read_csv(draft_path, col_types = cols(.default = col_character()), progress = FALSE)
  if (!"ref" %in% names(d) || !any(c("start_date", "end_date") %in% names(d))) {
    stop(draft_path, " must have a 'ref' column and an 'end_date' and/or 'start_date' column.",
         call. = FALSE)
  }
  if (!"start_date" %in% names(d)) d$start_date <- NA_character_
  if (!"end_date" %in% names(d)) d$end_date <- NA_character_
  dated <- d[!(is.na(d$start_date) & is.na(d$end_date)), ]
  dup <- unique(dated$ref[duplicated(dated$ref)])
  if (length(dup) > 0) {
    stop("Deployments listed more than once in ", draft_path, ": ",
         paste(dup, collapse = ", "), call. = FALSE)
  }
  parse_utc <- function(x) suppressWarnings(ymd_hms(x, truncated = 3, tz = "UTC", quiet = TRUE))
  bad <- c(dated$ref[!is.na(dated$start_date) & is.na(parse_utc(dated$start_date))],
           dated$ref[!is.na(dated$end_date) & is.na(parse_utc(dated$end_date))])
  if (length(bad) > 0) {
    stop("Unreadable dates in ", draft_path, " for: ", paste(unique(bad), collapse = ", "),
         call. = FALSE)
  }

  ## edit the config file as text, keeping whether it ends in a newline
  bytes <- readBin(config_path, "raw", file.info(config_path)$size)
  final_newline <- length(bytes) > 0 && bytes[length(bytes)] == as.raw(10)
  txt <- readLines(config_path, warn = FALSE)
  write_text <- function(lines, path) {
    cat(paste(lines, collapse = "\n"), if (final_newline) "\n", file = path, sep = "")
  }
  tri <- grep('"trimIDs"[[:space:]]*:', txt)
  if (length(tri) > 1) stop("More than one \"trimIDs\" line in ", config, call. = FALSE)
  if (length(tri) == 1) {
    txt[tri] <- sub('("trimIDs"[[:space:]]*:[[:space:]]*)(null|"[^"]*")',
                    paste0('\\1"', trim_file, '"'), txt[tri])
  } else {
    dl <- grep('"dropIDs"[[:space:]]*:', txt)
    if (length(dl) != 1) {
      stop("Cannot find a single \"dropIDs\" line in ", config,
           " to place \"trimIDs\" after; set \"trimIDs\" by hand.", call. = FALSE)
    }
    indent <- sub("^([[:space:]]*).*$", "\\1", txt[dl])
    has_comma <- grepl(",[[:space:]]*$", txt[dl])
    if (!has_comma) txt[dl] <- sub("[[:space:]]*$", ",", txt[dl])
    new_line <- paste0(indent, '"trimIDs":"', trim_file, '"', if (has_comma) "," else "")
    txt <- append(txt, new_line, after = dl)
  }

  if (set_download_false) {
    dli <- grep('"download"[[:space:]]*:', txt)
    if (length(dli) != 1) {
      stop("Cannot find a single \"download\" line in ", config,
           "; set it by hand, or use set_download_false = FALSE.", call. = FALSE)
    }
    txt[dli] <- sub('("download"[[:space:]]*:[[:space:]]*)(true|false|TRUE|FALSE|"[^"]*")',
                    "\\1false", txt[dli])
  }

  ## check the edited config before writing anything
  tmp <- tempfile(fileext = ".json")
  write_text(txt, tmp)
  chk <- tryCatch(read_json(tmp, simplifyVector = TRUE), error = function(e) NULL)
  ok <- !is.null(chk) && identical(as.character(chk$harvest$trimIDs), trim_file) &&
    (!set_download_false || identical(as.logical(chk$harvest$download), FALSE))
  if (!ok) {
    stop("Editing ", config, " failed its check; nothing has been changed.", call. = FALSE)
  }

  file.copy(draft_path, trim_path, overwrite = TRUE)
  write_text(txt, config_path)

  message(sprintf("Accepted %d suggested end dates: %s written.", sum(!is.na(d$end_date)), trim_path))
  message("Set \"trimIDs\": \"", trim_file, "\"",
          if (set_download_false) " and \"download\": false" else "", " in ", config_path)
  if (!set_download_false && isTRUE(as.logical(conf$harvest$download))) {
    message("Note: \"download\" is true, so smru_qc() will download the .mdb again.")
  }

  invisible(trim_path)
}
