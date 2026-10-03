##' @title Delayed-mode QC of one IMOS SMRU tag campaign, from config file to AODN upload
##'
##' @description Runs the whole delayed-mode (DM) QC of one IMOS SMRU tag
##' campaign (seals or turtles) and records it:
##' 1) writes, or checks and updates, the campaign config file;
##' 2) suggests track end dates with [smru_suggest_trim()] and opens the review
##'    PDF;
##' 3) asks the operator to accept, edit or reject the suggested end dates;
##' 4) runs [smru_qc()];
##' 5) zips the output .csv files;
##' 6) uploads the zip file to AODN by sftp, after the operator confirms.
##'
##' @details
##' Config file. If `config` exists, it is read and these settings are applied:
##' `"QCmode": "dm"`; `"download": false`; `"diag.dir": "qc/diag"`; for Weddell
##' seals, `"time.step": 6` and `"min.gap": 48`; the `meta` section derived as
##' below; and any `meta.file`, `dropIDs`, `smru.usr`, `smru.pwd` and
##' `model.args` supplied. Every change is listed and must be confirmed before
##' the file is rewritten. If `config` does not exist, it is built from the
##' species defaults and the arguments.
##'
##' The `meta` section. `species` and `release_site` are taken, in order of
##' precedence, from the arguments, from the IMOS metadata file (the `Species`
##' and `Location` of the campaign's deployments) and from the existing config
##' file. `common_name` comes from the argument or else from the species
##' lookup table. `state_country` comes from the argument or else from the
##' release site lookup table; the function stops if neither gives one.
##'
##' Track truncation review (`review = TRUE`). The choices are:
##' 1) accept all suggested end dates ([accept_trim_dates()]);
##' 2) edit `<cid>_trimIDs_draft.csv`, save it, then continue
##'    ([accept_trim_dates()] checks it and copies it to `<cid>_trimIDs.csv`);
##' 3) use an existing `<cid>_trimIDs.csv` unchanged;
##' 4) truncate no tracks;
##' 5) stop.
##'
##' With `review = FALSE`, the `trimIDs` file already set in the config file is
##' used, or no track is truncated if none is set. Use it to repeat a run with
##' the recorded config file and track truncation file.
##'
##' Records. Each run writes:
##' 1) the config file as used;
##' 2) `<cid>_trimIDs.csv` (if any track is truncated);
##' 3) `qc/diag/<cid>_dm_log.txt`, appended on each run, with the ArgosQC and
##'    aniMotum versions (and git commits where available), the review
##'    decision, the files zipped and the sftp outcome.
##'
##' @param cid the SMRU campaign id, e.g. `"ct155"`
##' @param wd the working directory for the campaign's DM QC
##' @param config the config file name (relative to `wd`) or path. Default:
##'   `config_<cid>.json`
##' @param meta.file the IMOS metadata .csv file. If `NULL`, the file named in
##'   an existing config file is used; if there is none, metadata are built
##'   from the SMRU server and the `meta` section of the config file
##' @param species the species scientific name. Required only if it cannot be
##'   read from `meta.file` or an existing config file
##' @param common_name the species common name. Default: from the species
##'   lookup table
##' @param release_site the release site. Default: the metadata `Location`, or
##'   the existing config value
##' @param state_country the country or territory of the release site.
##'   Default: from the release site lookup table
##' @param dropIDs a .csv file of deployments to exclude from the QC. Default:
##'   the existing config value, or none
##' @param model.args a named list of `model` settings that replace the
##'   species defaults or the existing config values, e.g. `list(dist = 50)`
##' @param smru.usr,smru.pwd the SMRU data server login. Required for a new
##'   config file
##' @param p2mdbtools the path to the mdbtools binaries, used for a new config
##'   file
##' @param review logical; review the suggested track end dates interactively
##'   (`TRUE`), or use the `trimIDs` file set in the config file (`FALSE`)
##' @param upload logical; offer to upload the zip file to AODN. The upload
##'   always needs confirmation in an interactive session
##' @param aodn.user the AODN sftp username. If blank, it is asked for at
##'   upload time
##' @param aodn.path the AODN sftp directory for DM QC files
##' @param ... further arguments passed to [smru_suggest_trim()]
##'
##' @return invisibly, a list with the config file path, the `trimIDs` file
##'   path (or `NULL`), the zip file path, whether the upload succeeded, and the
##'   value returned by [smru_qc()]
##'
##' @examples
##' \dontrun{
##' ## first run: review the suggested track end dates interactively
##' imos_smru_dm_qc(cid = "ct155",
##'                 wd = "/Volumes/work/R/imos/imos_sat/seals/past_qc/dm_qc",
##'                 aodn.user = "user@example.org")
##'
##' ## repeat a run with the recorded config and track truncation files
##' imos_smru_dm_qc(cid = "ct155",
##'                 wd = "/Volumes/work/R/imos/imos_sat/seals/past_qc/dm_qc",
##'                 review = FALSE)
##' }
##'
##' @importFrom jsonlite read_json toJSON
##' @importFrom readr read_csv cols col_character
##' @importFrom stringr str_extract regex
##' @importFrom utils menu packageVersion packageDescription unzip browseURL
##'
##' @md
##' @export

imos_smru_dm_qc <- function(cid,
                            wd,
                            config = NULL,
                            meta.file = NULL,
                            species = NULL,
                            common_name = NULL,
                            release_site = NULL,
                            state_country = NULL,
                            dropIDs = NULL,
                            model.args = list(),
                            smru.usr = NULL,
                            smru.pwd = NULL,
                            p2mdbtools = "/opt/homebrew/Cellar/mdbtools/1.0.1/bin/",
                            review = TRUE,
                            upload = TRUE,
                            aodn.user = "",
                            aodn.path = "/data/IMOS/AATAMS/ANIMAL_TRACKING_SATTAG_QC/dm/",
                            ...) {

  ## ---- checks and set-up ----
  if (!is.character(cid) || length(cid) != 1 || !nzchar(cid)) {
    stop("`cid` must be a single campaign id, e.g. \"ct155\".", call. = FALSE)
  }
  if (!dir.exists(wd)) stop("Working directory `wd` does not exist: ", wd, call. = FALSE)
  wd <- normalizePath(wd)
  if (review && !interactive()) {
    stop("review = TRUE needs an interactive R session. ",
         "Use review = FALSE to repeat a recorded run.", call. = FALSE)
  }
  if (!is.list(model.args) || (length(model.args) > 0 && is.null(names(model.args)))) {
    stop("`model.args` must be a named list, e.g. list(dist = 50).", call. = FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  resolve <- function(f) if (grepl("^(/|~)", f)) path.expand(f) else file.path(wd, f)
  if (is.null(config)) config <- paste0("config_", cid, ".json")
  config_path <- resolve(config)

  diag_dir <- "qc/diag"
  dir.create(file.path(wd, diag_dir), showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(wd, diag_dir, paste0(cid, "_dm_log.txt"))
  note <- function(...) {
    txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "  ", paste0(...))
    cat(txt, "\n", file = log_file, append = TRUE, sep = "")
    message(paste0(...))
  }

  versions <- c(dm_qc_pkg_info("ArgosQC"), dm_qc_pkg_info("aniMotum"))
  cat("\n", strrep("=", 72), "\n", file = log_file, append = TRUE, sep = "")
  note("imos_smru_dm_qc() started for ", cid, " in ", wd)
  note("Package versions: ", paste(versions, collapse = "; "))

  ## ---- config file ----
  new_config <- !file.exists(config_path)
  if (new_config) {
    conf <- NULL
  } else {
    conf0 <- read_json(config_path, simplifyVector = FALSE)
    conf <- if (is.null(names(conf0))) conf0[[1]] else conf0
    if (!identical(as.character(conf$harvest$cid), cid)) {
      stop(config_path, " is for campaign ", conf$harvest$cid, ", not ", cid, ".", call. = FALSE)
    }
  }

  ## metadata file
  mf <- meta.file
  if (is.null(mf) && !is.null(conf$setup$meta.file)) mf <- conf$setup$meta.file
  meta_rows <- NULL
  if (!is.null(mf)) {
    mf_path <- resolve(mf)
    if (!file.exists(mf_path)) stop("Metadata file not found: ", mf_path, call. = FALSE)
    md <- suppressWarnings(suppressMessages(
      read_csv(mf_path, col_types = cols(.default = col_character()), progress = FALSE)))
    names(md) <- trimws(names(md))
    if (all(c("SMRU_Ref", "Species", "Location") %in% names(md))) {
      prog <- str_extract(md$SMRU_Ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE))
      meta_rows <- md[!is.na(prog) & prog == cid, ]
      if (nrow(meta_rows) == 0) {
        stop("No deployments for ", cid, " in ", mf_path, call. = FALSE)
      }
    } else {
      message("The metadata file has no SMRU_Ref, Species and Location columns; ",
              "species and release site are not read from it.")
    }
  }

  ## species, common name, release site, state or country
  sp <- species
  if (is.null(sp) && !is.null(meta_rows)) sp <- dm_qc_most_common(meta_rows$Species, "Species")
  if (is.null(sp) && !is.null(conf$meta$species)) sp <- conf$meta$species
  if (is.null(sp)) {
    stop("The species is unknown: supply `species` (scientific name).", call. = FALSE)
  }
  spi <- match(imos_norm(sp), imos_norm(.imos_species$species))
  if (is.na(spi) || !.imos_species$species[spi] %in% names(.imos_dm_model)) {
    stop("No DM QC defaults for species \"", sp, "\". Species with defaults: ",
         paste(names(.imos_dm_model), collapse = ", "), ".", call. = FALSE)
  }
  sp <- .imos_species$species[spi]
  cn <- if (is.null(common_name)) .imos_species$common_name[spi] else common_name

  rs <- release_site
  if (is.null(rs) && !is.null(meta_rows)) rs <- dm_qc_most_common(meta_rows$Location, "Location")
  if (is.null(rs) && !is.null(conf$meta$release_site)) rs <- conf$meta$release_site
  if (is.null(rs)) stop("The release site is unknown: supply `release_site`.", call. = FALSE)
  rs <- imos_site(rs)

  sc <- state_country
  if (is.null(sc)) {
    sc <- imos_state_country(rs)
    if (is.na(sc)) sc <- NULL
  }
  if (is.null(sc)) {
    stop("No state_country for release site \"", rs, "\": supply `state_country`. ",
         "Known release sites: ", paste(.imos_sites$release_site, collapse = ", "), ".",
         call. = FALSE)
  }

  ## build or update the config
  if (new_config) {
    if (is.null(smru.usr) || is.null(smru.pwd)) {
      stop("A new config file needs `smru.usr` and `smru.pwd` (the SMRU data server login).",
           call. = FALSE)
    }
    new <- list(
      setup = list(program = "imos",
                   data.dir = "qc/mdb",
                   meta.file = mf,
                   maps.dir = "qc/maps",
                   diag.dir = diag_dir,
                   output.dir = "qc/aodn",
                   return.R = TRUE),
      harvest = list(download = FALSE,
                     cid = cid,
                     smru.usr = smru.usr,
                     smru.pwd = smru.pwd,
                     timeout = 600,
                     dropIDs = dropIDs,
                     trimIDs = NULL,
                     p2mdbtools = p2mdbtools),
      model = c(.imos_dm_model[[sp]], list(QCmode = "dm")),
      meta = list()
    )
  } else {
    new <- conf
    new$setup$diag.dir <- diag_dir
    if (!is.null(meta.file)) new$setup$meta.file <- meta.file
    new$harvest$download <- FALSE
    if (!is.null(dropIDs)) new$harvest$dropIDs <- dropIDs
    if (!is.null(smru.usr)) new$harvest$smru.usr <- smru.usr
    if (!is.null(smru.pwd)) new$harvest$smru.pwd <- smru.pwd
    if (!"trimIDs" %in% names(new$harvest)) {
      pos <- match("dropIDs", names(new$harvest))
      if (is.na(pos)) pos <- length(new$harvest)
      new$harvest <- append(new$harvest, list(trimIDs = NULL), after = pos)
    }
    new$model$QCmode <- "dm"
    if (sp == "Leptonychotes weddellii") {
      new$model$time.step <- 6
      new$model$min.gap <- 48
    }
  }
  for (n in names(model.args)) new$model[n] <- list(model.args[[n]])
  new$meta <- list(common_name = cn,
                   species = sp,
                   release_site = rs,
                   state_country = sc)

  if (new_config) {
    dm_qc_write_config(new, config_path)
    note("Config file written: ", config_path)
  } else {
    a <- dm_qc_flatten(conf)
    b <- dm_qc_flatten(new)
    keys <- union(names(a), names(b))
    av <- unname(a[keys]); av[is.na(av)] <- "(absent)"
    bv <- unname(b[keys]); bv[is.na(bv)] <- "(absent)"
    changed <- av != bv
    if (any(changed)) {
      chg <- paste0("  ", keys[changed], ": ", av[changed], " -> ", bv[changed])
      cat("\nChanges to ", config_path, ":\n", paste(chg, collapse = "\n"), "\n\n", sep = "")
      if (!interactive()) {
        stop("The config file needs changes, which must be confirmed in an interactive session.",
             call. = FALSE)
      }
      ans <- menu(c("Write these changes to the config file", "Stop"),
                  title = "Update the config file?")
      if (ans != 1) {
        note("Stopped by the operator: config file changes declined")
        return(invisible(NULL))
      }
      dm_qc_write_config(new, config_path)
      note("Config file updated: ", config_path, "\n", paste(chg, collapse = "\n"))
    } else {
      note("Config file unchanged: ", config_path)
    }
  }

  ## ---- tag data: download the .mdb file only if it is missing ----
  mdb <- file.path(wd, new$setup$data.dir, paste0(cid, ".mdb"))
  if (!file.exists(mdb)) {
    note("Downloading ", cid, ".mdb from the SMRU server")
    dir.create(dirname(mdb), showWarnings = FALSE, recursive = TRUE)
    download_data(dest = dirname(mdb),
                  source = "smru",
                  cid = cid,
                  user = new$harvest$smru.usr,
                  pwd = new$harvest$smru.pwd,
                  timeout = new$harvest$timeout)
    if (!file.exists(mdb)) stop("Download failed: ", mdb, " not found.", call. = FALSE)
  }
  note("Tag data file: ", mdb, " (modified ", format(file.mtime(mdb), "%Y-%m-%d %H:%M:%S"), ")")

  ## ---- track truncation ----
  trim_file <- paste0(cid, "_trimIDs.csv")
  draft_file <- paste0(cid, "_trimIDs_draft.csv")
  set_trimIDs <- function(value) {
    cur <- read_json(config_path, simplifyVector = FALSE)
    cur <- if (is.null(names(cur))) cur[[1]] else cur
    cur$harvest["trimIDs"] <- list(value)
    dm_qc_write_config(cur, config_path)
  }

  if (review) {
    note("Suggesting track end dates")
    trim <- smru_suggest_trim(wd = wd, config = config_path, ...)
    setwd(wd)
    pdf_file <- file.path(wd, diag_dir, paste0("trim_review_", cid, ".pdf"))
    dm_qc_open(pdf_file)

    sug <- trim[!is.na(trim$end_date), c("ref", "end_date", "rule")]
    cat("\n", nrow(trim), " deployments; ", nrow(sug), " with a suggested end date:\n", sep = "")
    if (nrow(sug) > 0) {
      cat(paste0("  ", format(sug$ref), "  ", format(sug$end_date), "  ", sug$rule), sep = "\n")
    }
    cat("Review plots: ", pdf_file, "\n", sep = "")

    has_trim <- file.exists(file.path(wd, trim_file))
    choices <- c(accept = "Accept all suggested end dates",
                 edit = paste0("Edit the end dates in ", draft_file, ", then continue"),
                 existing = paste0("Use the existing ", trim_file, " unchanged"),
                 none = "Truncate no tracks",
                 stop = "Stop without running the QC")
    if (!has_trim) choices <- choices[names(choices) != "existing"]

    decision <- NULL
    while (is.null(decision)) {
      ans <- menu(choices, title = paste0("Track end dates for ", cid, ":"))
      pick <- if (ans == 0) "stop" else names(choices)[ans]

      if (pick == "accept") {
        accept_trim_dates(wd = wd, config = config_path, overwrite = TRUE)
        decision <- paste0("Accepted all suggested end dates (", nrow(sug), " tracks truncated); ",
                           trim_file, " written")

      } else if (pick == "edit") {
        dm_qc_open(file.path(wd, draft_file))
        cat("Edit ", file.path(wd, draft_file), ": keep, change or delete each end_date,\n",
            "or add one. Save it as a .csv file with dates as YYYY-MM-DD HH:MM:SS.\n", sep = "")
        ans2 <- readline("Press Enter when it is saved, or type 'back' to return to the menu: ")
        if (tolower(trimws(ans2)) == "back") next
        ok <- tryCatch({
          accept_trim_dates(wd = wd, config = config_path, overwrite = TRUE)
          TRUE
        }, error = function(e) {
          message("The edited draft could not be used: ", conditionMessage(e))
          FALSE
        })
        if (ok) {
          d <- read_csv(file.path(wd, trim_file), col_types = cols(.default = col_character()),
                        progress = FALSE)
          decision <- paste0("Edited end dates (", sum(!is.na(d$end_date)), " tracks truncated); ",
                             trim_file, " written")
        }

      } else if (pick == "existing") {
        set_trimIDs(trim_file)
        decision <- paste0("Existing ", trim_file, " used unchanged")

      } else if (pick == "none") {
        set_trimIDs(NULL)
        decision <- "No tracks truncated"

      } else {
        note("Stopped by the operator at the track end date review")
        return(invisible(NULL))
      }
    }
    note("Track truncation decision: ", decision)

  } else {
    cur <- read_json(config_path, simplifyVector = TRUE)
    if (is.null(cur$harvest$trimIDs) || all(is.na(cur$harvest$trimIDs))) {
      decision <- "No tracks truncated (no trimIDs file set in the config file)"
    } else {
      tf <- resolve(cur$harvest$trimIDs)
      if (!file.exists(tf)) stop("trimIDs file not found: ", tf, call. = FALSE)
      decision <- paste0("Recorded track truncation file used: ", cur$harvest$trimIDs)
    }
    note("Track truncation decision: ", decision)
  }

  ## ---- QC ----
  note("smru_qc() started")
  qc <- tryCatch(smru_qc(wd = wd, config = config_path),
                 error = function(e) {
                   note("smru_qc() FAILED: ", conditionMessage(e))
                   stop(e)
                 })
  setwd(wd)
  note("smru_qc() completed")

  ## ---- zip the output files ----
  fin <- read_json(config_path, simplifyVector = TRUE)
  out_dir <- file.path(wd, fin$setup$output.dir)
  zip_file <- file.path(out_dir, paste0(cid, "_dm.zip"))
  ## remove the previous zip file, so no file from an earlier run is kept in it
  if (file.exists(zip_file)) file.remove(zip_file)
  push_2_aodn(cids = cid, path = out_dir, nopush = TRUE, suffix = "_dm")
  system(paste0("zip -d ", shQuote(zip_file), " __MACOSX/\\*"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (!file.exists(zip_file)) stop("Zip file not written: ", zip_file, call. = FALSE)
  zipped <- unzip(zip_file, list = TRUE)$Name
  note("Zip file written: ", zip_file, "\n", paste0("  ", zipped, collapse = "\n"))

  ## ---- upload to AODN ----
  uploaded <- FALSE
  if (upload) {
    if (!interactive()) {
      note("Upload skipped: the upload must be confirmed in an interactive session")
    } else {
      ans <- menu(c(paste0("Upload ", basename(zip_file), " to AODN (", aodn.path, ")"),
                    "Do not upload"),
                  title = paste0("Upload the DM QC files for ", cid, "?"))
      if (ans != 1) {
        note("Upload declined by the operator")
      } else {
        if (!nzchar(aodn.user)) aodn.user <- trimws(readline("AODN sftp username: "))
        if (!nzchar(aodn.user)) {
          note("Upload skipped: no AODN sftp username given")
        } else {
          host <- paste0(aodn.user, "@sftp.aodn.org.au")
          batch_file <- tempfile(fileext = ".sftp")
          on.exit(unlink(batch_file), add = TRUE)
          writeLines(c(paste("cd", aodn.path),
                       paste("put", shQuote(zip_file)),
                       "quit"),
                     batch_file)
          ## the SSH key passphrase is supplied by the SSH agent (macOS Keychain)
          response <- system2("sftp", args = c("-v", "-b", batch_file, host),
                              stdout = TRUE, stderr = TRUE)
          cat(response, sep = "\n")
          status <- attr(response, "status")
          if (!is.null(status)) {
            note("AODN sftp upload FAILED (status ", status, ")")
            stop("AODN sftp upload FAILED for ", cid, " (status ", status, ")", call. = FALSE)
          }
          uploaded <- TRUE
          note("Uploaded ", basename(zip_file), " to ", aodn.path)
        }
      }
    }
  } else {
    note("Upload not requested (upload = FALSE)")
  }

  note("imos_smru_dm_qc() completed for ", cid)

  invisible(list(config = config_path,
                 trimIDs = if (is.null(fin$harvest$trimIDs) || all(is.na(fin$harvest$trimIDs))) NULL
                           else resolve(fin$harvest$trimIDs),
                 zip = zip_file,
                 uploaded = uploaded,
                 qc = qc))
}


## ---- lookup tables (internal) ----

## DM QC model settings by species, as used in the IMOS DM config files
.imos_dm_model <- local({
  seal <- list(model = "rw",
               vmax = 2,
               time.step = 6,
               proj = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=100 +k=1 +ellps=WGS84 +units=km +no_defs",
               reroute = TRUE,
               dist = 20,
               barrier = NULL,
               buffer = 0.5,
               centroids = TRUE,
               cut = FALSE,
               min.gap = 72)
  weddell <- seal
  weddell$proj <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=160 +k=1 +ellps=WGS84 +units=km +no_defs"
  weddell$min.gap <- 48
  turtle <- list(model = "rw",
                 vmax = 2,
                 time.step = 3,
                 proj = "+proj=merc +ellps=WGS84 +units=km +no_defs",
                 reroute = TRUE,
                 dist = 100,
                 barrier = NULL,
                 buffer = 0.5,
                 centroids = TRUE,
                 cut = TRUE,
                 min.gap = 72)
  list("Mirounga leonina" = seal,
       "Leptonychotes weddellii" = weddell,
       "Lepidochelys olivacea" = turtle,
       "Natator depressus" = turtle,
       "Chelonia mydas" = turtle)
})


## ---- helpers (internal) ----

## most frequent non-blank value, with a message if there is more than one
dm_qc_most_common <- function(x, what) {
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NULL)
  tb <- sort(table(x), decreasing = TRUE)
  if (length(tb) > 1) {
    message("More than one metadata ", what, " for this campaign (",
            paste0(names(tb), " [", tb, "]", collapse = ", "), "); using \"", names(tb)[1], "\".")
  }
  names(tb)[1]
}

## flatten a config list to "section$key" = JSON value strings, for comparison
dm_qc_flatten <- function(x, prefix = "") {
  out <- character(0)
  for (n in names(x)) {
    key <- if (prefix == "") n else paste0(prefix, "$", n)
    v <- x[[n]]
    if (is.list(v) && !is.null(names(v))) {
      out <- c(out, dm_qc_flatten(v, key))
    } else {
      out[key] <- if (is.null(v)) "null" else
        as.character(toJSON(v, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
    }
  }
  out
}

## write a config list as a JSON array of one object (the form smru_qc() reads),
##  after checking that it reads back
dm_qc_write_config <- function(x, path) {
  txt <- toJSON(list(x), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(txt, tmp)
  chk <- tryCatch(read_json(tmp, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(chk) || !identical(as.character(chk$model$QCmode), "dm") ||
      !identical(as.character(chk$harvest$cid), as.character(x$harvest$cid))) {
    stop("The config file failed its check before writing; ", path, " is unchanged.", call. = FALSE)
  }
  if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write ", path, call. = FALSE)
  invisible(path)
}

## package version, with the git commit (source tree) or remote commit (installed)
dm_qc_pkg_info <- function(pkg) {
  v <- tryCatch(as.character(packageVersion(pkg)), error = function(e) NA_character_)
  if (is.na(v)) return(paste(pkg, "not installed"))
  p <- tryCatch(find.package(pkg), error = function(e) "")
  sha <- NA_character_
  if (nzchar(p) && dir.exists(file.path(p, ".git"))) {
    sha <- tryCatch(suppressWarnings(
      system2("git", c("-C", shQuote(p), "rev-parse", "--short", "HEAD"),
              stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    sha <- if (length(sha) == 1) sha else NA_character_
    if (!is.na(sha)) {
      dirty <- tryCatch(suppressWarnings(
        system2("git", c("-C", shQuote(p), "status", "--porcelain", "--untracked-files=no"),
                stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
      if (length(dirty) > 0) sha <- paste0(sha, ", with uncommitted changes")
      sha <- paste0("source tree, commit ", sha)
    }
  } else {
    d <- suppressWarnings(packageDescription(pkg))
    if (is.list(d) && !is.null(d$RemoteSha)) sha <- paste0("installed, commit ", substr(d$RemoteSha, 1, 7))
  }
  paste0(pkg, " ", v, if (!is.na(sha)) paste0(" (", sha, ")"))
}

## open a file with the system's default application
dm_qc_open <- function(f) {
  if (!file.exists(f)) return(invisible(FALSE))
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    system2("open", shQuote(f), wait = FALSE)
  } else {
    browseURL(f)
  }
  invisible(TRUE)
}
