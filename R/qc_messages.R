## Labelled, coloured console messages (internal)
##
## Each message type has a text label, written to the console and to log files,
## and a colour used on the console only (ANSI colours, via cli; plain text
## where the console has no colour support). Types:
##   progress  workflow steps (no label, default colour)
##   server    SMRU data server values used in place of metadata file values
##   reference metadata file references matched to SMRU references
##   built     metadata built for deployments missing from the metadata file
##   sites     release sites of a campaign
##   excluded  deployments left out of the QC
##   review    items that need checking before upload
##   done      decisions taken, files written, uploads completed
##   stop      stops, failures and declined steps

.qc_msg_types <- list(
  progress  = list(label = "",                 col = function(x) x),
  server    = list(label = "SMRU SERVER: ",    col = function(x) cli::col_yellow(x)),
  reference = list(label = "REFERENCE MATCH: ", col = function(x) cli::col_cyan(x)),
  built     = list(label = "BUILT METADATA: ", col = function(x) cli::col_magenta(x)),
  sites     = list(label = "RELEASE SITES: ",  col = function(x) cli::col_blue(x)),
  excluded  = list(label = "EXCLUDED: ",       col = function(x) cli::style_bold(cli::col_red(x))),
  review    = list(label = "REVIEW: ",         col = function(x) cli::style_bold(cli::col_red(x))),
  done      = list(label = "",                 col = function(x) cli::col_green(x)),
  stop      = list(label = "",                 col = function(x) cli::style_bold(cli::col_red(x)))
)

## the labelled text of a message (as written to log files)
qc_text <- function(type, ...) {
  t <- .qc_msg_types[[type]]
  if (is.null(t)) t <- .qc_msg_types$progress
  paste0(t$label, paste0(...))
}

## write a labelled, coloured message to the console
qc_msg <- function(type, ...) {
  t <- .qc_msg_types[[type]]
  if (is.null(t)) t <- .qc_msg_types$progress
  message(t$col(qc_text(type, ...)))
}

## write a named vector of notes (names are the message types) to the console
qc_msg_notes <- function(notes) {
  if (length(notes) == 0) return(invisible(NULL))
  types <- names(notes)
  if (is.null(types)) types <- rep("progress", length(notes))
  for (i in seq_along(notes)) qc_msg(types[i], notes[[i]])
  invisible(NULL)
}
