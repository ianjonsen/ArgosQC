##' @title Identify tags with Kinéis locations
##'
##' @description Flags, for each tag, whether its Doppler locations come from
##' the CLS Kinéis processing. Kinéis locations carry serially correlated
##' errors (CLS Kalman smoothing), so the `aniMotum` error ellipse scaling
##' parameter `psi` is not identifiable for them and must be fixed.
##'
##' @details Two data sources are recognised:
##' \describe{
##'   \item{SMRU}{SMRU encodes the Kinéis error radius as an error ellipse with
##'   semi-major axis equal to semi-minor axis and orientation 0. A tag is
##'   flagged when every location with error ellipse information has this
##'   circular form. Legacy Argos Kalman filter ellipses are never circular.}
##'   \item{Wildlife Computers}{the `Type` column of the Locations file is
##'   "Kineis" for Kinéis locations and "Argos" for Argos-Classic locations.
##'   A tag is flagged when any of its locations has `Type` "Kineis".}
##' }
##'
##' @param x a data.frame of locations
##' @param id name of the tag identifier column
##' @param source one of "smru" or "wc"
##'
##' @return a named logical vector, one element per tag
##'
##' @keywords internal

qc_kineis_tags <- function(x, id, source = c("smru", "wc")) {
  source <- match.arg(source)
  ids <- as.character(x[[id]])

  if (source == "smru") {
    if (!all(c("semi_major_axis", "semi_minor_axis", "ellipse_orientation") %in% names(x))) {
      out <- rep(FALSE, length(unique(ids)))
      names(out) <- unique(ids)
      return(out)
    }
    semi_major <- suppressWarnings(as.numeric(x$semi_major_axis))
    semi_minor <- suppressWarnings(as.numeric(x$semi_minor_axis))
    orientation <- suppressWarnings(as.numeric(x$ellipse_orientation))
    has_ellipse <- !is.na(semi_major) & !is.na(semi_minor) &
      !is.na(orientation) & semi_major > 0
    circular <- has_ellipse & semi_major == semi_minor & orientation == 0
    flags <- tapply(seq_along(ids), ids, function(i) {
      any(has_ellipse[i]) && all(circular[i][has_ellipse[i]])
    })

  } else {
    if (!"Type" %in% names(x)) {
      out <- rep(FALSE, length(unique(ids)))
      names(out) <- unique(ids)
      return(out)
    }
    ## accept "Kineis" or "Kin\u00e9is" in any case and any text encoding:
    ##  matched on bytes, the accented letter being one byte (Latin-1) or two
    ##  (UTF-8)
    is_kineis <- grepl("^kin.{1,2}is$", trimws(as.character(x$Type)),
                       ignore.case = TRUE, useBytes = TRUE)
    flags <- tapply(is_kineis, ids, any)
  }

  out <- as.logical(flags)
  names(out) <- names(flags)
  out
}

##' @title Decide, for each tag, whether to fix the aniMotum psi parameter
##'
##' @description `psi` is fixed for tags with Kinéis locations and estimated
##' otherwise, in both near real-time and delayed-mode QC. A `fix.psi` value
##' in the `model` block of the config file overrides this for all tags.
##'
##' @param ids tag identifiers, in the order of the tracks to be fitted
##' @param kineis named logical vector from `qc_kineis_tags()`
##' @param fix.psi optional logical from the config file
##'
##' @return a logical vector aligned with `ids`
##'
##' @keywords internal

qc_fix_psi <- function(ids, kineis, fix.psi = NULL) {
  ids <- as.character(ids)
  if (!is.null(fix.psi)) {
    if (!is.logical(fix.psi) || length(fix.psi) != 1 || is.na(fix.psi)) {
      stop("'fix.psi' in the config model block must be true or false", call. = FALSE)
    }
    return(rep(fix.psi, length(ids)))
  }
  out <- unname(kineis[ids])
  out[is.na(out)] <- FALSE
  out
}
