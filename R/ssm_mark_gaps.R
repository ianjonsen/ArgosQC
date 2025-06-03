##' @title Identify SSM predicted & rerouted (if present) location estimates in data gaps
##'
##' @description Identify & mark SSM predicted & rerouted (if present) location
##' estimates in track segments with data gaps of a specified minimum duration.
##'
##' @param ssm the SSM fit object from `redo_multi_filter()`
##' @param min.gap the minimum data gap duration from which SSM estimates are
##' removed (in hours)
##' @param mark logical; should the SSM data be marked (TRUE; default), otherwise
##' the function does no marking and returns the original SSM fit object
##'
##' @examples
##'
##' @export


ssm_mark_gaps <- function(ssm,
         min.gap = 24,
         mark = TRUE) {

  if(inherits(ssm, "list")) {
    n <- length(ssm)
    out <- lapply(1:n, function(i) {
      gaps <- lapply(ssm[[i]]$ssm, function(x) {
        idx <- which(difftime(x$fitted$date,
                              lag(x$fitted$date),
                              units = "hours") > min.gap)
        if(length(idx) > 0) {
          as.POSIXct(sapply(1:length(idx),
                            function(i) x$fitted$date[(idx[i] - 1):idx[i]]),
                     origin = "1970-01-01", tz = "UTC")
        }
      })

      gaps.ptf <- lapply(1:nrow(ssm[[i]]), function(j) {
        keep <- rep(TRUE, nrow(ssm[[i]]$ssm[[j]]$predicted))

        if(length(gaps[[j]]) > 0) {
          for(k in seq(1, length(gaps[[j]]), by = 2)) {
            keep[ssm[[i]]$ssm[[j]]$predicted$date > gaps[[j]][k] & ssm[[i]]$ssm[[j]]$predicted$date < gaps[[j]][k+1]] <- FALSE
          }
        }
        keep
      })

      gaps.rtf <- lapply(1:nrow(ssm[[i]]), function(j) {
        if("rerouted" %in% names(ssm[[i]]$ssm[[j]])) {
          keep <- rep(TRUE, nrow(ssm[[i]]$ssm[[j]]$rerouted))

        if(length(gaps[[j]]) > 0) {
          for(k in seq(1, length(gaps[[j]]), by = 2)) {
            keep[ssm[[i]]$ssm[[j]]$rerouted$date > gaps[[j]][k] & ssm[[i]]$ssm[[j]]$rerouted$date < gaps[[j]][k+1]] <- FALSE
          }
        }
        keep
        } else {
          NA
        }
      })

      for(j in 1:nrow(ssm[[i]])) {
        ssm[[i]]$ssm[[j]]$predicted$keep <- gaps.ptf[[j]]
        if("rerouted" %in% names(ssm[[i]]$ssm[[j]])) {
          ssm[[i]]$ssm[[j]]$rerouted$keep <- gaps.rtf[[j]]
        }
      }
      ssm[[i]]
    })

    names(out) <- names(ssm)

  } else if(inherits(ssm, "ssm_df")) {
    gaps <- lapply(ssm$ssm, function(x) {
      idx <- which(difftime(x$fitted$date, lag(x$fitted$date), units = "hours") > gap)
      if(length(idx) > 0) {
        as.POSIXct(sapply(1:length(idx), function(i) x$fitted$date[(idx[i] - 1):idx[i]]), origin = "1970-01-01", tz = "UTC")
      }
    })

    gaps.tf <- lapply(1:nrow(ssm), function(i) {
      keep <- rep(TRUE, nrow(ssm$ssm[[i]]$predicted))
      if(length(gaps[[i]]) > 0) {
        for(j in seq(1, length(gaps[[i]]), by = 2)) {
          keep[ssm$ssm[[i]]$predicted$date > gaps[[i]][j] & ssm$ssm[[i]]$predicted$date < gaps[[i]][j+1]] <- FALSE
        }
      }
      keep
    })

    for(i in 1:nrow(ssm)) {
      ssm$ssm[[i]]$predicted$keep <- gaps.tf[[i]]
    }

    out <- ssm

  } else {
    stop("'ssm' must either be a single SSM fit object with class 'ssm_df' or a list of SSM fit objects")
  }


return(out)
}

