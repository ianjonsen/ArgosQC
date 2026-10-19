## Near real-time QC (internal): stop if no tag in the campaign has transmitted
##  for more than `inactive.days`. The last transmission is the latest Argos
##  location time (diag d_date) or GPS fix time (gps d_date) of any deployment
##  not in dropIDs. Records dated more than 1 day in the future are ignored as
##  misdated. Called by smru_qc() after the tag data are read, so an inactive
##  campaign stops before the metadata are built and the SSM is fitted.
smru_check_active <- function(smru,
                              cid,
                              config,
                              dropIDs = NULL,
                              inactive.days = 7,
                              now = Sys.time()) {

  last_times <- function(tab) {
    if (!tab %in% names(smru) || is.null(smru[[tab]]) || nrow(smru[[tab]]) == 0 ||
        !all(c("ref", "d_date") %in% names(smru[[tab]]))) {
      return(data.frame(ref = character(0), t = as.POSIXct(character(0), tz = "UTC")))
    }
    d <- data.frame(ref = as.character(smru[[tab]]$ref),
                    t = as.POSIXct(smru[[tab]]$d_date, tz = "UTC"))
    ## records dated more than 1 day in the future are misdated: ignored
    d[!is.na(d$t) & !d$ref %in% dropIDs & d$t <= now + 86400, ]
  }
  tx <- rbind(last_times("diag"), last_times("gps"))
  cid <- paste(cid, collapse = ", ")

  if (nrow(tx) == 0) {
    stop("NRT QC stopped for ", cid, ": the tag data contain no Argos locations or GPS fixes.",
         call. = FALSE)
  }

  i <- which.max(as.numeric(tx$t))
  last_t <- tx$t[i]
  days <- as.numeric(difftime(now, last_t, units = "days"))

  if (days > inactive.days) {
    stop(sprintf(paste0("NRT QC stopped for %s: no tag has transmitted for %.1f days ",
                        "(last transmission %s UTC, %s). The limit is %g days; increase ",
                        "'inactive.days' in the model block of %s to run the QC anyway."),
                 cid, days, format(last_t, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tx$ref[i],
                 inactive.days, config),
         call. = FALSE)
  }

  message(sprintf("Last transmission %s UTC (%s), %.1f days ago.",
                  format(last_t, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tx$ref[i], days))
  invisible(last_t)
}
