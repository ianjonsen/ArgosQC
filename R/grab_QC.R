##' @title grab `tibble`'s by name from a `aniMotum` model object
##'
##' @description `grab_QC()` is the `ArgosQC` version of `aniMotum::grab()`. It lets
##' you obtain `fitted`, `predicted`, `rerouted` or `data` `tibble`'s from a
##' compound `tibble` created when fitting to multiple individual data sets. The
##' specified `tibble`'s are appended to a single output `tibble`. Unlike
##' `aniMotum::grab()`, `ArgosQC::grab_QC()` respects additional columns in the
##' predicted locations `tibble`, allowing a logical vector, `keep`, to be used
##' to delineate data gaps that may be associated with spurious predicted locations.
##'
##' @param x a `aniMotum` `ssm_df` or `mpm_df` model object
##' @param what the tibble to be grabbed; either `fitted`, `predicted`,
##' `rerouted` (`ssm_df` only), or `data` (single letters can be used).
##' @param cut logical; if TRUE then drop any predicted locations associated with a
##' `keep = FALSE` variable - ie. locations occurring within a data gap larger
##' than some user-specified threshold duration. default = FALSE
##' @param as_sf logical; if FALSE (default) then return a `tibble` with
##' un-projected longlat coordinates, otherwise return an `sf tibble`. Ignored
##' if x is an `mpm` model object.
##' @param normalise logical; if output includes a move persistence estimate,
##' should `g` (the move persistence index) be normalised to have minimum = 0 and
##' maximum = 1 (default = FALSE). Note, this normalisation is not applied to the
##' standard errors of the logit-scale move persistence estimates (`logit_g`,
##' `logit_g.se`).
##' @param group logical; should `g` be normalised among individuals as a group,
##' a 'relative g', or to individuals separately to highlight regions of lowest
##' and highest move persistence along single tracks (default = FALSE).
##'
##' @details if multiple `ssm_df` model objects are present in `x`, `as_sf = TRUE`,
##' and at least 1 estimated track has a coordinate reference system (`crs`) with
##' longitude centered on 180 (e.g. a track straddling -180,180) then all tracks
##' will be re-projected to that `crs`.
##'
##' @return a `tibble` with all individual `tibble`'s appended
##'
##' @importFrom sf st_crs st_coordinates st_transform st_geometry st_as_sf st_set_crs
##' @importFrom dplyr group_by mutate ungroup %>% bind_rows filter
##' @importFrom tibble as_tibble
##'
##'
##' @keywords internal

grab_QC <- function(x, what = "fitted", cut = FALSE, as_sf = FALSE, normalise = FALSE, group = FALSE) {

  what <- match.arg(what, choices = c("fitted","predicted","rerouted","data"))

  if(!any(inherits(x, "ssm_df"), inherits(x, "mpm_df"), inherits(x, "fG_ssm"), inherits(x, "fG_mpm")))
    stop("a aniMotum ssm or mpm model object must be supplied")
  if(!what %in% c("fitted","predicted","rerouted","data"))
    stop("only `fitted`, `predicted`, `rerouted`, or `data` objects can be grabbed from an ssm fit object")
  if(any(inherits(x, "mpm_df"), inherits(x, "fG_mpm")) & what == "predicted")
    stop("predicted values do not exist for `mpm` objects; use what = `fitted` instead")
  if(any(inherits(x, "ssm_df"), inherits(x, "fG_ssm"))) {
    if(any(sapply(x$ssm, function(.) is.na(.$ts))) && what == "predicted")
      stop("\n there are no predicted locations because you used time.step = NA when calling `fit_ssm`.
           \n Either grab `fitted` values or re-fit with a positive integer value for `time.step`")
  }

  ## coerce old aniMotum classes "fG_ssm" and "fG_mpm" to new classes
  if(inherits(x, "fG_ssm")) class(x)[1] <- "ssm_df"
  if(inherits(x, "fG_mpm")) class(x)[1] <- "mpm_df"

  switch(class(x)[1],
         ssm_df = {
           ## remove optimizer crash results from extraction
           nf <- which(sapply(x$ssm, length) < 15)
           if (length(nf) > 0) {
             sprintf("%d optimizer crashes removed from output", length(nf))
             sprintf("ids: %s", x[nf, "id"])
             x <- x[-nf,]
           }

           out_lst <- lapply(x$ssm, function(.) {
             x <-
               switch(
                 what,
                 fitted = .$fitted,
                 predicted = .$predicted,
                 rerouted = .$rerouted,
                 data = .$data
               )
             prj <- st_crs(x)
             xy <- as.data.frame(st_coordinates(x))
             names(xy) <- c("x", "y")
             ll <- st_transform(x, "+proj=longlat +datum=WGS84 +no_defs")
             ll <- as.data.frame(st_coordinates(ll))
             names(ll) <- c("lon", "lat")
             st_geometry(x) <- NULL
             cbind(x, xy, ll)
           })

           if (as_sf) {
             ## get crs from fit object x, allow for different crs' among individuals to handle -180,180; 0,360 wrapping
             prj <- lapply(x$ssm, function(.)
               switch(
                 what,
                 fitted = st_crs(.$fitted),
                 predicted = st_crs(.$predicted),
                 rerouted = st_crs(.$rerouted),
                 data = st_crs(.$data)
               ))

             if (nrow(x) > 1) {
               ## test if prj's are all identical
               tmp <- sapply(prj, function(.)
                 .[[1]])
               test <- sapply(2:length(tmp), function(i) {
                 identical(tmp[i - 1], tmp[i])
               })
               if (!any(test)) {
                 pos <- grep("lon_0=180", tmp)
                 if (length(pos) == 0) {
                   pos <- 1
                 } else {
                   cat(
                     "At least 1 model fit has locations centred on lon = 180, reprojecting all locations to that crs\n"
                   )
                 }

                 out <- lapply(1:length(out_lst), function(i) {
                   st_as_sf(out_lst[[i]], coords = c("lon", "lat")) %>%
                     st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
                     st_transform(prj[[pos]])
                 })
                 out <- bind_rows(out)

               } else if (any(test)) {
                 out <- lapply(1:length(out_lst), function(i) {
                   if(nrow(out_lst[[i]]) >= 1){
                    st_as_sf(out_lst[[i]], coords = c("lon", "lat")) %>%
                      st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
                       st_transform(prj[[1]])
                   }
                 })
                 out <- bind_rows(out)

               }
             } else if (nrow(x) == 1) {
               out <- st_as_sf(out_lst[[1]], coords = c("lon", "lat")) %>%
                 st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
                 st_transform(prj[[1]])
             }

             if (what %in% c("fitted","predicted")) {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = {
                   if("keep" %in% names(out)) out[, c("id", "date", "x.se", "y.se", "keep", "geometry")]
                   else out[, c("id", "date", "x.se", "y.se", "geometry")]
                   },
                 crw = {
                   ## deal w fit objects from <= 0.6-9, which don't contain s, s.se
                   if(all(c("s","s.se") %in% names(out))) {
                     if("keep" %in% names(out)) {
                      out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se",
                             "y.se", "s", "s.se", "keep", "geometry")]
                     } else {
                       out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se",
                               "y.se", "s", "s.se", "geometry")]
                     }
                     } else {
                       if("keep" %in% names(out)) {
                         out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se",
                                 "y.se", "keep", "geometry")]
                       } else {
                        out[, c("id", "date", "u", "v", "u.se", "v.se", "x.se",
                             "y.se", "geometry")]
                       }
                       }
                   },
                 mp = {
                   if("keep" %in% names(out)) {
                     out <- out[, c("id", "date", "x.se", "y.se", "logit_g", "logit_g.se",
                                    "g", "keep", "geometry")]
                   } else {
                      out <- out[, c("id", "date", "x.se", "y.se", "logit_g", "logit_g.se",
                           "g", "geometry")]
                   }
                   if(normalise & !group) {
                     out <- out %>%
                       group_by(id) %>%
                       mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                       ungroup()
                     } else if(normalise & group) {
                     out <- out %>%
                       mutate(g = (g - min(g))/(max(g) - min(g)))
                   }
                   out
                 })

             } else if (what == "rerouted") {
               if("g" %in% names(out)) {
                 if("keep" %in% names(out)) {
                   out <- out[, c("id", "date", "x.se", "y.se", "logit_g",
                                  "logit_g.se", "g", "keep", "geometry")]
                 } else {
                   out <- out[, c("id", "date", "x.se", "y.se", "logit_g",
                                "logit_g.se", "g", "geometry")]
                 }
                 if(normalise & !group) {
                   out <- out %>%
                     group_by(id) %>%
                     mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                     ungroup()
                 } else if(normalise & group) {
                   out <- out %>%
                     mutate(g = (g - min(g))/(max(g) - min(g)))
                 }
               } else {
                 if("keep" %in% names(out)) {
                  out <- out[, c("id", "date", "x.se", "y.se", "keep", "geometry")]
                 } else {
                   out <- out[, c("id", "date", "x.se", "y.se", "geometry")]
                 }
               }
             } else if (what == "data") {
               out <- out[, c("id", "date", "lc", "smaj", "smin", "eor", "keep",
                         "obs.type", "emf.x", "emf.y", "geometry")]
             }

            } else if (!as_sf) {
             out <- bind_rows(out_lst)
             if (what %in% c("fitted","predicted")) {
               out <- switch(
                 x$ssm[[1]]$pm,
                 rw = {
                   if("keep" %in% names(out)) {
                      out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se", "keep")]
                   } else {
                     out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se")]
                   }
                   },
                 crw = {
                   if(all(c("s","s.se") %in% names(out))) {
                     if("keep" %in% names(out)) {
                        out[, c("id", "date", "lon", "lat", "x", "y", "x.se",
                             "y.se", "u", "v", "u.se", "v.se", "s", "s.se", "keep")]
                     } else {
                       out[, c("id", "date", "lon", "lat", "x", "y", "x.se",
                               "y.se", "u", "v", "u.se", "v.se", "s", "s.se")]
                     }
                     } else {
                       if("keep" %in% names(out)) {
                          out[, c("id", "date", "lon", "lat", "x", "y",
                              "x.se", "y.se", "u", "v", "u.se", "v.se", "keep")]
                       } else {
                          out[, c("id", "date", "lon", "lat", "x", "y",
                                 "x.se", "y.se", "u", "v", "u.se", "v.se")]
                       }
                       }
                   },
                 mp = {
                   if("keep" %in% names(out)) {
                      out <- out[, c("id", "date", "lon", "lat", "x", "y",
                            "x.se", "y.se", "logit_g", "logit_g.se", "g", "keep")]
                   } else {
                     out <- out[, c("id", "date", "lon", "lat", "x", "y",
                                    "x.se", "y.se", "logit_g", "logit_g.se", "g")]
                   }

                    if(normalise & !group) {
                      out <- out %>%
                        group_by(id) %>%
                        mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                        ungroup()
                    } else if(normalise & group) {
                      out <- out %>%
                        mutate(g = (g - min(g))/(max(g) - min(g)))
                    }
                    out
                 })

               out <- as_tibble(out)
               if(cut & what != "data") out <- out %>% filter(keep)

             } else if (what == "rerouted") {
               if("g" %in% names(out)) {
                 if("keep" %in% names(out)) {
                    out <- out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se", "logit_g",
                               "logit_g.se", "g", "keep")]
                 } else {
                   out <- out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se", "logit_g",
                                  "logit_g.se", "g")]
                 }
                if(normalise & !group) {
                  out <- out %>%
                    group_by(id) %>%
                    mutate(g = (g - min(g))/(max(g) - min(g))) %>%
                    ungroup()
                } else if(normalise & group) {
                  out <- out %>%
                    mutate(g = (g - min(g))/(max(g) - min(g)))
                }
               } else {
                 if("keep" %in% names(out)) {
                    out <- out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se", "keep")]
                 } else {
                   out <- out[, c("id", "date", "lon", "lat", "x", "y", "x.se", "y.se")]
                 }
               }
               out <- as_tibble(out)
               if(cut & what != "data") out <- out %>% filter(keep)
             } else if (what == "data") {
               out <- out[, c("id", "date", "lc", "lon", "lat",
                              "smaj", "smin", "eor", "obs.type", "keep",
                              "x", "y", "emf.x", "emf.y")]
             }
            }

           if(!inherits(out, "sf")) {
             out <- as_tibble(out)
             if(cut & what != "data") out <- out %>% filter(keep)
           }
           else {
             ## coerce from tibble to data.frame so row.names can be set to 1:nrow(out)
             if(cut & what != "data") out <- out %>% filter(keep)
             class(out) <- c("sf", "data.frame")
             row.names(out) <- 1:nrow(out)
             class(out) <- c("sf", "tbl_df", "tbl", "data.frame")
           }
         },
         mpm_df = {
           ## remove optimiser crash results from extraction
           nf <- which(sapply(x$mpm, length) < 8)
           if (length(nf) > 0) {
             sprintf("%d optimiser crashes removed from output", length(nf))
             sprintf("ids: %s", x[nf, "id"])
             x <- x[-nf,]
           }

           out <- lapply(x$mpm, function(.) {
             x <- switch(what, fitted = .$fitted, data = .$data)
             })
           out <- bind_rows(out)
           out <- as_tibble(out)
           if(normalise & !group) {
             out <- out %>%
               group_by(id) %>%
               mutate(g = (g - min(g))/(max(g) - min(g))) %>%
               ungroup()
           } else if(normalise & group) {
             out <- out %>%
               mutate(g = (g - min(g))/(max(g) - min(g)))
           }
           out
         })

  return(out)

}
