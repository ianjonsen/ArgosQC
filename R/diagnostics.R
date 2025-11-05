##' @title run QC diagnostics for WC or SMRU tag workflows
##'
##' @description produces a map of all QC'd tracks and generates various diagnostics to assess QC run
##'
##' @param fit the final aniMotum fit object from QC process
##' @param fit1 the initial aniMotum fit object from QC process
##' @param what the SSM-estimated or rerouted locations to be used
##' @param cut logical; should predicted locations be dropped if keep = FALSE - ie. in a large data gap
##' @param data the standardized WC Locations or SMRU diag file (prior to truncation by metadata CTD start and end dates)
##' @param ssm the ssm-annotated WC/SMRU tables
##' @param meta metadata
##' @param lines add track lines to map (default = FALSE)
##' @param obs add observed locations to map (default = FALSE)
##' @param mpath path to write map file
##' @param dpath path to write all other diagnostic files
##' @param QCmode specify whether QC is near real-time (nrt) or delayed-mode (dm),
##' in latter case start end end of dive data are displayed rather than ctd data.
##' @param tag_mfr the tag manufacturer. Currently, only `smru` or `wc` are supported.
##' @param cid SMRU campaign id (from config file). Ignored if WC data is used.
##'
##'
##' @importFrom dplyr %>% group_by summarise pull
##' @importFrom sf st_as_sf st_transform st_cast st_bbox
##' @importFrom rnaturalearth ne_countries
##' @importFrom ggplot2 ggplot geom_sf geom_point geom_rect facet_wrap aes
##' @importFrom ggplot2 vars theme_minimal xlim ylim ggsave
##' @importFrom lubridate decimal_date
##' @importFrom aniMotum aes_lst
##' @importFrom kableExtra kable kable_styling
##' @importFrom readr write_csv
##'
##' @export
##'

diagnostics <-
  function(fit,
           fit1,
           what = "p",
           cut,
           data,
           ssm,
           meta,
           lines = FALSE,
           obs = FALSE,
           mpath = NULL,
           dpath = NULL,
           QCmode = "nrt",
           tag_mfr = "wc",
           cid = NULL) {

    if(is.null(mpath)) stop("A valid file path for the map must be provided")
    if(is.null(dpath)) stop("A valid file path for the diagnostics must be provided")

    my.aes <- aes_lst(conf = FALSE,
                      line = ifelse(lines, TRUE, FALSE),
                      obs = ifelse(obs, TRUE, FALSE))
    my.aes$df$size[1] <- 0.5

    if(obs) {
      my.aes$df$size[4] <- 0.75
      my.aes$df$col[4] <- "darkorange"
      my.aes$df$alpha[4] <- 0.5
    }


    if (inherits(fit, "list")) {
      n <- length(fit)

      flocs <- lapply(1:n, function(i) {
        locs <- grab_QC(fit[[i]], what = what, as_sf = TRUE)
        end.locs <- locs |>
          group_by(id) |>
          summarise(geometry = geometry[n(), ])

        flocs <- grab_QC(fit[[i]], what = "f")

        suppressMessages(map_QC(
          fit[[i]],
          what = what,
          by.id = FALSE,
          cut = cut,
          aes = my.aes
        ) +
          geom_sf(
            data = end.locs,
            size = 0.75,
            colour = "firebrick",
            inherit.aes = TRUE
          ) +
          coord_sf(xlim = st_bbox(locs)[c(1, 3)], ylim = st_bbox(locs)[c(2, 4)]) +
          theme_minimal())

        if(!is.null(cid)) {
          ggsave(
            file.path(mpath, paste0(
              "map_", cid, "_", Sys.Date(), ".png"
            )),
            width = 8,
            height = 10,
            units = "in",
            dpi = 300,
            bg = "white"
          )

        } else {
          ggsave(
            file.path(mpath, paste0(
              "map_", cid, "_", Sys.Date(), ".png"
            )),
            width = 8,
            height = 10,
            units = "in",
            dpi = 300,
            bg = "white"
          )
        }


        return(flocs)
      }) |>
        bind_rows()


    } else if(inherits(fit, "ssm_df")) {
      locs <- grab_QC(fit, what = what, as_sf = TRUE)
      end.locs <- locs |>
        group_by(id) |>
        summarise(geometry = geometry[n(), ])

      flocs <- grab_QC(fit, what = "f")

      suppressMessages(map_QC(
        fit,
        what = what,
        by.id = FALSE,
        cut = cut,
        aes = my.aes
      ) +
        geom_sf(
          data = end.locs,
          size = 0.75,
          colour = "firebrick"
        ) +
        coord_sf(xlim = st_bbox(locs)[c(1, 3)],
                 ylim = st_bbox(locs)[c(2, 4)]) +
        theme_minimal())

      if(!is.null(cid)) {
      ggsave(
        file.path(mpath, paste0(
          "map_", cid, "_", Sys.Date(), ".png"
        )),
        width = 8,
        height = 10,
        units = "in",
        dpi = 300,
        bg = "white"
      )
      } else {
        ggsave(
          file.path(mpath, paste0(
            "map_", Sys.Date(), ".png"
          )),
          width = 8,
          height = 10,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }

    }

    ## generate separate lon & lat coverage plots by individual
    ##  obs + estimates (fitted)

    if("DeploymentID" %in% names(meta)) {
      meta <- meta |>
        rename(device_id = DeploymentID)
    }

    if("DeploymentID" %in% names(data)) {
      olocs <- data |>
        rename(device_id = DeploymentID,
               lat = Latitude,
               lon = Longitude,
               date = Date)

    } else {
      olocs <- data |>
        rename(device_id = ref)
    }

    flocs <- flocs |>
      rename(device_id = id)

    dd <-
      olocs |>
      group_by(device_id) |>
      summarise(start_date = min(date), end_date = max(date))

    meta <- left_join(meta, dd, by = "device_id")

    ## Latitude coverage plots
    p.lat <- suppressWarnings(ggplot(olocs) +
      geom_point(aes(date, lat), col = "blue") +
      geom_point(data = flocs,
                 aes(date, lat),
                 size = 0.25,
                 col = 'red') +
      facet_wrap(vars(device_id),
                 scales = "free",
                 ncol = 6)
    )

    if(QCmode == "nrt" & tag_mfr == "smru") {
      p.lat <- suppressWarnings(p.lat + geom_rect(
        data = meta |> filter(!is.na(start_date),
                              !is.na(ctd_start)),
        aes(
          xmin = start_date,
          xmax = ctd_start,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.5,
        fill = grey(0.1),
        colour = NA
      ) +
        geom_rect(
          data = meta |> filter(!is.na(end_date),
                                !is.na(ctd_end)),
          aes(
            xmin = ctd_end,
            xmax = end_date,
            ymin = -Inf,
            ymax = Inf
          ),
          alpha = 0.5,
          fill = grey(0.1),
          colour = NA
        ))

    } else if(QCmode == "dm" | tag_mfr == "wc") {

      p.lat <- suppressWarnings(p.lat + geom_rect(
        data = meta |> filter(!is.na(start_date),
                              !is.na(QC_start_date)),
        aes(
          xmin = start_date,
          xmax = QC_start_date,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.5,
        fill = grey(0.1),
        colour = NA
      ) +
        geom_rect(
          data = meta |> filter(!is.na(end_date),
                                !is.na(QC_end_date)),
          aes(
            xmin = QC_end_date,
            xmax = end_date,
            ymin = -Inf,
            ymax = Inf
          ),
          alpha = 0.5,
          fill = grey(0.1),
          colour = NA
        ))
    }

    if(!is.null(cid)) {
      suppressWarnings(ggsave(
        file.path(dpath,
                  paste0("lat_coverage_", cid, ".jpg")),
        plot = p.lat,
        width = 15,
        height = 20,
        units = "in",
        dpi = 150
      ))

    } else {
      suppressWarnings(ggsave(
        file.path(dpath,
                  paste0("lat_coverage_", ".jpg")),
        plot = p.lat,
        width = 15,
        height = 20,
        units = "in",
        dpi = 150
      ))
    }



    ## Longitude coverage plots
    p.lon <- suppressWarnings(ggplot(olocs) +
      geom_point(aes(date, lon), col = "blue") +
      geom_point(data = flocs,
                 aes(date, lon),
                 size = 0.25,
                 col = 'red') +
        facet_wrap(vars(device_id),
          scales = "free",
          ncol = 6
        ))

    if(QCmode == "nrt" & tag_mfr == "smru") {
      p.lon <- suppressWarnings(p.lon + geom_rect(
        data = meta |> filter(!is.na(start_date),
                              !is.na(ctd_start)),
        aes(
          xmin = start_date,
          xmax = ctd_start,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.5,
        fill = grey(0.1),
        colour = NA
      ) +
        geom_rect(
          data = meta |> filter(!is.na(end_date),
                                !is.na(ctd_end)),
          aes(
            xmin = ctd_end,
            xmax = end_date,
            ymin = -Inf,
            ymax = Inf
          ),
          alpha = 0.5,
          fill = grey(0.1),
          colour = NA
        ))

    } else if(QCmode == "dm" | tag_mfr == "wc") {
      p.lon <- suppressWarnings(p.lon + geom_rect(
        data = meta |> filter(!is.na(start_date),
                              !is.na(QC_start_date)),
        aes(
          xmin = start_date,
          xmax = QC_start_date,
          ymin = -Inf,
          ymax = Inf
        ),
        alpha = 0.5,
        fill = grey(0.1),
        colour = NA
      ) +
        geom_rect(
          data = meta |> filter(!is.na(end_date),
                                !is.na(QC_end_date)),
          aes(
            xmin = QC_end_date,
            xmax = end_date,
            ymin = -Inf,
            ymax = Inf
          ),
          alpha = 0.5,
          fill = grey(0.1),
          colour = NA
        ))
    }

    if (!is.null(cid)) {
      suppressWarnings(ggsave(
        file.path(dpath, paste0("lon_coverage_", cid, ".jpg")),
        plot = p.lon,
        width = 15,
        height = 20,
        units = "in",
        dpi = 150
      ))
    } else {
      suppressWarnings(ggsave(
        file.path(dpath, paste0("lon_coverage_", ".jpg")),
        plot = p.lon,
        width = 15,
        height = 20,
        units = "in",
        dpi = 150
      ))
    }
  }

