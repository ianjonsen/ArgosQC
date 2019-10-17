##' @title reconfigure annotated SMRU tables for AODN
##'
##' @description reconfigure annotated tables - subsample predicted locations to 6-h interval, write to .csv and zip by campaign id
##'
##' @param smru_ssm SSM-appended SMRU table file - output of \code{append_ssm}
##' @param fit final \code{foieGras} fit object
##' @param meta metadata
##' @param path path to write .csv files
##' @param drop.ids individual ids to be dropped
##'
##' @examples
##'
##' @importFrom dplyr filter rename mutate "%>%"
##' @importFrom stringr str_extract regex
##' @importFrom readr write_csv
##' @importFrom purrr walk
##' @importFrom foieGras grab
##'
##' @export

write_2_csv <- function(smru_ssm, fit, meta, path = "~/Dropbox/collab/imos/qc/r/output/aodn", drop.ids = NULL) {

  ## get predicted locations from fits
  p <- grab(fit, "predicted", as_sf = FALSE) %>%
    rename(ref = id) %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE)))

  p.lst <- split(p, p$ref)

  ## subsample predicted locs to 6-h resolution
  p_out <- lapply(p.lst, function(x) {
    ts <- subset(fit, id == x$ref[1])$ssm[[1]]$ts
    if (ts <= 6)
      x[seq(1, nrow(x), by = ceiling(6 / ts)),]
    else
      stop("time step is > 6 h, can't subsample to 6 h")
  }) %>% do.call(rbind, .)

  ## split by campaign id & write .csv files
  p_out %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "ssmoutputs"), "_", .x$cid[1], ".csv")))

  smru_ssm$diag %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "diag"), "_", .x$cid[1], ".csv")))

  smru_ssm$haulout %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "haulout"), "_", .x$cid[1], ".csv")))

  smru_ssm$ctd %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "ctd"), "_", .x$cid[1], ".csv")))

  smru_ssm$dive %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "dive"), "_", .x$cid[1], ".csv")))

  smru_ssm$ssummary %>%
    filter(!ref %in% drop.ids) %>%
    mutate(cid = str_extract(ref, "[a-z]{1,2}[0-9]{2,3}")) %>%
    split(., .$cid) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "summary"), "_", .x$cid[1], ".csv")))

  meta %>%
    filter(!device_id %in% drop.ids) %>%
    split(., .$sattag_program) %>%
    walk( ~ write_csv(.x, path = paste0(file.path(path, "metadata"), "_",
                                               .x$sattag_program[1], ".csv")))

  cat("\nwrite to `*.csv` completed")

}
