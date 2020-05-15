##' @title right-truncate all SSM-appended SMRU tables using CTD end date for given individuals
##'
##' @description right-truncate all SSM-appended SMRU tables using CTD end date for given individuals, using CTD date-times from metadata
##'
##' @param smru_ssm SSM-appended SMRU file to use
##' @param meta metadata used to truncate SSM-appended SMRU tables for each individual
##' @param ref device_id's (SMRU ref's) to apply truncation
##'
##' @examples
##'
##' @importFrom dplyr select filter left_join "%>%"
##'
##' @export
##'

truncate_smru_ssm <- function(smru_ssm, meta, refs) {

  deploy_meta <- meta %>%
    select(device_id, ctd_end)



  ## right-truncate SSM-appended SMRU tables
  tsmru_diag <- smru_ssm$diag %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_dive <- smru_ssm$dive %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_haulout <- smru_ssm$haulout %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_summary <- trunc_smru_ssm$summary %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_ctd <- trunc_smru_ssm$ctd %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_ssm <- list(diag = tsmru_diag, ctd = tsmru_ctd, dive = tsmru_dive, haulout = tsmru_haulout, summary = tsmru_summary)

  return(tsmru_ssm)
}
