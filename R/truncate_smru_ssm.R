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

truncate_smru_ssm <- function(smru_ssm, meta, ref) {

  deploy_meta <- meta %>%
    select(device_id, ctd_end)

  trunc_smru_ssm <- smru_ssm

  ## right-truncate SSM-appended SMRU tables
  trunc_smru_ssm$diag <- smru$diag %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  trunc_smru_ssm$dive <- smru$dive %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  trunc_smru_ssm$haulout <- smru$haulout %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  trunc_smru_ssm$summary <- smru$summary %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)

  trunc_smru_ssm$ctd <- smru$ctd %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(date <= ctd_end) %>%
    select(-ctd_end)


  return(trunc_smru_ssm)
}
