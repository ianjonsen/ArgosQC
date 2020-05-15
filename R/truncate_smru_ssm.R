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
    filter(ref %in% refs) %>%
    filter(d.date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_dive <- smru_ssm$dive %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(ref %in% refs) %>%
    filter(de.date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_haulout <- smru_ssm$haulout %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(ref %in% refs) %>%
    filter(s.date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_summary <- smru_ssm$ssummary %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(ref %in% refs) %>%
    filter(e.date <= ctd_end) %>%
    select(-ctd_end)

  tsmru_ctd <- smru_ssm$ctd %>%
    left_join(., deploy_meta, by = c("ref" = "device_id")) %>%
    filter(ref %in% refs) %>%
    filter(end.date <= ctd_end) %>%
    select(-ctd_end)

  ssd <- split(smru_ssm$diag, smru_ssm$diag$ref)
  ssd[names(ssd) %in% refs] <- split(tsmru_diag, tsmru_diag$ref)
  ssd <- ssd %>% do.call(rbind, .)

  ssc <- split(smru_ssm$ctd, smru_ssm$ctd$ref)
  ssc[names(ssc) %in% refs] <- split(tsmru_ctd, tsmru_ctd$ref)
  ssc <- ssc %>% do.call(rbind, .)

  ssdi <- split(smru_ssm$dive, smru_ssm$dive$ref)
  ssdi[names(ssdi) %in% refs] <- split(tsmru_dive, tsmru_dive$ref)
  ssdi <- ssdi %>% do.call(rbind, .)

  ssh <- split(smru_ssm$haulout, smru_ssm$haulout$ref)
  ssh[names(ssh) %in% refs] <- split(tsmru_haulout, tsmru_haulout$ref)
  ssh <- ssh %>% do.call(rbind, .)

  sss <- split(smru_ssm$ssummary, smru_ssm$ssummary$ref)
  sss[names(sss) %in% refs] <- split(tsmru_summary, tsmru_summary$ref)
  sss <- sss %>% do.call(rbind, .)


  tsmru_ssm <- list(diag = ssd, ctd = ssc, dive = ssdi, haulout = ssh, ssummary = sss)

  return(tsmru_ssm)
}
