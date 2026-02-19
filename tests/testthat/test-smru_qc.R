test_that("smru_qc works", {
  ## use ct185 - single tag deployment to keep test time as short as possible
  ##  downloads data from SMRU server
  ##  Perhaps turn off download_data to further speed up testing?

  ## specify mdbtools location if running on Mac
  smru_qc(".", "config_ct185.json")
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_metadata_ct185_nrt.csv"))
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_ctd_ct185_nrt.csv"))
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_diag_ct185_nrt.csv"))
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_ssmoutputs_ct185_nrt.csv"))
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_summary_ct185_nrt.csv"))
  expect_true(file.exists("test/IMOS_ATF-SATTAG_Location-QC_haulout_ct185_nrt.csv"))
  expect_true(file.exists("test/lat_coverage_ct185.jpg"))
  expect_true(file.exists("test/lon_coverage_ct185.jpg"))
  expect_true(file.exists(paste0("test/map_ct185_", Sys.Date(), ".png")))
  expect_true(file.exists("test/ct185.mdb"))

  ## clean up
  unlink("test", recursive = TRUE)
})
