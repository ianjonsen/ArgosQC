test_that("smru_qc works", {
  ## use ct185 - single tag deployment to keep test time as short as possible
  ##  downloads data from SMRU server
  ##  Perhaps turn off download_data to further speed up testing?

  ## specify mdbtools location if running on Mac
  if(Sys.info()[1] == "Darwin") smru_qc(".", "config_ct185_mac.json")
  else smru_qc(".", "config_ct185.json")
  expect_true(file.exists("test/metadata_ct185_nrt.csv"))
  expect_true(file.exists("test/ctd_ct185_nrt.csv"))
  expect_true(file.exists("test/diag_ct185_nrt.csv"))
  expect_true(file.exists("test/ssmoutputs_ct185_nrt.csv"))
  expect_true(file.exists("test/summary_ct185_nrt.csv"))
  expect_true(file.exists("test/haulout_ct185_nrt.csv"))
  expect_true(file.exists("test/lat_coverage_ct185.jpg"))
  expect_true(file.exists("test/lon_coverage_ct185.jpg"))
  expect_true(file.exists(paste0("test/map_ct185_", Sys.Date(), ".png")))
  expect_true(file.exists("test/ct185.mdb"))

  ## clean up
  unlink("test", recursive = TRUE)
})
