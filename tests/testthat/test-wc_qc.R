test_that("wc_qc works", {
  ## use single IRAP turtle deployment to keep test time as short as possible
  ##  downloads data from WC Data Portal
  ##  Perhaps turn off download_data to further speed up testing?



  ## specify mdbtools location if running on Mac
  if(Sys.info()[1] == "Darwin") wc_qc(".", "config_lhtu_wc.json")
  else wc_qc(".", "config_lhtu_wc.json")
  expect_true(file.exists("test/metadata_loggerhead turtle_nrt.csv"))
  expect_true(file.exists("test/Locations_loggerhead turtle_nrt.csv"))
  expect_true(file.exists("test/FastGPS_loggerhead turtle_nrt.csv"))
  expect_true(file.exists("test/ssmoutputs_loggerhead turtle_nrt.csv"))
  expect_true(file.exists("test/Histos_loggerhead turtle_nrt.csv"))
  expect_true(file.exists("test/MinMaxDepth_loggerhead turtle_nrt.csv"))
  #expect_true(file.exists("test/SST_loggerhead turtle_nrt.csv"))
  #expect_true(file.exists("test/lat_coverage_ct185.jpg"))
  #expect_true(file.exists("test/lon_coverage_ct185.jpg"))
  #expect_true(file.exists(paste0("test/map_ct185_", Sys.Date(), ".png")))
  #expect_true(file.exists("test/ct185.mdb"))

  ## clean up
  unlink("test", recursive = TRUE)
})
