test_that("provisional metadata files keep the AODN columns and round-trip", {
  d <- withr::local_tempdir()
  rows <- data.frame(sattag_program = "ct999", device_id = c("ct999-001-26", "ct999-002-26"),
                     ptt = c(1L, 2L), body = c(10L, 20L), device_wmo_ref = c("Q1", NA),
                     tag_type = "CTD", common_name = "southern elephant seal",
                     species = "Mirounga leonina", release_longitude = c(70.2, 70.3),
                     release_latitude = c(-49.3, -49.4), release_site = "Iles Kerguelen",
                     release_date = as.POSIXct(c("2026-01-02 03:04:05", NA), tz = "UTC"),
                     recovery_date = as.POSIXct(NA, tz = "UTC"), age_class = NA_character_,
                     sex = NA_character_, length = NA_real_, estimated_mass = NA_integer_,
                     actual_mass = NA_real_, state_country = "French Overseas Territory",
                     t0_status = c("final", "provisional"), t0_source = "calculated from the locations",
                     t0_n_tests = 0L, t0_note = NA_character_, t0_days_after = c(30, NA),
                     prov_source = "SMRU server",
                     prov_refreshed = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
                     prov_written = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
                     prov_argosqc_version = "0.10.1")
  prov_write(rows, d)
  f <- file.path(d, "ct999_metadata_provisional.csv")
  expect_true(file.exists(f))
  expect_identical(names(readr::read_csv(f, show_col_types = FALSE))[1:19], .prov_aodn_cols)
  x <- prov_read(d, "ct999")
  expect_equal(x$release_date, rows$release_date)
  expect_type(x$ptt, "integer")
  expect_equal(nrow(prov_fixed_starts(x)), 1)

  ## a value of the wrong type stops with an error naming the column
  y <- readr::read_csv(f, col_types = readr::cols(.default = "c"))
  y$release_latitude[1] <- "abc"
  readr::write_csv(y, f, na = "")
  expect_error(prov_read(d, "ct999"), "release_latitude")
})

test_that("Wildlife Computers provisional metadata files round-trip", {
  d <- withr::local_tempdir()
  rows <- data.frame(DeploymentID = c("abc", "def"), sattag_program = "4135", tag_ptt = "1",
                     deploy_date = as.POSIXct(c("2024-06-21 01:53:00", NA), tz = "UTC"),
                     deploy_longitude = c(152.5, NA), deploy_latitude = c(-32.2, NA),
                     embark_date = as.POSIXct(NA, tz = "UTC"), common_name = "loggerhead turtle",
                     t0_date = as.POSIXct(c("2024-06-21 01:53:00", NA), tz = "UTC"),
                     t0_latitude = c(-32.2, NA), t0_longitude = c(152.5, NA),
                     t0_status = c("final", "provisional"), t0_source = NA_character_,
                     t0_n_tests = NA_integer_, t0_note = NA_character_, t0_days_after = c(30, NA),
                     prov_source = "Wildlife Computers Portal",
                     prov_refreshed = as.POSIXct("2024-06-22", tz = "UTC"),
                     prov_written = as.POSIXct("2024-06-22", tz = "UTC"),
                     prov_argosqc_version = "0.10.1")
  wc_prov_write(rows, d, "test_wc")
  x <- wc_prov_read(d, "test_wc")
  expect_equal(x$t0_date, rows$t0_date)
  expect_identical(names(x)[1:8], names(rows)[1:8])
  expect_equal(nrow(wc_prov_fixed_starts(x)), 1)
  expect_identical(wc_prov_name("/a/b/config_lhtu_wc.json"), "lhtu_wc")
})
