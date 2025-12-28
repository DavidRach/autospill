test_that("Test that check.critical still spots FALSE conditions", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- tempdir()
  #AutoTemp <- file.path(tmp, "Autospill")
  #if(!dir.exists(AutoTemp)){dir.create(AutoTemp)}
  #setwd(AutoTemp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  # Checking for Duplicates
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.scatter.parameter <- autospill:::read.scatter.parameter(asp)
  control <- read.csv(MetadataPath, stringsAsFactors = FALSE)
  expect_silent(
    autospill:::check.critical(!duplicated(control$filename), "Data contains duplicates 1")
  )
  control2 <- rbind(control, control)
  expect_error(
    autospill:::check.critical(!duplicated(control2$filename), "Data contains duplicates 2")
  )
})