test_that("Test that read.marker returns a data.frame", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- tempdir()
  #AutoTemp <- file.path(tmp, "Autospill")
  #if(!dir.exists(AutoTemp)){dir.create(AutoTemp)}
  #setwd(AutoTemp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  asp <- get.autospill.param("paper")

  flow.set.marker.table <- read.marker(control.dir=FolderPath,
     control.def.file=MetadataPath, asp=asp)

  expect_s3_class(flow.set.marker.table, "data.frame")
  expect_true("flow.set.marker.corrected" %in% colnames(flow.set.marker.table))
})