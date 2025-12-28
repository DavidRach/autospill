test_that("Test that get.worker.process returns a ", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  Value <- get.worker.process(asp$worker.process.n)

  expect_type(Value, "double")
  expect_true(Value > 0)
})