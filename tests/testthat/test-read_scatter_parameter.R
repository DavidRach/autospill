test_that("Test that read.scatter.parameter returns FSC and SSC", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.scatter.parameter <- read.scatter.parameter(asp)

  expect_type(flow.scatter.parameter, "character")
  expect_equal(flow.scatter.parameter, c("FSC-A", "SSC-A"))
})