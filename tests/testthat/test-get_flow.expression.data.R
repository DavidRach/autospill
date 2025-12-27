test_that("Test that get.flow.expression.data returns a", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper")
  flow.scatter.parameter <- read.scatter.parameter( asp )
})