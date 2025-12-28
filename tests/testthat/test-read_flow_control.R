test_that("Test that read.flow.control returns transform list", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)

  expect_type(flow.control, "list")
  expect_true("antigen" %in% names(flow.control))
})