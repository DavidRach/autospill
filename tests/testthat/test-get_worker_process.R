test_that("Test that get.worker.process returns a ", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper")
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)
  
  flow.gate <- gate.flow.data(flow.control=flow.control, asp=asp)

  expect_type(flow.gate, "list")
  expect_named(flow.gate)
  expect_true(is.numeric(flow.gate[[1]][1]))
})