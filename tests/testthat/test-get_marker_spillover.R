test_that("Test that get.marker spillover returns a spillover matrixe", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)
  
  flow.gate <- suppressWarnings(
    gate.flow.data(flow.control=flow.control, asp=asp)
  )

  marker.spillover.unco.untr <- get.marker.spillover(scale.untransformed=TRUE, flow.gate=flow.gate,
    flow.control=flow.control, asp=asp)

  expect_type(marker.spillover.unco.untr, "list")
  expect_named(marker.spillover.unco.untr)
  expect_true(is.numeric(marker.spillover.unco.untr[[1]][1]))
})