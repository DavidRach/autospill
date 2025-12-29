test_that("Test that refine spillover returns it's list", {
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
  
  refine.spillover.result <- refine.spillover( marker.spillover.unco.untr,
    marker.spillover.unco.tran, flow.gate, flow.control, asp )

  expect_type(compensation.error, "list")
  expect_named(compensation.error, c("spillover", "compensation",
   "error", "convergence"), ignore.order = TRUE)
})