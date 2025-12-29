test_that("Test that process_posnegpop returns its outputs", {
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
  
  spillover.error.posnegpop <- process.posnegpop(marker.spillover.unco.untr,
    flow.gate, flow.control, asp )

  expect_type(spillover.error.posnegpop, "list")
  expect_named(
    spillover.error.posnegpop,
    c("spillover", "compensation"),
    ignore.order = TRUE
  )
})