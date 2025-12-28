test_that("Test that read.gate.parameter returns gating specifications", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- tempdir()
  #AutoTemp <- file.path(tmp, "Autospill")
  #if(!dir.exists(AutoTemp)){dir.create(AutoTemp)}
  #setwd(AutoTemp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  asp <- get.autospill.param("paper", outpath=tmp)
  flow.scatter.parameter <- read.scatter.parameter(asp)
  Return <- FlowSetReturn(control.dir=FolderPath, control.def.file=MetadataPath, 
    asp=asp, flow.scatter.parameter = flow.scatter.parameter)
  
  flow.control <- Return[[2]]
  expect_true("scatter.and.marker.label" %in% names(flow.control))

  flow.gate.parameter <- read.gate.parameter(flow.control, asp)
  gp <- flow.gate.parameter[[1]]

    expect_type(gp, "list")
    expect_named(
      gp,
      c(
        "density.threshold",
        "region.auto",
        "region.factor.x.low",
        "region.factor.x.high",
        "region.factor.y.low",
        "region.factor.y.high"
      )
    )
})