test_that("Test that get.transformation returns forward and reverse transforms", {
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

  flow.transform.both <- get.transformation(flow.control, asp)
  expect_length(flow.transform.both, 2)

  tr <- flow.transform.both[[1]][[1]]
  expect_equal(attr(tr, "type"), "biexp")

  params <- attr(tr, "parameters")
  expect_type(params, "list")
  expect_named(
    params,
    c("channelRange", "maxValue", "neg", "pos", "widthBasis"))
})