test_that("Test that get.flow.expression.data returns a exprs matrix", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- tempdir()
  #AutoTemp <- file.path(tmp, "Autospill")
  #if(!dir.exists(AutoTemp)){dir.create(AutoTemp)}
  #setwd(AutoTemp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  asp <- get.autospill.param("paper", outpath=tmp)
  flow.scatter.parameter <- read.scatter.parameter( asp )
  Return <- FlowSetReturn(control.dir=FolderPath, control.def.file=MetadataPath, 
    asp=asp, flow.scatter.parameter = flow.scatter.parameter)
  
  flow.set <- Return[[1]]
  flow.control <- Return[[2]]
  expect_s4_class(flow.set[[1]], "flowFrame")
  expect_true("scatter.and.marker.label" %in% names(flow.control))

  flow.expr.data.untr <- get.flow.expression.data(flow.set, flow.control)
  expect_true(is.matrix(flow.expr.data.untr))
})