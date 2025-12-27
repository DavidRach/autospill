test_that("Test that create.directory creates directories", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- tempdir()
  #AutoTemp <- file.path(tmp, "Autospill")
  #if(!dir.exists(AutoTemp)){dir.create(AutoTemp)}
  #setwd(AutoTemp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  asp <- get.autospill.param("paper")
  flow.scatter.parameter <- read.scatter.parameter(asp)
  Return <- FlowSetReturn(control.dir=FolderPath, control.def.file=MetadataPath, 
    asp=asp, flow.scatter.parameter = flow.scatter.parameter)
  
  flow.control <- Return[[2]]
  expect_true("scatter.and.marker.label" %in% names(flow.control))

  flow.figure.scatter.dir <- create.directory(flow.control, asp, outpath=NULL)

  expect_type(flow.figure.scatter.dir, "character")

  dirs <- flow.figure.scatter.dir

  expect_true(
    all(
      basename(dirs) ==
        paste0("figure_scatter_", names(dirs))
    )
  )



})