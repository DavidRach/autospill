test_that("Test that do.gate returns indices for respective fluorophore", {
  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)

  the.asp <- get.autospill.param("paper", outpath=tmp)
  the.flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=the.asp)

  expect_true(length(the.flow.control) > 0)

  test_sample <- the.flow.control$sample[1]

  expr_data <- the.flow.control$expr.data.untr[
    the.flow.control$event.sample == test_sample,
    the.flow.control$scatter.parameter]

  gate_param <- the.flow.control$gate.parameter[[the.flow.control$marker.original[
    match(test_sample, the.flow.control$marker)]]]

  # Call do.gate directly
  gate_result <- suppressWarnings(
    autospill:::do.gate(expr_data, gate_param, test_sample,
       flow.control=the.flow.control, asp=the.asp)
  )

  expect_true(is.integer(gate_result)) #For return indices
})