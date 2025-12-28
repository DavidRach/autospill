test_that("Test that do.gate returns indices for respective fluorophore", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper")
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)
  
  test_sample <- flow.control$sample[1]

  expr_data <- flow.control$expr.data.untr[
    flow.control$event.sample == test_sample,
    flow.control$scatter.parameter]

  gate_param <- flow.control$gate.parameter[[ flow.control$marker.original[
    match(test_sample, flow.control$marker)]]]

  # Call do.gate directly
  gate_result <- autospill:::do.gate(expr_data, gate_param, test_sample, flow.control, asp)

  expect_true(is.integer(gate_result)) #For return indices
})