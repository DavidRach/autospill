test_that("Test that plot.gate returns plots", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)
  
  # Gathering the intermediates
  
  test_sample <- flow.control$sample[1]

  expr_data <- flow.control$expr.data.untr[
    flow.control$event.sample == test_sample,
    flow.control$scatter.parameter]

  gate_param <- flow.control$gate.parameter[[ flow.control$marker.original[
    match(test_sample, flow.control$marker)]]]
  
  # The Do-gate overpass
  
  # Checking the plotting
  #plot <- plot.gate(gate.stage, samp, gate.data, gate.marker, gate.bound,
  #  gate.region, gate.population, flow.control, asp, returnPlot=FALSE)

  #expect_true(is.integer(gate_result)) #For return indices
})