test_that("Test that gate.flow.data returns a list of indexed cells by fluorophore", {
  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  #tmp <- withr::local_tempdir(pattern = "Autospill")
  #withr::local_dir(tmp)
  
  expect_true(length(the.flow.control) > 0)
  
  flow.gate <- suppressWarnings(
    gate.flow.data(flow.control=the.flow.control, asp=the.asp)
  )

  expect_type(flow.gate, "list")
  expect_named(flow.gate)
  expect_true(is.numeric(flow.gate[[1]][1]))
})