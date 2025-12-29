test_that("Test that process_posnegpop returns its outputs", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  
  spillover.error.posnegpop <- process.posnegpop(
    marker.spillover.unco.untr=the.marker.spillover.unco.untr,
    flow.gate=the.flow.gate, flow.control=the.flow.control,
    asp=the.asp)

  expect_type(spillover.error.posnegpop, "list")
  expect_named(
    spillover.error.posnegpop,
    c("spillover", "compensation"),
    ignore.order = TRUE
  )
})