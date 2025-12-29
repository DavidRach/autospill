test_that("Test that refine spillover returns it's list", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  
  refine.spillover.result <- refine.spillover(
    marker.spillover.unco.untr=the.marker.spillover.unco.untr,
    marker.spillover.unco.tran=the.marker.spillover.unco.tran,
    flow.gate=the.flow.gate,
    flow.control=the.flow.control, asp=the.asp)

  expect_type(compensation.error, "list")
  expect_named(compensation.error, c("spillover", "compensation",
   "error", "convergence"), ignore.order = TRUE)
})