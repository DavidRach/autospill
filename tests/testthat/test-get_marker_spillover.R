test_that("Test that get.marker spillover returns a spillover matrixe", {
  expect_true(length(the.marker.spillover.unco.tran) > 0)
  
  expect_type(the.marker.spillover.unco.untr, "list")
  expect_named(the.marker.spillover.unco.untr)
  expect_true(is.numeric(the.marker.spillover.unco.untr[[1]][1]))
})