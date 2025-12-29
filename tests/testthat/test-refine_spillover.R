test_that("Test that refine spillover returns it's list", {
  expect_true(length(the.refine.spillover.result) > 0)
  
  expect_type(the.refine.spillover.result, "list")
  expect_named(the.refine.spillover.result, c("spillover", "compensation",
   "error", "convergence"), ignore.order = TRUE)
})