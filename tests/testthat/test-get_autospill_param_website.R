test_that("Test that get_autospill_param_website returns a list", {
  asp <- get.autospill.param.website()
  expect_true("worker.process.n" %in% names(asp[2]))
})