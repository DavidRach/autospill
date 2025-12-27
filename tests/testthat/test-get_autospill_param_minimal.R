test_that("Test that get_autospill_param_minimal returns a list", {
  asp <- get.autospill.param.minimal()
  expect_true("worker.process.n" %in% names(asp[2]))
})