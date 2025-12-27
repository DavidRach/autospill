test_that("Test that get_autospill_param_paper returns a list", {
  asp <- get.autospill.param.paper()
  expect_true("worker.process.n" %in% names(asp[2]))
})