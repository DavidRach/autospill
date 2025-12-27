test_that("Test that get_autospill_param_final_step returns a list", {
  asp <- get.autospill.param.final.step()
  expect_true("worker.process.n" %in% names(asp[2]))
})