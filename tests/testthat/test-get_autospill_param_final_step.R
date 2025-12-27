test_that("Test that various get_autospill_param inputs return lists", {
  asp <- get.autospill.param.final.step()
  expect_true("worker.process.n" %in% names(asp[2]))
})