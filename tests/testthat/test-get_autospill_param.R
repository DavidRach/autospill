test_that("Test that various get_autospill_param inputs return lists", {
  asp1 <- get.autospill.param("minimal")
  expect_true("worker.process.n" %in% names(asp1[2]))

  asp2 <- get.autospill.param("final.step")
  expect_true("worker.process.n" %in% names(asp2[2]))

  asp3 <- get.autospill.param("paper")
  expect_true("worker.process.n" %in% names(asp3[2]))

  asp4 <- get.autospill.param("website")
  expect_true("worker.process.n" %in% names(asp4[2]))

})