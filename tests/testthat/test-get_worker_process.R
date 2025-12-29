test_that("Test that get.worker.process returns a ", {
  expect_true(length(the.asp) > 0)

  Value <- get.worker.process(the.asp$worker.process.n)

  expect_type(Value, "double")
  expect_true(Value > 0)
})