test_that("Test that read.flow.control returns transform list", {
  expect_true(length(the.flow.control) > 0)

  expect_type(the.flow.control, "list")
  expect_true("antigen" %in% names(the.flow.control))
})