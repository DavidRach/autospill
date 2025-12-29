test_that("Test that read.scatter.parameter returns FSC and SSC", {
  expect_true(length(the.asp) > 0)

  flow.scatter.parameter <- read.scatter.parameter(the.asp)

  expect_type(flow.scatter.parameter, "character")
  expect_equal(flow.scatter.parameter, c("FSC-A", "SSC-A"))
})