test_that("Test that gate.flow.data returns a list of indexed cells by fluorophore", {
  
  expect_true(length(the.flow.gate) > 0)

  expect_type(the.flow.gate, "list")
  expect_named(the.flow.gate)
  expect_true(is.numeric(the.flow.gate[[1]][1]))
})