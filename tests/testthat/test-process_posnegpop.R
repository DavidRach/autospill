test_that("Test that process_posnegpop returns its outputs", {
  expect_true(length(the.spillover.error.posnegpop) > 0)

  expect_type(the.spillover.error.posnegpop, "list")
  expect_named(the.spillover.error.posnegpop,
    c("spillover", "error", "compensation"),
    ignore.order = TRUE
  )
})