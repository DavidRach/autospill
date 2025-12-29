test_that("Test that process_posnegpop returns its outputs", {
  expect_true(length(the.flow.gate) > 0)
  
  spillover.posnegpop <- get.marker.spillover.posnegpop(
    flow.gate=the.flow.gate, flow.control=the.flow.control,
    asp=the.asp)

   expect_true(is.matrix(spillover.posnegpop))
   expect_identical(rownames(spillover.posnegpop), colnames(spillover.posnegpop))
})