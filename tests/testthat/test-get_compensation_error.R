test_that("Test that get_compensation_error returns outputs", {
  expect_true(length(the.flow.control) > 0)
  expect_true(length(the.flow.gate) > 0)
  
  # get spillover and compensation matrices

  spillover.posnegpop <- get.marker.spillover.posnegpop(
    flow.gate=the.flow.gate, flow.control=the.flow.control,
    asp=the.asp)

  compensation.posnegpop <- solve(spillover.posnegpop)

    spillover.posnegpop.original <- spillover.posnegpop
    rownames( spillover.posnegpop.original ) <- the.flow.control$marker.original
    colnames( spillover.posnegpop.original ) <- the.flow.control$marker.original

    compensation.posnegpop.original <- compensation.posnegpop
    rownames( compensation.posnegpop.original ) <- the.flow.control$marker.original
    colnames( compensation.posnegpop.original ) <- the.flow.control$marker.original

    # get uncompensated expresion data

    expr.data.unco <- the.flow.control$expr.data.untr

    # get compensated expression data

    flow.set.comp <- lapply( the.flow.control$flow.set, compensate,
        compensation( spillover.posnegpop.original ) )

    expr.data.comp <- get.flow.expression.data(flow.set.comp,
       flow.control=the.flow.control )

    # get compensation error in compensated data

    compensation.error <- get.compensation.error(
        expr.data.unco, expr.data.comp,
        marker.spillover.unco=the.marker.spillover.unco.untr,
        TRUE, TRUE, the.asp$posnegpop.file.label, flow.gate=the.flow.gate,
        flow.control=the.flow.control, asp=the.asp
    )

  expect_type(compensation.error, "list")
  expect_named(compensation.error, c("inte", "coef", "slop", "skew"), ignore.order = TRUE)
})