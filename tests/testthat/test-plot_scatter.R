test_that("Test that get_compensation_error returns outputs", {
  expect_true(length(the.flow.control) > 0)
  expect_true(length(the.flow.gate) > 0)
  
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

  expr.data.unco <- the.flow.control$expr.data.untr

  flow.set.comp <- lapply( the.flow.control$flow.set, compensate,
        compensation( spillover.posnegpop.original ) )

  expr.data.comp <- get.flow.expression.data(flow.set.comp,
       flow.control=the.flow.control )

  #expr.data.unco
  #expr.data.comp,
  marker.spillover.unco <- the.marker.spillover.unco.untr
  #TRUE
  # RUE, 
  #the.asp$posnegpop.file.label
  flow.gate <-the.flow.gate
  flow.control <- the.flow.control
  asp <- the.asp
  
  marker.spillover.zero <- rep( 0, flow.control$marker.n )
  names( marker.spillover.zero ) <- flow.control$marker
  
  samp <- flow.control$sample[1]
  marker.proper <- samp

  marker.proper.expr.unco <- expr.data.unco[
    which( flow.control$event.sample == samp )[ flow.gate[[ samp ]] ],
                marker.proper ]
  marker.proper.expr.comp <- expr.data.comp[
                which( flow.control$event.sample == samp )[ flow.gate[[ samp ]] ],
                marker.proper ]

  marker.expr.n <- length( marker.proper.expr.unco )

  expr.trim.n <- round( marker.expr.n * asp$rlm.trim.factor )

  marker.proper.expr.low.unco <- sort( marker.proper.expr.unco )[
            expr.trim.n ]
  marker.proper.expr.high.unco <- sort( marker.proper.expr.unco,
            decreasing = TRUE )[ expr.trim.n ]

  marker.proper.expr.low.comp <- sort( marker.proper.expr.comp )[
            expr.trim.n ]
  marker.proper.expr.high.comp <- sort( marker.proper.expr.comp,
            decreasing = TRUE )[ expr.trim.n ]

  marker.spillover.comp.inte <- marker.spillover.zero
  marker.spillover.comp.coef <- marker.spillover.zero
  marker.spillover.comp.slop <- marker.spillover.zero
  marker.spillover.comp.skew <- marker.spillover.zero

    marker <- flow.control$marker[1]
        
                marker.expr.unco <- expr.data.unco[
                    which( flow.control$event.sample == samp )[
                        flow.gate[[ samp ]] ],
                    marker ]
    
                marker.expr.comp <- expr.data.comp[
                    which( flow.control$event.sample == samp )[
                        flow.gate[[ samp ]] ],
                    marker ]

            marker.expr.low.unco <- sort( marker.expr.unco )[ expr.trim.n ]
            marker.expr.high.unco <- sort( marker.expr.unco,
                decreasing = TRUE )[ expr.trim.n ]

            marker.expr.low.comp <- sort( marker.expr.comp )[ expr.trim.n ]
            marker.expr.high.comp <- sort( marker.expr.comp,
                decreasing = TRUE )[ expr.trim.n ]

            expr.trim.idx.unco <- which (
                marker.proper.expr.unco > marker.proper.expr.low.unco &
                    marker.proper.expr.unco < marker.proper.expr.high.unco &
                    marker.expr.unco > marker.expr.low.unco &
                    marker.expr.unco < marker.expr.high.unco
            )

            expr.trim.idx.comp <- which (
                marker.proper.expr.comp > marker.proper.expr.low.comp &
                    marker.proper.expr.comp < marker.proper.expr.high.comp &
                    marker.expr.comp > marker.expr.low.comp &
                    marker.expr.comp < marker.expr.high.comp
            )

            marker.proper.expr.trim.unco <- marker.proper.expr.unco[
                expr.trim.idx.unco ]
            marker.expr.trim.unco <- marker.expr.unco[ expr.trim.idx.unco ]

            marker.proper.expr.trim.comp <- marker.proper.expr.comp[
                expr.trim.idx.comp ]
            marker.expr.trim.comp <- marker.expr.comp[ expr.trim.idx.comp ]

            marker.spillover.comp.coef[ marker ] <- 1.0
            marker.spillover.comp.slop[ marker ] <- 1.0


    plot.scatter(
        expr.data.unco.x=expr.data.unco[flow.control$event.sample == samp, marker ],
        expr.data.unco.y=expr.data.unco[flow.control$event.sample == samp, marker.proper ],
        expr.data.comp.x=expr.data.comp[flow.control$event.sample == samp, marker ],
        expr.data.comp.y=expr.data.comp[flow.control$event.sample == samp,marker.proper ],
        spillover.unco.inte=marker.spillover.unco$inte[ marker.proper, marker ],
        spillover.unco.coef=marker.spillover.unco$coef[ marker.proper, marker ],
        spillover.comp.inte=marker.spillover.comp.inte[ marker ],
        spillover.comp.coef=marker.spillover.comp.coef[ marker ],
        spillover.comp.slop=marker.spillover.comp.slop[ marker ],
        marker.limit.range=range( c( marker.expr.trim.unco, marker.expr.trim.comp ) ),
        marker.proper.limit.range=range( c( marker.proper.expr.trim.unco,marker.proper.expr.trim.comp ) ),
        samp=samp,
        marker=marker,
        marker.proper=marker.proper,
        scale.untransformed=TRUE,
        figure.file.label=TRUE,
        flow.gate=flow.gate, #NULL
        flow.control=flow.control,
        asp=asp)

  expect_type(compensation.error, "list")
  expect_named(compensation.error, c("inte", "coef", "slop", "skew"), ignore.order = TRUE)
})