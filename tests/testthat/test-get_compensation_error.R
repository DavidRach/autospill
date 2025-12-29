test_that("Test that get_compensation_error returns outputs", {
  expect_true(length(MetadataPath) > 0)
  expect_true(length(FolderPath) > 0)

  #tmp <- "C:/Users/12692/Desktop/Autospill"
  #setwd(tmp)
  tmp <- withr::local_tempdir(pattern = "Autospill")
  withr::local_dir(tmp)
  asp <- get.autospill.param("paper", outpath=tmp)
  flow.control <- read.flow.control(control.dir=FolderPath,
    control.def.file=MetadataPath, asp=asp)
  
  flow.gate <- suppressWarnings(
    gate.flow.data(flow.control=flow.control, asp=asp)
  )

  marker.spillover.unco.untr <- get.marker.spillover(scale.untransformed=TRUE, flow.gate=flow.gate,
    flow.control=flow.control, asp=asp)
  
  spillover.posnegpop <- get.marker.spillover.posnegpop( flow.gate,
        flow.control, asp )
  
      # get spillover and compensation matrices

    spillover.posnegpop <- get.marker.spillover.posnegpop( flow.gate,
        flow.control, asp )

    compensation.posnegpop <- solve( spillover.posnegpop )

    spillover.posnegpop.original <- spillover.posnegpop
    rownames( spillover.posnegpop.original ) <- flow.control$marker.original
    colnames( spillover.posnegpop.original ) <- flow.control$marker.original

    compensation.posnegpop.original <- compensation.posnegpop
    rownames( compensation.posnegpop.original ) <- flow.control$marker.original
    colnames( compensation.posnegpop.original ) <- flow.control$marker.original

    # get uncompensated expresion data

    expr.data.unco <- flow.control$expr.data.untr

    # get compensated expression data

    flow.set.comp <- lapply( flow.control$flow.set, compensate,
        compensation( spillover.posnegpop.original ) )

    expr.data.comp <- get.flow.expression.data( flow.set.comp, flow.control )

    # get compensation error in compensated data

    compensation.error <- get.compensation.error(
        expr.data.unco, expr.data.comp, marker.spillover.unco.untr,
        TRUE, TRUE, asp$posnegpop.file.label, flow.gate, flow.control, asp
    )

  expect_type(compensation.error, "list")
  expect_named(compensation.error, c("inte", "coef", "slop", "skew"), ignore.order = TRUE)
})