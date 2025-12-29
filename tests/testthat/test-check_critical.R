test_that("Test that check.critical still spots FALSE conditions", {
  expect_true(length(MetadataPath) > 0)

  control <- read.csv(MetadataPath, stringsAsFactors = FALSE)
  
  expect_silent(
    autospill:::check.critical(!duplicated(control$filename), "Data contains duplicates 1")
  )
  control2 <- rbind(control, control)
  expect_error(
    autospill:::check.critical(!duplicated(control2$filename), "Data contains duplicates 2")
  )
})