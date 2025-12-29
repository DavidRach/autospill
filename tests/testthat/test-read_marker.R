test_that("Test that read.marker returns a data.frame", {
  expect_true(length(the.asp) > 0)

  flow.set.marker.table <- read.marker(control.dir=FolderPath,
     control.def.file=MetadataPath, asp=the.asp)

  expect_s3_class(flow.set.marker.table, "data.frame")
  expect_true("flow.set.marker.corrected" %in% colnames(flow.set.marker.table))
})