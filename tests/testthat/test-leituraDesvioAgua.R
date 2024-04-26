test_that("leituraDesvioAgua works", {
  expect_snapshot_value(leituraDesvioAgua("testData"), style = "json2")
})

test_that("leituraDesvioAgua error", {
  expect_error(leituraDesvioAgua("emptyData"))
  expect_error(leituraDesvioAgua())
})