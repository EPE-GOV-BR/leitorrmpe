test_that("definePeriodo works", {
  expect_snapshot_value(definePeriodo("testData"), style = "json2")
})

test_that("definePeriodo error", {
  expect_error(definePeriodo("emptyData"))
  expect_error(definePeriodo())
})