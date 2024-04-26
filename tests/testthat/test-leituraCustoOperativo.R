test_that("leituraCustoOperativo works", {
  expect_snapshot_value(leituraCustoOperativo("testData"), style = "json2")
})

test_that("leituraCustoOperativo error", {
  expect_error(leituraCustoOperativo("emptyData"))
  expect_error(leituraCustoOperativo())
})
