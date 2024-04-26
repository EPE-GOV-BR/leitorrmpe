test_that("leituraCustoMarginalDemanda works", {
  expect_snapshot_value(leituraCustoMarginalDemanda("testData"), style = "json2")
})

test_that("leituraCustoMarginalDemanda error", {
  expect_error(leituraCustoMarginalDemanda("emptyData"))
  expect_error(leituraCustoMarginalDemanda())
})
