test_that("leituraIntercambioEnergia works", {
  expect_snapshot_value(leituraIntercambioEnergia("testData"), style = "json2")
})

test_that("leituraIntercambioEnergia error", {
  expect_error(leituraIntercambioEnergia("emptyData"))
  expect_error(leituraIntercambioEnergia())
})