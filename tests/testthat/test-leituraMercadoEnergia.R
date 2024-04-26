test_that("leituraMercadoEnergia works", {
  expect_snapshot_value(leituraMercadoEnergia("testData"), style = "json2")
})

test_that("leituraMercadoEnergia error", {
  expect_error(leituraMercadoEnergia("emptyData"))
  expect_error(leituraMercadoEnergia())
})