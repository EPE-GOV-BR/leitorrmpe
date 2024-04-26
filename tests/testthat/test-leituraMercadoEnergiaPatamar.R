test_that("leituraMercadoEnergiaPatamar works", {
  expect_snapshot_value(leituraMercadoEnergiaPatamar("testData"), style = "json2")
})

test_that("leituraMercadoEnergiaPatamar error", {
  expect_error(leituraMercadoEnergiaPatamar("emptyData"))
  expect_error(leituraMercadoEnergiaPatamar())
})