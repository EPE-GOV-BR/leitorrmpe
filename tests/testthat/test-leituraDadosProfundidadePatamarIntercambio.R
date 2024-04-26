test_that("leituraDadosProfundidadePatamarIntercambio works", {
  expect_snapshot_value(leituraDadosProfundidadePatamarIntercambio("testData"), style = "json2")
})

test_that("leituraDadosProfundidadePatamarIntercambio error", {
  expect_error(leituraDadosProfundidadePatamarIntercambio("emptyData"))
  expect_error(leituraDadosProfundidadePatamarIntercambio())
})