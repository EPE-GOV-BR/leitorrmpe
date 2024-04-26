test_that("leituraDadosProfundidadePatamarCarga works", {
  expect_snapshot_value(leituraDadosProfundidadePatamarCarga("testData"), style = "json2")
})

test_that("leituraDadosProfundidadePatamarCarga error", {
  expect_error(leituraDadosProfundidadePatamarCarga("emptyData"))
  expect_error(leituraDadosProfundidadePatamarCarga())
})