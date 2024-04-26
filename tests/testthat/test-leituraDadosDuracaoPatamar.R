test_that("leituraDadosDuracaoPatamar works", {
  expect_snapshot_value(leituraDadosDuracaoPatamar("testData"), style = "json2")
})

test_that("leituraDadosDuracaoPatamar error", {
  expect_error(leituraDadosDuracaoPatamar("emptyData"))
  expect_error(leituraDadosDuracaoPatamar())
})