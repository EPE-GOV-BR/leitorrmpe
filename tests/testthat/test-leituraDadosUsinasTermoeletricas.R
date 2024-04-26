test_that("leituraDadosUsinasTermoeletricas works", {
  expect_snapshot_value(leituraDadosUsinasTermoeletricas("testData"), style = "json2")
})

test_that("leituraDadosUsinasTermoeletricas error", {
  expect_error(leituraDadosUsinasTermoeletricas("emptyData"))
  expect_error(leituraDadosUsinasTermoeletricas())
})