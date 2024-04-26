test_that("leituraDadosPatamarUsinasNaoSimuladas works", {
  expect_snapshot_value(leituraDadosPatamarUsinasNaoSimuladas("testData"), style = "json2")
})

test_that("leituraDadosPatamarUsinasNaoSimuladas error", {
  expect_error(leituraDadosPatamarUsinasNaoSimuladas("emptyData"))
  expect_error(leituraDadosPatamarUsinasNaoSimuladas())
})