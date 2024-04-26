test_that("leituraSeriesHistoricasSimulacaoFinal works", {
  expect_snapshot_value(leituraSeriesHistoricasSimulacaoFinal("testData"), style = "serialize")
})

test_that("leituraSeriesHistoricasSimulacaoFinal error", {
  expect_error(leituraSeriesHistoricasSimulacaoFinal("emptyData"))
  expect_error(leituraSeriesHistoricasSimulacaoFinal())
})