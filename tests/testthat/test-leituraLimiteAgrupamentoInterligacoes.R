test_that("leituraLimiteAgrupamentoInterligacoes works", {
  expect_snapshot_value(leituraLimiteAgrupamentoInterligacoes("testData"), style = "json2")
})

test_that("leituraLimiteAgrupamentoInterligacoes error", {
  expect_error(leituraLimiteAgrupamentoInterligacoes("emptyData"))
  expect_error(leituraLimiteAgrupamentoInterligacoes())
})