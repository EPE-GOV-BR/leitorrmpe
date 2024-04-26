test_that("leituraAgrupamentoInterligacoes works", {
  expect_snapshot_value(leituraAgrupamentoInterligacoes("testData"), style = "json2")
})

test_that("leituraAgrupamentoInterligacoes error", {
  expect_error(leituraAgrupamentoInterligacoes("emptyData"))
  expect_error(leituraAgrupamentoInterligacoes())
})
