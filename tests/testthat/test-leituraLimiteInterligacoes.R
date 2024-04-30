test_that("leituraLimiteInterligacoes works", {
  expect_snapshot_value(leituraLimiteInterligacoes("testData"), style = "json2")
})

test_that("leituraLimiteInterligacoes error", {
  expect_error(leituraLimiteInterligacoes("emptyData"))
  expect_error(leituraLimiteInterligacoes())
})