test_that("leituraConfiguracaoTermoeletrica works", {
  expect_snapshot_value(leituraConfiguracaoTermoeletrica("testData"), style = "json2")
})

test_that("leituraConfiguracaoTermoeletrica error", {
  expect_error(leituraConfiguracaoTermoeletrica("emptyData"))
  expect_error(leituraConfiguracaoTermoeletrica())
})
