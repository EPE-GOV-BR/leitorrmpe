test_that("leituraVertimentoFioDaguaTurbinavel works", {
  expect_snapshot_value(leituraVertimentoFioDaguaTurbinavel("testData"), style = "json2")
})

test_that("leituraVertimentoFioDaguaTurbinavel error", {
  expect_error(leituraVertimentoFioDaguaTurbinavel("emptyData"))
  expect_error(leituraVertimentoFioDaguaTurbinavel())
})