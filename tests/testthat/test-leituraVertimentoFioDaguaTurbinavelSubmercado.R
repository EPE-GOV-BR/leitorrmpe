test_that("leituraVertimentoFioDaguaTurbinavelSubmercado works", {
  expect_snapshot_value(leituraVertimentoFioDaguaTurbinavelSubmercado("testData"), style = "json2")
})

test_that("leituraVertimentoFioDaguaTurbinavelSubmercado error", {
  expect_error(leituraVertimentoFioDaguaTurbinavelSubmercado("emptyData"))
  expect_error(leituraVertimentoFioDaguaTurbinavelSubmercado())
})