test_that("leituraVertimentoFioDaguaTurbinavelSin works", {
  expect_snapshot_value(leituraVertimentoFioDaguaTurbinavelSin("testData"), style = "json2")
})

test_that("leituraVertimentoFioDaguaTurbinavelSin error", {
  expect_error(leituraVertimentoFioDaguaTurbinavelSin("emptyData"))
  expect_error(leituraVertimentoFioDaguaTurbinavelSin())
})