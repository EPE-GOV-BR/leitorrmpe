test_that("leituraEnergiaArmazenadaFinalSubmercado works", {
  expect_snapshot_value(leituraEnergiaArmazenadaFinalSubmercado("testData"), style = "json2")
})

test_that("leituraEnergiaArmazenadaFinalSubmercado error", {
  expect_error(leituraEnergiaArmazenadaFinalSubmercado("emptyData"))
  expect_error(leituraEnergiaArmazenadaFinalSubmercado())
})