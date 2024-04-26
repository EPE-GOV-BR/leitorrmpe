test_that("leituraEnergiaArmazenadaFinalPercentualSin works", {
  expect_snapshot_value(leituraEnergiaArmazenadaFinalPercentualSin("testData"), style = "json2")
})

test_that("leituraEnergiaArmazenadaFinalPercentualSin error", {
  expect_error(leituraEnergiaArmazenadaFinalPercentualSin("emptyData"))
  expect_error(leituraEnergiaArmazenadaFinalPercentualSin())
})