test_that("leituraEnergiaArmazenadaFinalPercentual works", {
  expect_snapshot_value(leituraEnergiaArmazenadaFinalPercentual("testData"), style = "json2")
})

test_that("leituraEnergiaArmazenadaFinalPercentual error", {
  expect_error(leituraEnergiaArmazenadaFinalPercentual("emptyData"))
  expect_error(leituraEnergiaArmazenadaFinalPercentual())
})