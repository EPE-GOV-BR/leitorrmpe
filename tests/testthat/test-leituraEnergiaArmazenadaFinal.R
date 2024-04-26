test_that("leituraEnergiaArmazenadaFinal works", {
  expect_snapshot_value(leituraEnergiaArmazenadaFinal("testData"), style = "json2")
})

test_that("leituraEnergiaArmazenadaFinal error", {
  expect_error(leituraEnergiaArmazenadaFinal("emptyData"))
  expect_error(leituraEnergiaArmazenadaFinal())
})