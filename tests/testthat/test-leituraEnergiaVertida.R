test_that("leituraEnergiaVertida works", {
  expect_snapshot_value(leituraEnergiaVertida("testData"), style = "json2")
})

test_that("leituraEnergiaVertida error", {
  expect_error(leituraEnergiaVertida("emptyData"))
  expect_error(leituraEnergiaVertida())
})