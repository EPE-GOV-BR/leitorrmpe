test_that("leituraEnergiaVertidaSin works", {
  expect_snapshot_value(leituraEnergiaVertidaSin("testData"), style = "json2")
})

test_that("leituraEnergiaVertidaSin error", {
  expect_error(leituraEnergiaVertidaSin("emptyData"))
  expect_error(leituraEnergiaVertidaSin())
})