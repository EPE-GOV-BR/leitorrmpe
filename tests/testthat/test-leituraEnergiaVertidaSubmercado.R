test_that("leituraEnergiaVertidaSubmercado works", {
  expect_snapshot_value(leituraEnergiaVertidaSubmercado("testData"), style = "json2")
})

test_that("leituraEnergiaVertidaSubmercado error", {
  expect_error(leituraEnergiaVertidaSubmercado("emptyData"))
  expect_error(leituraEnergiaVertidaSubmercado())
})