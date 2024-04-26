test_that("leituraEnergiaAfluenteTotalBrutaMercado works", {
  expect_snapshot_value(leituraEnergiaAfluenteTotalBrutaMercado("testData"), style = "json2")
})

test_that("leituraEnergiaAfluenteTotalBrutaMercado error", {
  expect_error(leituraEnergiaAfluenteTotalBrutaMercado("emptyData"))
  expect_error(leituraEnergiaAfluenteTotalBrutaMercado())
})