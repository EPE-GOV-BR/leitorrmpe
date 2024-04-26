test_that("leituraEnergiaAfluenteTotalBruta works", {
  expect_snapshot_value(leituraEnergiaAfluenteTotalBruta("testData"), style = "json2")
})

test_that("leituraEnergiaAfluenteTotalBruta error", {
  expect_error(leituraEnergiaAfluenteTotalBruta("emptyData"))
  expect_error(leituraEnergiaAfluenteTotalBruta())
})