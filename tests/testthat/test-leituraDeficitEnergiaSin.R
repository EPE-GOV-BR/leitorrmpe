test_that("leituraDeficitEnergiaSin works", {
  expect_snapshot_value(leituraDeficitEnergiaSin("testData"), style = "json2")
})

test_that("leituraDeficitEnergiaSin error", {
  expect_error(leituraDeficitEnergiaSin("emptyData"))
  expect_error(leituraDeficitEnergiaSin())
})