test_that("leituraDeficitEnergia works", {
  expect_snapshot_value(leituraDeficitEnergia("testData"), style = "json2")
})

test_that("leituraDeficitEnergia error", {
  expect_error(leituraDeficitEnergia("emptyData"))
  expect_error(leituraDeficitEnergia())
})