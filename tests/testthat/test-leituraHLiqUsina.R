test_that("leituraHLiqUsina works", {
  expect_snapshot_value(leituraHLiqUsina("testData"), style = "json2")
})

test_that("leituraHLiqUsina error", {
  expect_error(leituraHLiqUsina("emptyData"))
  expect_error(leituraHLiqUsina())
})