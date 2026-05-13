test_that("leituraVazoes works", {
  expect_snapshot_value(leituraVazoes("testData"), style = "json2")
})

test_that("leituraVazoes error", {
  expect_error(leituraVazoes("emptyData"))
  expect_error(leituraVazoes())
})