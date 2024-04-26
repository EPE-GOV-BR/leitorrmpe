test_that("leituraGeracaoHidroTotal works", {
  expect_snapshot_value(leituraGeracaoHidroTotal("testData"), style = "json2")
})

test_that("leituraGeracaoHidroTotal error", {
  expect_error(leituraGeracaoHidroTotal("emptyData"))
  expect_error(leituraGeracaoHidroTotal())
})