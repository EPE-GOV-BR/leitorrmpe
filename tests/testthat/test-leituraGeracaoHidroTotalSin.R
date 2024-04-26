test_that("leituraGeracaoHidroTotalSin works", {
  expect_snapshot_value(leituraGeracaoHidroTotalSin("testData"), style = "json2")
})

test_that("leituraGeracaoHidroTotalSin error", {
  expect_error(leituraGeracaoHidroTotalSin("emptyData"))
  expect_error(leituraGeracaoHidroTotalSin())
})