test_that("leituraGeracaoHidroTotalSubmercado works", {
  expect_snapshot_value(leituraGeracaoHidroTotalSubmercado("testData"), style = "json2")
})

test_that("leituraGeracaoHidroTotalSubmercado error", {
  expect_error(leituraGeracaoHidroTotalSubmercado("emptyData"))
  expect_error(leituraGeracaoHidroTotalSubmercado())
})