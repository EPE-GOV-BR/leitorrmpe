test_that("leituraOperacaoHidroSUISHI works", {
  expect_snapshot_value(leituraOperacaoHidroSUISHI("testData"), style = "json2")
})

test_that("leituraOperacaoHidroSUISHI error", {
  expect_error(leituraOperacaoHidroSUISHI("emptyData"))
  expect_error(leituraOperacaoHidroSUISHI())
})