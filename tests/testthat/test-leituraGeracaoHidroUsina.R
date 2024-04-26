test_that("leituraGeracaoHidroUsina works", {
  expect_snapshot_value(leituraGeracaoHidroUsina("testData"), style = "json2")
})

test_that("leituraGeracaoHidroUsina error", {
  expect_error(leituraGeracaoHidroUsina("emptyData"))
  expect_error(leituraGeracaoHidroUsina())
})