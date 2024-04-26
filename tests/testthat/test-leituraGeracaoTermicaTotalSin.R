test_that("leituraGeracaoTermicaTotalSin works", {
  expect_snapshot_value(leituraGeracaoTermicaTotalSin("testData"), style = "json2")
})

test_that("leituraGeracaoTermicaTotalSin error", {
  expect_error(leituraGeracaoTermicaTotalSin("emptyData"))
  expect_error(leituraGeracaoTermicaTotalSin())
})