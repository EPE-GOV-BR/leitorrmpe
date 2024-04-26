test_that("leituraArquivos works", {
  expect_snapshot_value(leituraArquivos("testData"), style = "json2")
})

test_that("leituraArquivos error", {
  expect_error(leituraArquivos("emptyData"))
  expect_error(leituraArquivos())
})