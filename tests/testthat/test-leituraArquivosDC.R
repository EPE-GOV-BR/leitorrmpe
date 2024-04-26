test_that("leituraArquivosDC works", {
  expect_snapshot_value(leituraArquivosDC("testDataDC"), style = "json2")
})

test_that("leituraArquivosDC error", {
  expect_error(leituraArquivosDC("emptyData"))
  expect_error(leituraArquivosDC())
})
