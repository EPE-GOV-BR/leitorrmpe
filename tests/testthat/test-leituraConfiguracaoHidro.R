test_that("leituraConfiguracaoHidro works", {
  expect_snapshot_value(leituraConfiguracaoHidro("testData"), style = "serialize")
})

test_that("leituraConfiguracaoHidro error", {
  expect_error(leituraConfiguracaoHidro("emptyData"))
  expect_error(leituraConfiguracaoHidro())
})