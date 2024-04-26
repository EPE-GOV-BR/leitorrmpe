test_that("leituraConfiguracaoHidroDC works", {
  expect_snapshot_value(leituraConfiguracaoHidroDC("testDataDC"), style = "json2")
})

test_that("leituraConfiguracaoHidroDC error", {
  expect_error(leituraConfiguracaoHidroDC("emptyData"))
  expect_error(leituraConfiguracaoHidroDC())
})
