test_that("leituraRestricoesVazaoHidroDC works", {
  expect_snapshot_value(leituraRestricoesVazaoHidroDC("testDataDC"), style = "json2")
})

test_that("leituraRestricoesVazaoHidroDC error", {
  expect_error(leituraRestricoesVazaoHidroDC("emptyData"))
  expect_error(leituraRestricoesVazaoHidroDC())
})