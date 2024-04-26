test_that("leituraRestricoesVolumeHidroDC works", {
  expect_snapshot_value(leituraRestricoesVolumeHidroDC("testDataDC"), style = "json2")
})

test_that("leituraRestricoesVolumeHidroDC error", {
  expect_error(leituraRestricoesVolumeHidroDC("emptyData"))
  expect_error(leituraRestricoesVolumeHidroDC())
})