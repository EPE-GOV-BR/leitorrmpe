test_that("leituraValorDagua works", {
  expect_snapshot_value(leituraValorDagua("testData"), style = "json2")
})

test_that("leituraValorDagua error", {
  expect_error(leituraValorDagua("emptyData"))
  expect_error(leituraValorDagua())
})