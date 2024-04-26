test_that("leituraSubmotorizacaoREE works", {
  expect_snapshot_value(leituraSubmotorizacaoREE("testData"), style = "json2")
})

test_that("leituraSubmotorizacaoREE error", {
  expect_error(leituraSubmotorizacaoREE("emptyData"))
  expect_error(leituraSubmotorizacaoREE())
})