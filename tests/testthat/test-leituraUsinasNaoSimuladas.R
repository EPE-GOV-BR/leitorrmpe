test_that("leituraUsinasNaoSimuladas works", {
  expect_snapshot_value(leituraUsinasNaoSimuladas("testData"), style = "json2")
})

test_that("leituraUsinasNaoSimuladas error", {
  expect_error(leituraUsinasNaoSimuladas("emptyData"))
  expect_error(leituraUsinasNaoSimuladas())
})