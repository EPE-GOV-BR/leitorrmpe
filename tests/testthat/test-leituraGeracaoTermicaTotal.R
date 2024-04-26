test_that("leituraGeracaoTermicaTotal works", {
  expect_snapshot_value(leituraGeracaoTermicaTotal("testData"), style = "json2")
})

test_that("leituraGeracaoTermicaTotal error", {
  expect_error(leituraGeracaoTermicaTotal("emptyData"))
  expect_error(leituraGeracaoTermicaTotal())
})