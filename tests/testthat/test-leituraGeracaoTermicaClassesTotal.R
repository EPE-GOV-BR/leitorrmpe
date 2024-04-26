test_that("leituraGeracaoTermicaClassesTotal works", {
  expect_snapshot_value(leituraGeracaoTermicaClassesTotal("testData"), style = "json2")
})

test_that("leituraGeracaoTermicaClassesTotal error", {
  expect_error(leituraGeracaoTermicaClassesTotal("emptyData"))
  expect_error(leituraGeracaoTermicaClassesTotal())
})