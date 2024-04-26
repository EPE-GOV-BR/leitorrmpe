test_that("leituraGeracaoTermicaMediaClasses works", {
  expect_snapshot_value(leituraGeracaoTermicaMediaClasses("testData"), style = "json2")
})

test_that("leituraGeracaoTermicaMediaClasses error", {
  expect_error(leituraGeracaoTermicaMediaClasses("emptyData"))
  expect_error(leituraGeracaoTermicaMediaClasses())
})