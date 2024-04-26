test_that("leituraGeracaoHidraulicaMinima works", {
  expect_snapshot_value(leituraGeracaoHidraulicaMinima("testData"), style = "json2")
})

test_that("leituraGeracaoHidraulicaMinima error", {
  expect_error(leituraGeracaoHidraulicaMinima("emptyData"))
  expect_error(leituraGeracaoHidraulicaMinima())
})