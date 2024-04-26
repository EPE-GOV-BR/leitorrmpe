test_that("leituraDadosExpansaoUsinasHidro works", {
  expect_snapshot_value(leituraDadosExpansaoUsinasHidro("testData"), style = "json2")
})

test_that("leituraDadosExpansaoUsinasHidro error", {
  expect_error(leituraDadosExpansaoUsinasHidro("emptyData"))
  expect_error(leituraDadosExpansaoUsinasHidro())
})