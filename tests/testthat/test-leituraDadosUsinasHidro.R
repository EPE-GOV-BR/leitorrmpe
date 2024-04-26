test_that("leituraDadosUsinasHidro works", {
  expect_snapshot_value(leituraDadosUsinasHidro("testData"), style = "serialize")
})

test_that("leituraDadosUsinasHidro error", {
  expect_error(leituraDadosUsinasHidro("emptyData"))
  expect_error(leituraDadosUsinasHidro())
})