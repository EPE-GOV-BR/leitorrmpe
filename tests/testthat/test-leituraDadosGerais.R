test_that("leituraDadosGerais works", {
  expect_snapshot_value(leituraDadosGerais("testData"), style = "json2")
})

test_that("leituraDadosGerais error", {
  expect_error(leituraDadosGerais("emptyData"))
  expect_error(leituraDadosGerais())
})
