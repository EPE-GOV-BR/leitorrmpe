test_that("leituraDadosSubsistema works", {
  expect_snapshot_value(leituraDadosSubsistema("testData"), style = "json2")
})

test_that("leituraDadosSubsistema error", {
  expect_error(leituraDadosSubsistema("emptyData"))
  expect_error(leituraDadosSubsistema())
})