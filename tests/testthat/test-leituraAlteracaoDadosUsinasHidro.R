test_that("leituraAlteracaoDadosUsinasHidro works", {
  expect_snapshot_value(leituraAlteracaoDadosUsinasHidro("testData"), style = "json2")
})

test_that("leituraAlteracaoDadosUsinasHidro error", {
  expect_error(leituraAlteracaoDadosUsinasHidro("emptyData"))
  expect_error(leituraAlteracaoDadosUsinasHidro())
})
