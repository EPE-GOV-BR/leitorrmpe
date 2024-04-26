test_that("leituraDeficitSistema works", {
  expect_snapshot_value(leituraDeficitSistema("testData"), style = "json2")
})

test_that("leituraDeficitSistema error", {
  expect_error(leituraDeficitSistema("emptyData"))
  expect_error(leituraDeficitSistema())
})