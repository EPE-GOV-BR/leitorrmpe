test_that("leituraExpansaoTermoeletrica works", {
  expect_snapshot_value(leituraExpansaoTermoeletrica("testData"), style = "json2")
})

test_that("leituraExpansaoTermoeletrica error", {
  expect_error(leituraExpansaoTermoeletrica("emptyData"))
  expect_error(leituraExpansaoTermoeletrica())
})