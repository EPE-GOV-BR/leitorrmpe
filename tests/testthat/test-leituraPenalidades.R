test_that("leituraPenalidades works", {
  expect_snapshot_value(leituraPenalidades("testData"), style = "json2")
})

test_that("leituraPenalidades error", {
  expect_error(leituraPenalidades("emptyData"))
  expect_error(leituraPenalidades())
})