test_that("leituraPARP works", {
  expect_snapshot_value(leituraPARP("testData"), style = "json2")
})

test_that("leituraPARP error", {
  expect_error(leituraPARP("emptyData"))
  expect_error(leituraPARP())
})