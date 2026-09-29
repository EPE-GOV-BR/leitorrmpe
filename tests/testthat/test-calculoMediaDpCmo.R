test_that("calculoMediaDpCmo works", {
  expect_snapshot_value(calculoMediaDpCmo("testData"), style = "json2")
})

test_that("calculoMediaDpCmo error", {
  expect_error(calculoMediaDpCmo("emptyData"))
  expect_error(calculoMediaDpCmo())
})
