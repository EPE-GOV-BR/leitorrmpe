test_that("calculoMediaDpCMO works", {
  expect_snapshot_value(calculoMediaDpCMO("testData"), style = "json2")
})

test_that("calculoMediaDpCMO error", {
  expect_error(calculoMediaDpCMO("emptyData"))
  expect_error(calculoMediaDpCMO())
})
