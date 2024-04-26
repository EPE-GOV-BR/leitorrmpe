test_that("calculoMediaDpCMO works", {
  expect_snapshot_value(calculoMediaDpGenericoComPat("testData", "ghtot", 9, 12), style = "json2")
})

test_that("calculoMediaDpCMO error", {
  expect_error(calculoMediaDpGenericoComPat("emptyData", "ghtot", 9, 12))
  expect_error(calculoMediaDpGenericoComPat("testData", "ghtot", 9))
  expect_error(calculoMediaDpGenericoComPat("testData"))
  expect_error(calculoMediaDpGenericoComPat())
})