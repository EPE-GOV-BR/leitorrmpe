test_that("calculoMediaDpCMO works", {
  expect_snapshot_value(calculoMediaDpGenericoSemPat("testData", "earmf", 9, 7), style = "json2")
})

test_that("calculoMediaDpCMO error", {
  expect_error(calculoMediaDpGenericoSemPat("emptyData", "earmf", 9, 7))
  expect_error(calculoMediaDpGenericoSemPat("testData", "earmf", 9))
  expect_error(calculoMediaDpGenericoSemPat("testData"))
  expect_error(calculoMediaDpGenericoSemPat())
})