test_that("leituraNwlistopGenericaComPatamaresTotal works", {
  expect_snapshot_value(leituraNwlistopGenericaComPatamaresTotal("testData", "ghtot", 9, 12, TRUE), style = "json2")
  expect_snapshot_value(leituraNwlistopGenericaComPatamaresTotal("testData", "ghtot", 9, 12, FALSE), style = "json2")
})

test_that("leituraNwlistopGenericaComPatamaresTotal error", {
  expect_error(leituraNwlistopGenericaComPatamaresTotal("emptyData", "ghtot", 9, 12, TRUE))
  expect_error(leituraNwlistopGenericaComPatamaresTotal("testData", "ghtot", 9))
  expect_error(leituraNwlistopGenericaComPatamaresTotal("testData"))
  expect_error(leituraNwlistopGenericaComPatamaresTotal())
})