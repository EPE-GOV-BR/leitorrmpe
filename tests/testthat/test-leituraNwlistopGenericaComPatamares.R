test_that("leituraNwlistopGenericaComPatamares works", {
  expect_snapshot_value(leituraNwlistopGenericaComPatamares("testData", "ghtot", 9, 12), style = "json2")
})

test_that("leituraNwlistopGenericaComPatamares error", {
  expect_error(leituraNwlistopGenericaComPatamares("emptyData", "ghtot", 9, 12))
  expect_error(leituraNwlistopGenericaComPatamares("testData", "ghtot", 9))
  expect_error(leituraNwlistopGenericaComPatamares("testData"))
  expect_error(leituraNwlistopGenericaComPatamares())
})