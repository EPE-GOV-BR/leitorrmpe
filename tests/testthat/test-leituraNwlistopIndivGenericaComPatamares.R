test_that("leituraNwlistopIndivGenericaComPatamares works", {
  expect_snapshot_value(leituraNwlistopIndivGenericaComPatamares("testData", "ghiduh", 9, 7), style = "json2")
  expect_snapshot_value(leituraNwlistopIndivGenericaComPatamares("testData", "ghiduh", NA, 7), style = "json2")
})

test_that("leituraNwlistopIndivGenericaComPatamares error", {
  expect_error(leituraNwlistopIndivGenericaComPatamares("emptyData", "ghiduh", 9, 7))
  expect_error(leituraNwlistopIndivGenericaComPatamares("testData", "ghiduh", 9))
  expect_error(leituraNwlistopIndivGenericaComPatamares("testData"))
  expect_error(leituraNwlistopIndivGenericaComPatamares())
})