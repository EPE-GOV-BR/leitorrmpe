test_that("leituraNwlistopGenericaSemPatamares works", {
  expect_snapshot_value(leituraNwlistopGenericaSemPatamares("testData", "earmf", 9, 7), style = "json2")
})

test_that("leituraNwlistopGenericaSemPatamares error", {
  expect_error(leituraNwlistopGenericaSemPatamares("emptyData", "earmf", 9, 7))
  expect_error(leituraNwlistopGenericaSemPatamares("testData", "earmf", 9))
  expect_error(leituraNwlistopGenericaSemPatamares("testData"))
  expect_error(leituraNwlistopGenericaSemPatamares())
})