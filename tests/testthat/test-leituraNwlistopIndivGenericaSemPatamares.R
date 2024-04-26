test_that("leituraNwlistopIndivGenericaSemPatamares works", {
  expect_snapshot_value(leituraNwlistopIndivGenericaSemPatamares("testData", "hmont", 15 ,8), style = "json2")
})

test_that("leituraNwlistopIndivGenericaSemPatamares error", {
  expect_error(leituraNwlistopIndivGenericaSemPatamares("emptyData", "hmont", 15 ,8))
  expect_error(leituraNwlistopIndivGenericaSemPatamares("testData", "hmont", 15))
  expect_error(leituraNwlistopIndivGenericaSemPatamares("testData"))
  expect_error(leituraNwlistopIndivGenericaSemPatamares())
})