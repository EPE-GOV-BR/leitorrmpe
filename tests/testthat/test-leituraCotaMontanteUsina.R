test_that("leituraCotaMontanteUsina works", {
  expect_snapshot_value(leituraCotaMontanteUsina("testData"), style = "json2")
})

test_that("leituraCotaMontanteUsina error", {
  expect_error(leituraCotaMontanteUsina("emptyData"))
  expect_error(leituraCotaMontanteUsina())
})