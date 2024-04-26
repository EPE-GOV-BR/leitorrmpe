test_that("leituraCurvaAversaoRisco works", {
  expect_snapshot_value(leituraCurvaAversaoRisco("testData"), style = "serialize")
})

test_that("leituraCurvaAversaoRisco error", {
  expect_error(leituraCurvaAversaoRisco("emptyData"))
  expect_error(leituraCurvaAversaoRisco())
})
