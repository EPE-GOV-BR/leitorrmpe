test_that("leituraREE works", {
  expect_snapshot_value(leituraREE("testData"), style = "json2")
})

test_that("leituraREE error", {
  expect_error(leituraREE("emptyData"))
  expect_error(leituraREE())
})