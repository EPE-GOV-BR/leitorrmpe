test_that("leituraClassesTermicas works", {
  expect_snapshot_value(leituraClassesTermicas("testData"), style = "json2")
})

test_that("leituraClassesTermicas error", {
  expect_error(leituraClassesTermicas("emptyData"))
  expect_error(leituraClassesTermicas())
})
