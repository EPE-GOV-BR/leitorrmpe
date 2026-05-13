test_that("leituraPostos works", {
  expect_snapshot_value(leituraPostos("testData"), style = "json2")
})

test_that("leituraPostos error", {
  expect_error(leituraPostos("emptyData"))
  expect_error(leituraPostos())
})
