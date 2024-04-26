test_that("leituraVolArmPercentualUsina works", {
  expect_snapshot_value(leituraVolArmPercentualUsina("testData"), style = "json2")
})

test_that("leituraVolArmPercentualUsina error", {
  expect_error(leituraVolArmPercentualUsina("emptyData"))
  expect_error(leituraVolArmPercentualUsina())
})