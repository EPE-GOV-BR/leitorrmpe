test_that("leituraVolArmUsina works", {
  expect_snapshot_value(leituraVolArmUsina("testData"), style = "json2")
})

test_that("leituraVolArmUsina error", {
  expect_error(leituraVolArmUsina("emptyData"))
  expect_error(leituraVolArmUsina())
})