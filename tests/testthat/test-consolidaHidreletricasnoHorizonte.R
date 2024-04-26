test_that("consolidaHidreletricasnoHorizonte works", {
  expect_snapshot_value(consolidaHidreletricasnoHorizonte("testData"), style = "json2")
})

test_that("consolidaHidreletricasnoHorizonte error", {
  expect_error(consolidaHidreletricasnoHorizonte("emptyData"))
  expect_error(consolidaHidreletricasnoHorizonte())
})