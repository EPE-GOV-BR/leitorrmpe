test_that("consolidaTermeletricasnoHorizonte works", {
  expect_snapshot_value(consolidaTermeletricasnoHorizonte("testData"), style = "json2")
})

test_that("consolidaTermeletricasnoHorizonte error", {
  expect_error(consolidaTermeletricasnoHorizonte("emptyData"))
  expect_error(consolidaTermeletricasnoHorizonte())
})
