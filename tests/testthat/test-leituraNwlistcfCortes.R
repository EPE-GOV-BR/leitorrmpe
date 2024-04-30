test_that("leituraNwlistcfCortes works", {
  expect_snapshot_value(leituraNwlistcfCortes(test_path("testData", "nwlistcf.rel")), style = "json2")
})

test_that("leituraNwlistcfCortes error", {
  expect_error(leituraNwlistcfCortes(test_path("empatyData", "nwlistcf.rel")))
  expect_error(leituraNwlistcfCortes())
})
