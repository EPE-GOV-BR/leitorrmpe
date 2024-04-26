test_that("leituraNumPat works", {
  expect_equal(leituraNumPat("testData"), 3)
})

test_that("leituraNumPat error", {
  expect_error(leituraNumPat("emptyData"))
  expect_error(leituraNumPat())
})
