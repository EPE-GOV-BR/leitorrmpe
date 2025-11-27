test_that("leituraFamiliasCurvasJusante works", {
  expect_snapshot_value(leituraFamiliasCurvasJusante(test_path("testData", "polinjus.dat")), style = "serialize")
})

test_that("leituraFamiliasCurvasJusante csv works", {
  expect_snapshot_value(leituraFamiliasCurvasJusante(test_path("testData", "polinjus.csv")), style = "serialize")
})

test_that("leituraFamiliasCurvasJusante error", {
  expect_error(leituraFamiliasCurvasJusante(test_path("emptyData", "polinjus.dat")))
  expect_error(leituraFamiliasCurvasJusante())
})