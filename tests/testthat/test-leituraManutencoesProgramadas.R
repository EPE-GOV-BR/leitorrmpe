test_that("leituraManutencoesProgramadas works", {
  expect_snapshot_value(leituraManutencoesProgramadas("testData"), style = "json2")
})

test_that("leituraManutencoesProgramadas error", {
  expect_error(leituraManutencoesProgramadas("emptyData"))
  expect_error(leituraManutencoesProgramadas())
})