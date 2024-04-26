test_that("leituraAntecipacaoGnl works", {
  expect_snapshot_value(leituraAntecipacaoGnl("testData"), style = "json2")
})

test_that("leituraAntecipacaoGnl error", {
  expect_error(leituraAntecipacaoGnl("emptyData"))
  expect_error(leituraAntecipacaoGnl())
})
