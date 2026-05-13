test_that("escreveVazoes works", {
  caminho <- withr::local_tempdir()
  df.vazoes <- leituraVazoes("testData")
  expect_equal(escreveVazoes(df.vazoes, paste0(caminho, "/vazoes.dat")),
               paste0("Arquivo ", caminho, "/vazoes.dat", " criado com sucesso!"))
})

test_that("escreveVazoes error", {
  caminho <- withr::local_tempdir()
  df.vazoes <- leituraVazoes("testData")
  expect_error(escreveVazoes(NULL, paste0(caminho, "/vazoes.dat")))
  expect_error(escreveVazoes(df.vazoes, NULL))
})