test_that("escreveHidr works", {
  caminho <- withr::local_tempdir()
  lt.hidr <- leituraDadosUsinasHidro("testData")
  expect_equal(escreveHidr(lt.hidr, paste0(caminho, "/hidr.dat")),
               paste0("Arquivo ", caminho, "/hidr.dat", " criado com sucesso!"))
})

test_that("escreveHidr error", {
  caminho <- withr::local_tempdir()
  lt.hidr <- leituraDadosUsinasHidro("testData")
  expect_error(escreveHidr(arquivo = paste0(caminho, "/hidr.dat")))
  expect_error(escreveHidr(lt.dadosUsinasHidroeletricas = lt.hidr))
})