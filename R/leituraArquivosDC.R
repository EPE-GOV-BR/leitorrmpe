#' Leitor dos nomes dos arquivos do DECOMP
#'
#' Procura o arquivo caso.dat (nos formatos Caso.dat, caso.dat e CASO.DAT) e faz a leitura do arquivo do CEPEL
#' indicado nele (geralmente rvX) para recuperar a lista de arquivos de entrada utilizados pelo DECOMP.
#'
#' @param pastaCaso string com localizacao do arquivo caso.dat.
#'
#' @return \code{df.arquivos} data frame com os arquivos utilizados pelo DECOMP.
#' \itemize{
#' \item nomes dos arquivos (\code{$arquivo})
#' }
#'
#' @examples
#' \dontrun{
#' leituraArquivosDC("C:/Pasta_Deck_DECOMP")
#' }
#'
#' @export
leituraArquivosDC <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do DECOMP")
  }

  if (!dir.exists(pastaCaso)) {
    stop(paste0("pasta ", pastaCaso, " n\u00E3o encontrada!"))
  }

  caso <- list.files(path = pastaCaso, pattern = "[cC]aso\\.dat|CASO\\.DAT")
  if (length(caso) != 1) {
    stop("caso.dat n\u00E3o encontrado ou multiplos arquivos similares!")
  }
  arquivo <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, caso, sep = "/")), locale = readr::locale(encoding = "latin1")) %>%
    stringr::str_squish() %>%
    dplyr::first()

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  df.arquivos <- data.frame(arquivo = readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1")))
  if (nrow(df.arquivos) == 0) {
    stop(nomeArquivo, " vazio")
  }

  return(df.arquivos)
}
