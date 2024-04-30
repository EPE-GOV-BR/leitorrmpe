#' Leitor dos nomes dos arquivos do NEWAVE
#'
#' Procura o arquivo caso.dat (nos formatos Caso.dat, caso.dat e CASO.DAT) e faz a leitura do arquivo do CEPEL
#' indicado nele (geralmente arquivos.dat) para recuperar a lista de arquivos de entrada e resultados utilizados pelo NEWAVE.
#'
#' @param pastaCaso caracter com localizacao do arquivo caso.dat.
#'
#' @return \code{df.arquivos} data frame com os arquivos utilizados pelo NEWAVE.
#' \itemize{
#' \item descricao (\code{$descricao})
#' \item nomes dos arquivos (\code{$arquivo})
#' }
#'
#' @examples
#' \dontrun{
#' leituraArquivos("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraArquivos <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
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

  df.arquivos <- readr::read_delim(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")),
    delim = ":",
    col_names = F, locale = readr::locale(encoding = "latin1"), col_types = "cc"
  )
  if (nrow(df.arquivos) == 0) {
    stop(nomeArquivo, " vazio")
  }
  colnames(df.arquivos) <- c("descricao", "arquivo")
  # remove os espacos no inicio e fim da descricao e garante que estao em maiusculo
  df.arquivos <- df.arquivos %>% dplyr::mutate(
    descricao = stringr::str_squish(toupper(descricao)),
    arquivo = stringr::str_squish(arquivo)
  )

  return(df.arquivos)
}
