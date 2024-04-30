#' Leitor dos dados de antecipação de despacho de usinas térmicas GNL
#'
#' Faz a leitura do arquivo do NEWAVE com dados de antecipação de despacho de usinas térmicas GNL (adterm.dat)
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.antecipacaoDespachoGnl} data frame com os dados de antecipação de despacho de usinas térmicas GNL
#' \itemize{
#' \item numero da usina térmica (\code{$codUsina})
#' \item numero do mês para despacho antecipado (\code{$numMes})
#' \item patamar de carga (\code{$patamar})
#' \item despacho antecipado [MWmes] (\code{$despacho})
#' }
#'
#' @examples
#' \dontrun{
#' leituraAntecipacaoGnl("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraAntecipacaoGnl <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo adterm de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(32) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o numero de patamares do caso
  nPat <- leituraNumPat(pastaCaso)

  # le o arquivo sistema como um vetor de caracteres
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))

  # Encontra as colunas
  posicaoColunasInicio <- c(2, seq.int(25, 25 + 12 * (nPat - 1), 12))
  posicaoColunasFim <- c(5, seq.int(34, 34 + 12 * (nPat - 1), 12))

  df.antecipacaoDespachoGnl <- readr::read_fwf(I(dadosBrutos[-length(dadosBrutos)]),
    col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
      posicaoColunasInicio,
      # vetor com as posicoes finais de cada campo
      posicaoColunasFim,
      c("codUsina", seq(1, nPat))
    ),
    skip = 2,
    show_col_types = FALSE
  ) %>%
    tidyr::fill(codUsina) %>%
    dplyr::filter(!is.na(`1`)) %>%
    dplyr::group_by(codUsina) %>%
    dplyr::mutate(lag = 1:dplyr::n()) %>%
    dplyr::select(codUsina, lag, dplyr::everything()) %>%
    tidyr::pivot_longer(3:(2 + nPat), names_to = "patamar", values_to = "despacho") %>%
    dplyr::mutate(patamar = as.numeric(patamar))

  return(df.antecipacaoDespachoGnl)
}
