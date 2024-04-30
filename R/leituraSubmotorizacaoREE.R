#' Leitor dos dados de submotorizacao por REE
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de submotorizacao por REE (relat.d**). Usa a funcao \code{\link{leituraArquivos}}
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.submotorizacao} data frame com os valores de submotorizacao por REE
#' \itemize{
#' \item nome do REE (\code{$nomeREE})
#' \item valor de ano e mes (\code{$anoMes})
#' \item energia de submotorizacao [MWmes] (\code{$submotorizacao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraSubmotorizacaoREE("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraSubmotorizacaoREE <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(13) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))

  # encontra o inicio da informacao de submotorizacao
  inicioDados <- which(stringr::str_detect(dadosBrutos, "SUBMOTORIZACAO TOTAL POR REE"))
  if (length(inicioDados) == 0) {
    stop("localiza\u00E7\u00E3o da SUBMOTORIZACAO TOTAL POR REE n\u00E3o encontrada!")
  }
  # encontra todos o fins de paginas
  fimPagina <- which(stringr::str_detect(dadosBrutos, "\f"))
  # encontra o fim da submotorizacao para casos com restricao por REE
  fimPMO <- which(stringr::str_detect(dadosBrutos, "SUBMOTORIZACAO TOTAL COM RESTRICAO ELETRICA POR REE"))
  # encontra o fim da informacao de submotorizacao
  if (length(fimPMO) == 0) {
    fimDados <- min(fimPagina[fimPagina > inicioDados])
  } else {
    fimDados <- fimPMO
  }
  # filtra somente a parte do vetor que tem os dados de interesse
  submotorizacaoTXT <- dadosBrutos[inicioDados:fimDados]
  # encontra as REEs
  nomeREE <- submotorizacaoTXT[which(stringr::str_detect(submotorizacaoTXT, "REE:"))] %>%
    stringr::str_remove("REE:") %>%
    stringr::str_trim()
  # localiza a posicao do inicio de dados de submotorizacao de cada REE
  inicioREE <- which(stringr::str_detect(submotorizacaoTXT, "ANO"))
  # localiza a posicao do fim de dados de submotorizacao de cada REE
  fimREE <- which(stringr::str_detect(submotorizacaoTXT, "X------------------------------------------------------------------------------------------X")) - 2

  # cria data frame de base para armazenar os dados de submotorizacao de todas as REEs
  df.submotorizacao <- tidyr::tibble(nomeREE = character(), anoMes = numeric(), submotorizacao = numeric())

  for (andaRee in 1:length(nomeREE)) {
    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
    df.submotorizacaoRee <- submotorizacaoTXT[inicioREE[andaRee]:fimREE[andaRee]] %>%
      stringr::str_squish() %>%
      I() %>%
      readr::read_delim(" ", show_col_types = FALSE) %>%
      tidyr::pivot_longer(cols = -ANO, names_to = "mes", values_to = "submotorizacao") %>%
      dplyr::mutate(nomeREE = nomeREE[andaRee], anoMes = (ANO * 100 + as.numeric(mes))) %>%
      dplyr::select(nomeREE, anoMes, submotorizacao)
    # concatena dados num data frame unico
    df.submotorizacao <- rbind(df.submotorizacao, df.submotorizacaoRee)
  }
  return(df.submotorizacao)
}
