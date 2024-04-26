#' Leitor do número de patamares de carga de um caso NEWAVE
#'
#' Faz a leitura do arquivo do NEWAVE com dados de patamares de carga (patamar.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{nPat} número de patamares de carga
#'
#' @examples
#' \dontrun{
#' leituraNumPat("C:/PDE2027_Caso080")}
#'
#' @export
leituraNumPat <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de duracao de patamar (patamar.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(10) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo patamar como um vetor de caracteres
  dadosPatamar <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o a informação do numero de patamares de carga
  nPat <- as.numeric(stringr::str_sub(dadosPatamar[3], 2, 3))
  
  return(nPat)
}  
