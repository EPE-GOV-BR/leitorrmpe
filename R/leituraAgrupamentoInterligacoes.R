#' Leitor dos dados de agrupamento livre de interligacoes 
#'
#' Faz a leitura do arquivo do NEWAVE com dados de agrupamento livre de interligacoes (agrint.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.agrupamentoInterligacoes} data frame com dados de agrupamento livre de interligacoes
#' \itemize{
#' \item numero do agrupamento (\code{$codAgrup})
#' \item subsistema/submercado de origem da interligacao que compoe o agrupamento (\code{$codSubsistemaOrigem})
#' \item subsistema/submercado de destino da interligacao que compoe o agrupamento (\code{$codSubsistemaDestino})
#' \item coeficiente associado a interligacao que compoe o agrupamento (\code{$coeficiente})
#' }
#'
#' @examples
#' \dontrun{
#' leituraAgrupamentoInterligacoes("C:/PDE2027_Caso080")}
#' 
#' @importFrom dplyr %>%
#'
#' @export
leituraAgrupamentoInterligacoes <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de agrupamento livre de interligacoes (agrint.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(31) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo agrint 
  dadosAgrint <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioAgrint <- which(stringr::str_detect(dadosAgrint, "AGRUPAMENTOS DE"))
  # encontra o fim da informacao
  fimAgrint <- which(stringr::str_detect(dadosAgrint, "LIMITES POR GRUPO"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosAgrintTXT <- dadosAgrint[(inicioAgrint+3):(fimAgrint-2)]
  
  # recupera a estrutura de dados 
  df.agrupamentoInterligacoes <- data.frame(codAgrup = as.numeric(stringr::str_sub(dadosAgrintTXT, 2 , 4)),
                                            codSubsistemaOrigem = as.numeric(stringr::str_sub(dadosAgrintTXT , 6 , 8)),
                                            codSubsistemaDestino = as.numeric(stringr::str_sub(dadosAgrintTXT , 10 , 12)),
                                            coeficiente = as.numeric(stringr::str_sub(dadosAgrintTXT , 14 , 20)))
  
  return(df.agrupamentoInterligacoes)
}
