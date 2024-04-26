#' Leitor dos dados de series historicas para a simulacao final
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de series historicas para a simulacao final (shist.dat). Usa a funcao \code{\link{leituraArquivos}}
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{lt.seriesHistoriaSimulacaoFinal} lista com data frames com dados das series historicas para a simulacao final
#' \itemize{
#' \item \code{df.varredura} data frame com dados de varredura
#' \itemize{
#' \item simulacao com varredura da serie historica - 0: nao faz varredura; 1: faz (\code{$varredura})
#' \item ano inicio da varredura (a partir de 1932 inclusive) (\code{$anoInicio})
#' }
#' \item \code{df.seriesHistoricas} data frame com ano de inicio de simulacao das series historicas
#' \itemize{
#' \item ano historico de inicio da simulacao final (\code{$anoInicio})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraSeriesHistoricasSimulacaoFinal("C:/PDE2027_Caso080")}
#'
#' @export
leituraSeriesHistoricasSimulacaoFinal <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de series historicas para a simulacao final de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(17) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  
  # a varredura e um registro tipo 1
  df.varredura <- readr::read_fwf(I(dadosBrutos[2:3]),
                                  col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                    c(1, 5),
                                    # vetor com as posicoes finais de cada campo
                                    c(4, 8),
                                    # nome colunas
                                    c('varredura', 'anoInicio')),
                                  col_types = "ii", 
                                  na = "XXX.") %>% dplyr::filter(!is.na(varredura))
  
  
  
  # filtra somente a parte do vetor que tem os dados de interesse
  inicio <- which(stringr::str_detect(dadosBrutos, "SERIES PARA SIMULACAO"))
  fim <- which(stringr::str_detect(dadosBrutos, "^9999"))
  
  if (fim - inicio > 2) {
    # data frame de base para armazenar os anos de inicio da simulacao final das series historicas a serem simuladas
    df.seriesHistoricas <- readr::read_fwf(I(dadosBrutos[(inicio + 1):(fim - 1)]),
                                           col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                             c(1),
                                             # vetor com as posicoes finais de cada campo
                                             c(4),
                                             # nome colunas
                                             c('anoInicio')),
                                           col_types = "i",
                                           na = "XXXX")
  } else {
    df.seriesHistoricas <- tidyr::tibble(anoInicio = numeric())
  }
  
  lt.seriesHistoriaSimulacaoFinal <- list(df.varredura = df.varredura, df.seriesHistoricas = df.seriesHistoricas)
  
  return(lt.seriesHistoriaSimulacaoFinal)
}
