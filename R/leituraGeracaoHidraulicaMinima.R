#' Leitor de dados de geracao hidraulica minima
#'
#' Faz a leitura do arquivo do NEWAVE com dados de geracao hidraulica minima (ghmin.dat).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo de planejamento da operacao de sistemas hidrotermicos interligados de longo e medio prazos do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 65
#' OBS: O leitor desconsidera os dados para periodos PRE e POS.
#' 
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.geracaoHidraulicaMinima} lista com data frames com dados das usinas hidroeletricas
#' \itemize{
#' \item numero da usina hidroeletrica (\code{$codUsina})
#' \item mes e ano de inicio da restricao de geracao hidraulica minima (\code{$anoMes})
#' \item numero do patamar de carga (\code{$patamar})
#' \item geracao hidraulica minima da usina [MWmedio] (\code{$geracao})
#' 
#' }
#' 
#'
#' @examples
#' \dontrun{
#' leituraGeracaoHidraulicaMinima("C:/PDE2027_Caso080")}
#'
#' @export
leituraGeracaoHidraulicaMinima <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de geracao hidraulica minima (ghmin.dat) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(33) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  
  # le o arquivo curva como um vetor de caracteres
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"), skip_empty_rows = T)
  
  # filtra linhas de PRE e POS
  filtro <- readr::read_fwf(I(dadosBrutos),
                            col_positions = readr::fwf_positions(9, 12, "classe"),
                            col_types = "c", skip_empty_rows = F) %>%
    tidyr::fill(everything()) %>% dplyr::pull() %>% stringr::str_detect("^[0-9]+")
  dadosBrutos <- dadosBrutos[filtro]
  
  
  
  # data frame com posicoes e nomes das variaveis
  df.geracaoHidraulicaMinima <- readr::read_fwf(I(dadosBrutos),
                                                col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                  c(1, 6, 9, 15, 18),
                                                  # vetor com as posicoes finais de cada campo
                                                  c(3, 7, 12, 15, NA),
                                                  # nome colunas
                                                  c('codUsina', 'mes', 'ano', 'patamar', 'geracao')),
                                                col_types = "iiiid")
  
  
  df.geracaoHidraulicaMinima <- df.geracaoHidraulicaMinima %>%
    dplyr::mutate(ano = ano, anoMes = (ano * 100 + as.numeric(mes))) %>%
    dplyr::select(codUsina,anoMes, patamar, geracao)
  
  return(df.geracaoHidraulicaMinima)
}
