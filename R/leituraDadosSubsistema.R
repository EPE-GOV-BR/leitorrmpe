#' Leitor dos dados dos subsistemas/submercados
#'
#' Faz a leitura do arquivo do NEWAVE com dados dos subsistemas/submercados (sistema.*) 
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#' 
#' @return \code{df.subsistema} data frame com os dados dos subsistemas/submercados
#' \itemize{
#' \item numero do subsistema/submercado (\code{$codSubsistema}) 
#' \item nome do subsistema/submercado (\code{$nomeSubsistema})
#' \item tipo de subsistema: ficticio [1], ou real [0] (\code{$tipoFicticio})
#' \item custo de deficit em $/MWh (\code{$deficit})
#' }
#'
#' @examples
#' \dontrun{
#' leituraSubsistema("C:/PDE2027_Caso080") }
#'
#' @export
leituraDadosSubsistema <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(2) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  
  # encontra o inicio da informacao
  inicioSistema <- which(stringr::str_detect(dadosBrutos, "XXX\\|XXXXXXXXXX\\|..\\|XXXX.XX XXXX.XX XXXX.XX XXXX.XX\\|X.XXX X.XXX X.XXX X.XXX\\|"))
  # encontra o fim da informacao
  fimSistema <- which(stringr::str_detect(dadosBrutos, "LIMITES DE INTERCAMBIO"))
  # filtra somente a parte do vetor que tem os dados de interesse
  sistemaTXT <- dadosBrutos[(inicioSistema+1):(fimSistema-2)]
  # recupera os tipos de sistema - 0 real, 1 ficticio
  tipoSistema <- stringr::str_sub(sistemaTXT, 16, 18) %>% as.numeric()
  # recupera a estrutura de dados quando sistema real (com campo de custo de defict)
  df.sistema <- data.frame(codSubsistema = as.numeric(stringr::str_sub(sistemaTXT, 2, 4)),
                           nomeSubsistema = stringr::str_sub(sistemaTXT, 6, 15), 
                           tipoFicticio = as.numeric(stringr::str_sub(sistemaTXT, 18, 18)),
                           deficit = as.numeric(stringr::str_sub(sistemaTXT, 20, 26)))
  
  return(df.sistema)
}
