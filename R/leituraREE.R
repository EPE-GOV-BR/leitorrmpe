#' Leitor dos dados dos Reservatorios Equivalentes de Energia (REE)
#'
#' Faz a leitura do arquivo do NEWAVE com informacao dos REE (ree.dat) e recupera o codigo, nome e sistema de cada REE cadastrada. Usa a funcao \code{\link{leituraArquivos}}
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#' 
#' @return \code{df.ree} data frame com os dados dos REE
#' \itemize{
#'   \item codigo do REE (\code{$codREE})
#'   \item nome (\code{$nomeREE})
#'   \item codigo do subsistema do REE (\code{$codSubsistema})
#'   }
#'
#' @examples
#' \dontrun{
#' leituraREE("C:/PDE2027_Caso080") }
#'
#' @export
leituraREE <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(36) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioDados <- which(stringr::str_detect(dadosBrutos, "XXX\\|XXXXXXXXXX\\|  XXX"))
  if (length(inicioDados) == 0) {
    stop("localiza\u00E7\u00E3o de in\u00EDcio de dados n\u00E3o encontrada!")
  }
  # encontra o fim da informacao
  fimDados <- which(stringr::str_detect(dadosBrutos, "999"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dados <- dadosBrutos[(inicioDados+1):(fimDados-1)]
  # recupera a estrutura de dados
  df.ree <- data.frame(codREE = as.numeric(stringr::str_sub(dados, 2, 4)),
                       nomeREE = stringr::str_squish(stringr::str_sub(dados, 6, 15)),
                       codSubsistema = as.numeric(stringr::str_sub(dados, 19, 21)))
  return(df.ree)
}
