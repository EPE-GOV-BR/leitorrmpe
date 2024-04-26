#' Leitor dos dados de profundidade patamares de carga
#'
#' Faz a leitura do arquivo do NEWAVE com dados de profundidade de patamares de carga (patamar.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosProfundidadePatamarCarga} data frame com dados de profundidade de patamares de carga 
#' \itemize{
#' \item sistema (\code{$codSubsistema}) 
#' \item numero do patamar (\code{$patamar})
#' \item ano e mes do dado (\code{$anoMes})
#' \item profundidade de patamar de carga (\code{$profundidadeCarga})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosProfundidadePatamarCarga("C:/PDE2027_Caso080")}
#'
#' @export
leituraDadosProfundidadePatamarCarga <- function(pastaCaso){
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
  # encontra o inicio da informacao
  inicioPatamar <- which(stringr::str_detect(dadosPatamar, "SUBSISTEMA"))[1]
  # encontra o fim da informacao
  fimPatamar <- which(stringr::str_detect(dadosPatamar, "^9999"))[1]
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosPatamarTXT <- dadosPatamar[(inicioPatamar+4):(fimPatamar-1)]
  
  # recupera a estrutura de dados
  df.dadosProfundidadePatamarCarga <- data.frame(ano = as.numeric(stringr::str_sub(dadosPatamarTXT, 4, 7)),
                                                 JAN =as.numeric(stringr::str_sub(dadosPatamarTXT ,9,14)),
                                                 FEV = as.numeric(stringr::str_sub(dadosPatamarTXT, 16, 21)),
                                                 MAR = as.numeric(stringr::str_sub(dadosPatamarTXT, 23,28)), 
                                                 ABR = as.numeric(stringr::str_sub(dadosPatamarTXT ,30,35)),
                                                 MAI = as.numeric(stringr::str_sub(dadosPatamarTXT, 37,42)), 
                                                 JUN = as.numeric(stringr::str_sub(dadosPatamarTXT, 44,49)),
                                                 JUL = as.numeric(stringr::str_sub(dadosPatamarTXT, 51,56)), 
                                                 AGO = as.numeric(stringr::str_sub(dadosPatamarTXT, 58,63)), 
                                                 SET = as.numeric(stringr::str_sub(dadosPatamarTXT ,65,70)),
                                                 OUT = as.numeric(stringr::str_sub(dadosPatamarTXT, 72,77)), 
                                                 NOV = as.numeric(stringr::str_sub(dadosPatamarTXT, 79,84)), 
                                                 DEZ = as.numeric(stringr::str_sub(dadosPatamarTXT ,86,91)),
                                                 codSubsistema = as.numeric(stringr::str_sub(dadosPatamarTXT ,2,4)))
  
  # Ajusta as linhas das colunas "ano" e "codSubsistema", mantendo apenas os valores originais  
  # Estende o valor referente ao "codSubsistema" para os demais anos
  # Estende o valor referente ao "ano" para os demais anos
  # Cria uma sequencia numerica para a representacao dos patamares. O valor retorna ao inicio no momento em que se altera o ANO.
  df.dadosProfundidadePatamarCarga <- df.dadosProfundidadePatamarCarga %>% 
    dplyr::mutate(ano = ifelse(ano < 1000, NA, ano)) %>% 
    dplyr::mutate(codSubsistema = ifelse(!is.na(ano), NA, codSubsistema)) %>% 
    dplyr::mutate(codSubsistema = zoo::na.locf(codSubsistema)) %>% 
    dplyr::filter(!is.na(DEZ)) %>% dplyr::mutate(ano = zoo::na.locf(ano)) %>% 
    dplyr::mutate(patamar = sequence(rle(ano)$lengths))
  
  # renomeia colunas
  colnames(df.dadosProfundidadePatamarCarga)[2:13] <- c("1","2","3","4","5","6","7","8","9","10","11","12")
  
  # tranforma colunas em linhas, exceto as colunas "ano", "codSubsistema" e "patamar". O objetivo e criar duas colunas com as informacoes do "mes" e "profundidadeCarga"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.dadosProfundidadePatamarCarga <- tidyr::pivot_longer(df.dadosProfundidadePatamarCarga, 
                                                          -c("ano","codSubsistema","patamar"), 
                                                          names_to = "mes", 
                                                          values_to = "profundidadeCarga") %>% 
    dplyr::mutate(anoMes = as.numeric(ano)*100 + as.numeric(mes)) %>% 
    dplyr::select(codSubsistema,patamar,anoMes,profundidadeCarga)
  
  return(df.dadosProfundidadePatamarCarga)
}  
