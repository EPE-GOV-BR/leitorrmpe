#' Leitor dos dados de duracao de patamares de mercado 
#'
#' Faz a leitura do arquivo do NEWAVE com dados de duracao de patamares (patamar.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosDuracaoPatamar} data frame com dados de patamares de carga
#' \itemize{
#' \item numero do patamar (\code{$patamar})
#' \item ano e mes do dado (\code{$anoMes})
#' \item duracao do patamar (\code{$duracaoPatamar})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosDuracaoPatamar("C:/PDE2027_Caso080")}
#'
#' @export
leituraDadosDuracaoPatamar <- function(pastaCaso){
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
  inicioPatamar <- which(stringr::str_detect(dadosPatamar, " NUMERO DE PATAMARES"))
  # encontra o fim da informacao
  fimPatamar <- which(stringr::str_detect(dadosPatamar, "SUBSISTEMA"))[1]
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosPatamarTXT <- dadosPatamar[(inicioPatamar+6):(fimPatamar-1)]
  
  # recupera a estrutura de dados
  df.dadosDuracaoPatamar <- data.frame(ano = as.numeric(stringr::str_sub(dadosPatamarTXT, 1, 4)),
                                       JAN =as.numeric(stringr::str_sub(dadosPatamarTXT ,7,12)),
                                       FEV = as.numeric(stringr::str_sub(dadosPatamarTXT, 15, 20)),
                                       MAR = as.numeric(stringr::str_sub(dadosPatamarTXT, 23,28)), 
                                       ABR = as.numeric(stringr::str_sub(dadosPatamarTXT ,31,36)),
                                       MAI = as.numeric(stringr::str_sub(dadosPatamarTXT, 39,44)), 
                                       JUN = as.numeric(stringr::str_sub(dadosPatamarTXT, 47,52)),
                                       JUL = as.numeric(stringr::str_sub(dadosPatamarTXT, 55,60)), 
                                       AGO = as.numeric(stringr::str_sub(dadosPatamarTXT, 63,68)), 
                                       SET = as.numeric(stringr::str_sub(dadosPatamarTXT ,71,76)),
                                       OUT = as.numeric(stringr::str_sub(dadosPatamarTXT, 79,84)), 
                                       NOV = as.numeric(stringr::str_sub(dadosPatamarTXT, 87,92)), 
                                       DEZ = as.numeric(stringr::str_sub(dadosPatamarTXT ,95,100)))
  
  # Estende o valor referente ao ANO para os demais patamares.
  # Cria uma sequencia numerica para a representacao dos patamares. O valor retorna ao inicio no momento em que se altera o ANO.
  df.dadosDuracaoPatamar <- df.dadosDuracaoPatamar %>% 
    dplyr::mutate(ano = zoo::na.locf(ano)) %>% 
    dplyr::mutate(patamar = sequence(rle(ano)$lengths))
  
  # renomeia colunas
  colnames(df.dadosDuracaoPatamar)[2:13] <- c("1","2","3","4","5","6","7","8","9","10","11","12")
  
  # tranforma colunas em linhas, exceto as colunas "ano" e "patamar". 
  # o objetivo e criar duas colunas com as informacoes do "mes" e "duracaoPatamar"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.dadosDuracaoPatamar <-  tidyr::pivot_longer(df.dadosDuracaoPatamar, 
                                                 -c("ano", "patamar"), 
                                                 names_to = "mes", 
                                                 values_to = "duracaoPatamar") %>% 
    dplyr::mutate(anoMes = as.numeric(ano)*100 + as.numeric(mes)) %>% 
    dplyr::select(c("patamar","anoMes","duracaoPatamar"))
  
  # verifica se ha somente 1 patamar e passa o valor padrao de vazio para 1
  numeroPatamares <- df.dadosDuracaoPatamar %>% dplyr::pull(patamar) %>% unique() %>% length()
  if (numeroPatamares == 1) {
    df.dadosDuracaoPatamar  <- df.dadosDuracaoPatamar %>% 
      dplyr::mutate(duracaoPatamar = ifelse(is.na(duracaoPatamar), 1, duracaoPatamar))
  }
  
  return(df.dadosDuracaoPatamar)
}  
