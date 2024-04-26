#' Leitor dos dados de usinas termoeletricas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de usinas termoeletricas (term.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosUsinasTermoeletricas} data frame com dados de usinas termoeletricas
#' \itemize{
#' \item numero da usina termica (\code{$codUsinaTermica})
#' \item periodo (anoMes-AAAAMM) (\code{$anoMes})
#' \item capacidade instalada (MW) (\code{$capacidaInstalada})
#' \item fator de capacidade maximo (\%) (\code{$FCMaximo})
#' \item TEIF da usina termica (\%) (\code{$TEIF})
#' \item indisponibilidade programada (IP) da usina termica para os demais anos do estudo (\%) (\code{$IP})
#' \item geracao termica minima (MWmes) (\code{$GTMin})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosUsinasTermoeletricas("C:/PDE2027_Caso080")}
#'
#' @export
leituraDadosUsinasTermoeletricas <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de usinas termoeletricas (term.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(6) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema como um vetor de caracteres
  term_dados <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  
  # cria a estrutura de dados contendo as informacoes iniciais do arquivo term
  df.dadosUsinasTermoeletricas <- data.frame(codUsinaTermica = as.numeric(stringr::str_sub(term_dados[3:length(term_dados)], 2, 4)),
                                             nomeUsina = stringr::str_sub(term_dados[3:length(term_dados)], 6, 17), 
                                             capacidaInstalada = as.numeric(stringr::str_sub(term_dados[3:length(term_dados)], 20, 24)),
                                             FCMaximo = as.numeric(stringr::str_sub(term_dados[3:length(term_dados)], 26, 29)),
                                             TEIF = as.numeric(stringr::str_sub(term_dados[3:length(term_dados)], 32, 37)),
                                             IP = as.numeric(stringr::str_sub(term_dados[3:length(term_dados)], 39, 44)))
  
  # cria a estrutura de dados contendo as informacoes adicionais do arquivo term, referente a geracao minima
  df.dadosUsinasTermoeletricasSub <- paste0(stringr::str_sub(term_dados[2:length(term_dados)], 1, 4), " ", stringr::str_sub(term_dados[2:length(term_dados)], 46, 128)) %>% 
    stringr::str_squish() %>% 
    I() %>% 
    readr::read_delim(" ", show_col_types = FALSE)
  
  # renomeia coluna dos meses
  names(df.dadosUsinasTermoeletricasSub)[c(1:13)] <- c("codUsinaTermica",seq(1,12))
  
  # transforma todas as colunas (exceto a primeira) em linhas
  df.dadosUsinasTermoeletricasSub <- tidyr::pivot_longer(df.dadosUsinasTermoeletricasSub, -1, names_to = "mes", values_to = "GTMin")
  
  # cria a estrutura de dados contendo as informacoes adicionais do arquivo term, referente a geracao minima
  df.dadosUsinasTermoeletricasSubDemaisANos <- paste0(stringr::str_sub(term_dados[2:length(term_dados)], 1, 4), " ", stringr::str_sub(term_dados[2:length(term_dados)], 130, 135)) %>% 
    stringr::str_squish() %>% 
    I() %>% 
    readr::read_delim(" ", show_col_types = FALSE)
  
  # renomeia colunas
  names(df.dadosUsinasTermoeletricasSubDemaisANos)[c(1:2)] <- c("codUsinaTermica","GTMin")
  
  # transforma todas as colunas (exceto a primeira) em linhas
  df.dadosUsinasTermoeletricasSubDemaisANos <- tidyr::pivot_longer(df.dadosUsinasTermoeletricasSubDemaisANos, -c(1), names_to = "parametros", values_to = "GTMinDA")
  
  # leitura dos dados gerais do caso
  df.dadosGerais <- leituraDadosGerais(pastaCaso)
  
  # mescla as informacoes das duas estruturas anteriores, para a mesma usina
  # mescla o dataframe anterior com a estrutura que define o periodo de simulacao (anoMes - AAAAMM)
  # tranforma colunas em linhas, com a coluna "parametros" definindo as informacoes de interesse das termicas
  # mescla as informacoes de Geracao Minima dos demais anos
  # reescreve o valor de Geracao Minima para os demais anos do horizonte de estudo, a partir do 2o ano
  # transforma linhas (coluna "parametros") em colunas
  df.dadosUsinasTermoeletricas <- dplyr::inner_join(df.dadosUsinasTermoeletricas, df.dadosUsinasTermoeletricasSub, by = c("codUsinaTermica" = "codUsinaTermica"), relationship = "many-to-many") %>% 
    dplyr::mutate(mes = as.numeric(mes), GTMin = as.numeric(GTMin)) %>% 
    dplyr::select(-c("nomeUsina")) %>% 
    dplyr::inner_join(definePeriodo(pastaCaso), by = c("mes" = "mes"), relationship = "many-to-many") %>% 
    dplyr::filter(!is.na(codUsinaTermica)) %>% dplyr::select(-c("mes", "ano")) %>% 
    tidyr::pivot_longer(-c(1, 7), names_to = "parametros", values_to = "valor") %>% 
    dplyr::left_join(df.dadosUsinasTermoeletricasSubDemaisANos, by=c("codUsinaTermica","parametros")) %>% 
    dplyr::mutate(valor = ifelse((anoMes >= (((df.dadosGerais$anoInicio+1)*100+1))) & (parametros == "GTMin"), GTMinDA, valor)) %>% 
    dplyr::select(-c("GTMinDA")) %>% 
    dplyr::group_by_at(dplyr::vars(-valor)) %>%  
    dplyr::mutate(row_id=1:dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = "parametros", values_from = "valor") %>% 
    dplyr::select(-row_id) 
  
  return(df.dadosUsinasTermoeletricas)
}
