#' Leitor dos dados de profundidade patamares das usinas nao simuladas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de profundidade patamares de usinas nao simuladas (patamar.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosPatamarUsinasNaoSimuladas} data frame com dados de patamares de usinas nao simuladas 
#' \itemize{
#' \item numero do subsistema/submercado  (\code{$codSubsistema})
#' \item numero do bloco de usinas nao simuladas  (\code{$codBlocoUNS})
#' \item numero do patamar (\code{$patamar})
#' \item ano e mes referente a informacao (\code{$anoMes})
#' \item profundidade de patamar das usinas nao simuladas (\code{$profundidadeUNS})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosPatamarUsinasNaoSimuladas("C:/PDE2027_Caso080")}
#'
#' @export
leituraDadosPatamarUsinasNaoSimuladas <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de duracao de patamar (patamar.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(10) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema como um vetor de caracteres 
  dadosPatamarUsinas <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioPatamarUsina <- which(stringr::str_detect(dadosPatamarUsinas, "BLOCO DE USINAS NAO SIMULADAS"))
  
  if (length(inicioPatamarUsina) != 0) {
    
    # encontra o fim da informacao
    fimPatamarUsina <- if (tail(dadosPatamarUsinas, n=1) == "9999") {length(dadosPatamarUsinas)-1 } else {length(dadosPatamarUsinas)}
    # filtra somente a parte do vetor que tem os dados de interesse
    dadosPatamarUsinasTXT <- dadosPatamarUsinas[(inicioPatamarUsina+2):fimPatamarUsina]
    
    # recupera a estrutura de dados
    df.dadosPatamarUsinasNaoSimuladas <- readr::read_fwf(I(dadosPatamarUsinasTXT),
                                                         col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                           c(4, 9, 16, 23, 30, 37, 44, 51, 58, 65, 72, 79, 86, 2, 6),
                                                           # vetor com as posicoes finais de cada campo
                                                           c(7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 4, 8),
                                                           # nome colunas
                                                           c('ano', 'JAN', 'FEV', 'MAR', 'ABR', 'MAI', 'JUN', 'JUL', 'AGO', 'SET', 'OUT', 'NOV', 'DEZ', 'codSubsistema', 'codBlocoUNS')),
                                                         col_types = "iddddddddddddii") %>% 
      suppressWarnings()
    
    # Ajusta as linhas das colunas "ano", "codBlocoUNS" e "codSubsistema", mantendo apenas os valores originais  
    # Estende o valor referente ao "ano" para os demais anos
    # Estende o valor referente ao "codBlocoUNS" para os demais anos
    # Estende o valor referente ao "codSubsistema" para os demais anos
    # Filtra para manter apenas as linhas com os valores anuais
    # Cria uma sequencia numerica para a representacao dos patamares. O valor retorna ao inicio no momento em que se altera o ANO.
    df.dadosPatamarUsinasNaoSimuladas <- df.dadosPatamarUsinasNaoSimuladas %>% 
      dplyr::mutate(codBlocoUNS = ifelse(!is.na(DEZ), NA , codBlocoUNS)) %>%
      dplyr::mutate(codSubsistema = ifelse(!is.na(DEZ), NA , codSubsistema)) %>% 
      dplyr::mutate(codSubsistema = zoo::na.locf(codSubsistema), 
                    codBlocoUNS = zoo::na.locf(codBlocoUNS)) %>% 
      dplyr::filter(!is.na(DEZ)) %>% 
      dplyr::mutate(ano = zoo::na.locf(ano)) %>% 
      dplyr::mutate(patamar = sequence(rle(ano)$lengths))
    
    # renomeia colunas
    colnames(df.dadosPatamarUsinasNaoSimuladas)[2:13] <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    
    # tranforma colunas em linhas, exceto as colunas "ano","codSubsistema","codBlocoUNS" e "patamar". 
    # o objetivo  criar duas colunas com as informacoes do "mes" e "profundidadeUNS"
    # cria o campo anoMes (AAAAMM)
    # seleciona apenas os campos de interesse
    df.dadosPatamarUsinasNaoSimuladas <-  tidyr::pivot_longer(df.dadosPatamarUsinasNaoSimuladas, 
                                                              -c("ano","codSubsistema","codBlocoUNS","patamar"), 
                                                              names_to = "mes", 
                                                              values_to = "profundidadeUNS") %>% 
      dplyr::mutate(anoMes = as.numeric(ano)*100 + as.numeric(mes)) %>% 
      dplyr::select(codSubsistema,codBlocoUNS,patamar,anoMes,profundidadeUNS)
    
  } else {
    # cria estrutura vazia caso nao seja identificado o "inicioPatamarUsina"
    df.dadosPatamarUsinasNaoSimuladas <- data.frame(codSubsistema=as.integer(),
                                                    codBlocoUNS=as.integer(),
                                                    patamar=as.integer(),
                                                    anoMes=as.integer(),
                                                    profundidadeUNS=as.numeric())
  }
  return(df.dadosPatamarUsinasNaoSimuladas)  
}  
