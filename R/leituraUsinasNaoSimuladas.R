#' Leitor dos dados das usinas nao simuladas por subsistema/submercado
#'
#' Faz a leitura do arquivo do NEWAVE com dados das usinas nao simuladas por subsistema/submercado (sistema.*) 
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#' 
#' @return \code{df.UsinasNaoSimuladas} data frame com os dados das usinas nao simuladas
#' \itemize{
#' \item numero do subsistema/submercado (\code{$codSubsistema}) 
#' \item numero do bloco de usinas nao simuladas (\code{$codBlocoUNS})
#' \item nome do bloco de usinas nao simuladas (\code{$nomeBlocoUNS})
#' \item ano e mes do dado (\code{$anoMes})
#' \item energia em MWmedios (\code{$geracaoUNS})
#' }
#'
#' @examples
#' \dontrun{
#' leituraUsinasNaoSimuladas("C:/PDE2027_Caso080")}
#'
#' @export
leituraUsinasNaoSimuladas <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(2) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  # le o arquivo sistema como um vetor de caracteres
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioUsinasNaoSimuladas <- which(stringr::str_detect(dadosBrutos, "GERACAO DE USINAS NAO SIMULADAS|GERACAO DE PEQUENAS USINAS"))
  # encontra o fim da informacao
  fimUsinasNaoSimuladas <- which(stringr::str_detect(dadosBrutos, " MAXIMO RECEBIMENTO"))
  fimUsinasNaoSimuladas <- ifelse(length(fimUsinasNaoSimuladas) == 0,length(dadosBrutos),fimUsinasNaoSimuladas-2)
  # filtra somente a parte do vetor que tem os dados de interesse
  UsinasNaoSimuladasTXT <- dadosBrutos[(inicioUsinasNaoSimuladas+3):(fimUsinasNaoSimuladas)]
  
  # filtra somente linhas com nomes e numeros dos blocos
  blocos <- UsinasNaoSimuladasTXT %>% stringr::str_detect("^ ")
  blocos <- UsinasNaoSimuladasTXT[blocos]
  df.blocos <- data.frame(codSubsistema = as.integer(stringr::str_sub(blocos, 2, 4)),
                          codBlocoUNS = as.integer(stringr::str_sub(blocos, 7, 9)),
                          nomeBlocoUNS = stringr::str_sub(blocos, 12, 31)) %>% 
    dplyr::filter(codSubsistema != 999) %>% 
    dplyr::mutate(aux = seq(1:nrow(.)))
  
  # fitlra somente linhas com dados
  dados <- UsinasNaoSimuladasTXT %>% stringr::str_detect("^[0-9]")
  dados <- UsinasNaoSimuladasTXT[dados]
  
  # recupera a estrutura de dados
  df.dados <- data.frame(ano = as.integer(stringr::str_sub(dados, 1, 4)),
                         JAN = as.numeric(stringr::str_sub(dados, 8, 14)),
                         FEV = as.numeric(stringr::str_sub(dados, 16, 22)),
                         MAR = as.numeric(stringr::str_sub(dados, 24, 30)), 
                         ABR = as.numeric(stringr::str_sub(dados, 32, 38)),
                         MAI = as.numeric(stringr::str_sub(dados, 40, 46)), 
                         JUN = as.numeric(stringr::str_sub(dados, 48, 54)), 
                         JUL = as.numeric(stringr::str_sub(dados, 56, 62)), 
                         AGO = as.numeric(stringr::str_sub(dados, 64, 70)),
                         OUT = as.numeric(stringr::str_sub(dados, 72, 78)), 
                         SEP = as.numeric(stringr::str_sub(dados, 80, 86)), 
                         NOV = as.numeric(stringr::str_sub(dados, 88, 94)),
                         DEZ = as.numeric(stringr::str_sub(dados, 96, 102)))
  
  nAnos <- nrow(df.dados)/nrow(df.blocos)
  
  df.dados <- df.dados %>% 
    dplyr::mutate(aux = rep(seq(1:nrow(df.blocos)), each = nAnos)) %>% 
    dplyr::left_join(df.blocos, by = "aux") %>% 
    dplyr::select(-aux) %>% 
    dplyr::select(codSubsistema, codBlocoUNS, nomeBlocoUNS, dplyr::everything())
  
  # renomeia colunas
  colnames(df.dados)[5:16] <- c("1","2","3","4","5","6","7","8","9","10","11","12")
  
  # tranforma colunas em linhas, exceto as colunas "ano","codBlocoUNS","nomeBlocoUNS" e "codSubsistema". 
  # o objetivo  criar duas colunas com as informacoes do "mes" e "geracaoUNS"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.UsinasNaoSimuladas <- tidyr::pivot_longer(df.dados, 
                                               -c("ano", "codBlocoUNS", "nomeBlocoUNS", "codSubsistema"), 
                                               names_to = "mes", 
                                               values_to = "geracaoUNS") %>% 
    dplyr::mutate(anoMes = as.numeric(ano)*100 + as.numeric(mes)) %>% 
    dplyr::select(codSubsistema, codBlocoUNS, nomeBlocoUNS, anoMes, geracaoUNS)
  
  return(df.UsinasNaoSimuladas)
}  
