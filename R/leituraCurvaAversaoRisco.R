#' Leitor dos dados da curva de aversao a risco
#'
#' Faz a leitura do arquivo do NEWAVE com dados da curva de aversao a risco (curva.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 59
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#'@return \code{lt.curvaAversaoRisco} lista com data frames com dados da curva de aversao a risco
#' \itemize{
#' \item \code{df.penalidade} data frame com dados de alteracao de configuracao hidroeletrica
#' \itemize{
#' \item numero do REE (\code{$codREE})
#' \item penalidade por violacao da curva de seguranca ou restricao de volume minimo operativo, por REE [$/MWh] (\code{$custo})
#' }
#' 
#' \item \code{df.curvaSeguranca} data frame com dados de alteracao de configuracao hidroeletrica
#' \itemize{
#' \item numero do REE (\code{$codREE})
#' \item ano e mes (\code{$anoMes})
#' \item percentual da energia armazenavel maxima (EM % DE EARMX) (\code{$energiaMaxima})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraCurvaAversaoRisco("C:/PDE2027_Caso080")}
#'
#' @export
leituraCurvaAversaoRisco <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de mercado (curva.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivoCurva <- leituraArquivos(pastaCaso) %>% dplyr::slice(30) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivoCurva, sep = "/"))) {
    stop(paste0(arquivoCurva, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo curva como um vetor de caracteres
  dadosCurvaAversaoRisco <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivoCurva, sep = "/")), locale = readr::locale(encoding = "latin1"), skip_empty_rows = T)
  
  verificaVazio <- which(stringr::str_detect(dadosCurvaAversaoRisco, "SISTEMA"))
  
  #verifica se o arquivo possui dados para penalidade 
  if(stringr::str_detect(dadosCurvaAversaoRisco[verificaVazio+2], "999") == FALSE){
    
    # criacao do dataframe df.penalidades
    # encontra o inicio da informacao
    inicioPenalidades <- which(stringr::str_detect(dadosCurvaAversaoRisco, "SISTEMA"))
    # encontra o fim da informacao
    fimPenalidades <- which(stringr::str_detect(dadosCurvaAversaoRisco, "CURVA DE SEGURANCA"))
    # filtra somente a parte do vetor que tem os dados de interesse
    dadosPenalidadesTXT <- dadosCurvaAversaoRisco[(inicioPenalidades+2):(fimPenalidades-2)]
    
    df.penalidade <- readr::read_fwf(I(dadosPenalidadesTXT),
                                     col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                       c(2, 12),
                                       # vetor com as posicoes finais de cada campo
                                       c(4, 18),
                                       # nome colunas
                                       c('codREE', 'custo')),
                                     col_types = "id")
    
    # criacao do dataframe df.curvaSeguranca
    # encontra o inicio da informacao
    inicioCurvaSeguranca <- which(stringr::str_detect(dadosCurvaAversaoRisco, "CURVA DE SEGURANCA"))
    # encontra o fim da informacao
    fimCurvaSeguranca <- which(stringr::str_detect(dadosCurvaAversaoRisco, "^9999"))
    # filtra somente a parte do vetor que tem os dados de interesse
    dadosCurvaSegurancaTXT <- dadosCurvaAversaoRisco[(inicioCurvaSeguranca+3):(fimCurvaSeguranca-1)]
    
    #filtro para separar REE
    filtro <- readr::read_fwf(I(dadosCurvaSegurancaTXT),
                              col_positions = readr::fwf_positions(1, 4, "classe"),
                              col_types = "c", skip_empty_rows = F) %>%
      tidyr::fill(everything()) %>% dplyr::pull()
    codREE <- filtro[!filtro %in% (filtro[duplicated(filtro)] %>% unique())]
    quantosAnos <- filtro[filtro %in% (filtro[duplicated(filtro)] %>% unique())] %>% unique() %>% length()
    filtro <- !filtro %in% codREE
    dadosCurvaSegurancaTXT <- dadosCurvaSegurancaTXT[filtro]
    
    df.curvaSeguranca <- readr::read_fwf(I(dadosCurvaSegurancaTXT),
                                         col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                           c(1, 7, 13, 19, 25, 31, 37, 43, 49, 55, 61, 67, 73),
                                           # vetor com as posicoes finais de cada campo
                                           c(4, 11, 17, 23, 29, 35, 41, 47, 53, 59, 65, 71, 77),
                                           # nome colunas
                                           c('ano', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                         col_types = "idddddddddddd")
    
    # inclusao do codREE
    codREE <- rep(codREE, each = quantosAnos) %>% as.numeric()
    df.curvaSeguranca <- df.curvaSeguranca %>% 
      dplyr::mutate(codREE = codREE)
    
    # tranforma colunas em linhas, exceto as colunas "ano" e "codREE". 
    # o objetivo  criar duas colunas com as informacoes do "mes" e "energiaMaxima"
    # cria o campo anoMes (AAAAMM)
    # seleciona apenas os campos de interesse
    df.curvaSeguranca <-  tidyr::pivot_longer(df.curvaSeguranca, -c("ano", "codREE"), 
                                              names_to = "mes", 
                                              values_to = "energiaMaxima") %>% 
      dplyr::mutate(anoMes = as.numeric(ano)*100 + as.numeric(mes)) %>% 
      dplyr::select(codREE,anoMes,energiaMaxima) %>% 
      dplyr::filter(!is.na(energiaMaxima))
    
    
    lt.curvaAversaoRisco <- list(df.penalidade = df.penalidade, df.curvaSeguranca = df.curvaSeguranca)
    
    return(lt.curvaAversaoRisco)
  }
}
