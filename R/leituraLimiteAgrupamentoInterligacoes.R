#' Leitor dos dados do limite de agrupamento de intercambio 
#'
#' Faz a leitura do arquivo do NEWAVE com dados do limite de agrupamento de intercambio (agrint.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.limiteAgrupamentoInterligacoes} data frame com dados de agrupamento livre de interligacoes
#' \itemize{
#' \item numero do agrupamento (\code{$codAgrup})
#' \item ano e mes (\code{$anoMes})
#' \item patamar (\code{$patamar})
#' \item limite de agrupamento de intercambio (\code{$limiteAgrup})
#' }
#'
#' @examples
#' \dontrun{
#' leituraLimiteAgrupamentoInterligacoes("C:/PDE2027_Caso080")}
#'
#' @export
leituraLimiteAgrupamentoInterligacoes <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de agrupamento livre de interligacoes (agrint.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(31) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema 
  dadosLimiteAgrint <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioLimiteAgrint <- which(stringr::str_detect(dadosLimiteAgrint, "LIMITES POR GRUPO"))
  # encontra o fim da informacao
  fimLimiteAgrint <- which(stringr::str_detect(dadosLimiteAgrint, "^ 999"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosLimiteAgrintTXT <- dadosLimiteAgrint[(inicioLimiteAgrint+3):(fimLimiteAgrint[2]-1)]
  
  # recupera a estrutura de dados
  suppressWarnings(
    df.limiteAgrupamentoInterligacoes <- data.frame(codAgrup = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT, 2 , 4)),
                                                    MES_INI = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 7 , 8)),
                                                    ANO_INI = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 10 , 13)),
                                                    MES_FIN = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 15 , 16)),
                                                    ANO_FIN = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 18 , 21)),
                                                    LIM_P1 = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 23 , 29)),
                                                    LIM_P2 = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 31 , 37)),
                                                    LIM_P3 = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 39 , 45)),
                                                    LIM_P4 = as.numeric(stringr::str_sub(dadosLimiteAgrintTXT , 47 , 53)))
  )
  
  df.limiteAgrupamentoInterligacoes <- df.limiteAgrupamentoInterligacoes %>% 
    dplyr::mutate(dataini = as.numeric(ANO_INI)*100 +  as.numeric(MES_INI), 
                  datafim = as.numeric(ANO_FIN)*100 + as.numeric(MES_FIN)) %>%
    dplyr::select(-c("ANO_INI", "ANO_FIN", "MES_INI", "MES_FIN"))
  
  df.limiteAgrupamentoInterligacoes <- dplyr::inner_join(dplyr::mutate(df.limiteAgrupamentoInterligacoes, aux = 1), dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = c("aux"), relationship = "many-to-many") %>% 
    dplyr::select(-c("aux")) %>% dplyr::mutate(valexpt = ifelse(((anoMes >= dataini) & (anoMes <= datafim | is.na(datafim))), "sim", "nao")) %>% 
    dplyr::filter(valexpt != "nao") %>% dplyr::select(-c("valexpt", "dataini", "datafim", "ano", "mes"))
  
  colnames(df.limiteAgrupamentoInterligacoes)[2:5] <- c("1","2","3","4")
  df.limiteAgrupamentoInterligacoes <- tidyr::pivot_longer(df.limiteAgrupamentoInterligacoes, 
                                                           -c("codAgrup","anoMes"), 
                                                           names_to = "patamar", 
                                                           values_to = "limiteAgrup") %>% 
    dplyr::mutate(patamar = as.integer(patamar)) %>% dplyr::filter(!is.na(limiteAgrup))
  
  df.pat <- dplyr::inner_join(dplyr::mutate(dplyr::distinct(dplyr::select(leituraAgrupamentoInterligacoes(pastaCaso),"codAgrup")),aux=1),
                              data.frame(patamar=1:nrow(dplyr::distinct(dplyr::select(leituraDadosDuracaoPatamar(pastaCaso),"patamar"))),aux=1), by="aux", relationship = "many-to-many")
  
  df.limiteAgrupamentoInterligacoes <- df.limiteAgrupamentoInterligacoes %>% 
    dplyr::right_join(dplyr::inner_join(dplyr::mutate(definePeriodo(pastaCaso),aux=1), df.pat,by= "aux", relationship = "many-to-many"),by=c("anoMes", "codAgrup", "patamar")) %>% 
    dplyr::mutate(limiteAgrup = ifelse(is.na(limiteAgrup),999999,limiteAgrup)) %>% 
    dplyr::select(-c("ano","mes","aux"))
  
  return(df.limiteAgrupamentoInterligacoes)
}
