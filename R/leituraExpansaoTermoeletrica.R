#' Leitor dos dados de expansao termoeletrica
#'
#' Faz a leitura do arquivo do NEWAVE com dados de expansao termoeletrica (expt.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.ExpansaoTermoeletrica} data frame com dados de usinas termoeletricas
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
#' leituraExpansaoTermoeletrica("C:/PDE2027_Caso080")}
#'
#' @export
leituraExpansaoTermoeletrica <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de usinas termoeletricas (expt*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(9) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema como um vetor de caracteres
  expt_dados <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # cria a estrutura de dados contendo as informacoes sobre alteracoes dos parametros das usinas termicas
  df.ExpansaoTermoeletrica <- data.frame(codUsinaTermica = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 1, 4)),
                                         parametros = as.character(stringr::str_sub(expt_dados[3:length(expt_dados)], 6, 10)), 
                                         valornew = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 12, 19)),
                                         mi = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 21, 22)),
                                         anoi = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 24, 27)),
                                         mf = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 29, 30)),
                                         anof = as.numeric(stringr::str_sub(expt_dados[3:length(expt_dados)], 32, 35)))
  
  # verifica se o arquivo expt esta vazio
  if(nrow(dplyr::filter(df.ExpansaoTermoeletrica, !is.na(codUsinaTermica))) == 0){
    df.ExpansaoTermoeletrica <- data.frame(codUsinaTermica = numeric(), anoMes = numeric(), capacidaInstalada = numeric(), FCMaximo = numeric(), TEIF = numeric(), IP = numeric(), GTMin = numeric())
  }else{
    # se nao for vazio executa as modificacoes
    
    df.dadosGerais <- leituraDadosGerais(pastaCaso)
    # ordena o data frame para criar coluna com o ano de inicio da expansao seguinte
    df.ExpansaoTermoeletrica <- df.ExpansaoTermoeletrica %>% dplyr::arrange(codUsinaTermica, parametros, anoi)
    df.ExpansaoTermoeletrica <- df.ExpansaoTermoeletrica %>% 
      dplyr::mutate(anoSeg = ifelse((dplyr::lead(codUsinaTermica) == codUsinaTermica) & (dplyr::lead(parametros) == parametros), dplyr::lead(anoi), NA))
    
    # ajusta campos "mf-mes final" e "anof-ano final", substiutindo valores NA pelo ulimto periodo do horizonte de simulacao 
    # ou pelo ano inicial - 1 da expansao seguinte, se houver 
    # cria colunas de data inicial (datainci) e data final (datafim) no formato AAAAMM
    # mescla o dataframe anterior com a estrutura que define o periodo de simulacao (anoMes - AAAAMM)
    # avalia se o valor do CVU COnjuntural e valido para cada mes. 
    # Se a um determinado mes for (maior ou igual) a data inicial e (menor ou igual) a data final, o valor fica vigente
    # filtro dos valores vigentes, considerando apenas aqueles diferentes de "nao"
    df.ExpansaoTermoeletrica <- dplyr::mutate(df.ExpansaoTermoeletrica, mf = ifelse(is.na(mf), 12, mf),
                                              anof = ifelse(is.na(anof),
                                                            ifelse(is.na(anoSeg),
                                                                   df.dadosGerais$anoInicio+df.dadosGerais$duracaoEstudo-1,
                                                                   anoSeg - 1),
                                                            anof)) %>%
      dplyr::mutate(datainc = as.numeric(anoi*100 + mi),datafim = as.numeric(anof*100 + mf),aux = 1) %>% 
      dplyr::filter(!is.na(codUsinaTermica)) %>% 
      dplyr::select(-c("anoi", "anof", "mi", "mf", "anoSeg")) %>% 
      dplyr::inner_join(dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = "aux", relationship = "many-to-many") %>% 
      dplyr::select(-c("aux")) %>% 
      dplyr::mutate(validadeExpt = ifelse(((anoMes >= datainc) & (anoMes <= datafim)), "sim", "nao")) %>% 
      dplyr::filter(validadeExpt != "nao") %>% 
      dplyr::select(-c("validadeExpt","datainc","datafim","ano","mes")) %>% 
      dplyr::group_by_at(dplyr::vars(-valornew)) %>%  
      dplyr::mutate(row_id=1:dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = "parametros", values_from = "valornew")
    
    # adiciona campos caso nao existam no arquivo
    if(("TEIFT" %in% colnames(df.ExpansaoTermoeletrica))==FALSE) {df.ExpansaoTermoeletrica <- dplyr::mutate(df.ExpansaoTermoeletrica, TEIFT=NA)}
    if(("IPTER" %in% colnames(df.ExpansaoTermoeletrica))==FALSE)  {df.ExpansaoTermoeletrica <- dplyr::mutate(df.ExpansaoTermoeletrica, IPTER=NA)}
    if(("GTMIN" %in% colnames(df.ExpansaoTermoeletrica))==FALSE)  {df.ExpansaoTermoeletrica <- dplyr::mutate(df.ExpansaoTermoeletrica, GTMIN=NA)}
    if(("FCMAX" %in% colnames(df.ExpansaoTermoeletrica))==FALSE)  {df.ExpansaoTermoeletrica <- dplyr::mutate(df.ExpansaoTermoeletrica, FCMAX=NA)}
    
    df.ExpansaoTermoeletrica <- dplyr::select(df.ExpansaoTermoeletrica, codUsinaTermica,anoMes,POTEF,FCMAX,TEIFT,IPTER,GTMIN) %>% 
      dplyr::mutate(GTMIN = ifelse(is.na(GTMIN), 0, GTMIN))
    
    # renomeia colunas dos parametros conforme padrao de saida do arquivo term.*
    names(df.ExpansaoTermoeletrica)[c(3:7)] <- c("capacidaInstalada","FCMaximo","TEIF","IP","GTMin")
  }
  
  return(df.ExpansaoTermoeletrica)
}
