#' Leitor dos dados de manutencoes programadas de usinas termetricas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de manutencoes programadas de usinas termetricas (manutt.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosManutencao} data frame com dados de usinas termoeletricas
#' \itemize{
#' \item numero da usina termica (\code{$codUsinaTermica})
#' \item periodo (anoMes-AAAAMM) (\code{$anoMes})
#' \item indisponibilidade programada (IP) da usina termica (%) (\code{$IP})
#' }
#'
#' @examples
#' \dontrun{
#' leituraManutencoesProgramadas("C:/PDE2027_Caso80")}
#'
#' @export
leituraManutencoesProgramadas <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de manutencoes programadas de usinas termetricas (manutt.*) 
  # de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(18) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  df.dadosGerais <- leituraDadosGerais(pastaCaso)
  
  df.dadosManutencao <- readr::read_fwf(paste(pastaCaso, arquivo, sep = "/"),
                                        locale = readr::locale(encoding = "latin1"),
                                        col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                          c(18, 38, 41, 50, 56),
                                          # vetor com as posicoes finais de cada campo
                                          c(20, 39, 48, 52, 62),
                                          # nome colunas
                                          c('codUsinaTermica', 'ug', 'dataInicio', 'duracao', 'pot')),
                                        col_types = "iicid",
                                        skip = 2)
  
  if (nrow(df.dadosManutencao) > 0 && sum(!is.na(df.dadosManutencao$codUsinaTermica)) > 0) {
    anoMesInicio <- zoo::as.yearmon(paste0(df.dadosGerais$anoInicio, df.dadosGerais$mesInicio), "%Y%m")
    # manutencao sempre ano calendario, portanto termina em dezembro do ano atual (anosManutencao = 1), 
    # ou em dezembro dos anos posteriores dependendo do valor de anosManutencao
    anoMesFim <- zoo::as.yearmon(paste0((df.dadosGerais$anoInicio - 1), "12"), "%Y%m") + df.dadosGerais$anosManutencao
    # define o horizonte de manutencao programada
    df.horizonte <- data.frame(anoMes = seq(anoMesInicio, anoMesFim, 1/12)) %>% 
      dplyr::mutate(diasMes = as.integer(zoo::as.Date(anoMes + 1/12) - zoo::as.Date(anoMes)),
                    aux = 1)
    
    df.dadosManutencao <- df.dadosManutencao %>% dplyr::mutate(dataInicio = zoo::as.Date(dataInicio, "%d%m%Y"), 
                                                               dataFim = dataInicio + duracao,
                                                               aux = 1)
    
    df.dadosManutencao <- dplyr::inner_join(df.dadosManutencao, df.horizonte, by = "aux", relationship = "many-to-many") %>% 
      dplyr::select(-aux)
    
    # calcula o indice de indisponibilidade de acordo com o numero de dias em manutencao em cada mes dentro do periodo de manutencao de cada par usina-unidade
    df.dadosManutencao <- df.dadosManutencao %>% dplyr::mutate(entreMes = anoMes >= zoo::as.yearmon(dataInicio) & anoMes <= zoo::as.yearmon(dataFim),
                                                               noInicio = anoMes == zoo::as.yearmon(dataInicio),
                                                               noFim = anoMes == zoo::as.yearmon(dataFim),
                                                               IP = ifelse(noInicio & !noFim,
                                                                           as.numeric(zoo::as.Date(anoMes + 1/12) - dataInicio) / diasMes,
                                                                           ifelse(noInicio & noFim,
                                                                                  as.numeric(dataFim - dataInicio) / diasMes,
                                                                                  ifelse(noFim,
                                                                                         as.numeric(dataFim - zoo::as.Date(anoMes)) / diasMes,
                                                                                         ifelse(entreMes, 1, 0)))),
                                                               anoMes = as.integer(format(anoMes, "%Y%m")),
                                                               PDISPIP = (1-IP) * pot) %>% 
      dplyr::select(codUsinaTermica, ug, pot, anoMes, IP, PDISPIP)
    
    # resgata dados das utes para garantir valores de capacidade instalada atual para usinas
    df.ute <- dplyr::inner_join(leituraConfiguracaoTermoeletrica(pastaCaso), 
                                dplyr::left_join(tidyr::pivot_longer(leituraDadosUsinasTermoeletricas(pastaCaso), -c(1,2), names_to="atributo", values_to="valor"), 
                                                 tidyr::pivot_longer(leituraExpansaoTermoeletrica(pastaCaso), -c(1,2), names_to="atributo", values_to="valornew"),
                                                 by = c("codUsinaTermica","atributo","anoMes")),
                                by = c("codUsinaTermica"), relationship = "many-to-many") %>% 
      dplyr::mutate(valorfim = ifelse((status == "EX"), valor, 
                                      ifelse((status == "NC"), 0, 
                                             ifelse((atributo == "capacidaInstalada"), ifelse(is.na(valornew), 0, valornew), 
                                                    ifelse(is.na(valornew), valor, valornew))))) %>% 
      dplyr::select(-c("valor", "valornew", "status","codClasseTermica","tecnologia")) %>% 
      dplyr::group_by_at(dplyr::vars(-valorfim)) %>%  
      dplyr::mutate(row_id=1:dplyr::n()) %>% dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = "atributo", values_from = "valorfim") %>% 
      dplyr::select(-row_id) %>% 
      dplyr::select(codUsinaTermica, capacidaInstalada, anoMes)
    
    df.dadosManutencao <- df.dadosManutencao %>% dplyr::arrange(desc(IP)) %>% 
      dplyr::distinct(codUsinaTermica, ug, pot, anoMes, .keep_all = TRUE) %>% 
      dplyr::group_by(codUsinaTermica, anoMes) %>% 
      dplyr::summarise(PotIP = sum(PDISPIP), pot = sum(pot)) %>% 
      dplyr::inner_join(df.ute, by = c("codUsinaTermica", "anoMes")) %>%
      dplyr::mutate(IP = (1 - (PotIP + (capacidaInstalada - pot)) / capacidaInstalada) * 100) %>% 
      dplyr::select(-PotIP, -pot, -capacidaInstalada)
  } else {
    df.dadosManutencao <- data.frame(codUsinaTermica = numeric(), anoMes = numeric(), IP = numeric())
  }
  
  return(df.dadosManutencao)
}
