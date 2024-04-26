#' Dados consolidados de termoeletricas ao longo do horizonte
#'
#' Faz a consolidacao dos dados das termoeletricas ao longo do horizonte de estudo definido no NEWAVE.
#' Usa como referencia para a leitura dos arquivos as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.consolidadoUTEnoHorizonte} data frame com dados consolidados de termoeletricas ao longo do horizonte
#' \itemize{
#' \item numero da usina termica (\code{$codUsinaTermica})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item periodo (anoMes-AAAAMM) (\code{$anoMes})
#' \item capacidade instalada (MW) (\code{$capacidaInstalada})
#' \item fator de capacidade maximo (\%) (\code{$FCMaximo})
#' \item TEIF da usina termica (\%) (\code{$TEIF})
#' \item geracao termica minima (MWmes) (\code{$GTMin})
#' \item indisponibilidade programada (IP) da usina termica para os demais anos do estudo (\%) (\code{$IP})
#' \item potencia disponivel maxima (MW) (\code{$PDISP}) 
#' \item tipo de combustivel da classe termica (\code{$tipoComb})
#' \item custo de operacao da classe termica ($/MWh) (\code{$CVU})
#' }
#'
#' @examples
#' \dontrun{
#' consolidaTermeletricasnoHorizonte(pastaCaso)}
#' 
#' @export
consolidaTermeletricasnoHorizonte <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados das usinas termeletricas
  
  df.conft <- leituraConfiguracaoTermoeletrica(pastaCaso)
  df.clast <- leituraClassesTermicas(pastaCaso)
  df.expt <- leituraExpansaoTermoeletrica(pastaCaso)
  df.term <- leituraDadosUsinasTermoeletricas(pastaCaso) 
  df.dadosGerais <- leituraDadosGerais(pastaCaso)
  if (df.dadosGerais$anosManutencao == 0) {
    df.manut <- data.frame(codUsinaTermica = numeric(), anoMes = numeric(), IP = numeric())
  } else{
    df.manut <- leituraManutencoesProgramadas(pastaCaso)
  }
  
  
  
  # Cria estrutura com a evolucao dos parametros (atributos) das usinas termeletricas ao longo do horizonte de simulacao
  # As primeiras informacoes atribuidas as usinas sao lidas no arquivo de dados de configuracao termoeletrica (CONFT), onde cria-se uma estrutura mesclada com o 
  # arquivo de dados das usinas termoeletricas (TERM) que contem alguns dados cadastrais das usinas. 
  # Posteriormente, os dados contidos nessa estrutura podem ser alterados no arquivo de expansao termoeletrica (EXPT), atraves de uma nova mescla de informacoes
  # e importante frisar que para usinas com status EE ou NE, a potencia efetiva e a geracao minima serao iguais a zero para os periodos que nao estao declarados no arquivo EXPT. 
  # Ja os valores do fator de capacidade maximo e da taxa de indisponibilidade programada serao iguais aqueles fornecidos no arquivo TERM para os periodos nao declarados no arquivo EXPT.
  # O arquivo de dados das codClasseTermicas termicas (CLAST) contem informacoes somente sobre as codClasseTermicas termicas.
  
  df.consolidadoUTEnoHorizonte <- dplyr::inner_join(df.conft, 
                                                    dplyr::left_join(tidyr::pivot_longer(df.term, -c(1,2), names_to="atributo", values_to="valor"), 
                                                                     tidyr::pivot_longer(df.expt, -c(1,2), names_to="atributo", values_to="valornew"),
                                                                     by = c("codUsinaTermica","atributo","anoMes")),
                                                    by = c("codUsinaTermica")) %>% 
    dplyr::mutate(valorfim = ifelse((status == "EX"), valor, 
                                    ifelse((status == "NC"), 0, 
                                           ifelse((atributo == "capacidaInstalada"), ifelse(is.na(valornew), 0, valornew), 
                                                  ifelse(is.na(valornew), valor, valornew)))),
                  valorfim = ifelse(atributo == "GTMin" & (status == "EE" | status == "NE"),
                                    valornew,
                                    valorfim)
    ) %>% 
    dplyr::select(-c("valor","valornew", "status","codClasseTermica","tecnologia")) %>% 
    dplyr::group_by_at(dplyr::vars(-valorfim)) %>% dplyr::mutate(row_id=1:dplyr::n()) %>% dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = "atributo", values_from = "valorfim") %>% dplyr::select(-row_id) %>% 
    dplyr::left_join(df.manut,by=c("codUsinaTermica","anoMes")) %>% 
    dplyr::mutate(IP = ifelse(is.na(IP.y), 
                              ifelse(anoMes <= ((df.dadosGerais$anoInicio + df.dadosGerais$anosManutencao - 1)*100 + 12),
                                     0, 
                                     IP.x), 
                              IP.y)) %>% 
    dplyr::select(-c("IP.x","IP.y")) %>% 
    dplyr::mutate(PDISP = capacidaInstalada * (FCMaximo/100) * (1 - TEIF/100) * (1 - IP/100),
                  GTMin = ifelse(is.na(GTMin), 0, GTMin),
                  GTMin = ifelse(GTMin > PDISP, PDISP, GTMin)) %>% 
    dplyr::inner_join(df.clast, by = c("codUsinaTermica","anoMes"))
  
  return(df.consolidadoUTEnoHorizonte)
}
