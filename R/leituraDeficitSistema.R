#' Leitor dos dados de deficit de sistema
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de deficit do sistema (sistema.d**). Usa a funcao \code{\link{leituraArquivos}}
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#' 
#' @return \code{df.sistema} data frame com informacoes de sistema
#' \itemize{
#'   \item codigo do subsistema/submercado (\code{$codSubsistema})
#'   \item nome do subsistema/submercado (\code{$nomeSubsistema})
#'   \item tipo de subsistema/submercado (\code{$tipoFicticio}) - pode assumir 1 para ficticio, ou 0 para real
#'   \item patamar de carga (\code{$patamar})
#'   \item custo de deficit em R$/MWh (\code{$custoDeficit})
#'   \item profundidade de deficit em p.u. (\code{$profundidadeDefict})
#'   }
#'
#' @examples
#' \dontrun{
#' leituraDeficitSistema("C:/PDE2027_Caso080")}
#'
#' @export
leituraDeficitSistema <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(2) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema como um vetor de caracteres nx1
  sistema <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"), n_max = 1000)
  # encontra o inicio da informacao
  inicioSistema <- which(stringr::str_detect(sistema, "CUSTO DO DEFICIT"))
  # encontra o fim da informacao
  fimSistema <- which(stringr::str_detect(sistema, "999")) %>% head(1)
  
  # filtra somente a parte do vetor que tem os dados de interesse
  sistemaTXT <- sistema[(inicioSistema+3):(fimSistema-1)]
  
  # recupera os tipos de sistema - 0 real, 1 ficticio
  tipoSistema <- stringr::str_sub(sistemaTXT, 16, 18) %>% as.numeric()
  
  # como os dados estao extendidos pelas colunas e representam informacao por patamar, temos que fazer duas leituras com dois pivoteamentos.
  # um pivot por custo e outro por profundidade de defict
  # custo
  df.deficitSistema <- readr::read_fwf(I(sistemaTXT[tipoSistema == 0]),
                                       col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                         c(2, 6, 18, 20, 28, 36, 44),
                                         # vetor com as posicoes finais de cada campo
                                         c(4, 15, 18, 26, 34, 42, 50),
                                         # nome colunas
                                         c('codSubsistema', 'nomeSubsistema', 'tipoFicticio', '1', '2', '3', '4')),
                                       # tipo dos dados
                                       col_types = "icidddd")
  
  df.deficitSistema <- df.deficitSistema %>% tidyr::pivot_longer(cols = c('1', '2', '3', '4'), names_to = "patamar", values_to = "custoDefict")
  
  df.sistemaProfundiade <- readr::read_fwf(I(sistemaTXT[tipoSistema == 0]),
                                           col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                             c(2, 6, 18, 52, 58, 64, 70),
                                             # vetor com as posicoes finais de cada campo
                                             c(4, 15, 18, 56, 62, 68, 74),
                                             # nome colunas
                                             c('codSubsistema', 'nomeSubsistema', 'tipoFicticio', '1', '2', '3', '4')),
                                           # tipo dos dados
                                           col_types = "icidddd")
  # profundidade
  df.sistemaProfundiade <- df.sistemaProfundiade %>% tidyr::pivot_longer(cols = c('1', '2', '3', '4'), names_to = "patamar", values_to = "profundidadeDefict")
  
  df.deficitSistema <- dplyr::inner_join(df.deficitSistema, df.sistemaProfundiade, by = c('codSubsistema', 'nomeSubsistema', 'tipoFicticio', 'patamar'))
  
  # le os dados dos sistemas ficticios - eles foram separados para nao dar erro de leitura por falta de dados
  df.sistemaFicticio <- tidyr::tibble(codSubsistema = stringr::str_sub(sistemaTXT[tipoSistema == 1], 1, 4) %>% as.numeric(),
                                      nomeSubsistema = stringr::str_sub(sistemaTXT[tipoSistema == 1], 6, 16) %>% as.character() %>% stringr::str_trim(),
                                      tipoFicticio = stringr::str_sub(sistemaTXT[tipoSistema == 1], 18, 18) %>% as.numeric(),
                                      patamar = NA, custoDefict = NA, profundidadeDefict = NA)
  
  
  
  # junta os data frames em um unico
  df.deficitSistema <- rbind(df.deficitSistema, df.sistemaFicticio) %>% dplyr::arrange(codSubsistema, patamar)
  
  return(df.deficitSistema)
}
