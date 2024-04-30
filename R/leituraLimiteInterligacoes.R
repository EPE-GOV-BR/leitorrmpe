#' Leitor dos dados do limite de interligacoes
#'
#' Faz a leitura do arquivo do NEWAVE com dados do limite de interligacoes (sistema.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.limiteInterligacoes} data frame com dados de limite de interligacoes
#' \itemize{
#' \item subsistema/submercado de origem da interligacao (\code{$codSubsistemaOrigem})
#' \item subsistema/submercado de destino da interligacao (\code{$codSubsistemaDestino})
#' \item ano e mes (\code{$anoMes})
#' \item limite da interligacao (\code{$limiteInterligacao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraLimiteInterligacoes("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraLimiteInterligacoes <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo com dados de agrupamento livre de interligacoes (sistema.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(2) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo sistema como um vetor de caracteres nx1
  dadosLimiteSistema <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioLimiteSistema <- which(stringr::str_detect(dadosLimiteSistema, "LIMITES DE INTERCAMBIO"))
  # encontra o fim da informacao
  fimLimiteSistema <- which(stringr::str_detect(dadosLimiteSistema, "MERCADO DE ENERGIA TOTAL"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosLimiteSistemaTXT <- dadosLimiteSistema[(inicioLimiteSistema + 3):(fimLimiteSistema - 2)]

  # recupera a estrutura de dados
  df.limiteInterligacoes <- readr::read_fwf(I(dadosLimiteSistemaTXT),
    col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
      c(1, 2, 6, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 6, 2),
      # vetor com as posicoes finais de cada campo
      c(7, 4, 8, 14, 22, 30, 38, 46, 54, 62, 70, 78, 86, 94, 102, 8, 4),
      # nome colunas
      c("ano", "codSubsistemaOrigem", "codSubsistemaDestino", "JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ", "codSubsistemaOrigem.i", "codSubsistemaDestino.i")
    ),
    col_types = "iiiddddddddddddii"
  ) %>%
    suppressWarnings()

  # Ajusta as linhas das colunas "ano", "codSubsistemaOrigem", "codSubsistemaOrigem.i", "codSubsistemaDestino" e "codSubsistemaDestino.i", mantendo apenas os valores originais
  # Estende os valores referentes ao codSubsistemaOrigem e codSubsistemaOrigem.i para os demais anos
  # Estende os valores referentes ao codSubsistemaDestino e codSubsistemaDestino.i para os demais anos
  df.limiteInterligacoes <- df.limiteInterligacoes %>%
    dplyr::mutate(
      codSubsistemaOrigem = ifelse(!is.na(DEZ), NA, codSubsistemaOrigem),
      codSubsistemaDestino = ifelse(!is.na(DEZ), NA, codSubsistemaDestino),
      codSubsistemaOrigem.i = ifelse(!is.na(DEZ), NA, codSubsistemaOrigem.i),
      codSubsistemaDestino.i = ifelse(!is.na(DEZ), NA, codSubsistemaDestino.i),
      ano = ifelse(is.na(DEZ), NA, ano)
    ) %>%
    dplyr::mutate(
      codSubsistemaOrigem.i = zoo::na.locf(codSubsistemaOrigem.i),
      codSubsistemaDestino.i = zoo::na.locf(codSubsistemaDestino.i)
    ) %>%
    dplyr::mutate(
      codSubsistemaOrigem = ifelse(is.na(ano) & is.na(codSubsistemaOrigem), codSubsistemaOrigem.i, codSubsistemaOrigem),
      codSubsistemaDestino = ifelse(is.na(ano) & is.na(codSubsistemaDestino), codSubsistemaDestino.i, codSubsistemaDestino)
    ) %>%
    dplyr::mutate(codSubsistemaOrigem = zoo::na.locf(codSubsistemaOrigem), codSubsistemaDestino = zoo::na.locf(codSubsistemaDestino)) %>%
    dplyr::filter(!is.na(DEZ)) %>%
    dplyr::select(-c("codSubsistemaOrigem.i", "codSubsistemaDestino.i"))

  # renomeia colunas
  colnames(df.limiteInterligacoes)[4:15] <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

  # tranforma colunas em linhas, exceto as colunas "ano","codSubsistemaOrigem" e "codSubsistemaDestino".
  # o objetivo  criar duas colunas com as informacoes do "mes" e "limiteInterligacao"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.limiteInterligacoes <- tidyr::pivot_longer(df.limiteInterligacoes,
    -c("ano", "codSubsistemaOrigem", "codSubsistemaDestino"),
    names_to = "mes",
    values_to = "limiteInterligacao"
  ) %>%
    dplyr::mutate(anoMes = as.numeric(ano) * 100 + as.numeric(mes)) %>%
    dplyr::select(codSubsistemaOrigem, codSubsistemaDestino, anoMes, limiteInterligacao) %>%
    dplyr::filter(!is.na(limiteInterligacao))

  return(df.limiteInterligacoes)
}
