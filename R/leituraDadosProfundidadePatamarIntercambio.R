#' Leitor dos dados de profundidade patamares de intercambio
#'
#' Faz a leitura do arquivo do NEWAVE com dados de profundidade de patamares de intercambio (patamar.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.dadosProfundidadePatamarIntercambio} data frame com dados de profundidade de patamares de intercambio
#' \itemize{
#' \item origem do sistema (numero do subsistema/submercado A) (\code{$codSubsistemaOrigem})
#' \item destino do sistema (numero do subsistema/submercado B) (\code{$codSubsistemaDestino})
#' \item numero do patamar (\code{$patamar})
#' \item ano e mes referente a informacao (\code{$anoMes})
#' \item profundidade de patamar de intercambio (\code{$profundidadeIntercambio})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosProfundidadePatamarIntercambio("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraDadosProfundidadePatamarIntercambio <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo com dados de duracao de patamar (patamar.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(10) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo patamar como um vetor de caracteres
  dadosPatamarIntercambio <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioPatamar <- which(stringr::str_detect(dadosPatamarIntercambio, "INTERCAMBIO"))
  # encontra o fim da informacao
  fimPatamar <- which(stringr::str_detect(dadosPatamarIntercambio, "BLOCO DE USINAS NAO SIMULADAS"))
  fimPatamar <- ifelse(length(fimPatamar) == 0, length(dadosPatamarIntercambio), fimPatamar - 4)
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosPatamarIntercambioTXT <- dadosPatamarIntercambio[(inicioPatamar + 2):(fimPatamar)]

  # recupera a estrutura de dados
  df.dadosProfundidadePatamarIntercambio <- readr::read_fwf(I(dadosPatamarIntercambioTXT),
    col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
      c(4, 9, 16, 23, 30, 37, 44, 51, 58, 65, 72, 79, 86, 2, 6),
      # vetor com as posicoes finais de cada campo
      c(7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 4, 8),
      # nome colunas
      c("ano", "JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ", "codSubsistemaOrigem", "codSubsistemaDestino")
    ),
    col_types = "iddddddddddddii"
  ) %>%
    suppressWarnings()

  # Ajusta as linhas das colunas "ano", "codSubsistemaDestino" e "codSubsistemaOrigem", mantendo apenas os valores originais
  # Estende o valor referente ao "codSubsistemaOrigem" para os demais anos
  # Estende o valor referente ao "codSubsistemaDestino" para os demais anos
  # Filtra para manter apenas as linhas com os valores anuais
  # Estende o valor referente ao "ano" para os demais anos
  # Cria uma sequencia numerica para a representacao dos patamares. O valor retorna ao inicio no momento em que se altera o ANO.
  df.dadosProfundidadePatamarIntercambio <- df.dadosProfundidadePatamarIntercambio %>%
    dplyr::mutate(
      codSubsistemaDestino = ifelse(!is.na(DEZ), NA, codSubsistemaDestino),
      codSubsistemaOrigem = ifelse(!is.na(DEZ), NA, codSubsistemaOrigem),
      ano = ifelse(is.na(DEZ), NA, ano)
    )

  if (length(dplyr::filter(df.dadosProfundidadePatamarIntercambio, !is.na(ano))$ano) == 0) {
    df.dadosProfundidadePatamarIntercambio <- leituraLimiteInterligacoes(diretorio) %>%
      dplyr::mutate(patamar = 1, valor = 1) %>%
      dplyr::select(codSubsistemaOrigem, codSubsistemaDestino, patamar, anoMes, profundidadeIntercambio)
  } else {
    df.dadosProfundidadePatamarIntercambio <- df.dadosProfundidadePatamarIntercambio %>%
      dplyr::mutate(
        codSubsistemaOrigem = zoo::na.locf(codSubsistemaOrigem),
        codSubsistemaDestino = zoo::na.locf(codSubsistemaDestino)
      ) %>%
      dplyr::filter(!is.na(DEZ)) %>%
      dplyr::mutate(ano = zoo::na.locf(ano)) %>%
      dplyr::mutate(patamar = sequence(rle(ano)$lengths))

    # renomeia colunas
    colnames(df.dadosProfundidadePatamarIntercambio)[2:13] <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

    # tranforma colunas em linhas, exceto as colunas "ano","codSubsistemaOrigem","codSubsistemaDestino" e "patamar".
    # o objetivo  criar duas colunas com as informacoes do "mes" e "profundidadeIntercambio"
    # cria o campo anoMes (AAAAMM)
    # seleciona apenas os campos de interesse
    df.dadosProfundidadePatamarIntercambio <- tidyr::pivot_longer(df.dadosProfundidadePatamarIntercambio,
      -c("ano", "codSubsistemaOrigem", "codSubsistemaDestino", "patamar"),
      names_to = "mes",
      values_to = "profundidadeIntercambio"
    ) %>%
      dplyr::mutate(anoMes = as.numeric(ano) * 100 + as.numeric(mes)) %>%
      dplyr::select(codSubsistemaOrigem, codSubsistemaDestino, patamar, anoMes, profundidadeIntercambio)
  }


  return(df.dadosProfundidadePatamarIntercambio)
}
