#' Leitor dos dados do arquivo com dados para outros usos da agua (dsvagua.dat)
#'
#' Faz a leitura do arquivo do NEWAVE com dados para outros usos da agua (dsvagua.dat).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.Desvioagua} data frame com dados para outros usos da agua
#' \itemize{
#' \item numero do subsistema/submercado (\code{$codUsina})
#' \item ano e mes (\code{$anoMes})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente devido a uso consuntiv0 (\code{$desvioUsoConsuntivo})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente devido a vazao remanescente (\code{$desvioVazaoRemanescente})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente devido a transposicao (\code{$desvioTransposicao})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente devido a reitrada de agua (\code{$desvioRetiradaDagua})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente devido a escada de peixe (\code{$desvioEscadaPeixe})
#' \item vazao adicionada (positivo) ou desviada (negativo) no mes correspondente nao identificado (\code{$desvioNaoIdentificado})
#' \item vazao adicionada (positivo) ou desviada (negativo) total no mes correspondente (\code{$desvioTotal})
#' \item flag para a consideracao do registro de desvio, caso a usina seja NC (\code{$flagDesvio})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDesvioAgua("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraDesvioAgua <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo com dados de desvio de agua (dsvagua.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo_dsvagua <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(28) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo_dsvagua, sep = "/"))) {
    stop(paste0(arquivo_dsvagua, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo como um vetor de caracteres
  dadosDesvioAgua <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo_dsvagua, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o fim da informacao
  fimDesvioAgua <- which(stringr::str_detect(dadosDesvioAgua, "^9999"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosDesvioAguaTXT <- dadosDesvioAgua[3:(fimDesvioAgua - 1)]

  # recupera a estrutura de dados
  df.DesvioAgua <- readr::read_fwf(I(dadosDesvioAguaTXT),
    col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
      c(1, 6, 10, 17, 24, 31, 38, 45, 52, 59, 66, 73, 80, 87, 98, 104),
      # vetor com as posicoes finais de cada campo
      c(4, 9, 16, 23, 30, 37, 44, 51, 58, 65, 72, 79, 86, 93, 101, 113),
      # nome colunas
      c("ano", "codUsina", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "flagDesvio", "comentario")
    ),
    col_types = "iiddddddddddddic"
  ) %>% dplyr::mutate(comentario = zoo::na.locf(stringr::str_trim(comentario))) # tira espacos do comentario e preenche linhas com comentario NA


  # tranforma colunas em linhas, exceto as colunas "ano" e "codUsina".
  # o objetivo  criar duas colunas com as informacoes do "mes" e "valDesvio"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.DesvioAgua <- tidyr::pivot_longer(df.DesvioAgua, -c("ano", "codUsina", "flagDesvio", "comentario"),
    names_to = "mes",
    values_to = "valDesvio"
  ) %>%
    dplyr::mutate(anoMes = as.numeric(ano) * 100 + as.numeric(mes)) %>%
    dplyr::select(codUsina, anoMes, valDesvio, flagDesvio, comentario) %>%
    dplyr::filter(!is.na(valDesvio)) %>%
    # separa os desvios por tipo
    dplyr::mutate(
      desvioUsoConsuntivo = ifelse(stringr::str_detect(comentario, "^Usos_C"), valDesvio, 0.0),
      desvioVazaoRemanescente = ifelse(stringr::str_detect(comentario, "^Vazao_R"), valDesvio, 0.0),
      desvioTransposicao = ifelse(stringr::str_detect(comentario, "^Transpo"), valDesvio, 0.0),
      desvioRetiradaDagua = ifelse(stringr::str_detect(comentario, "^Retirada"), valDesvio, 0.0),
      desvioEscadaPeixe = ifelse(stringr::str_detect(comentario, "Escada_"), valDesvio, 0.0),
      desvioNaoIdentificado = ifelse((!stringr::str_detect(comentario, "^Usos_C") &
        !stringr::str_detect(comentario, "^Vazao_R") &
        !stringr::str_detect(comentario, "^Transpo") &
        !stringr::str_detect(comentario, "^Retirada") &
        !stringr::str_detect(comentario, "Escada_")),
      valDesvio, 0.0
      )
    ) %>%
    dplyr::select(-c("valDesvio", "comentario")) %>%
    # agrupa por usina e periodo
    dplyr::group_by(codUsina, anoMes) %>%
    dplyr::summarise(
      flagDesvio = mean(flagDesvio),
      desvioUsoConsuntivo = sum(desvioUsoConsuntivo),
      desvioVazaoRemanescente = sum(desvioVazaoRemanescente),
      desvioTransposicao = sum(desvioTransposicao),
      desvioRetiradaDagua = sum(desvioRetiradaDagua),
      desvioEscadaPeixe = sum(desvioEscadaPeixe),
      desvioNaoIdentificado = sum(desvioNaoIdentificado)
    ) %>%
    dplyr::mutate(desvioTotal = desvioUsoConsuntivo + desvioVazaoRemanescente + desvioTransposicao + desvioRetiradaDagua + desvioEscadaPeixe + desvioNaoIdentificado)

  return(df.DesvioAgua)
}
