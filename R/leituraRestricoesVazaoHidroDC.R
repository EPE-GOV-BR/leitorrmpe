#' Leitor de dados de restricoes de vazao defluente para UHEs do DECOMP
#'
#' Faz a leitura dos dados de restricao de vazao defluente do DECOMP no arquivo (dadger.*).
#' Utiliza as informacoes do BLOCO 36 do DADGER
#'
#' @param pastaCaso caracter com localizacao dos arquivos DECOMP
#'
#' @return \code{df.RHQ} data frame com dados de restricoes de vazao
#' \itemize{
#' \item numero de cadastro da restricao (\code{$numRest})
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item estagio do horizonte de estudo (\code{$estagio})
#' \item limite inferior da restricao no patamar 1 (\code{$limInf_p1})
#' \item limite superior da restricao no patamar 1 (\code{$limSup_p1})
#' \item limite inferior da restricao no patamar 2 (\code{$limInf_p2})
#' \item limite superior da restricao no patamar 2 (\code{$limSup_p2})
#' \item limite inferior da restricao no patamar 3 (\code{$limInf_p3})
#' \item limite superior da restricao no patamar 3 (\code{$limSup_p3})
#' \item coeficiente de participacao da UHE na restricao (\code{$coef})
#' \item tipo da restricao (\code{$tipoRest})
#' }
#'
#' @examples
#' \dontrun{
#' leituraRestricoesVolumeHidroDC("C:/Pasta_Deck_DECOMP")
#' }
#'
#' @export
leituraRestricoesVazaoHidroDC <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do DECOMP")
  }

  # arquivo dadger
  arquivo <- leituraArquivosDC(pastaCaso) %>%
    dplyr::slice(1) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo dadger como um vetor de caracteres
  dadger <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra a data inicial do estudo
  dataInicial <- dadger[grepl("^DT  ", dadger)]
  dataInicial <- as.Date(paste0(as.numeric(stringr::str_sub(dataInicial, 15, 18)), "-", as.numeric(stringr::str_sub(dataInicial, 10, 11)), "-", as.numeric(stringr::str_sub(dataInicial, 5, 7))))
  # encontra o inicio da informacao
  inicioRest <- which(stringr::str_detect(dadger, "BLOCO 36"))[1]
  # encontra o fim da informacao
  fimRest <- which(stringr::str_detect(dadger, "-------- CVAR ---------"))[1]
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosRest <- dadger[(inicioRest + 10):(fimRest - 2)]

  # separa os registros por tipo: HQ, LQ e CQ
  registrosHQ <- dadosRest[grepl("^HQ", dadosRest)]
  registrosCQ <- dadosRest[grepl("^CQ", dadosRest)]
  registrosLQ <- dadosRest[grepl("^LQ", dadosRest)]

  # posicoes e nomes de acordo com manual do DECOMP
  df.HQ <- data.frame(
    numRest = as.numeric(stringr::str_sub(registrosHQ, 5, 7)),
    periodoIni = as.numeric(stringr::str_sub(registrosHQ, 10, 11)),
    periodoFim = as.numeric(stringr::str_sub(registrosHQ, 15, 16)),
    aux = 1
  )
  nEstagios <- max(df.HQ$periodoFim)
  df.HQ <- dplyr::left_join(df.HQ, data.frame(aux = 1, estagio = 1:nEstagios), by = "aux", relationship = "many-to-many") %>%
    dplyr::filter(estagio <= periodoFim, estagio >= periodoIni) %>%
    dplyr::select(numRest, estagio)

  df.LQ <- data.frame(
    numRest = as.numeric(stringr::str_sub(registrosLQ, 5, 7)),
    estagioIni = as.numeric(stringr::str_sub(registrosLQ, 10, 11)),
    limInf_p1 = as.numeric(stringr::str_sub(registrosLQ, 15, 24)),
    limSup_p1 = as.numeric(stringr::str_sub(registrosLQ, 25, 34)),
    limInf_p2 = as.numeric(stringr::str_sub(registrosLQ, 35, 44)),
    limSup_p2 = as.numeric(stringr::str_sub(registrosLQ, 45, 54)),
    limInf_p3 = as.numeric(stringr::str_sub(registrosLQ, 55, 64)),
    limSup_p3 = as.numeric(stringr::str_sub(registrosLQ, 65, 74))
  ) %>%
    dplyr::group_by(numRest) %>%
    dplyr::mutate(
      estagioFim = ifelse(dplyr::n() == 1, nEstagios, ifelse(estagioIni == nEstagios, nEstagios, dplyr::lead(estagioIni) - 1)),
      estagioFim = ifelse(estagioIni > estagioFim, estagioIni, estagioFim)
    )

  df.CQ <- data.frame(
    numRest = as.numeric(stringr::str_sub(registrosCQ, 5, 7)),
    estagio = as.numeric(stringr::str_sub(registrosCQ, 10, 11)),
    codUsina = as.numeric(stringr::str_sub(registrosCQ, 15, 17)),
    coef = as.numeric(stringr::str_sub(registrosCQ, 20, 29)),
    tipoRest = as.character(stringr::str_sub(registrosCQ, 35, 38))
  )

  df.RHQ <- dplyr::inner_join(df.HQ, df.LQ, by = "numRest", relationship = "many-to-many") %>%
    dplyr::mutate(valeRest = ifelse(((estagio >= estagioIni) & (estagio <= estagioFim)), "sim", "nao")) %>%
    dplyr::filter(valeRest != "nao") %>%
    dplyr::select(-c("valeRest", "estagioIni", "estagioFim")) %>%
    dplyr::inner_join(dplyr::select(df.CQ, -estagio), by = "numRest", relationship = "many-to-many") %>%
    dplyr::select(numRest, codUsina, everything()) %>%
    tidyr::pivot_longer(starts_with("limSup"), names_to = "patamarSup", values_to = "limSup") %>%
    tidyr::pivot_longer(starts_with("limInf"), names_to = "patamarInf", values_to = "limInf") %>%
    dplyr::mutate(patamarSup = as.numeric(stringr::str_sub(patamarSup, 9, 9)), patamarInf = as.numeric(stringr::str_sub(patamarInf, 9, 9))) %>%
    dplyr::filter(patamarSup == patamarInf) %>%
    dplyr::rename(patamar = patamarSup) %>%
    dplyr::select(-patamarInf)

  return(df.RHQ)
}
