#' Leitor de dados de restricoes de volume armazenado para UHEs do DECOMP
#'
#' Faz a leitura dos dados de restricao de volume armazenado do DECOMP no arquivo (dadger.*).
#' Utiliza as informacoes do BLOCO 35 do DADGER
#'
#' @param pastaCaso caracter com localizacao dos arquivos DECOMP
#'
#' @return \code{df.RHV} data frame com dados de restricoes de volume armazenado
#' \itemize{
#' \item numero de cadastro da restricao (\code{$numRest})
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item estagio do horizonte de estudo (\code{$estagio})
#' \item limite inferior da restricao (\code{$limInf})
#' \item limite superior da restricao (\code{$limSup})
#' \item coeficiente de participacao da UHE na restricao (\code{$coef})
#' \item tipo da restricao (\code{$tipoRest})
#' }
#'
#' @examples
#' \dontrun{
#' leituraRestricoesVolumeHidroDC("C:/Pasta_Deck_DECOMP")}
#'
#' @export
leituraRestricoesVolumeHidroDC <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do DECOMP")
  }
  
  # arquivo dadger
  arquivo <- leituraArquivosDC(pastaCaso) %>% dplyr::slice(1) %>% dplyr::pull(arquivo)
  
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
  inicioRest <- which(stringr::str_detect(dadger, "BLOCO 35"))[1]
  # encontra o fim da informacao
  fimRest <- which(stringr::str_detect(dadger, "BLOCO 36"))[1]
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosRest <- dadger[(inicioRest+10):(fimRest-3)]
  
  # separa os registros por tipo: HV, LV e CV
  registrosHV <- dadosRest[grepl("^HV", dadosRest)]
  registrosCV <- dadosRest[grepl("^CV", dadosRest)]
  registrosLV <- dadosRest[grepl("^LV", dadosRest)]
  
  # posicoes e nomes de acordo com manual do DECOMP
  df.HV <- data.frame(numRest = as.numeric(stringr::str_sub(registrosHV, 5, 7)),
                      periodoIni = as.numeric(stringr::str_sub(registrosHV, 10, 11)),
                      periodoFim = as.numeric(stringr::str_sub(registrosHV, 15, 16)),
                      aux = 1)
  nEstagios <- max(df.HV$periodoFim)
  df.HV <- dplyr::left_join(df.HV, data.frame(aux = 1, estagio = 1:nEstagios), by = "aux", relationship = "many-to-many") %>% 
    dplyr::filter(estagio <= periodoFim, estagio >= periodoIni) %>% 
    dplyr::select(numRest, estagio)
  
  df.LV <- data.frame(numRest = as.numeric(stringr::str_sub(registrosLV, 5, 7)),
                      estagioIni = as.numeric(stringr::str_sub(registrosLV, 10, 11)),
                      limInf = as.numeric(stringr::str_sub(registrosLV, 15, 24)),
                      limSup = as.numeric(stringr::str_sub(registrosLV, 25, 34))) %>% 
    dplyr::group_by(numRest) %>% dplyr::mutate(estagioFim = ifelse(dplyr::n() == 1, nEstagios, ifelse(estagioIni == nEstagios, nEstagios, dplyr::lead(estagioIni) - 1)), 
                                               estagioFim = ifelse(estagioIni > estagioFim, estagioIni, estagioFim))
  
  df.CV <- data.frame(numRest = as.numeric(stringr::str_sub(registrosCV, 5, 7)),
                      estagio = as.numeric(stringr::str_sub(registrosCV, 10, 11)),
                      codUsina = as.numeric(stringr::str_sub(registrosCV, 15, 17)),
                      coef = as.numeric(stringr::str_sub(registrosCV, 20, 29)),
                      tipoRest = as.character(stringr::str_sub(registrosCV, 35, 38)))
  
  df.RHV <- dplyr::inner_join(df.HV, df.LV, by = "numRest") %>% 
    dplyr::mutate(valeRest = ifelse(((estagio >= estagioIni) & (estagio <= estagioFim)), "sim", "nao")) %>% 
    dplyr::filter(valeRest != "nao") %>% 
    dplyr::select(-c("valeRest", "estagioIni", "estagioFim")) %>% 
    dplyr::inner_join(dplyr::select(df.CV, -estagio), by = "numRest", relationship = "many-to-many") %>% 
    dplyr::select(numRest, codUsina, everything())
  
  return(df.RHV)
}
