#' Leitor dos dados de configuracao termoeletrica
#'
#' Faz a leitura do arquivo do NEWAVE com dados de configuracao termoeletrica (conft.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.configuracaoTermoeletrica} data frame com dados de configuracao termoeletrica
#' \itemize{
#' \item numero da usina termica (\code{$codUsinaTermica})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item indicador de usina termica existente (EX = usina existente; EE = existente, com expansao; NE = nao existente, com expansao; NC = usina nao considerada) (\code{$status})
#' \item numero da classe termica da usina (\code{$codClasseTermica})
#' \item tecnologia da usina para efeito de calculo de emissoes de GEE (\code{$tecnologia})
#' }
#'
#' @examples
#' \dontrun{
#' leituraConfiguracaoTermoeletrica("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraConfiguracaoTermoeletrica <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo com dados de usinas termoeletricas (conft.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(5) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  # le o arquivo sistema como um vetor de caracteres
  conft_dados <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # cria a estrutura de dados contendo as informacoes do arquivo conft
  df.configuracaoTermoeletrica <- data.frame(
    codUsinaTermica = as.numeric(stringr::str_sub(conft_dados[3:length(conft_dados)], 2, 5)),
    nomeUsina = as.character(stringr::str_sub(conft_dados[3:length(conft_dados)], 7, 18)),
    codSubsistema = as.numeric(stringr::str_sub(conft_dados[3:length(conft_dados)], 22, 25)),
    status = stringr::str_sub(conft_dados[3:length(conft_dados)], 31, 32),
    codClasseTermica = stringr::str_sub(conft_dados[3:length(conft_dados)], 36, 39),
    tecnologia = stringr::str_sub(conft_dados[3:length(conft_dados)], 46, 48)
  ) %>%
    dplyr::filter(!is.na(codUsinaTermica))

  return(df.configuracaoTermoeletrica)
}
