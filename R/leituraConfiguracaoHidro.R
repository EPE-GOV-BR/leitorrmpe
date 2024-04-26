#' Leitor de dados de configuracao hidroeletrica
#'
#' Faz a leitura do arquivo do NEWAVE com dados de configuracao hidroeletrica (confhd.*). Usa a funcao \code{\link{leituraArquivos}}.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.dadosConfiguracaoHidro} data frame com dados de configuracao hidroeletrica
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do posto de vazoes da usina (\code{$posto})
#' \item codigo da usina jusante no cadastro de usinas hidroeletricas (\code{$codUsinaJusante})
#' \item codigo do REE (\code{$codREE})
#' \item volume armazenado inicial em percentagem do volume util (\code{$volumeInical})
#' \item indicador de usina existente e/ou em expansao (EX existente; EE existente com expansao; NE nao existente; NC nao considerada)
#' (\code{$idUsinaExistente})
#' \item indice de modificacao de dados da usina (0 nao modifica, 1 modifica) (\code{$idModificacaoUsina})
#' \item primeiro ano do historico de vazoes do posto correspondente a usina (\code{$inicioHistorico})
#' \item ultimo ano do historico de vazoes do posto correspondente a usina (\code{$fimHistorico})
#' \item tecnologia da usina para efeito de calculo de emissoes de GEE (\code{$tecnologia})
#' }
#'
#' @examples
#' \dontrun{
#' leituraConfiguracaoHidro("C:/PDE2027_Caso080")}
#'
#' @export
leituraConfiguracaoHidro <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(3) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # posicoes e nomes de acordo com manual do NEWAVE
  df.dadosConfiguracaoHidro <- readr::read_fwf(paste(pastaCaso, arquivo, sep = "/"),
                                               col_positions = readr::fwf_positions(c(2, 7, 20, 26, 31, 36, 45, 50, 59, 68, 74), # vetor com as posicoes iniciais de cada campo
                                                                                    c(5, 18, 23, 29, 34, 41, 46, 53, 62, 71, 76), # vetor com as posicoes finais de cada campo
                                                                                    c("codUsina", "nomeUsina", "posto", "codUsinaJusante", "codREE", "volumeInical",
                                                                                      "idUsinaExistente", "idModificacaoUsina", "inicioHistorico", "fimHistorico", "tecnologia")),
                                               col_types = readr::cols(codUsina = readr::col_double(), nomeUsina = readr::col_character(), posto = readr::col_double(), codUsinaJusante = readr::col_double(),
                                                                       codREE = readr::col_double(), volumeInical = readr::col_double(), idUsinaExistente = readr::col_character(),
                                                                       idModificacaoUsina = readr::col_double(), inicioHistorico = readr::col_double(), fimHistorico = readr::col_double(),
                                                                       tecnologia = readr::col_character()),
                                               skip = 2, 
                                               trim_ws = T) %>% 
    dplyr::filter(!is.na(codUsina))
  
  return(df.dadosConfiguracaoHidro)
}
