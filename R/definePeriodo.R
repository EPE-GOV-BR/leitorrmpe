#' Data frama com periodo do caso NEWAVE
#'
#' Com o auxilio da funcao \code{\link{leituraDadosGerais}} cria um data frame com o horizonte do caso
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.periodo} data frame anos e meses do horizonte do estudo
#' \itemize{
#' \item ano (\code{$ano})
#' \item mes (\code{$mes})
#' \item anoMes (\code{$anoMes})
#' }
#'
#' @examples
#' \dontrun{
#' definePeriodo("C:/PDE2027_Caso080")
#' }
#'
#' @export
definePeriodo <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  df.dadosGerais <- leituraDadosGerais(pastaCaso)

  # cria estruturas dos meses e dos anos do periodo de simulacao
  df.mes <- data.frame(mes = seq(1, 12), index = 1)
  df.ano <- data.frame(ano = seq(df.dadosGerais$anoInicio, df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1), index = 1)
  # cria estrutura mista contendo todos os anos e meses do periodo de simulacao, no formato AAAAMM
  df.periodo <- dplyr::inner_join(df.ano, df.mes, by = c("index" = "index"), relationship = "many-to-many") %>%
    dplyr::mutate(anoMes = (ano * 100 + mes)) %>%
    dplyr::filter(anoMes >= (df.dadosGerais$anoInicio * 100 + df.dadosGerais$mesInicio)) %>%
    dplyr::select(-index)

  return(df.periodo)
}
