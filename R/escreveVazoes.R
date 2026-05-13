#' Escritor de dados de vazoes historicas
#'
#' Faz a escrita do arquivo do NEWAVE com dados de vazoes naturais historicas 
#' afluentes as usinas hidroeletricas (vazoes.dat).
#' Arquivo de acesso direto, nao formatado não formatado, com 320 / 600 
#' postos, cada registro correspondendo a um mes do historico.
#'
#' @param df.vazoes data frame com os dados de vazoes naturais historicas no
#' mesmo formato obtido na leitura
#' @param arquivo caracter com o nome do arquivo que sera criado
#'
#' @examples
#' \dontrun{
#' escreveVazoes(df.vazoes, "C:/PDE2027_Caso080/vazoes.dat")
#' }
#'
#' @export
escreveVazoes <- function(df.vazoes, arquivo) {
  if (missing(df.vazoes)) {
    stop("favor fornecer o data frame contendo as vazoes")
  }
  if (missing(arquivo)) {
    stop("favor indicar o nome do arquivo a ser criado")
  }
  
  vazoes <- df.vazoes %>% 
    dplyr::arrange(ano, mes, posto) %>%
    dplyr::pull(vazao)
  
  writeBin(vazoes, arquivo, size = 4)
  
  return(paste0("Arquivo ", arquivo, " criado com sucesso!"))
}