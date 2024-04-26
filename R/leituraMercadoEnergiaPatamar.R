#' Leitor dos dados de mercado de energia por patamar
#'
#' Utiliza os leitores de mercado de energia e profundidade de patamar de carga para criar um dataframe com os dados de mercado por patamar.
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.mercadoEnergiaPatamar} data frame com dados de mercado de energia por patamar
#' \itemize{
#' \item numero do subsistema/submercado (\code{$codSubsistema})
#' \item patamar de carga (\code{$patamar})
#' \item ano e mes (\code{$anoMes})
#' \item mercado de Energia do subsistema/submercado (\code{$energiaMercado})
#' }
#'
#' @examples
#' \dontrun{
#' leituraMercadoEnergiaPatamar("C:/PDE2027_Caso080")}
#'
#' @export
leituraMercadoEnergiaPatamar <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # le o arquivo sistema
  dadosMercadoEnergia <- leituraMercadoEnergia(pastaCaso)
  # le o arquivo patamar
  dadosPatamar <- leituraDadosProfundidadePatamarCarga(pastaCaso)
  
  # cria o df com os dados por patamar
  df.mercadoEnergiaPatamar <- dplyr::inner_join(dadosPatamar, dadosMercadoEnergia, by = c("codSubsistema", "anoMes")) %>% 
    dplyr::mutate(energiaMercado = energiaMercado*profundidadeCarga) %>% 
    dplyr::select(-profundidadeCarga)
  
  
  return(df.mercadoEnergiaPatamar)
}

