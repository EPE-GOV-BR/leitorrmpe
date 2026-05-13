#' Leitor de dados de vazoes historicas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de vazoes naturais historicas 
#' afluentes as usinas hidroeletricas (vazoes.dat).
#' Arquivo de acesso direto, nao formatado não formatado, com 320 / 600 
#' postos, cada registro correspondendo a um mes do historico.
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.vazoes} data frame com dados de vazoes naturais afluentes 
#' as usinas hidroeletricas
#' \itemize{
#' \item codigo do posto fluviometrico (\code{$posto})
#' \item ano do historico de vazoes (\code{$ano})
#' \item mes do historico de vazoes (\code{$mes})
#' \item vazao natural afluente (\code{$vazao})
#' }
#' 
#' @examples
#' \dontrun{
#' leituraVazoes("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraVazoes <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  arquivos <- list.files(pastaCaso)
  
  # verifica existencia do arquivo hidr.dat
  if (!any(stringr::str_detect(arquivos, "(?i)vazoes.dat"))) {
    stop(paste0("vazoes.dat n\u00E3o encontrado em ", pastaCaso))
  } else {
    if (sum(stringr::str_detect(arquivos, "(?i)vazoes.dat")) > 1) {
      stop(paste0("mais de um arquivo vazoes.dat encontrado em ", pastaCaso))
    } else {
      arqVazoes <- arquivos[stringr::str_detect(arquivos, "(?i)vazoes.dat")]
    }
  }
  
  arquivoVazoes <- paste(pastaCaso, arqVazoes, sep = "/")
  tamanhoArquivo <- suppressWarnings(file.info(arquivoVazoes)$size)
  
  if(tamanhoArquivo / 15360 > 100){
    nAnos <- tamanhoArquivo / 28800
    nPostos <- 600
  }else{
    nAnos <- tamanhoArquivo / (48 * 320)
    nPostos <- 320
  }
  
  vazoes <- readBin(arquivoVazoes, what = integer(), n = nAnos * nPostos * 12)
  
  df.vazoes <- data.frame(posto = rep(1:nPostos, times = nAnos*12), 
                          ano = rep(1931:(1930 + nAnos), each = nPostos*12), 
                          mes = rep(rep(1:12, each = nPostos), times = nAnos),
                          vazao = vazoes) %>% 
    dplyr::arrange(posto, ano)
  
  return(df.vazoes)
}


