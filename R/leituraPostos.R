#' Leitor de dados de postos fluviometricos
#'
#' Faz a leitura do arquivo do NEWAVE com dados de postos fluviometricos 
#' (postos.dat).
#' Arquivo de acesso direto, nao formatado não formatado, com 320 / 600 
#' postos, cada registro correspondendo a um posto fluviometrico.
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.postos} data frame com dados de postos fluviometricos 
#' \itemize{
#' \item codigo do posto fluviometrico (\code{$posto})
#' \item ano inicial do historico de vazoes (\code{$anoInicial})
#' \item ano final do historico de vazoes (\code{$anoFinal})
#' }
#' 
#' @examples
#' \dontrun{
#' leituraPostos("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraPostos <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  arquivos <- list.files(pastaCaso)
  
  # verifica existencia do arquivo hidr.dat
  if (!any(stringr::str_detect(arquivos, "(?i)postos.dat"))) {
    stop(paste0("postos.dat n\u00E3o encontrado em ", pastaCaso))
  } else {
    if (sum(stringr::str_detect(arquivos, "(?i)postos.dat")) > 1) {
      stop(paste0("mais de um arquivo postos.dat encontrado em ", pastaCaso))
    } else {
      arqPostos <- arquivos[stringr::str_detect(arquivos, "(?i)postos.dat")]
    }
  }
  
  arquivoPostos <- paste(pastaCaso, arqPostos, sep = "/")
  tamanhoArquivo <- suppressWarnings(file.info(arquivoPostos)$size)
  
  if(tamanhoArquivo > 7000){
    nPostos <- 600
  }else{
    nPostos <- 320
  }
  
  # abre conexao com o arquivo binario.
  conexaoArquivoBinario <- file(arquivoPostos, "rb", encoding = "ISO-8859-1")
  
  df.postos <- data.frame(posto = integer(nPostos), 
                          nomePosto = character(nPostos), 
                          anoInicial = integer(nPostos),
                          anoFinal = integer(nPostos))
  
  for(posto in 1:nPostos){
    df.postos$posto[posto] <- posto
    df.postos$nomePosto[posto] <- readBin(conexaoArquivoBinario, raw(), 12) %>% 
      rawToChar() %>% 
      iconv(from = "latin1", to = "UTF-8") %>% 
      trimws()
    df.postos$anoInicial[posto] <- readBin(conexaoArquivoBinario, integer())
    df.postos$anoFinal[posto] <- readBin(conexaoArquivoBinario, integer())
  }
  
  close(conexaoArquivoBinario)
  
  return(df.postos)
}


