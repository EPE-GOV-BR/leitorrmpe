#' Leitor de dados de configuracao hidroeletrica do DECOMP
#'
#' Faz a leitura dos dados de configuracao hidroeletrica do DECOMP no arquivo (dadger.*).
#'
#' @param pastaCaso caracter com localizacao dos arquivos DECOMP
#'
#' @return \code{df.dadosConfiguracaoHidroDC} data frame com dados de configuracao hidroeletrica
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item codigo do REE (\code{$codREE})
#' \item volume armazenado inicial em percentagem do volume util (\code{$volumeInical})
#' \item flag de evaporacao (0 nao considera; 1 considera) (\code{$flagEvap})
#' }
#'
#' @examples
#' \dontrun{
#' leituraConfiguracaoHidroDC("C:/Pasta_Deck_DECOMP")}
#'
#' @export
leituraConfiguracaoHidroDC <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # arquivo dadger
  arquivo <- leituraArquivosDC(pastaCaso) %>% dplyr::slice(1) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo dadger como um vetor de caracteres
  dadger <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioUH <- which(stringr::str_detect(dadger, "BLOCO 3"))[1]
  # encontra o fim da informacao
  fimUH <- which(stringr::str_detect(dadger, "BLOCO 4"))[1]
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosUH <- dadger[(inicioUH+10):(fimUH-3)]
  
  # posicoes e nomes de acordo com manual do DECOMP
  suppressWarnings(
    df.dadosConfiguracaoHidroDC <- data.frame(registro = as.character(stringr::str_sub(dadosUH, 1, 2)),
                                              codUsina =as.numeric(stringr::str_sub(dadosUH ,5,7)),
                                              codREE = as.numeric(stringr::str_sub(dadosUH, 10, 11)),
                                              volumeInical = as.numeric(stringr::str_sub(dadosUH, 15,25)),
                                              flagEvap =as.numeric(stringr::str_sub(dadosUH ,40,40))) %>% 
      dplyr::filter(registro == "UH") %>% 
      dplyr::select(-registro)
  )
  
  return(df.dadosConfiguracaoHidroDC)
}
