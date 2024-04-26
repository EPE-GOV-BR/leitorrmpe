#' Leitor de dados de armazenamento percentual para o Sistema Interligado Nacional
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de armazenamento percentual para o Sistema Interligado Nacional  (earmfpsin.*) e recupera esses valores por ano, mes e serie.
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem. 
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de armazenamento percentual do SIN
#'
#' @return \code{df.earmfpSin} data frame com os valores de energia vertida para o SIN
#' \itemize{
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de armazenamento percentual [%] (\code{$earmfp})
#' }
#'
#' @examples
#' \dontrun{
#' leituraEnergiaVertidaSin("C:/PDE2027_Caso080")}
#'
#' @export

leituraEnergiaArmazenadaFinalPercentualSin <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # seleciona somente o arquivo earmfpsin
  arquivo <-  list.files(pasta, pattern = "^earmfpsin")
  if (length(arquivo) != 1) {
    stop(paste0("N\u00E3o foi encontrado o arquivo earmfpsin em ", pasta, " ou existe mais de um arquivo."))
  }
  
  # cria data frame de base 
  df.earmfpSin <- tidyr::tibble( serie = numeric(), anoMes = numeric(), earmfp = numeric())
  
  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
  # encontra os anos
  anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
  # localiza a posicao do inicio de dados
  inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
  # localiza a posicao do fim de dados pela informacao de desvio padrao
  fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))
  
  
  df.earmfpSin <- purrr::map_df(1:length(anos), function(andaAnos) {
    # posicoes e nomes das variaveis
    df.earmfp <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
                                 col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                   c(2, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121),
                                   # vetor com as posicoes finais de cada campo
                                   c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125),
                                   # nome colunas
                                   c('serie', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                 col_types = "idddddddddddd",
                                 skip = 2)
    
    # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano 
    # pelo valor dentro de uma sequencia para cada ano.
    series <- 1:nrow(df.earmfp)
    df.earmfp$serie <- series
    
    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
    df.earmfp <- df.earmfp %>%
      tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "earmfp") %>%
      dplyr::mutate(ano = anos[andaAnos], anoMes = (ano * 100 + as.numeric(mes))) %>%
      dplyr::select(serie, anoMes, earmfp)
    # concatena dados num data frame unico
    df.earmfpSin <- rbind(df.earmfpSin, df.earmfp)
  })
  
  return(df.earmfpSin)
}
