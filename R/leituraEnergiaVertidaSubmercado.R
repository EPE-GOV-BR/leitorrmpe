#' Leitor de dados de energia vertida por submercado
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de energia vertida por submercado (evertmxxx.*) e recupera esses valores por ano, mes e serie.
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem. 
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de energia vertida
#'
#' @return \code{df.energiaVertidaSubmercado} data frame com os valores de energia vertida por submercado
#' \itemize{
#' \item codigo do submercado (\code{$codSubmercado})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de vertimento [MWmes] (\code{$vertimento})
#' }
#'
#' @examples
#' \dontrun{
#' leituraEnergiaVertidaSubmercado("C:/PDE2027_Caso080")}
#'
#' @export

leituraEnergiaVertidaSubmercado <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # cria data frame de base para armazenar os dados de energia vertida para todos os anos
  df.energiaVertidaSubmercado <- tidyr::tibble(codSubmercado = numeric(), serie = numeric(), anoMes = numeric(), vertimento = numeric())
  
  # seleciona somente os arquivos evertm
  arquivos <-  list.files(pasta, pattern = "^evertm")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos evertmXXX.out em ", pasta))
  }
  
  df.energiaVertidaSubmercado <- purrr::map_df(arquivos, function(arquivo) {
    
    
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    
    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))
    
    # pega informacao de submercado no nome do arquivo
    inicioSubmercado <- stringr::str_locate(arquivo, "evertm") %>% {.[1,2] + 1} %>% unname()
    codSubmercado <- stringr::str_sub(arquivo, inicioSubmercado, inicioSubmercado+2) %>% as.integer()
    
    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.energiaVertidaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
                                                col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                  c(3, 8, 17, 26, 35, 44, 53, 62, 71, 80, 89, 98, 107),
                                                  # vetor com as posicoes finais de cada campo
                                                  c(6, 15, 24, 33, 42, 51, 60, 69, 78, 87, 96, 105, 114),
                                                  # nome colunas
                                                  c('serie', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                                col_types = "idddddddddddd",
                                                skip = 2)
      
      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano 
      # pelo valor dentro de uma sequencia para cada ano.
      series <- 1:nrow(df.energiaVertidaAnual)
      df.energiaVertidaAnual$serie <- series
      
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.energiaVertidaAnual <- df.energiaVertidaAnual %>%
        tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "vertimento") %>%
        dplyr::mutate(ano = anos[andaAnos], codSubmercado = codSubmercado, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codSubmercado, serie, anoMes, vertimento)
      # concatena dados num data frame unico
      df.energiaVertidaSubmercado <- rbind(df.energiaVertidaSubmercado, df.energiaVertidaAnual)
    })
  })
  return(df.energiaVertidaSubmercado)
}
