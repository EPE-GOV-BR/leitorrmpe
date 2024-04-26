#' Leitor de dados do valor dagua por REE 
#'
#' Faz a leitura dos arquivos do NEWAVE com dados do valor dagua por REE  (vaguaxxx.*) e recupera esses valores por ano, mes e serie.
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem. 
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados do valor dagua
#'
#' @return \code{df.valorDagua} data frame com os valores dagua
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor dagua [$/MWh] (\code{$valorDagua})
#' }
#'
#' @examples
#' \dontrun{
#' leituraValorDagua("C:/PDE2027_Caso080")}
#'
#' @export

leituraValorDagua <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # cria data frame de base 
  df.valorDagua <- tidyr::tibble(codREE = numeric(), serie = numeric(), anoMes = numeric(), valorDagua = numeric())
  
  # seleciona somente os arquivos vagua
  arquivos <-  setdiff(list.files(pasta, pattern = "^vagua"), list.files(pasta, pattern = "^vagua[pms]"))
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos vaguaXXX.out em ", pasta))
  }
  
  df.valorDagua <- purrr::map_df(arquivos, function(arquivo) {
    
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    
    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))
    
    # pega informacao de ree no nome do arquivo
    inicioREE <- stringr::str_locate(arquivo, "vagua") %>% {.[1,2] + 1} %>% unname()
    codREE <- stringr::str_sub(arquivo, inicioREE, inicioREE+2) %>% as.integer()
    
    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.valorDaguaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
                                            col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                              c(3, 7, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108),
                                              # vetor com as posicoes finais de cada campo
                                              c(6, 17, 26, 35, 44, 53, 62, 71, 80, 89, 98, 107, 116),
                                              # nome colunas
                                              c('serie', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                            col_types = "idddddddddddd",
                                            skip = 2)
      
      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano 
      # pelo valor dentro de uma sequencia para cada ano.
      series <- 1:nrow(df.valorDaguaAnual)
      df.valorDaguaAnual$serie <- series
      
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.valorDaguaAnual <- df.valorDaguaAnual %>%
        tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "valorDagua") %>%
        dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codREE, serie, anoMes, valorDagua)
      # concatena dados num data frame unico
      df.valorDagua <- rbind(df.valorDagua, df.valorDaguaAnual)
    })
  })
  return(df.valorDagua)
}

