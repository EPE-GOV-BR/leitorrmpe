#' Leitor de custo operativo do Sistema Interligado Nacional
#'
#' Faz a leitura do arquivo do NEWAVE com dados de custo operativo para o Sistema Interligado Nacional  (coper.*) e recupera esses valores por ano, mes e serie.
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de custo operativo
#'
#' @return \code{df.custoOperativo} data frame com os valores de custo operativo
#' \itemize{
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de custo operativo [R$] (\code{$custoOperativo})
#' }
#'
#' @examples
#' \dontrun{
#' leituraEnergiaVertidaSin("C:/PDE2027_Caso080")
#' }
#'
#' @export

leituraCustoOperativo <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base
  df.custoOperativo <- tidyr::tibble(codSubmercado = numeric(), serie = numeric(), anoMes = numeric(), custoOperativo = numeric())

  # seleciona somente o arquivo evertsin
  arquivo <- list.files(pasta, pattern = "^coper\\.")
  if (length(arquivo) != 1) {
    stop(paste0("N\u00E3o foi encontrado o arquivo coper.out em ", pasta, " ou existe mais de um arquivo."))
  }

  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
  # encontra os anos
  anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>%
    stringr::str_remove("ANO:") %>%
    as.integer()
  # localiza a posicao do inicio de dados
  inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
  # localiza a posicao do fim de dados pela informacao de desvio padrao
  fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))


  df.custoOperativo <- purrr::map_df(1:length(anos), function(andaAnos) {
    # posicoes e nomes das variaveis
    df.energiaVertidaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(3, 9, 19, 29, 39, 49, 59, 69, 79, 89, 99, 109, 119),
        # vetor com as posicoes finais de cada campo
        c(6, 17, 27, 37, 47, 57, 67, 77, 87, 97, 107, 117, 127),
        # nome colunas
        c("serie", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
      ),
      col_types = "idddddddddddd",
      skip = 2
    )

    # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
    # pelo valor dentro de uma sequencia para cada ano.
    series <- 1:nrow(df.energiaVertidaAnual)
    df.energiaVertidaAnual$serie <- series

    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
    df.custoOperativoAnual <- df.energiaVertidaAnual %>%
      tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "custoOperativo") %>%
      dplyr::mutate(ano = anos[andaAnos], anoMes = (ano * 100 + as.numeric(mes))) %>%
      dplyr::select(serie, anoMes, custoOperativo)
    # concatena dados num data frame unico
    df.custoOperativo <- rbind(df.custoOperativo, df.custoOperativoAnual)
  })


  return(df.custoOperativo)
}
