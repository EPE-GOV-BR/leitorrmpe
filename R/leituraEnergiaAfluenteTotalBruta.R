#' Leitor dos dados de energia afluente total bruta por REE
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de energia afluente total bruta por REE (eafbXXX.out) e recupera esses valores por ano, mes e serie.
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de energia armazenada final
#'
#' @return \code{df.energiaAfluenteTotalBruta} data frame com os valores de energia afluente total bruta por submercado
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de energia afluente total bruta [MWMes] (\code{$eafb})
#' }
#'
#' @examples
#' \dontrun{
#' leituraEnergiaAfluenteTotalBruta("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraEnergiaAfluenteTotalBruta <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base para armazenar os dados de energia armazenada final para todos os anos
  df.energiaAfluenteTotalBruta <- tidyr::tibble(codREE = numeric(), serie = numeric(), anoMes = numeric(), eafb = numeric())

  # seleciona somente os arquivos earmf
  arquivos <- setdiff(list.files(pasta, pattern = "^eafb"), list.files(pasta, pattern = "^eafb[ms]"))
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos eafbXXX.out em ", pasta))
  }

  df.energiaAfluenteTotalBruta <- purrr::map_df(arquivos, function(arquivo) {
    ## le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")

    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>%
      stringr::str_remove("ANO:") %>%
      as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))

    # pega informacao de ree no nome do arquivo
    inicioREE <- stringr::str_locate(arquivo, "eafb") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codREE <- stringr::str_sub(arquivo, inicioREE, inicioREE + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes de acordo com manual do NEWAVE
      df.energiaAfluenteTotalBrutaAno <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(3, 8, 17, 26, 35, 44, 53, 62, 71, 80, 89, 98, 107),
          # vetor com as posicoes finais de cada campo
          c(6, 15, 24, 33, 42, 51, 60, 69, 78, 87, 96, 105, 114),
          # nome colunas
          c("serie", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "idddddddddddd",
        skip = 2
      )

      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
      # pelo valor dentro de uma sequencia para cada ano.
      series <- 1:nrow(df.energiaAfluenteTotalBrutaAno)
      df.energiaAfluenteTotalBrutaAno <- df.energiaAfluenteTotalBrutaAno %>% dplyr::mutate(serie = series)

      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.energiaAfluenteTotalBrutaAno <- df.energiaAfluenteTotalBrutaAno %>%
        tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "eafb") %>%
        dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codREE, serie, anoMes, eafb)
      # concatena dados num data frame unico
      df.energiaAfluenteTotalBruta <- rbind(df.energiaAfluenteTotalBruta, df.energiaAfluenteTotalBrutaAno)
    })
  })
  return(df.energiaAfluenteTotalBruta)
}
