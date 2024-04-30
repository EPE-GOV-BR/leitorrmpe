#' Leitor de dados de cota de montante por usina
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de cota de montante por usina  (hmontxxx.*) e
#' recupera esses valores por codigo da usina, ano, mes e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de geracao hidro por usina
#'
#' @return \code{df.CotaMontanteUsina} data frame com os valores de geracao termica por classes e total
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor da cota de montante [m] (\code{$cota})
#' }
#'
#' @examples
#' \dontrun{
#' leituraCotaMontanteUsina("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraCotaMontanteUsina <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base
  df.CotaMontanteUsina <- tidyr::tibble()

  # seleciona somente os arquivos hmont
  arquivos <- list.files(pasta, pattern = "^hmont[0-9]")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos hmontXXX.out em ", pasta))
  }

  df.CotaMontanteUsina <- purrr::map_df(arquivos, function(arquivo) {
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    # filtra as linhas com totais
    localizaTotais <- stringr::str_detect(dadosBrutos, "^ +TOTAL")
    dadosBrutos <- dadosBrutos[!localizaTotais]

    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>%
      stringr::str_remove("ANO:") %>%
      as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))

    # pega informacao da usina no nome do arquivo
    inicioUsina <- stringr::str_locate(arquivo, "hmont") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codUsina <- stringr::str_sub(arquivo, inicioUsina, inicioUsina + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.CotaMontanteAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(3, 10, 24, 39, 54, 69, 84, 99, 114, 129, 144, 159, 174),
          # vetor com as posicoes finais de cada campo
          c(6, 23, 37, 52, 67, 82, 97, 112, 127, 142, 157, 172, 187),
          # nome colunas
          c("serie", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "idddddddddddd",
        skip = 2
      )

      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
      # pelo valor dentro de uma sequencia para cada ano.
      series <- 1:nrow(df.CotaMontanteAnual)
      df.CotaMontanteAnual <- df.CotaMontanteAnual %>% dplyr::mutate(serie = series)

      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.CotaMontanteAnual <- df.CotaMontanteAnual %>%
        tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "cota") %>%
        dplyr::mutate(ano = anos[andaAnos], codUsina = codUsina, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codUsina, serie, anoMes, cota)
      # concatena dados num data frame unico
      df.CotaMontanteUsina <- rbind(df.CotaMontanteUsina, df.CotaMontanteAnual)
    })
  })
  return(df.CotaMontanteUsina)
}
