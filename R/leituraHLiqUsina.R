#' Leitor de dados de altura queda liquida por usina
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de altura de queda liquida por usina  (hliqxxx.*) e
#' recupera esses valores por codigo da usina, ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de altura queda liquida por usina
#'
#' @return \code{df.HLiqUsina} data frame com os valores de altura queda liquida por usina
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor da altura de queda liquida [m] (\code{$hliq})
#' }
#'
#' @examples
#' \dontrun{
#' leituraHLiqUsina("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraHLiqUsina <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base
  df.HLiqUsina <- tidyr::tibble()

  # seleciona somente os arquivos hliq
  arquivos <- list.files(pasta, pattern = "^hliq[0-9]")
  arquivos <- arquivos[stringr::str_detect(arquivos, "\\.out\\.*$")]
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos hliqXXX.out em ", pasta))
  }

  df.HLiqUsina <- purrr::map_df(arquivos, function(arquivo) {
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
    fimAnos <- c(inicioAnos[-1] - 2, length(dadosBrutos))

    # pega informacao da usina no nome do arquivo
    inicioUsina <- stringr::str_locate(arquivo, "hliq") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codUsina <- stringr::str_sub(arquivo, inicioUsina, inicioUsina + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.HLiqAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:fimAnos[andaAnos]]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(3, 9, 15, 24, 33, 42, 51, 60, 69, 78, 87, 96, 105, 114),
          # vetor com as posicoes finais de cada campo
          c(6, 11, 23, 32, 41, 50, 59, 68, 77, 86, 95, 104, 113, 122),
          # nome colunas
          c("serie", "patamar", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "iidddddddddddd",
        skip = 2
      )

      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
      # pelo valor dentro de uma sequencia para cada ano.
      numeroPatamar <- df.HLiqAnual %>%
        dplyr::distinct(patamar) %>%
        dplyr::pull() %>%
        max()
      series <- rep(1:(nrow(df.HLiqAnual) / numeroPatamar), each = numeroPatamar)
      df.HLiqAnual$serie <- series
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.HLiqAnual <- df.HLiqAnual %>%
        tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "hliq") %>%
        dplyr::mutate(ano = anos[andaAnos], codUsina = codUsina, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codUsina, serie, patamar, anoMes, hliq)
      # concatena dados num data frame unico
      df.HLiqUsina <- rbind(df.HLiqUsina, df.HLiqAnual)
    })
  })
  return(df.HLiqUsina)
}
