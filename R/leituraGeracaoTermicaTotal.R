#' Leitor de dados de geracao termica total
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de geracao termica total por submercado  (gttotxxx.*) e recupera esses valores por ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de geracao termica total
#'
#' @return \code{df.geracaoTermicaTotal} data frame com os valores de geracao termica total
#' \itemize{
#' \item codigo do submercado (\code{$codSubmercado})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de geracao termica [MWmes] (\code{$geracao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraGeracaoTermicaTotal("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraGeracaoTermicaTotal <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base
  df.geracaoTermicaTotal <- tidyr::tibble()

  # seleciona somente os arquivos gttot
  arquivos <- setdiff(list.files(pasta, pattern = "^gttot"), list.files(pasta, pattern = "^gttot[ps]"))
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos gttotXXX.out em ", pasta))
  }

  df.geracaoTermicaTotal <- purrr::map_df(arquivos, function(arquivo) {
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

    # pega informacao de submercado no nome do arquivo
    inicioSubmercado <- stringr::str_locate(arquivo, "gttot") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codSubmercado <- stringr::str_sub(arquivo, inicioSubmercado, inicioSubmercado + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.geracaoTermicaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(3, 10, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112),
          # vetor com as posicoes finais de cada campo
          c(6, 11, 20, 29, 38, 47, 56, 65, 74, 83, 92, 101, 110, 119),
          # nome colunas
          c("serie", "patamar", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "iidddddddddddd",
        skip = 2
      )

      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
      # pelo valor dentro de uma sequencia para cada ano.
      numeroPatamar <- df.geracaoTermicaAnual %>%
        dplyr::distinct(patamar) %>%
        dplyr::pull() %>%
        max()
      series <- rep(1:(nrow(df.geracaoTermicaAnual) / numeroPatamar), each = numeroPatamar)
      df.geracaoTermicaAnual$serie <- series
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.geracaoTermicaAnual <- df.geracaoTermicaAnual %>%
        tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "geracao") %>%
        dplyr::mutate(ano = anos[andaAnos], codSubmercado = codSubmercado, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codSubmercado, serie, patamar, anoMes, geracao)
      # concatena dados num data frame unico
      df.geracaoTermicaTotal <- rbind(df.geracaoTermicaTotal, df.geracaoTermicaAnual)
    })
  })
  return(df.geracaoTermicaTotal)
}
