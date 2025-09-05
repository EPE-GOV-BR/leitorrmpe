#' Leitor de dados de geracao termica por classes e total
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de geracao termica por classes e total do submercado  (gtertxxx.*) e
#' recupera esses valores por codigo da usina, ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de geracao termica por classes e total
#'
#' @return \code{df.geracaoTermicaClassesTotal} data frame com os valores de geracao termica por classes e total
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item codigo do submercado (\code{$codSubmercado})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor da geracao termica [MWmes] (\code{$geracao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraGeracaoTermicaClassesTotal("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraGeracaoTermicaClassesTotal <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base
  df.geracaoTermicaClassesTotal <- tidyr::tibble()

  # seleciona somente os arquivos gtert
  arquivos <- list.files(pasta, pattern = "^gtert[0-9]")
  arquivos <- arquivos[stringr::str_detect(arquivos, "\\.out\\.*$")]
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos gtertXXX.out em ", pasta))
  }


  df.geracaoTermicaClassesTotal <- purrr::map_df(arquivos, function(arquivo) {
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")

    filtro <- iotools::dstrfw(I(dadosBrutos), col_types = c("character", "character"), widths = c(2L, 3L)) %>%
      dplyr::select(2) %>%
      dplyr::mutate(V2 = trimws(V2), V2 = ifelse(V2 == "", NA, V2)) %>%
      tidyr::fill(everything()) %>%
      dplyr::pull() %>%
      stringr::str_detect("^[0-9]+")

    # se todos valores forem falsos, subsistema nao tem usinas
    if (any(filtro)) {
      filtroAnos <- stringr::str_detect(dadosBrutos, "ANO:")
      filtro <- filtro | filtroAnos
      dadosBrutos <- dadosBrutos[filtro]

      anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>%
        stringr::str_remove("ANO:") %>%
        as.integer()

      # localiza a posicao do inicio de dados
      inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))

      # localiza a posicao do fim de dados
      fimAnos <- c(inicioAnos[-1] - 2, length(dadosBrutos))

      # pega informacao de submercado no nome do arquivo
      inicioSubmercado <- stringr::str_locate(arquivo, "gtert") %>%
        {
          .[1, 2] + 1
        } %>%
        unname()
      codSubmercado <- stringr::str_sub(arquivo, inicioSubmercado, inicioSubmercado + 2) %>% as.integer()

      purrr::map_df(1:length(anos), function(andaAnos) {
        # posicoes e nomes das variaveis
        df.geracaoTermicaClassesTotalAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos])]),
          col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
            c(2, 7, 13, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108, 117),
            # vetor com as posicoes finais de cada campo
            c(6, 12, 17, 25, 34, 43, 52, 61, 70, 79, 88, 97, 106, 115, 124),
            # nome colunas
            c("codUsina", "serie", "patamar", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
          ),
          col_types = "iiidddddddddddd",
          skip = 1
        )

        df.geracaoTermicaClassesTotalAnual <- df.geracaoTermicaClassesTotalAnual %>% tidyr::fill(codUsina, serie)

        # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
        # pelo valor dentro de uma sequencia para cada ano.
        numeroPatamar <- df.geracaoTermicaClassesTotalAnual %>%
          dplyr::distinct(patamar) %>%
          dplyr::pull() %>%
          max()
        numeroSerie <- df.geracaoTermicaClassesTotalAnual %>%
          dplyr::distinct(serie) %>%
          dplyr::pull() %>%
          length()
        numeroUsinas <- df.geracaoTermicaClassesTotalAnual %>%
          dplyr::distinct(codUsina) %>%
          dplyr::pull() %>%
          length()
        series <- rep(1:numeroSerie, each = numeroPatamar) %>% rep(numeroUsinas)
        df.geracaoTermicaClassesTotalAnual <- df.geracaoTermicaClassesTotalAnual %>%
          dplyr::mutate(serie = series, ano = anos[andaAnos], codSubmercado = codSubmercado)

        # concatena dados num data frame unico
        df.geracaoTermicaClassesTotal <- rbind(df.geracaoTermicaClassesTotal, df.geracaoTermicaClassesTotalAnual)
      })
    }
  })
  # faz o "pivot" da tabela para dados normalizados (tidy)
  df.geracaoTermicaClassesTotal <- df.geracaoTermicaClassesTotal %>%
    tidyr::pivot_longer(cols = c(-codUsina, -codSubmercado, -serie, -patamar, -ano), names_to = "mes", values_to = "geracao") %>%
    dplyr::mutate(anoMes = (ano * 100 + as.numeric(mes))) %>%
    dplyr::select(codUsina, codSubmercado, anoMes, serie, patamar, geracao)

  return(df.geracaoTermicaClassesTotal)
}
