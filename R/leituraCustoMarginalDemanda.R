#' Leitor de dados de custo marginal de demanda do submercado
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de custo marginal de demanda do submercado  (cmargxxx.*) e
#' recupera esses valores por ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de custo marginal de demanda
#'
#' @return \code{df.custoMarginalDemanda} data frame com os valores de custo marginal de demanda
#' \itemize{
#' \item codigo do submercado (\code{$codSubmercado})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor do custo marginal de demanda [$/MWh] (\code{$custoMarginalDemanda})
#' }
#'
#' @examples
#' \dontrun{
#' leituraCustoMarginalDemanda("C:/PDE2027_Caso080")
#' }
#'
#' @export

leituraCustoMarginalDemanda <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base para armazenar os dados de custo marginal para todos os anos
  df.custoMarginalDemanda <- tidyr::tibble()

  # seleciona somente os arquivos cmarg
  arquivos <- list.files(pasta, pattern = "^cmarg[0-9]*\\.")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos cmargXXX.out em ", pasta))
  }


  df.custoMarginalDemanda <- purrr::map_df(arquivos, function(arquivos) {
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivos, sep = "/")), sep = "\n")
    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>%
      stringr::str_remove("ANO:") %>%
      as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))

    # pega informacao de submercado no nome do arquivo
    inicioSubmercado <- stringr::str_locate(arquivos, "cmarg") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codSubmercado <- stringr::str_sub(arquivos, inicioSubmercado, inicioSubmercado + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      df.custoMarginalDemandaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(3, 10, 13, 25, 34, 43, 52, 61, 70, 79, 88, 97, 106, 115),
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
      numeroPatamar <- df.custoMarginalDemandaAnual %>%
        dplyr::distinct(patamar) %>%
        dplyr::pull() %>%
        max()
      series <- rep(1:(nrow(df.custoMarginalDemandaAnual) / numeroPatamar), each = numeroPatamar)
      df.custoMarginalDemandaAnual$serie <- series
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.custoMarginalDemandaAnual <- df.custoMarginalDemandaAnual %>%
        tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "custoMarginalDemanda") %>%
        dplyr::mutate(ano = anos[andaAnos], codSubmercado = codSubmercado, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codSubmercado, serie, patamar, anoMes, custoMarginalDemanda)
      # concatena dados num data frame unico
      df.custoMarginalDemanda <- rbind(df.custoMarginalDemanda, df.custoMarginalDemandaAnual)
    })
  })

  return(df.custoMarginalDemanda)
}
