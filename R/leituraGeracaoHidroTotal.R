#' Leitor de dados de geracao hidroeletrica total
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de geracao hidroeletrica total por REE (ghtotxxx.*) e recupera esses valores por ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de geracao hidroeletrica total
#'
#' @return \code{df.geracaoHidroTotal} data frame com os valores de geracao hidroeletrica total por REE
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor de geracao hidroeletrica [MWmes] (\code{$geracao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraGeracaoHidroTotal("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraGeracaoHidroTotal <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base para armazenar os dados de energia armazenada final para todos os anos
  df.geracaoHidroTotal <- tidyr::tibble()

  # seleciona somente os arquivos ghtot
  arquivos <- setdiff(list.files(pasta, pattern = "^ghtot"), list.files(pasta, pattern = "^ghtot[pms]"))

  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos ghtotXXX.out em ", pasta))
  }

  df.geracaoHidroTotal <- purrr::map_df(arquivos, function(arquivo) {
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

    # pega informacao de ree no nome do arquivo
    inicioREE <- stringr::str_locate(arquivo, "ghtot") %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codREE <- stringr::str_sub(arquivo, inicioREE, inicioREE + 2) %>% as.integer()

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.geracaoHidroAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
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
      numeroPatamar <- df.geracaoHidroAnual %>%
        dplyr::distinct(patamar) %>%
        dplyr::pull() %>%
        max()
      series <- rep(1:(nrow(df.geracaoHidroAnual) / numeroPatamar), each = numeroPatamar)
      df.geracaoHidroAnual$serie <- series
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.geracaoHidroAnual <- df.geracaoHidroAnual %>%
        tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "geracao") %>%
        dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codREE, serie, patamar, anoMes, geracao)
      # concatena dados num data frame unico
      df.geracaoHidroTotal <- rbind(df.geracaoHidroTotal, df.geracaoHidroAnual)
    })
  })
  return(df.geracaoHidroTotal)
}
