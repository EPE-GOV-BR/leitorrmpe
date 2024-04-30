#' Leitor de tabela Nwlistop sem patamar
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de inicio e fim da coluna
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop
#' @param nomeTabela Nome da tabela do Nwlistop ex (efiol)
#' @param passo tamanho do campo
#' @param colunaInicialJaneiro coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao da serie)
#'
#'
#' @return \code{df.dadosNwlistop} data frame com os dados lidos
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor dos dados da tabela escolhida (\code{$dados})
#' }
#'
#' @examples
#' \dontrun{
#' leituraNwlistopGenericaSemPatamares("C:/PDE2027_Caso080", "efiol", 9, 7)
#' }
#'
#' @export
leituraNwlistopGenericaSemPatamares <- function(pasta, nomeTabela, passo, colunaInicialJaneiro) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  if (missing(nomeTabela)) {
    stop("favor indicar a tabela do Nwlistop a ser lida")
  }
  if (missing(passo)) {
    stop("favor indicar o passo entre colunas da tabela")
  }
  if (missing(colunaInicialJaneiro)) {
    stop("favor indicar a coluna inicial de janeiro, logo apos a serie")
  }

  # cria data frame de base para armazenar os dados
  df.dadosNwlistop <- tidyr::tibble(codREE = numeric(), serie = numeric(), anoMes = numeric(), dados = numeric())

  tabela <- paste("^", nomeTabela, sep = "", "[0-9]*\\.", collapse = NULL)
  tabela_REE <- paste("^", nomeTabela, "[msd]", sep = "", collapse = NULL)

  # seleciona somente os arquivos escolhidos
  arquivos <- setdiff(list.files(pasta, pattern = tabela), list.files(pasta, pattern = tabela_REE))

  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos ", nomeTabela, "XXX.out em ", pasta))
  }

  df.dadosNwlistop <- purrr::map_df(arquivos, function(arquivo) {
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
    ifelse(length(fimAnos) == 0, fimAnos <- inicioAnos + inicioAnos[2] - inicioAnos[1] - 1, fimAnos <- fimAnos)

    # pega informacao de ree no nome do arquivo
    inicioREE <- stringr::str_locate(arquivo, nomeTabela) %>%
      {
        .[1, 2] + 1
      } %>%
      unname()
    codREE <- stringr::str_sub(arquivo, inicioREE, inicioREE + 2) %>% as.integer()

    # Encontra as colunas
    posicaoColunasInicio <- c(3, seq.int(colunaInicialJaneiro, colunaInicialJaneiro + passo * 11, passo))
    posicaoColunasFim <- c(6, seq.int(colunaInicialJaneiro + passo - 1, colunaInicialJaneiro + passo * 12 - 1, passo))

    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes de acordo com manual do NEWAVE
      df.dadosNwlistopAno <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          posicaoColunasInicio,
          # vetor com as posicoes finais de cada campo
          posicaoColunasFim,
          # nome colunas
          c("serie", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "idddddddddddd",
        skip = 2
      )

      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano
      # pelo valor dentro de uma sequencia para cada ano.
      series <- 1:nrow(df.dadosNwlistopAno)
      df.dadosNwlistopAno <- df.dadosNwlistopAno %>% dplyr::mutate(serie = series)

      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.dadosNwlistopAno <- df.dadosNwlistopAno %>%
        tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "dados") %>%
        dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codREE, serie, anoMes, dados)
      # concatena dados num data frame unico
      df.dadosNwlistop <- rbind(df.dadosNwlistop, df.dadosNwlistopAno)
      # View(df.dadosNwlistopAno)
    })
  })
  return(df.dadosNwlistop)
}
