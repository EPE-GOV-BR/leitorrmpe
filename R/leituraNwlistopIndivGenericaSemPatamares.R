#' Leitor de tabela Nwlistop Individualizado sem patamar
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de inicio e fim da coluna
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop
#' @param nomeTabela Nome da tabela do Nwlistop ex (efiol)
#' @param passo tamanho do campo (opcional). Caso seja vazio ou NA, o passo será calculado pelo cabeçalho
#' @param colunaInicialJaneiro coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao da serie)
#'
#'
#' @return \code{df.dadosNwlistop} data frame com os dados lidos
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item serie (\code{$serie})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor dos dados da tabela escolhida (\code{$dados})
#' }
#'
#' @examples
#' \dontrun{
#' leituraNwlistopIndivGenericaSemPatamares("C:/PDE2027_Caso080", "hmont", 15, 8)
#' }
#'
#' @export
leituraNwlistopIndivGenericaSemPatamares <- function(pasta, nomeTabela, passo, colunaInicialJaneiro) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  if (missing(nomeTabela)) {
    stop("favor indicar a tabela do Nwlistop a ser lida")
  }
  if (missing(colunaInicialJaneiro)) {
    stop("favor indicar a coluna inicial de janeiro, logo apos a serie")
  }

  # cria data frame de base para armazenar os dados
  df.dadosNwlistop <- tidyr::tibble(codREE = numeric(), serie = numeric(), anoMes = numeric(), dados = numeric())

  # seleciona somente os arquivos escolhidos
  arquivos <- list.files(pasta, pattern = paste0("^", nomeTabela, "[0-9]"))

  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos ", nomeTabela, "XXX.out em ", pasta))
  }
  
  # se o passo nao for informado, encontra pelo espacamento da linha 5 do arquivo
  if(is.na(passo)){
    linha <- readr::read_lines(paste(pasta, arquivos[1], sep = "/"), n_max = 5)[5]
    posicoes <- gregexpr("[0-9]", linha)[[1]]  
    espacamento <- diff(posicoes)  
    passo <- as.numeric(names(sort(-table(espacamento)))[1])
  }

  # Encontra as colunas
  posicaoColunasInicio <- c(3, seq.int(colunaInicialJaneiro, colunaInicialJaneiro + passo * 11, passo))
  posicaoColunasFim <- c(6, seq.int(colunaInicialJaneiro + passo - 1, colunaInicialJaneiro + passo * 12 - 1, passo))

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
    if (length(fimAnos) == 0) {
      fimAnos <- inicioAnos + inicioAnos[2] - inicioAnos[1] - 1
    } else {
      fimAnos <- fimAnos
    }

    # pega informacao da usina no nome do arquivo
    inicioUsina <- stringr::str_locate(arquivo, nomeTabela) %>% {.[1, 2] + 1} %>% unname()
    codUsina <- stringr::str_sub(arquivo, inicioUsina, inicioUsina + 2) %>% as.integer()

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
        dplyr::mutate(ano = anos[andaAnos], codUsina, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(codUsina, serie, anoMes, dados)
      # concatena dados num data frame unico
      df.dadosNwlistop <- rbind(df.dadosNwlistop, df.dadosNwlistopAno)
      # View(df.dadosNwlistopAno)
    })
  })
  return(df.dadosNwlistop)
}
