#' Leitor dos dados do arquivo PARP
#'
#' Faz a leitura do arquivo do NEWAVE com memoria de calculo do modelo autorregressivo periodico (parp.dat).
#'
#' @param pasta localizacao do arquivo do NEWAVE PARP
#'
#' @return \code{lt.dadosParp} lista com data frames com dados das usinas hidroeletricas para cada reservatorio equivalente (REE)
#' \itemize{
#' \item \code{df.energiahist} data frame com a energia natural afluente historica (ENA)
#' \itemize{
#' \item nome da usina (\code{$nomeUsina})
#' \item configuracao (\code{$configuracao})
#' \item ano e mes da ENA para uma determinada configuracao (\code{$anomes})
#' \item energia natural afluente historica (ENA) (\code{$enahist})
#' }
#' \item \code{df.parametros} data frame com dados de evaporacao mensal por usina
#' \itemize{
#' \item nome da usina (\code{$nomeUsina})
#' \item ano e mes da ENA para uma determinada configuracao (\code{$anomes})
#' \item ordem do modelo PARP antes do processo de reducao da ordem (\code{$ordemOriginal})
#' \item ordem do modelo PARP apos o processo de reducao da ordem (\code{$ordemFinal})
#' \item parametro fi do modelo PARP, lag1 (\code{$fiLag1})
#' \item parametro fi do modelo PARP, lag2 (\code{$fiLag2})
#' \item parametro fi do modelo PARP, lag3 (\code{$fiLag3})
#' \item parametro fi do modelo PARP, lag4 (\code{$fiLag4})
#' \item parametro fi do modelo PARP, lag5 (\code{$fiLag5})
#' \item parametro fi do modelo PARP, lag6 (\code{$fiLag6})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag1 (\code{$coefLag1})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag2 (\code{$coefLag2})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag3 (\code{$coefLag3})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag4 (\code{$coefLag4})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag5 (\code{$coefLag5})
#' \item parametro coef (fi*desvio padrao t / desvio padrao t-1) do modelo PARP, lag6 (\code{$coefLag6})
#' \item parametro fiA do modelo PARP com parcela anual, lag1 (\code{$fiLag1})
#' \item parametro fiA do modelo PARP com parcela anual, lag2 (\code{$fiLag2})
#' \item parametro fiA do modelo PARP com parcela anual, lag3 (\code{$fiLag3})
#' \item parametro fiA do modelo PARP com parcela anual, lag4 (\code{$fiLag4})
#' \item parametro fiA do modelo PARP com parcela anual, lag5 (\code{$fiLag5})
#' \item parametro fiA do modelo PARP com parcela anual, lag6 (\code{$fiLag6})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag1 (\code{$coefLag1})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag2 (\code{$coefLag2})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag3 (\code{$coefLag3})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag4 (\code{$coefLag4})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag5 (\code{$coefLag5})
#' \item parametro coefA (fi*desvio padrao t / desvio padrao t-1) do modelo PARP com parcela anual, lag6 (\code{$coefLag6})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraPARP("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraPARP <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # cria data frame de base para armazenar os dados de energia natural afluente para as configuracoes
  df.energiahist <- tidyr::tibble(NomeREE = character(), configuracao = numeric(), anomes = numeric(), enahist = numeric())

  df.ordemOriginal <- tidyr::tibble(NomeREE = character(), ano = character(), mes = numeric(), ordem = numeric())
  df.ordemFinal <- tidyr::tibble(NomeREE = character(), ano = character(), mes = numeric(), ordem = numeric())

  df.parametros <- tidyr::tibble(fiLag1 = numeric(), fiLag2 = numeric(), fiLag3 = numeric(), fiLag4 = numeric(), fiLag5 = numeric(), fiLag6 = numeric(), coefLag1 = numeric(), coefLag2 = numeric(), coefLag3 = numeric(), coefLag4 = numeric(), coefLag5 = numeric(), coefLag6 = numeric(), fiALag1 = numeric(), fiALag2 = numeric(), fiALag3 = numeric(), fiALag4 = numeric(), fiALag5 = numeric(), fiALag6 = numeric(), coefALag1 = numeric(), coefALag2 = numeric(), coefALag3 = numeric(), coefALag4 = numeric(), coefALag5 = numeric(), coefALag6 = numeric())
  df.fiParp <- tidyr::tibble(fiLag1 = numeric(), fiLag2 = numeric(), fiLag3 = numeric(), fiLag4 = numeric(), fiLag5 = numeric(), fiLag6 = numeric())
  df.coefParp <- tidyr::tibble(coefLag1 = numeric(), coefLag2 = numeric(), coefLag3 = numeric(), coefLag4 = numeric(), coefLag5 = numeric(), coefLag6 = numeric())
  df.fiParpA <- tidyr::tibble(fiALag1 = numeric(), fiALag2 = numeric(), fiALag3 = numeric(), fiALag4 = numeric(), fiALag5 = numeric(), fiALag6 = numeric())
  df.coefParpA <- tidyr::tibble(coefALag1 = numeric(), coefALag2 = numeric(), coefALag3 = numeric(), coefALag4 = numeric(), coefALag5 = numeric(), coefALag6 = numeric())
  df.energiahistFinal <- NULL
  # seleciona somente arquivo parp
  arquivos <- setdiff(list.files(pasta, pattern = "^parp"), list.files(pasta, pattern = "^parp[ms]"))
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos parp.dat em ", pasta))
  }

  arquivo <- arquivos[1]

  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")

  # leitura da ENA Historica

  # encontra os anos
  nomeREE_configuracao <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "SERIE  DE ENERGIAS DO REE"))] %>%
    stringr::str_remove("SERIE  DE ENERGIAS DO REE") %>%
    stringr::str_remove("CONFIGURACAO No.")
  nomeREE <- unique(stringr::str_extract(nomeREE_configuracao, pattern = "[[:alpha:]]..........."))
  configuracao <- as.numeric(unique(stringr::str_extract(nomeREE_configuracao, pattern = ".[[:digit:]].") %>% stringr::str_remove(pattern = "[[:punct:]]")))


  # localiza a posicao do inicio de dados
  inicioEnahist <- which(stringr::str_detect(dadosBrutos, "SERIE  DE ENERGIAS DO REE")) + 5
  # localiza a posicao do fim de dados pela informacao de media amostral
  fimEnahist <- which(stringr::str_detect(dadosBrutos, "MEDIA AMOSTRAL DAS ENERGIAS")) - 2


  # leitura da ordem dos modelos

  posicaoInicialOrdemOriginal <- which(stringr::str_detect(dadosBrutos, "ORDEM ORIGINAL DO MODELO AUTORREGRESSIVO PARA CADA PERIODO")) + 4
  posicaoInicialOrdemFinal <- which(stringr::str_detect(dadosBrutos, "ORDEM FINAL DO MODELO AUTORREGRESSIVO PARA CADA PERIODO")) + 4

  numeroLinhasOrdem <- posicaoInicialOrdemFinal - posicaoInicialOrdemOriginal - 6


  posicaoPeriodos <- as.numeric(which(stringr::str_detect(dadosBrutos, "PERIODO:")))
  posicaoCorrelogramo <- (which(stringr::str_detect(dadosBrutos, "CORRELOGRAMO DA SERIE DE RUIDOS")))


  lista <- crossmap::map_vec(1:length(nomeREE), function(andaREE) {
    # print(andaREE)

    df.energiahistFinal <- purrr::map_df(1:length(configuracao), function(andaconfig) {
      # posicoes e nomes de acordo com manual do NEWAVE
      df.energiaHistConfiguracao <- readr::read_fwf(I(dadosBrutos[inicioEnahist[andaconfig]:fimEnahist[andaconfig]]),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(1, 5, 15, 26, 37, 48, 59, 70, 81, 92, 103, 114, 125),
          # vetor com as posicoes finais de cada campo
          c(4, 14, 25, 36, 47, 58, 69, 80, 91, 102, 113, 124, 135),
          # nome colunas
          c("ano", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        skip = 0,
        show_col_types = FALSE
      )


      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy) nomeREE[andaREE]
      df.energiahistAux <- df.energiaHistConfiguracao %>%
        tidyr::pivot_longer(cols = -ano, names_to = "mes", values_to = "enahist") %>%
        dplyr::mutate(NomeREE = nomeREE[andaREE], configuracao = configuracao[andaconfig], anomes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(NomeREE, configuracao, anomes, enahist)

      df.energiahist <- rbind(df.energiahist, df.energiahistAux)
    })


    # Le a ordem original do modelo autorregressivo periodico para cada REE

    df.ordemAux <- readr::read_fwf(I(dadosBrutos[posicaoInicialOrdemOriginal[andaREE]:(posicaoInicialOrdemOriginal[andaREE] + numeroLinhasOrdem[andaREE] - 1)]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(33, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92),
        # vetor com as posicoes finais de cada campo
        c(36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96),
        # nome colunas
        c("ano", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy) nomeREE[andaREE]

    df.ordemAux <- df.ordemAux %>%
      tidyr::pivot_longer(cols = -ano, names_to = "mes", values_to = "ordem_original") %>%
      dplyr::mutate(NomeREE = nomeREE[andaREE]) %>%
      dplyr::select(NomeREE, ano, mes, ordem_original)

    df.ordemOriginal <- rbind(df.ordemOriginal, df.ordemAux)

    # Le a ordem final do modelo autorregressivo periodico para cada REE

    df.ordemfAux <- readr::read_fwf(I(dadosBrutos[posicaoInicialOrdemOriginal[andaREE]:(posicaoInicialOrdemOriginal[andaREE] + numeroLinhasOrdem[andaREE] - 1)]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(33, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92),
        # vetor com as posicoes finais de cada campo
        c(36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96),
        # nome colunas
        c("ano", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy) nomeREE[andaREE]

    df.ordemfAux <- df.ordemfAux %>%
      tidyr::pivot_longer(cols = -ano, names_to = "mes", values_to = "ordem_final") %>%
      dplyr::mutate(NomeREE = nomeREE[andaREE]) %>%
      dplyr::select(NomeREE, ano, mes, ordem_final)

    df.ordemFinal <- rbind(df.ordemFinal, df.ordemfAux)


    # Le coeficientes do modelo PARP

    posicaoPeriodosREE <- as.numeric(which((posicaoPeriodos > posicaoInicialOrdemOriginal[andaREE] & posicaoPeriodos < posicaoCorrelogramo[andaREE])))

    fiParpAux <- readr::read_fwf(I(dadosBrutos[posicaoPeriodos[posicaoPeriodosREE] + 2]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(1, 12, 23, 34, 45, 56),
        # vetor com as posicoes finais de cada campo
        c(9, 20, 31, 42, 53, 64),
        # nome colunas
        c("fiLag1", "fiLag2", "fiLag3", "fiLag4", "fiLag5", "fiLag6")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    fiParpAux[is.na(fiParpAux)] <- 0

    df.fiParp <- rbind(df.fiParp, fiParpAux)

    coefParpAux <- readr::read_fwf(I(dadosBrutos[posicaoPeriodos[posicaoPeriodosREE] + 3]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(1, 12, 23, 34, 45, 56),
        # vetor com as posicoes finais de cada campo
        c(9, 20, 31, 42, 53, 64),
        # nome colunas
        c("coefLag1", "coefLag2", "coefLag3", "coefLag4", "coefLag5", "coefLag6")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    coefParpAux[is.na(coefParpAux)] <- 0
    df.coefParp <- rbind(df.coefParp, coefParpAux)

    fiParpAAaux <- readr::read_fwf(I(dadosBrutos[posicaoPeriodos[posicaoPeriodosREE] + 4]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(1, 12, 23, 34, 45, 56),
        # vetor com as posicoes finais de cada campo
        c(9, 20, 31, 42, 53, 64),
        # nome colunas
        c("fiALag1", "fiALag2", "fiALag3", "fiALag4", "fiALag5", "fiALag6")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    fiParpAAaux[is.na(fiParpAAaux)] <- 0
    df.fiParpA <- rbind(df.fiParpA, fiParpAAaux)

    coefParpAAaux <- readr::read_fwf(I(dadosBrutos[posicaoPeriodos[posicaoPeriodosREE] + 5]),
      col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
        c(1, 12, 23, 34, 45, 56),
        # vetor com as posicoes finais de cada campo
        c(9, 20, 31, 42, 53, 64),
        # nome colunas
        c("coefALag1", "coefALag2", "coefALag3", "coefALag4", "coefALag5", "coefALag6")
      ),
      skip = 0,
      show_col_types = FALSE
    )

    coefParpAAaux[is.na(coefParpAAaux)] <- 0
    df.coefParpA <- rbind(df.coefParpA, coefParpAAaux)

    lista <- list(
      df.ordemOriginal = df.ordemOriginal,
      df.ordemFinal = df.ordemFinal,
      df.fiParp = df.fiParp,
      df.coefParp = df.coefParp,
      df.fiParpA = df.fiParpA,
      df.coefParpA = df.coefParpA,
      df.energiahistFinal = df.energiahistFinal
    )
  })

  lista <- list(
    df.ordemOriginal = purrr::map_df(lista, ~ .[[1]]),
    df.ordemFinal = purrr::map_df(lista, ~ .[[2]]),
    df.fiParp = purrr::map_df(lista, ~ .[[3]]),
    df.coefParp = purrr::map_df(lista, ~ .[[4]]),
    df.fiParpA = purrr::map_df(lista, ~ .[[5]]),
    df.coefParpA = purrr::map_df(lista, ~ .[[6]]),
    df.energiahistFinal = purrr::map_df(lista, ~ .[[7]])
  )


  df.ordemFinal <- lista$df.ordemFinal

  df.parametros <- cbind(lista$df.ordemOriginal, dplyr::select(df.ordemFinal, ordem_final), lista$df.fiParp, lista$df.coefParp, lista$df.fiParpA, lista$df.coefParpA)
  lt.dadosParp <- list(df.energiahist = lista$df.energiahistFinal, df.parametros = df.parametros)
  return(lt.dadosParp)
}
