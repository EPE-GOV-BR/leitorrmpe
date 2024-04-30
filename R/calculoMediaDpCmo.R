#' Calculo da media e desvio padrao do CMO
#'
#' Faz o calculo da media e do desvio padrao dos CMOs por submercado e por ano de estudo,
#' reproduzindo o calculo que e encontrado ao final do relatorio de saida do NEWAVE
#'
#' @param pasta localizacao dos arquivos do NEWAVE para os quais deseja efetuar o calculo
#'
#' @return \code{df.cmoMedio} data frame com os valores de custo marginal de demanda
#' \itemize{
#' \item codigo do submercado (\code{$codSubmercado})
#' \item valor do ano (\code{$ano})
#' \item media do CMO anual [$/MWh] (\code{$mediaCMO})
#' \item desvio padrao da media do CMO [$/MWh] (\code{$dpCMO})
#' }
#'
#' @examples
#' \dontrun{
#' calculoMediaDpCMO("C:/PDE2030_Caso080")
#' }
#'
#' @export
calculoMediaDpCMO <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # leitura dos CMO por patamar
  df.cmo <- leituraCustoMarginalDemanda(pasta)

  # leitura da duracao dos patamares
  df.duracao <- leituraDadosDuracaoPatamar(pasta)

  # pega dados gerais do NEWAVE
  df.dadosGerais <- leituraDadosGerais(pasta)

  # pega dados de configuracao hidro
  df.configuracaoHidro <- leituraConfiguracaoHidro(pasta)

  # define inicio e fim de caso
  inicioCaso <- df.dadosGerais$anoInicio * 100 + df.dadosGerais$mesInicio
  fimCaso <- (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1) * 100 + 12

  # define o numero de series utilizadas no estudo
  if (df.dadosGerais$tipoSimulacao == 1) {
    seriesHidro <- df.dadosGerais$seriesSinteticas
  } else if (df.dadosGerais$tipoSimulacao == 2) {
    # veririfica se todas as series do confhd possuem o mesmo tamanho
    seriesHidro <- df.configuracaoHidro %>%
      dplyr::mutate(seriesHidro = fimHistorico - inicioHistorico + 1) %>%
      dplyr::summarise(media = mean(seriesHidro), minimo = min(seriesHidro), maximo = max(seriesHidro))
    if (seriesHidro$media != seriesHidro$minimo | seriesHidro$media != seriesHidro$maximo | seriesHidro$minimo != seriesHidro$maximo) {
      stop("Series hidro nao possuem mesmo horizonte cadastrado no arquivo confhd!")
    }
    fimHistorico <- df.configuracaoHidro %>%
      dplyr::pull(fimHistorico) %>%
      max()
    # resgata o valor de inicio de varredura da serie historica para contabilizar quantidade de series
    inicioSimulacaoHistorico <- leituraSeriesHistoricasSimulacaoFinal(pastaCaso) %>%
      magrittr::extract2("df.varredura") %>%
      dplyr::pull(anoInicio)
    seriesHidro <- fimHistorico - inicioSimulacaoHistorico + 1
  } else {
    stop("Simulacao final apos convergencia PDDE do NEWAVE deve ser com series sinteticas ou historicas")
  }

  # calculo do cmo medio e desvio padrao
  df.cmoMedio <- dplyr::left_join(df.cmo, df.duracao, by = c("patamar", "anoMes")) %>%
    dplyr::mutate(cmoMedio = custoMarginalDemanda * duracaoPatamar) %>%
    dplyr::group_by(codSubmercado, serie, anoMes) %>%
    dplyr::summarise(cmoMedio = sum(cmoMedio)) %>%
    dplyr::filter(anoMes >= inicioCaso) %>%
    dplyr::mutate(ano = anoMes %/% 100) %>%
    dplyr::group_by(codSubmercado, serie, ano) %>%
    dplyr::summarise(cmoMedio = mean(cmoMedio)) %>%
    dplyr::group_by(codSubmercado, ano) %>%
    dplyr::summarise(mediaCMO = round(mean(cmoMedio), 2), dpCMO = round(stats::sd(cmoMedio) / (sqrt(seriesHidro - 1)), 2)) %>%
    dplyr::mutate(ano = as.character(ano))

  df.cmoMedioPeriodoPlanej <- dplyr::left_join(df.cmo, df.duracao, by = c("patamar", "anoMes")) %>%
    dplyr::mutate(cmoMedio = custoMarginalDemanda * duracaoPatamar) %>%
    dplyr::group_by(codSubmercado, serie, anoMes) %>%
    dplyr::summarise(cmoMedio = sum(cmoMedio)) %>%
    dplyr::filter(
      anoMes >= inicioCaso,
      anoMes <= fimCaso
    ) %>%
    dplyr::group_by(codSubmercado, serie) %>%
    dplyr::summarise(cmoMedio = mean(cmoMedio)) %>%
    dplyr::group_by(codSubmercado) %>%
    dplyr::summarise(mediaCMO = round(mean(cmoMedio), 2), dpCMO = round(stats::sd(cmoMedio) / (sqrt(seriesHidro - 1)), 2)) %>%
    dplyr::mutate(ano = "Periodo de Planejamento")

  df.cmoMedio <- rbind(df.cmoMedio, df.cmoMedioPeriodoPlanej)

  return(df.cmoMedio)
}
