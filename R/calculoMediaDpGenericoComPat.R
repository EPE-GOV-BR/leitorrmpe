#' Calculo da media e desvio padrao de tabela Nwlistop COM patamar
#'
#' Faz o calculo da media e do desvio padrao da variavel de interesse por submercado e por ano de estudo,
#'
#' @param pasta localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop
#' @param nomeTabela nome da tabela do Nwlistop ex (efiol)
#' @param passo tamanho do campo
#' @param colunaInicialJaneiro coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao do patamar)
#'
#' @return \code{df.cmoMedio} data frame com os valores de custo marginal de demanda
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item valor do ano (\code{$ano})
#' \item media da variavel (\code{$media})
#' \item desvio padrao da media da variavel (\code{$dp})
#' }
#'
#' @examples
#' \dontrun{
#' calculoMediaDpGenericoComPat("C:/PDE2027_Caso080", "ghtot", 9, 12)
#' }
#'
#' @export
calculoMediaDpGenericoComPat <- function(pasta, nomeTabela, passo, colunaInicialJaneiro) {
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
    stop("favor indicar a coluna inicial de janeiro, logo apos o patamar")
  }

  # leitura dos valores
  lt.dados <- leituraNwlistopGenericaComPatamares(pasta, nomeTabela, passo, colunaInicialJaneiro)

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
      stop("Series hidro nao possuem mesmo horizonte cadastrado no arquivo confhd")
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

  # calculo dos valores medios e desvio padrao
  df.valoresMedios <- lt.dados[[2]] %>%
    dplyr::filter(anoMes >= inicioCaso) %>%
    dplyr::mutate(ano = anoMes %/% 100) %>%
    dplyr::group_by(codREE, serie, ano) %>%
    dplyr::summarise(valorMedio = mean(dados)) %>%
    dplyr::group_by(codREE, ano) %>%
    dplyr::summarise(media = round(mean(valorMedio), 2), dp = round(stats::sd(valorMedio) / (sqrt(seriesHidro - 1)), 2)) %>%
    dplyr::mutate(ano = as.character(ano))

  df.valoresMediosPeriodoPlanej <- lt.dados[[2]] %>%
    dplyr::filter(
      anoMes >= inicioCaso,
      anoMes <= fimCaso
    ) %>%
    dplyr::group_by(codREE, serie) %>%
    dplyr::summarise(valorMedio = mean(dados)) %>%
    dplyr::group_by(codREE) %>%
    dplyr::summarise(media = round(mean(valorMedio), 2), dp = round(stats::sd(valorMedio) / (sqrt(seriesHidro - 1)), 2)) %>%
    dplyr::mutate(ano = "Periodo de Planejamento")

  df.valoresMedios <- rbind(df.valoresMedios, df.valoresMediosPeriodoPlanej)

  return(df.valoresMedios)
}
