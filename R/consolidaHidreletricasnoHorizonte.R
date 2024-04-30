#' Dados consolidados de hidreletricas ao longo do horizonte
#'
#' Faz a consolidacao dos dados das hidreletricas ao longo do horizonte de estudo definido no NEWAVE.
#' Usa como referencia para a leitura dos arquivos as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.consolidadoUHEnoHorizonte} data frame com dados consolidados de hidreletricas ao longo do horizonte
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item ano e mes de entrada em operacao da unidade hidroeletrica (\code{$anoMes})
#' \item codigo do REE (\code{$codREE})
#' \item canal de fuga medio [m] (\code{$canalFugaMedio})
#' \item volume Maximo [hm3] (\code{$volumeMaximoFinal})
#' \item volume Minimo [hm3] (\code{$volumeMinimoFinal})
#' \item vazao minima do historico [m3/s] (\code{$vazaoMinimaFinal})
#' \item numero de conjuntos de maquinas (\code{$numeroConjuntos})
#' \item taxa esperada de indisponibilidade forcada (TEIF) [\%] (\code{$TEIF})
#' \item indisponibilidade programada (IP) [\%] (\code{$IP})
#' \item potencia efetiva da usina [MW] (\code{$potenciaEfetivaUsina})
#' }
#'
#' @examples
#' \dontrun{
#' consolidaHidreletricasnoHorizonte(pastaCaso)
#' }
#'
#' @export
consolidaHidreletricasnoHorizonte <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados das usinas hidreletricas

  lt.dadosUsinasHidro <- leituraDadosUsinasHidro(pastaCaso)
  df.dadosConfiguracao <- lt.dadosUsinasHidro$df.dadosConfiguracao %>% dplyr::select(-nomeUsina)
  df.dadosUsinasHidroeletricas <- lt.dadosUsinasHidro$df.dadosUsinasHidroeletricas %>% dplyr::select(-nomeUsina)

  df.configuracaoHidro <- leituraConfiguracaoHidro(pastaCaso) %>%
    dplyr::select(codUsina, nomeUsina, codREE, idUsinaExistente, idModificacaoUsina)

  lt.alteracaoDadosUsinasHidro <- leituraAlteracaoDadosUsinasHidro(pastaCaso)
  df.alteracaoConjunto <- lt.alteracaoDadosUsinasHidro$df.alteracaoConjunto %>% dplyr::rename(potenciaUnitaria = potenciaEfetiva)
  df.alteracaoHidro <- lt.alteracaoDadosUsinasHidro$df.alteracaoHidro %>% dplyr::select(codUsina, anoMes, canalFuga, volumeMaximo, volumeMinimo, vazaoMinima)

  lt.dadosExpansaoHidro <- leituraDadosExpansaoUsinasHidro(pastaCaso)
  df.dadosExpansaoHidroTempo <- lt.dadosExpansaoHidro$df.dadosExpansaoHidroTempo %>% dplyr::select(-nomeUsina)

  df.dadosGerais <- leituraDadosGerais(pastaCaso)

  # confere se existem declaracoes no arquivo modif em usinas que nao estao com flag ativo no confh

  df.usinasComModif <- dplyr::distinct(rbind(dplyr::select(df.alteracaoConjunto, codUsina), dplyr::select(df.alteracaoHidro, codUsina))) %>%
    dplyr::mutate(modif = TRUE)

  df.verificaModifConfh <- dplyr::left_join(df.configuracaoHidro, df.usinasComModif, by = "codUsina") %>%
    dplyr::mutate(verifica = ifelse(idModificacaoUsina == 0 & modif == TRUE, TRUE, FALSE)) %>%
    dplyr::filter(!is.na(verifica))

  if (any(df.verificaModifConfh$verifica)) {
    usinas <- ""
    df.verificaModifConfh <- df.verificaModifConfh %>% dplyr::filter(verifica == TRUE)
    for (i in 1:nrow(df.verificaModifConfh)) {
      usinas <- paste(usinas, df.verificaModifConfh$codUsina[i], " ", df.verificaModifConfh$nomeUsina[i])
    }
    stop(paste("Existem UHEs representadas no arquivo modif sem o flag ativo no arquivo confh: ", usinas))
  }

  # define o dataframe com as informacoes da potencia efetiva total e numero de conjuntos de cada hidreletrica

  df.potef <- dplyr::inner_join(df.dadosConfiguracao, df.configuracaoHidro, by = c("codUsina")) %>%
    dplyr::filter((!stringr::str_detect(nomeUsina, "FIC ") & !stringr::str_detect(nomeUsina, "FICT"))) %>%
    dplyr::select(codUsina, idUsinaExistente, idModificacaoUsina, conjunto, numeroMaquinas, potenciaUnitaria, quedaEfetiva) %>%
    dplyr::mutate(aux = 1) %>%
    dplyr::inner_join(dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = c("aux"), relationship = "many-to-many") %>%
    dplyr::left_join(df.alteracaoConjunto, by = c("codUsina", "conjunto", "anoMes")) %>%
    dplyr::left_join(df.dadosExpansaoHidroTempo, by = c("codUsina", "conjunto", "anoMes")) %>%
    dplyr::mutate(numeroMaquinasFinal = ifelse((idUsinaExistente == "NC"),
      0,
      ifelse(!is.na(numeroMaquinas.y) & idModificacaoUsina == 1 & is.na(numeroMaquinas),
        numeroMaquinas.y,
        ifelse((idUsinaExistente == "EE" | idUsinaExistente == "NE"),
          ifelse(!is.na(numeroMaquinas),
            numeroMaquinas,
            0
          ),
          numeroMaquinas.x
        )
      )
    )) %>%
    dplyr::mutate(potenciaUnitariaFinal = ifelse((idUsinaExistente == "NC"),
      0,
      ifelse(!is.na(potenciaUnitaria.y) & idModificacaoUsina == 1 & is.na(potenciaUnitaria),
        potenciaUnitaria.y,
        ifelse((idUsinaExistente == "EE" | idUsinaExistente == "NE"),
          ifelse(!is.na(potenciaUnitaria),
            potenciaUnitaria,
            potenciaUnitaria.x
          ),
          potenciaUnitaria.x
        )
      )
    )) %>%
    dplyr::mutate(potenciaUnitariaFinal = ifelse(numeroMaquinasFinal == 0, 0, potenciaUnitariaFinal)) %>%
    dplyr::mutate(potenciaEfetivaConjunto = numeroMaquinasFinal * potenciaUnitariaFinal) %>%
    dplyr::group_by(codUsina, anoMes) %>%
    dplyr::summarise(
      potenciaEfetivaUsina = sum(potenciaEfetivaConjunto),
      numeroConjuntos = dplyr::n_distinct(conjunto)
    )

  # Cria estrutura com a evolucao dos parametros (atributos) das usinas hidreletricas ao longo do horizonte de simulacao

  df.consolidadoUHEnoHorizonte <- dplyr::inner_join(df.dadosUsinasHidroeletricas, df.configuracaoHidro, by = c("codUsina")) %>%
    dplyr::filter((!stringr::str_detect(nomeUsina, "FIC ") & !stringr::str_detect(nomeUsina, "FICT"))) %>%
    dplyr::select(codUsina, nomeUsina, idUsinaExistente, idModificacaoUsina, codREE, canalFugaMedio, volumeMaximo, volumeMinimo, vazaoMinimaHistorico, TEIF, IP) %>%
    dplyr::mutate(aux = 1) %>%
    dplyr::inner_join(dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = c("aux"), relationship = "many-to-many") %>%
    dplyr::left_join(df.alteracaoHidro, by = c("codUsina", "anoMes"), relationship = "many-to-many") %>%
    dplyr::mutate(
      volumeMaximoFinal = ifelse(!is.na(volumeMaximo.y) & idModificacaoUsina == 1, volumeMaximo.y, volumeMaximo.x),
      volumeMinimoFinal = ifelse(!is.na(volumeMinimo.y) & idModificacaoUsina == 1, volumeMinimo.y, volumeMinimo.x),
      vazaoMinimaFinal = ifelse(!is.na(vazaoMinima) & idModificacaoUsina == 1, vazaoMinima, vazaoMinimaHistorico),
      canalFugaFinal = ifelse(!is.na(canalFuga) & idModificacaoUsina == 1, canalFuga, canalFugaMedio),
      TEIF = TEIF / 100, IP = IP / 100
    ) %>%
    dplyr::inner_join(df.potef, by = c("codUsina", "anoMes")) %>%
    dplyr::select(
      codUsina, nomeUsina, anoMes, codREE, canalFugaFinal, volumeMaximoFinal, volumeMinimoFinal,
      vazaoMinimaFinal, numeroConjuntos, TEIF, IP, potenciaEfetivaUsina
    ) %>%
    dplyr::distinct(.keep_all = TRUE)


  return(df.consolidadoUHEnoHorizonte)
}
