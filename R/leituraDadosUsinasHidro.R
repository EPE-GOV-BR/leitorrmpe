#' Leitor de dados das usinas hidroeletricas
#'
#' Faz a leitura do arquivo do NEWAVE com dados das usinas hidroeletricas (hidr.dat).
#' O arquivo de dados das usinas hidroelrtricas corresponde ao arquivo de cadastro com os dados das usinas hidroeletricas.
#' Arquivo de acesso direto, nao formatado, com 320/600 registros, cada registro correspondendo a uma usina.
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{lt.dadosUsinasHidroeletricas} lista com data frames com dados das usinas hidroeletricas
#' \itemize{
#' \item \code{df.dadosUsinasHidroeletricas} data frame com dados gerais das usinas hidroeletricas
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do posto de vazoes da usina (\code{$posto})
#' \item numero do posto BDH (\code{$postoBDH})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item codigo da empresa da usina (\code{$codEmpresa})
#' \item codigo da usina jusante no cadastro de usinas hidroeletricas (\code{$codUsinaJusante})
#' \item codigo da usina de desvio no cadastro de usinas hidroeletricas (\code{$codUsinaDesvio})
#' \item volume Minimo [hm3] (\code{$volumeMinimo})
#' \item volume Maximo [hm3] (\code{$volumeMaximo})
#' \item volume Vertedouro [hm3] (\code{$volumeVertedouro})
#' \item volume Desvio [hm3] (\code{$volumeDesvio})
#' \item volume Referencia [hm3] (\code{$volumeReferencia})
#' \item cota Minima [m] (\code{$cotaMinima})
#' \item cota Maxima [m] (\code{$cotaMaxima})
#' \item coeficiente A0 do polinomio cota volume (\code{$poliCotaVolumeA0})
#' \item coeficiente A1 do polinomio cota volume (\code{$poliCotaVolumeA1})
#' \item coeficiente A2 do polinomio cota volume (\code{$poliCotaVolumeA2})
#' \item coeficiente A3 do polinomio cota volume (\code{$poliCotaVolumeA3})
#' \item coeficiente A4 do polinomio cota volume (\code{$poliCotaVolumeA4})
#' \item coeficiente A0 do polinomio area cota (\code{$poliAreaCotaA0})
#' \item coeficiente A1 do polinomio area cota (\code{$poliAreaCotaA1})
#' \item coeficiente A2 do polinomio area cota (\code{$poliAreaCotaA2})
#' \item coeficiente A3 do polinomio area cota (\code{$poliAreaCotaA3})
#' \item coeficiente A4 do polinomio area cota (\code{$poliAreaCotaA4})
#' \item numero de conjuntos de maquinas (\code{$numeroConjuntos})
#' \item produtibilidade especifica [MW/m3/s/m] (\code{$produtibilidade})
#' \item perdas (\code{$perda})
#' \item numero de polinomios de vazao de jusante (\code{$numPoliVazaoNivelJusante})
#' \item canal de fuga medio [m] (\code{$canalFugaMedio})
#' \item vazao minima do historico [m3/s] (\code{$vazaoMinimaHistorico})
#' \item numero de unidades base (\code{$numUnidadesBase})
#' \item tipo de turbina - 0:Nao Ha; 1:Francis; 2:Kaplan/Propeller; 3:Pelton (\code{$tipoTurbina})
#' \item representacao do conjunto (\code{$representacaoConjunto})
#' \item taxa esperada de indisponibilidade forcada (TEIF) [\%] (\code{$TEIF})
#' \item indisponibilidade programada (IP) [\%] (\code{$IP})
#' \item tipo de perda (\code{$tipoPerda})
#' \item data [dd/mm/aa] (\code{$data})
#' \item observacao (\code{$observacao})
#' \item regulacao - D:diaria; M:mensal (\code{$regulacao})
#' }
#' \item \code{df.evaporacaoMensal} data frame com dados de evaporacao mensal por usina
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item mes (\code{$mes})
#' \item evaporacao mensal [mm/mes] (\code{$evaporacao})
#' }
#' \item \code{df.dadosConfiguracao} data frame com dados de configuracao por usina
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item conjunto de maquinas (\code{$conjunto})
#' \item numero de maquinas (\code{$numeroMaquinas})
#' \item potencia unitaria por conjunto [MW] (\code{$potenciaUnitaria})
#' \item queda efetiva por conjunto [m] (\code{$quedaEfetiva})
#' \item vazao efetiva por conjunto [m3/s] (\code{$vazaoEfetiva})
#' }
#' \item \code{df.polinomiosVazaoNivelJusante} data frame com dados dos polinomios de vazao de nivel de jusante por usina
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
#' \item numero do polinomio de vazao de nivel de jusante. valor entre 1 e 6 (\code{$polinomio})
#' \item coeficiente A0 do polinomio de vazao de nivel de jusante (\code{$coeficienteA0})
#' \item coeficiente A1 do polinomio de vazao de nivel de jusante (\code{$coeficienteA1})
#' \item coeficiente A2 do polinomio de vazao de nivel de jusante (\code{$coeficienteA2})
#' \item coeficiente A3 do polinomio de vazao de nivel de jusante (\code{$coeficienteA3})
#' \item coeficiente A4 do polinomio de vazao de nivel de jusante (\code{$coeficienteA4})
#' \item altura de referencia (\code{$alturaReferencia})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosUsinasHidro("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraDadosUsinasHidro <- function(pastaCaso) {
  # verifica existencia do arquivo hidr.dat
  if (!file.exists(paste(pastaCaso, "hidr.dat", sep = "/"))) {
    stop(paste0("hidr.dat n\u00E3o encontrado em ", pastaCaso))
  }

  # o arquivo hidr possui usinas do codigo 1 ate o maior cadastrado. O arquivo nao possui buracos.
  # logo quando nao existir um determinado codigo, existira uma linha com seus dados mas com valores zerados.
  maiorCodUsina <- leituraConfiguracaoHidro(pastaCaso) %>%
    dplyr::pull(codUsina) %>%
    max()

  # abre conexao com o arquivo binario.
  conexaoArquivoBinario <- file(paste(pastaCaso, "hidr.dat", sep = "/"), "rb")

  # data frames de saida
  df.dadosUsinasHidroeletricas <- tidyr::tibble()
  df.evaporacaoMensal <- tidyr::tibble()
  df.dadosConfiguracao <- tidyr::tibble()
  df.polinomiosVazaoNivelJusante <- tidyr::tibble()

  for (andaUsina in 1:maiorCodUsina) {
    # a cada leitura feita no arquivo binario, o ponteiro com a posicao sera atualizado para posicao logo apos o fim dos dados lidos
    nomeUsina <- readBin(conexaoArquivoBinario, raw(), 12) %>% rawToChar()
    posto <- readBin(conexaoArquivoBinario, integer())
    postoBDH <- readBin(conexaoArquivoBinario, raw(), 8) %>% rawToChar()
    codSubsistema <- readBin(conexaoArquivoBinario, integer())
    codEmpresa <- readBin(conexaoArquivoBinario, integer())
    codUsinaJusante <- readBin(conexaoArquivoBinario, integer())
    codUsinaDesvio <- readBin(conexaoArquivoBinario, integer())
    volumeMinimo <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    volumeMaximo <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    volumeVertedouro <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    volumeDesvio <- readBin(conexaoArquivoBinario, numeric(), size = 4)
    cotaMinima <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    cotaMaxima <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    poliCotaVolume <- readBin(conexaoArquivoBinario, numeric(), 5, size = 4) %>% signif(6)
    poliAreaCota <- readBin(conexaoArquivoBinario, numeric(), 5, size = 4) %>% signif(6)
    evaporacaoMensal <- readBin(conexaoArquivoBinario, integer(), 12)
    numeroConjuntos <- readBin(conexaoArquivoBinario, integer())
    numeroMaquinas <- readBin(conexaoArquivoBinario, integer(), 5)
    potenciaUnitaria <- readBin(conexaoArquivoBinario, numeric(), 5, size = 4) %>% round(3)
    qht <- readBin(conexaoArquivoBinario, numeric(), 25, size = 4)
    qhg <- readBin(conexaoArquivoBinario, numeric(), 25, size = 4)
    PHmaq <- readBin(conexaoArquivoBinario, numeric(), 25, size = 4)
    quedaEfetiva <- readBin(conexaoArquivoBinario, numeric(), 5, size = 4) %>% round(2)
    vazaoEfetiva <- readBin(conexaoArquivoBinario, integer(), 5)
    produtibilidade <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(6)
    perda <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    numPoliVazaoNivelJusante <- readBin(conexaoArquivoBinario, integer())
    poliVazaoNivelJusante <- readBin(conexaoArquivoBinario, numeric(), 30, size = 4) %>% signif(6)
    naref <- readBin(conexaoArquivoBinario, numeric(), 6, size = 4) %>% round(2)
    canalFugaMedio <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    influenciaVertimentoCanalFuga <- readBin(conexaoArquivoBinario, integer())
    fatorCargaMaximo <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    fatorCargaMinimo <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    vazaoMinimaHistorico <- readBin(conexaoArquivoBinario, integer())
    numUnidadesBase <- readBin(conexaoArquivoBinario, integer())
    tipoTurbina <- readBin(conexaoArquivoBinario, integer())
    representacaoConjunto <- readBin(conexaoArquivoBinario, integer())
    TEIF <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(3)
    IP <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(3)
    tipoPerda <- readBin(conexaoArquivoBinario, integer())
    data <- readBin(conexaoArquivoBinario, raw(), 8) %>% rawToChar()
    observacao <- readBin(conexaoArquivoBinario, raw(), 43) %>% rawToChar()
    volumeReferencia <- readBin(conexaoArquivoBinario, numeric(), size = 4) %>% round(2)
    regulacao <- readBin(conexaoArquivoBinario, raw(), 1) %>% rawToChar()

    # data frame auxiliar com dados de evaporacao mensal
    df.evaporacaoMensalAux <- data.frame(
      codUsina = andaUsina,
      nomeUsina = stringr::str_trim(nomeUsina),
      codSubsistema = codSubsistema,
      mes = 1:12,
      evaporacao = evaporacaoMensal,
      stringsAsFactors = F
    )

    # data frame auxiliar com dados de configuracao das usinas
    df.dadosConfiguracaoAux <- data.frame(
      codUsina = andaUsina,
      nomeUsina = stringr::str_trim(nomeUsina),
      codSubsistema = codSubsistema,
      numeroConjuntos = numeroConjuntos,
      conjunto = 1:5,
      numeroMaquinas = numeroMaquinas,
      potenciaUnitaria = potenciaUnitaria,
      quedaEfetiva = quedaEfetiva,
      vazaoEfetiva = vazaoEfetiva,
      stringsAsFactors = F
    )

    # data frame auxiliar com dados dos polinomios de vazao de nivel jusante
    df.polinomiosVazaoNivelJusanteAux <- data.frame(
      codUsina = andaUsina,
      nomeUsina = stringr::str_trim(nomeUsina),
      codSubsistema = codSubsistema,
      numPoliVazaoNivelJusante = numPoliVazaoNivelJusante,
      polinomio = 1:6,
      coeficienteA0 = poliVazaoNivelJusante[seq(1, 30, 5)],
      coeficienteA1 = poliVazaoNivelJusante[seq(2, 30, 5)],
      coeficienteA2 = poliVazaoNivelJusante[seq(3, 30, 5)],
      coeficienteA3 = poliVazaoNivelJusante[seq(4, 30, 5)],
      coeficienteA4 = poliVazaoNivelJusante[seq(5, 30, 5)],
      alturaReferencia = naref,
      stringsAsFactors = F
    )

    # data frame auxiliar com dados gerais das usinas hidroeletricas
    df.dadosUsinasHidroeletricasAux <- data.frame(
      codUsina = andaUsina,
      nomeUsina = stringr::str_trim(nomeUsina),
      posto = posto,
      postoBDH = postoBDH,
      codSubsistema = codSubsistema,
      codEmpresa = codEmpresa,
      codUsinaJusante = codUsinaJusante,
      codUsinaDesvio = codUsinaDesvio,
      volumeMinimo = volumeMinimo,
      volumeMaximo = volumeMaximo,
      volumeVertedouro = volumeVertedouro,
      volumeDesvio = volumeDesvio,
      volumeReferencia = volumeReferencia,
      cotaMinima = cotaMinima,
      cotaMaxima = cotaMaxima,
      poliCotaVolumeA0 = poliCotaVolume[1],
      poliCotaVolumeA1 = poliCotaVolume[2],
      poliCotaVolumeA2 = poliCotaVolume[3],
      poliCotaVolumeA3 = poliCotaVolume[4],
      poliCotaVolumeA4 = poliCotaVolume[5],
      poliAreaCotaA0 = poliAreaCota[1],
      poliAreaCotaA1 = poliAreaCota[2],
      poliAreaCotaA2 = poliAreaCota[3],
      poliAreaCotaA3 = poliAreaCota[4],
      poliAreaCotaA4 = poliAreaCota[5],
      numeroConjuntos = numeroConjuntos,
      produtibilidade = produtibilidade,
      perda = perda,
      numPoliVazaoNivelJusante = numPoliVazaoNivelJusante,
      canalFugaMedio = canalFugaMedio,
      vazaoMinimaHistorico = vazaoMinimaHistorico,
      numUnidadesBase = numUnidadesBase,
      tipoTurbina = tipoTurbina,
      representacaoConjunto = representacaoConjunto,
      TEIF = TEIF,
      IP = IP,
      tipoPerda = tipoPerda,
      data = data,
      observacao = observacao,
      regulacao = regulacao,
      stringsAsFactors = F
    )

    # junta data frames
    df.dadosUsinasHidroeletricas <- rbind(df.dadosUsinasHidroeletricas, df.dadosUsinasHidroeletricasAux)
    df.evaporacaoMensal <- rbind(df.evaporacaoMensal, df.evaporacaoMensalAux)
    df.dadosConfiguracao <- rbind(df.dadosConfiguracao, df.dadosConfiguracaoAux)
    df.polinomiosVazaoNivelJusante <- rbind(df.polinomiosVazaoNivelJusante, df.polinomiosVazaoNivelJusanteAux)
  }

  # fecha conexao com arquivo binario
  close(conexaoArquivoBinario)

  # limpa cadastros inexistentes
  df.dadosUsinasHidroeletricas <- dplyr::filter(df.dadosUsinasHidroeletricas, nomeUsina != "")
  df.evaporacaoMensal <- dplyr::filter(df.evaporacaoMensal, nomeUsina != "")
  df.dadosConfiguracao <- dplyr::filter(df.dadosConfiguracao, nomeUsina != "", conjunto <= numeroConjuntos) %>%
    dplyr::select(-numeroConjuntos)
  df.polinomiosVazaoNivelJusante <- dplyr::filter(df.polinomiosVazaoNivelJusante, nomeUsina != "", polinomio <= numPoliVazaoNivelJusante) %>%
    dplyr::select(-numPoliVazaoNivelJusante)

  # cria lista com todos os data frames criados
  lt.dadosUsinasHidroeletricas <- list(
    df.dadosUsinasHidroeletricas = df.dadosUsinasHidroeletricas,
    df.evaporacaoMensal = df.evaporacaoMensal,
    df.dadosConfiguracao = df.dadosConfiguracao,
    df.polinomiosVazaoNivelJusante = df.polinomiosVazaoNivelJusante
  )

  return(lt.dadosUsinasHidroeletricas)
}
