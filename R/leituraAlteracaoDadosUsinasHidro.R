#' Leitor de dados de alteracao de configuracao hidroeletrica
#'
#' Faz a leitura do arquivo do NEWAVE com dados de alteracao de configuracao hidroeletrica (modif.*). Usa as funcoes internas \code{\link{leituraArquivos}},
#' \code{\link{definePeriodo}}, \code{\link{leituraDadosUsinasHidro}}.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#' Retorna as ateracoes no tempo (por todos os meses e anos do horizonte mesmo que o valor seja o mesmo) e sem os periodos de pre e pos
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{lt.alteracaoDadosUsinasHidro} lista com data frames com dados alteracao de configuracao hidroeletrica
#' \itemize{
#' \item \code{df.alteracaoHidro} data frame com dados de alteracao de configuracao hidroeletrica
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item anoMes no formato aaaamm (\code{$anoMes})
#' \item canal de fuga [m] (\code{$canalFuga})
#' \item nivel de montante [m] (\code{$nivelMontante})
#' \item vazao minima [m3/s] (\code{$vazaoMinima})
#' \item vazao maxima [m3/s] (\code{$vazaoMaxima})
#' \item volume minimo operativo no tempo [hm3] (\code{$volumeMinimo}) - como e no tempo este campo representa tanto VOLMIN e VMINT
#' \item volume minimo operativo no tempo [p.u.] do volume util (\code{$volumeMinimoPU}) - como e no tempo este campo representa tanto VOLMIN e VMINT
#' \item volume maximo operativo no tempo [hm3] (\code{$volumeMaximo}) - como e no tempo este campo representa tanto VOLMAX e VMAXT
#' \item volume maximo operativo no tempo [p.u.] do volume util (\code{$volumeMaximoPU}) - como e no tempo este campo representa tanto VOLMAX e VMAXT
#' \item numero de unidades de base (\code{$numeroUnidadeBase})
#' \item total de conjuntos de maquinas (considerados apenas os n primeiros conjuntos, de acordo com a ordem destes conjuntos no cadastro)
#' (\code{$numeroConjuntos})
#' \item produtibilidade especifica [MW/m3/s/m] (\code{$produtibilidade})
#' \item taxa esperada de indisponibilidade forcada (TEIF) [\%] (\code{$TEIF})
#' \item indisponibilidade programada (IP) [\%] (\code{$IP})
#' \item perda hidraulica [unidade do cadastro] (\code{$perda})
#' }
#' \item \code{df.alteracaoConjunto} data frame com dados de alteracao de configuracao por conjunto de maquinas
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item anoMes no formato aaaamm (\code{$anoMes})
#' \item conjunto de maquinas (\code{$conjunto})
#' \item numero de maquinas (\code{$numeroMaquinas})
#' \item valor da potencia efetiva [MW] (\code{$potenciaEfetiva})
#' }}
#'
#' @examples
#' \dontrun{
#' leituraAlteracaoDadosUsinasHidro("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraAlteracaoDadosUsinasHidro <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(4) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }

  df.alteracaoHidro <- readr::read_fwf(paste(pastaCaso, arquivo, sep = "/"),
    readr::fwf_widths(c(9, NA), col_names = c("chave", "valores")),
    col_types = readr::cols(chave = readr::col_character(), valores = readr::col_character()),
    skip = 2
  )
  df.alteracaoHidro <- df.alteracaoHidro %>% dplyr::mutate(
    chave = toupper(chave), # garante compatibilidade das chaves
    valores = ifelse(chave == "USINA", stringr::str_sub(valores, 1, 8), valores),
    codUsina = ifelse(chave == "USINA", valores, NA)
  )
  df.alteracaoHidro <- df.alteracaoHidro %>% tidyr::fill(codUsina) # preenche campos vazios com o valor anterior (da usina)
  df.alteracaoHidro <- dplyr::filter(df.alteracaoHidro, chave != "USINA") # retira linhas com os codigos das usinas como chave
  df.alteracaoHidro <- df.alteracaoHidro %>% dplyr::mutate(codUsina = as.integer(codUsina))

  df.dadosHidro <- leituraDadosUsinasHidro(pastaCaso) %>%
    magrittr::use_series(df.dadosUsinasHidroeletricas) %>%
    dplyr::mutate(volumeMinDados = volumeMinimo, volumeMaxDados = volumeMaximo) %>%
    dplyr::select(codUsina, volumeMinDados, volumeMaxDados)

  # volume maximo e minimo operativos
  # estes dados serao usados como volumes maximo e minimo ate que apareca informacao em algum ponto no horizonte as atualizando (VMAXT e VMINT)
  df.volume <- df.alteracaoHidro %>% dplyr::filter((chave == "VOLMAX") | (chave == "VOLMIN"))
  df.volume <- tidyr::separate(df.volume, valores, into = c("valor", "subvalor"), sep = ("\\s+"), fill = "right") %>%
    dplyr::mutate(
      dataFato = "",
      chave = ifelse((chave == "VOLMAX" & stringr::str_detect(subvalor, "h")), "volumeMaximo", ifelse(chave == "VOLMAX", "volumeMaximoPU", chave)),
      chave = ifelse((chave == "VOLMIN" & stringr::str_detect(subvalor, "h")), "volumeMinimo", ifelse(chave == "VOLMIN", "volumeMinimoPU", chave)),
      valor = as.double(valor)
    ) %>%
    dplyr::select(codUsina, dataFato, chave, valor) %>%
    tidyr::pivot_wider(names_from = chave, values_from = valor) # faz pivot para criar campos existentes em outas unidades

  if (!"volumeMaximo" %in% names(df.volume)) {
    df.volume <- df.volume %>% dplyr::mutate(volumeMaximo = NA)
  }
  if (!"volumeMinimo" %in% names(df.volume)) {
    df.volume <- df.volume %>% dplyr::mutate(volumeMinimo = NA)
  }
  if (!"volumeMaximoPU" %in% names(df.volume)) {
    df.volume <- df.volume %>% dplyr::mutate(volumeMaximoPU = NA)
  }
  if (!"volumeMinimoPU" %in% names(df.volume)) {
    df.volume <- df.volume %>% dplyr::mutate(volumeMinimoPU = NA)
  }

  # calcula campos a partir dos que existem
  df.volume <- dplyr::left_join(df.volume, df.dadosHidro, by = "codUsina") %>%
    dplyr::mutate(
      volumeMaximo = ifelse(!is.na(volumeMaximo), volumeMaximo, volumeMinDados + (volumeMaxDados - volumeMinDados) * volumeMaximoPU / 100),
      volumeMinimo = ifelse(!is.na(volumeMinimo), volumeMinimo, volumeMinDados + (volumeMaxDados - volumeMinDados) * volumeMinimoPU / 100),
      volumeMaximoPU = ifelse(!is.na(volumeMaximoPU), volumeMaximoPU, (volumeMaximo - volumeMinDados) * 100 / (volumeMaxDados - volumeMinDados)),
      volumeMinimoPU = ifelse(!is.na(volumeMinimoPU), volumeMinimoPU, (volumeMinimo - volumeMinDados) * 100 / (volumeMaxDados - volumeMinDados))
    ) %>%
    dplyr::select(-volumeMinDados, -volumeMaxDados)

  df.dadosHidro <- dplyr::left_join(df.dadosHidro, dplyr::select(df.volume, -volumeMaximoPU, -volumeMinimoPU, -dataFato), by = "codUsina") %>%
    dplyr::mutate(
      volumeMaxDados = ifelse(!is.na(volumeMaximo), volumeMaximo, volumeMaxDados),
      volumeMinDados = ifelse(!is.na(volumeMinimo), volumeMinimo, volumeMinDados)
    ) %>%
    dplyr::select(-volumeMinimo, -volumeMaximo)

  df.volume <- df.volume %>%
    tidyr::pivot_longer(cols = c(-codUsina, -dataFato), names_to = "chave", values_to = "valor") %>%
    dplyr::filter(!is.na(valor))

  # volume maximo e minimo (com data)
  df.volumeTempo <- dplyr::filter(df.alteracaoHidro, ((chave == "VMINT") | (chave == "VMAXT")))
  df.volumeTempo <- tidyr::separate(df.volumeTempo, valores, into = c("mes", "ano", "valor", "subvalor"), sep = ("\\s+"), fill = "right") %>%
    dplyr::mutate(
      ano = stringr::str_replace(ano, pattern = "[a-z|A-Z]+", ""), # para evitar warning de NA da transformacao para numerico
      ano = as.numeric(ano),
      chave = ifelse((chave == "VMAXT" & stringr::str_detect(subvalor, "h")), "volumeMaximo", ifelse(chave == "VMAXT", "volumeMaximoPU", chave)),
      chave = ifelse((chave == "VMINT" & stringr::str_detect(subvalor, "h")), "volumeMinimo", ifelse(chave == "VMINT", "volumeMinimoPU", chave)),
      valor = as.double(valor)
    ) %>%
    dplyr::filter(!is.na(ano)) %>% # remove anos de pre e pos
    dplyr::mutate(dataFato = ano * 100 + as.numeric(mes)) %>%
    dplyr::select(codUsina, dataFato, chave, valor) %>%
    tidyr::pivot_wider(names_from = chave, values_from = valor) # faz pivot para criar campos existentes em outas unidades

  if (!"volumeMaximo" %in% names(df.volumeTempo)) {
    df.volumeTempo <- df.volumeTempo %>% dplyr::mutate(volumeMaximo = NA)
  }
  if (!"volumeMinimo" %in% names(df.volumeTempo)) {
    df.volumeTempo <- df.volumeTempo %>% dplyr::mutate(volumeMinimo = NA)
  }
  if (!"volumeMaximoPU" %in% names(df.volumeTempo)) {
    df.volumeTempo <- df.volumeTempo %>% dplyr::mutate(volumeMaximoPU = NA)
  }
  if (!"volumeMinimoPU" %in% names(df.volumeTempo)) {
    df.volumeTempo <- df.volumeTempo %>% dplyr::mutate(volumeMinimoPU = NA)
  }

  # calcula campos a partir dos que existem
  df.volumeTempo <- dplyr::left_join(df.volumeTempo, df.dadosHidro, by = "codUsina") %>%
    dplyr::mutate(
      volumeMaximo = ifelse(!is.na(volumeMaximo), volumeMaximo, volumeMinDados + (volumeMaxDados - volumeMinDados) * volumeMaximoPU / 100),
      volumeMinimo = ifelse(!is.na(volumeMinimo), volumeMinimo, volumeMinDados + (volumeMaxDados - volumeMinDados) * volumeMinimoPU / 100),
      volumeMaximoPU = ifelse(!is.na(volumeMaximoPU), volumeMaximoPU, (volumeMaximo - volumeMinDados) * 100 / (volumeMaxDados - volumeMinDados)),
      volumeMinimoPU = ifelse(!is.na(volumeMinimoPU), volumeMinimoPU, (volumeMinimo - volumeMinDados) * 100 / (volumeMaxDados - volumeMinDados))
    ) %>%
    dplyr::select(-volumeMinDados, -volumeMaxDados) %>%
    tidyr::pivot_longer(cols = c(-codUsina, -dataFato), names_to = "chave", values_to = "valor") %>%
    dplyr::filter(!is.na(valor))

  # dados com estrutura de valor e tempo (mes e ano)
  # vazao maxima e minima, nivel de montante (com data) e canal de fuga
  df.dadosTempo <- dplyr::filter(df.alteracaoHidro, ((chave == "CFUGA") | (chave == "CMONT") | (chave == "VAZMINT") | (chave == "VAZMAXT")))
  df.dadosTempo <- tidyr::separate(df.dadosTempo, valores, into = c("mes", "ano", "valor"), sep = ("\\s+"), fill = "right") %>%
    dplyr::mutate(
      ano = stringr::str_replace(ano, pattern = "[a-z|A-Z]+", ""), # para evitar warning de NA da transformacao para numerico
      ano = as.numeric(ano),
      chave = stringr::str_replace(chave, "CFUGA", "canalFuga"),
      chave = stringr::str_replace(chave, "CMONT", "nivelMontante"),
      chave = stringr::str_replace(chave, "VAZMINT", "vazaoMinima"),
      chave = stringr::str_replace(chave, "VAZMAXT", "vazaoMaxima"),
      valor = as.double(valor)
    ) %>%
    dplyr::filter(!is.na(ano)) %>% # remove anos de pre e pos
    dplyr::mutate(dataFato = ano * 100 + as.numeric(mes)) %>%
    dplyr::select(codUsina, dataFato, chave, valor)

  # dados com estrutura de valor
  # numero de unidades de base, total de conjuntos de maquinas, vazao maxima e minima
  df.dadosValorUnico <- dplyr::filter(df.alteracaoHidro, ((chave == "NUMBAS") | (chave == "NUMCNJ") | (chave == "VAZMIN") | (chave == "VAZMAX") |
    (chave == "PRODESP") | (chave == "TEIF") | (chave == "IP") | (chave == "PERDHIDR"))) %>%
    dplyr::mutate(
      dataFato = "",
      chave = stringr::str_replace(chave, "NUMBAS", "numeroUnidadeBase"),
      chave = stringr::str_replace(chave, "NUMCNJ", "numeroConjuntos"),
      chave = stringr::str_replace(chave, "VAZMIN", "vazaoMinima"),
      chave = stringr::str_replace(chave, "VAZMAX", "vazaoMaxima"),
      chave = stringr::str_replace(chave, "PRODESP", "produtibilidade"),
      chave = stringr::str_replace(chave, "TEIF", "TEIF"),
      chave = stringr::str_replace(chave, "IP", "IP"),
      chave = stringr::str_replace(chave, "PERDHIDR", "perda"),
      valor = as.double(valores)
    ) %>%
    dplyr::select(codUsina, dataFato, chave, valor)

  # dados com estrutura de valor e por conjunto
  # numero de maquinas e valor da potencia efetiva de um determinado conjunto
  df.alteracaoConjunto <- dplyr::filter(df.alteracaoHidro, ((chave == "NUMMAQ") | (chave == "POTEFE")))
  # pode ocorrer de nao haver este tipo de registro no arquivo modif
  if (length(df.alteracaoConjunto$chave != 0)) {
    df.alteracaoConjunto <- tidyr::separate(df.alteracaoConjunto, valores, into = c("valor", "conjunto"), sep = ("\\s+"), fill = "right") %>%
      dplyr::mutate(
        chave = stringr::str_replace(chave, "NUMMAQ", "numeroMaquinas"),
        chave = stringr::str_replace(chave, "POTEFE", "potenciaEfetiva"),
        valor = as.double(valor),
        conjunto = as.integer(conjunto)
      ) %>%
      tidyr::pivot_wider(names_from = chave, values_from = valor)
    df.alteracaoConjunto <- dplyr::inner_join(dplyr::mutate(df.alteracaoConjunto, aux = 1), dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = c("aux"), relationship = "many-to-many") %>%
      dplyr::select(codUsina, anoMes, conjunto, everything(), -ano, -mes, -aux)
    # se nao houver a coluna potenciaEfetiva, adiciona com NA
    if (!"potenciaEfetiva" %in% colnames(df.alteracaoConjunto)) {
      df.alteracaoConjunto <- df.alteracaoConjunto %>% dplyr::mutate(potenciaEfetiva = NA)
    }
    # se nao houver a coluna potenciaEfetiva, adiciona com NA
    if (!"numeroMaquinas" %in% colnames(df.alteracaoConjunto)) {
      df.alteracaoConjunto <- df.alteracaoConjunto %>% dplyr::mutate(numeroMaquinas = NA)
    }
  } else {
    df.alteracaoConjunto <- data.frame(codUsina = 999, anoMes = 999999, conjunto = 99, numeroMaquinas = NA, potenciaEfetiva = NA)
  }

  # junta os data frames tratados separadamente
  df.alteracaoHidro <- rbind(df.volume, df.volumeTempo, df.dadosTempo, df.dadosValorUnico)

  # coloca dados ao longo do tempo restringindo a existencia de dados anteriores as datas de modificacao
  df.alteracaoHidro <- dplyr::inner_join(dplyr::mutate(df.alteracaoHidro, aux = 1), dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = c("aux"), relationship = "many-to-many") %>%
    dplyr::select(-aux, -ano, -mes) %>%
    dplyr::mutate(vale = ifelse(((anoMes >= dataFato) | is.na(dataFato)), "s", "n")) %>%
    dplyr::filter(vale != "n") %>%
    dplyr::select(-vale)

  # elimina os dados duplicados das modificacoes, ficando somente a correta modificao ao longo do tempo
  df.alteracaoHidro <- df.alteracaoHidro %>%
    dplyr::arrange(desc(dataFato)) %>%
    dplyr::distinct(codUsina, chave, anoMes, .keep_all = TRUE) %>%
    dplyr::select(-c("dataFato")) %>%
    tidyr::pivot_wider(names_from = chave, values_from = valor) %>%
    dplyr::arrange(codUsina, anoMes)
  
  # adiciona as colunas TEIF e IP caso nao existam
  if (!"TEIF" %in% names(df.alteracaoHidro)) {
    df.alteracaoHidro <- df.alteracaoHidro %>% dplyr::mutate(TEIF = NA)
  }
  if (!"IP" %in% names(df.alteracaoHidro)) {
    df.alteracaoHidro <- df.alteracaoHidro %>% dplyr::mutate(IP = NA)
  }

  lt.alteracaoDadosUsinasHidro <- list(df.alteracaoHidro = df.alteracaoHidro, df.alteracaoConjunto = df.alteracaoConjunto)

  return(lt.alteracaoDadosUsinasHidro)
}
