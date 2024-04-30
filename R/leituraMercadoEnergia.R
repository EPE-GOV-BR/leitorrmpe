#' Leitor dos dados de mercado de energia
#'
#' Faz a leitura do arquivo do NEWAVE com dados de mercado de energia de um subsistema/submercado (sistema.*).
#' Se houver cargas adicionais (c_adic.*), serao somadas ao subsistema/submercado correspondente.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.mercadoEnergia} data frame com dados de mercado de energia
#' \itemize{
#' \item numero do subsistema/submercado (\code{$codSubsistema})
#' \item ano e mes (\code{$anoMes})
#' \item mercado de Energia do subsistema/submercado (\code{$energiaMercado})
#' }
#'
#' @examples
#' \dontrun{
#' leituraMercadoEnergia("C:/PDE2027_Caso080")
#' }
#'
#' @export
leituraMercadoEnergia <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }

  # encontra o nome do arquivo com dados de mercado (sistema.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivoSistema <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(2) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivoSistema, sep = "/"))) {
    stop(paste0(arquivoSistema, " n\u00E3o encontrado em ", pastaCaso))
  }

  # encontra o nome do arquivo com dados gerais com a indicacao sobre a consideracao da carga adicional de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivoDadosGerais <- leituraArquivos(pastaCaso) %>%
    dplyr::slice(1) %>%
    dplyr::pull(arquivo)

  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivoDadosGerais, sep = "/"))) {
    stop(paste0(arquivoDadosGerais, " n\u00E3o encontrado em ", pastaCaso))
  }

  # caso seja considerada carga adicional no arquivo de dados gerais busca o nome e verifica existencia do arquivo de carga adicional
  flagConsideraCargaAdicional <- leituraDadosGerais(pastaCaso) %>% dplyr::pull(cargaAdicional)

  if (flagConsideraCargaAdicional == 1) {
    # encontra o nome do arquivo com dados de carga adicional (c_adic.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
    arquivoCadic <- leituraArquivos(pastaCaso) %>%
      dplyr::slice(23) %>%
      dplyr::pull(arquivo)

    # verifica existencia do arquivo
    if (!file.exists(paste(pastaCaso, arquivoCadic, sep = "/"))) {
      stop(paste0(arquivoCadic, " n\u00E3o encontrado em ", pastaCaso))
    }
  }


  # le o arquivo sistema como um vetor de caracteres
  dadosMercadoEnergia <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivoSistema, sep = "/")), locale = readr::locale(encoding = "latin1"), skip_empty_rows = T)

  # encontra o inicio da informacao
  inicioMercadoEnergia <- which(stringr::str_detect(dadosMercadoEnergia, "MERCADO DE ENERGIA TOTAL"))
  # encontra o fim da informacao
  fimMercadoEnergia <- which(stringr::str_detect(dadosMercadoEnergia, "GERACAO DE USINAS NAO SIMULADAS|GERACAO DE PEQUENAS USINAS"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dadosMercadoEnergiaTXT <- dadosMercadoEnergia[(inicioMercadoEnergia + 3):(fimMercadoEnergia - 2)]

  # filtra linhas de POS
  filtroPos <- readr::read_fwf(I(dadosMercadoEnergiaTXT),
    col_positions = readr::fwf_positions(1, 4, "classe"),
    col_types = "c", skip_empty_rows = F
  ) %>%
    tidyr::fill(everything()) %>%
    dplyr::pull() %>%
    stringr::str_detect("^[0-9]+")
  dadosMercadoEnergiaTXT <- dadosMercadoEnergiaTXT[filtroPos]

  # filtro codSubsistema
  leituraSubsistema <- leituraDadosSubsistema(pastaCaso)
  leituraSubsistema <- leituraSubsistema$codSubsistema

  filtroSub <- readr::read_fwf(I(dadosMercadoEnergiaTXT),
    col_positions = readr::fwf_positions(1, 4, "classe"),
    col_types = "c", skip_empty_rows = F
  ) %>%
    tidyr::fill(everything()) %>%
    dplyr::pull() %>%
    as.numeric()

  anos <- filtroSub[!filtroSub %in% leituraSubsistema]
  quantosAnos <- anos[anos %in% (anos[duplicated(anos)] %>% unique())] %>%
    unique() %>%
    length()
  codSubsistema <- filtroSub[filtroSub %in% leituraSubsistema]
  filtroSub <- filtroSub %in% leituraSubsistema
  dadosMercadoEnergiaTXT <- dadosMercadoEnergiaTXT[!filtroSub]


  df.mercadoEnergia <- readr::read_fwf(I(dadosMercadoEnergiaTXT),
    col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
      c(1, 6, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96),
      # vetor com as posicoes finais de cada campo
      c(4, 14, 22, 30, 38, 46, 54, 62, 70, 78, 86, 94, 102),
      # nome colunas
      c("ano", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    ),
    col_types = "idddddddddddd"
  )
  # inclusao do codSubsistema
  codSubsistema <- rep(codSubsistema, each = quantosAnos) %>% as.numeric()
  df.mercadoEnergia <- df.mercadoEnergia %>%
    dplyr::mutate(codSubsistema = codSubsistema)

  # tranforma colunas em linhas, exceto as colunas "ano" e "codSubsistema".
  # o objetivo  criar duas colunas com as informacoes do "mes" e "energiaMercado"
  # cria o campo anoMes (AAAAMM)
  # seleciona apenas os campos de interesse
  df.mercadoEnergia <- tidyr::pivot_longer(df.mercadoEnergia, -c("ano", "codSubsistema"),
    names_to = "mes",
    values_to = "energiaMercado"
  ) %>%
    dplyr::mutate(anoMes = as.numeric(ano) * 100 + as.numeric(mes)) %>%
    dplyr::select(codSubsistema, anoMes, energiaMercado) %>%
    dplyr::filter(!is.na(energiaMercado))


  ################ C_ADIC ######################

  # checa se o flag para consideracao de carga adicional esta ativo no arquivo de dados gerais
  if (flagConsideraCargaAdicional == 1) {
    # checa se o arquivo c_adic nao esta vazio
    if (file.info(paste(pastaCaso, arquivoCadic, sep = "/"))$size != 0) {
      # le o arquivo c_adic como um vetor de caracteres
      dadosCAdic <- readr::read_lines(paste(pastaCaso, arquivoCadic, sep = "/"), locale = readr::locale(encoding = "latin1"))
      # encontra o fim da informacao
      fimCAdic <- which(stringr::str_detect(dadosCAdic, "999"))
      # filtra somente a parte do vetor que tem os dados de interesse
      dadosCAdicTXT <- dadosCAdic[(3):(fimCAdic - 2)]

      # filtra linhas de POS
      filtro <- readr::read_fwf(I(dadosCAdicTXT),
        col_positions = readr::fwf_positions(1, 4, "classe"),
        col_types = "c", skip_empty_rows = F
      ) %>%
        tidyr::fill(everything()) %>%
        dplyr::pull() %>%
        stringr::str_detect("^[0-9]+")
      dadosCAdicTXT <- dadosCAdicTXT[filtro]

      # lendo codSubsistema
      filtroSub <- readr::read_fwf(I(dadosCAdicTXT),
        col_positions = readr::fwf_positions(1, 4, "classe"),
        col_types = "c", skip_empty_rows = F
      ) %>%
        tidyr::fill(everything()) %>%
        dplyr::pull() %>%
        as.numeric()

      anos <- filtroSub[!filtroSub %in% leituraSubsistema]
      quantosAnos <- anos[anos %in% (anos[duplicated(anos)] %>% unique())] %>%
        unique() %>%
        length()
      codSubsistema <- filtroSub[filtroSub %in% leituraSubsistema]
      filtroSub <- filtroSub %in% leituraSubsistema
      dadosCAdicTXT <- dadosCAdicTXT[!filtroSub]

      # dataframe cAdic
      df.cAdic <- readr::read_fwf(I(dadosCAdicTXT),
        col_positions = readr::fwf_positions( # vetor com as posicoes iniciais de cada campo
          c(1, 6, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96),
          # vetor com as posicoes finais de cada campo
          c(4, 14, 22, 30, 38, 46, 54, 62, 70, 78, 86, 94, 102),
          # nome colunas
          c("ano", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
        ),
        col_types = "idddddddddddd"
      )

      # inclusao do codSubsistema
      codSubsistema <- rep(codSubsistema, each = quantosAnos) %>% as.numeric()
      df.cAdic <- df.cAdic %>%
        dplyr::mutate(codSubsistema = codSubsistema)

      # tranforma colunas em linhas, exceto as colunas "ano" e "codSubsistema".
      # o objetivo  criar duas colunas com as informacoes do "mes" e "cAdic"
      # cria o campo anoMes (AAAAMM)
      # seleciona apenas os campos de interesse
      df.cAdic <- tidyr::pivot_longer(df.cAdic, -c("ano", "codSubsistema"),
        names_to = "mes",
        values_to = "energiaMercado"
      ) %>%
        dplyr::mutate(anoMes = as.numeric(ano) * 100 + as.numeric(mes)) %>%
        dplyr::select(codSubsistema, anoMes, energiaMercado) %>%
        dplyr::filter(!is.na(energiaMercado))

      # junta o mercado e a carga adicional
      df.mercadoEnergia <- rbind(df.mercadoEnergia, df.cAdic) %>%
        dplyr::group_by(codSubsistema, anoMes) %>%
        dplyr::summarise(energiaMercado = sum(energiaMercado)) %>%
        dplyr::ungroup()
    }
  }
  return(df.mercadoEnergia)
}
