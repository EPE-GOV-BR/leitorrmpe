#' Leitor NWLISTCF Cortes
#'
#' Faz a leitura do arquivo de saida do NEWAVE nwlistcf que gera um relatorio com dados da funcao de custo futuro.
#' Usa como referencia para a descricao e saida do arquivo as definicoes do Manual do Usuario do
#' Modelo de Planejamento da Operacao de Sistemas Hidrotermicos Interligados de Longo e Medio Prazos do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 158.
#'
#'
#' @param arquivo caracter indicando o nome do arquivo do NWLISTCF incluindo o caminho. Ex: "C:/nwlistcf.rel"
#'
#' @return \code{df.nwlistcfCortes} data frame com saida do arquivo NWLISTCF
#'
#' @examples
#' \dontrun{
#' leituraNwlistcfCortes("C:/PDE2027_Caso080/nwlistcf.rel")
#' }
#'
leituraNwlistcfCortes <- function(arquivo) {
  if (missing(arquivo)) {
    stop("favor indicar o arquivo do NWLISTCF")
  }


  # verifica a existencia do arquivo
  if (!file.exists(arquivo)) {
    stop(paste0("N\u00E3o foi encontrado o arquivo ", arquivo))
  }

  # cria data frame de base
  df.nwlistcfCortes <- tidyr::tibble()

  # leitura bruta do arquivo
  dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(arquivo, sep = "/")), sep = "\n")

  # vetor com os periodos
  periodo <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "PERIODO:"))] %>%
    stringr::str_remove("PERIODO:") %>%
    as.integer()

  # vetores com o inicio e fim dos campos
  campos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "^ X"))]
  campos <- stringr::str_locate_all(campos[1], "X") %>% do.call(rbind, .)
  iniCampos <- campos[, 1] + 1
  iniCampos <- iniCampos[-length(iniCampos)]
  fimCampos <- campos[, 1]
  fimCampos <- fimCampos[-1]

  # localiza a posicao do inicio de dados
  inicioPeriodo <- which(stringr::str_detect(dadosBrutos, "PERIODO:"))

  # localiza a posicao do fim de dados
  fimPeriodo <- c(inicioPeriodo[-1] - 1, length(dadosBrutos))

  # formacao do data frame a partir de blocos
  df.nwlistcfCortes <- purrr::map_df(periodo, function(andaPeriodo) {
    # faz leitura se existitrem dados para o período
    if (fimPeriodo[andaPeriodo] - inicioPeriodo[andaPeriodo] > 3) {
      # posicoes e nomes de acordo com manual do NEWAVE
      df.dadosNwlistcfCortes <- readr::read_fwf(I(dadosBrutos[inicioPeriodo[andaPeriodo]:(fimPeriodo[andaPeriodo])]),
        col_positions = readr::fwf_positions(
          # vetor com as posicoes iniciais de cada campo
          iniCampos,
          # vetor com as posicoes finais de cada campo
          fimCampos
        ),
        skip = 3,
        show_col_types = FALSE
      )

      # adiciona o a coluna periodo
      df.dadosNwlistcfCortes <- df.dadosNwlistcfCortes %>%
        dplyr::mutate(periodo = periodo[andaPeriodo]) %>%
        dplyr::select(periodo, dplyr::everything())

      # concatena dados num data frame unico
      df.nwlistcfCortes <- rbind(df.nwlistcfCortes, df.dadosNwlistcfCortes)
    }
  })

  return(df.nwlistcfCortes)
}
