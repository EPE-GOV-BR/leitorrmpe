#' Leitor dos dados de operacao das usinas hidro do SUISHI
#'
#' Faz a leitura dos arquivos do SUISHI com informacao de operacao das usinas hidro (usihid_xxx.csv) e recupera esses valores por ano, mes, serie e patamar.
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do SUISHI com dados de operacao das usinas hidro
#'
#' @return \code{df.operacaoHidroSUISHI} data frame com dados de operacao das usinas hidro
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item valor de ano e mes (\code{$anoMes})
#' \item serie (\code{$serie})
#' \item patamar de carga (\code{$patamar})
#' \item codigo do REE onde a usina se encontra (\code{$codREE})
#' \item todas as colunas representando dados que forem disponibilizados no SUISHI  
#' }
#'
#' @examples
#' \dontrun{
#' leituraOperacaoHidroSUISHI("C:/SUISHI")}
#'
#' @export
leituraOperacaoHidroSUISHI <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do SUISHI")
  }
  
  # seleciona somente os arquivos usihid_xxx.csv
  arquivos <- list.files(pasta, pattern = "usihid_[0-9]{3}\\.csv|USIHID_[0-9]{3}\\.CSV")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos usihid_xxx.csv em ", pasta))
  }
  
  df.operacaoHidroSUISHI <- data.frame()
  
  df.operacaoHidroSUISHI <- purrr::map_df(arquivos, function(andaArquivos) {
    arquivo <- paste0(pasta, "\\", andaArquivos)
    # limpa eventuais , no fim das linhas para nao dar mensagem de aviso
    df.operacaoHidroSUISHIUsina <- readr::read_lines(stringi::stri_enc_toutf8(arquivo)) %>% 
      stringr::str_remove(",$|,.$") %>% 
      I() %>% 
      readr::read_csv(col_types = readr::cols(.default = readr::col_double()))
    
    # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano 
    # pelo valor dentro de uma sequencia para cada ano. Aqui a serie e montada pela quantidade de anos x quantidade de patamares repetidos pela quantidade
    # de registros compondo ano e mes do dado
    series <- df.operacaoHidroSUISHIUsina %>% 
      dplyr::summarise(maximo = max(SERIE), minimo = min(SERIE)) %>% 
      dplyr::mutate(series = maximo - minimo + 1) %>% 
      dplyr::pull(series)
    
    patamares <- df.operacaoHidroSUISHIUsina %>% 
      dplyr::summarise(maximo = max(PAT)) %>% 
      dplyr::pull()
    
    series <- rep(1:series, each = patamares)
    
    series <- rep(series, (nrow(df.operacaoHidroSUISHIUsina) / length(series)))
    
    df.operacaoHidroSUISHIUsina <- df.operacaoHidroSUISHIUsina %>% 
      dplyr::mutate(SERIE = series, anoMes = ((ANO * 100) + MES)) %>% 
      dplyr::select(-ANO, -MES) %>%
      dplyr::select(codUsina = NUMUHE, anoMes, serie = SERIE, patamar = PAT, codREE = REE, everything())
    
    df.operacaoHidroSUISHI <- rbind(df.operacaoHidroSUISHI, df.operacaoHidroSUISHIUsina)
  })
  return(df.operacaoHidroSUISHI)
}
