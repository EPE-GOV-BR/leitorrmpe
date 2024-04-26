#' Leitor de dados de geracao termica media por classes
#'
#' Faz a leitura dos arquivos do NEWAVE com dados de geracao termica por classes e total do submercado  (gtertxxx.*) e 
#' recupera os valores medios por codigo da usina, ano e mes, na media de todas as series da politica.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de geracao termica por classes e total
#'
#' @return \code{df.geracaoTermicaMediaClasses} data frame com os valores de geracao termica media por classes
#' \itemize{
#' \item codigo da usina (\code{$codUsina})
#' \item codigo do submercado (\code{$codSubmercado})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor da geracao termica media [MWmes] (\code{$geracao})
#' }
#'
#' @examples
#' \dontrun{
#' leituraGeracaoTermicaMediaClasses("C:/PDE2027_Caso080")}
#'
#' @export
leituraGeracaoTermicaMediaClasses <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # cria data frame de base 
  df.geracaoTermicaMediaClasses <- tidyr::tibble()
  
  # seleciona somente os arquivos gtert
  arquivos <- list.files(pasta, pattern = "^gtert[0-9]")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos gtertXXX.out em ", pasta))
  }
  
  df.geracaoTermicaMediaClasses <- purrr::map_df(arquivos, function(arquivo) {
    
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    
    classes <- iotools::dstrfw(I(dadosBrutos), col_types = c("character", "character"), widths = c(2L, 3L)) %>%
      dplyr::select(2) %>% 
      dplyr::mutate(V2 = trimws(V2)) %>% 
      dplyr::filter(stringr::str_detect(V2, "^[0-9]+")) %>% 
      dplyr::distinct(V2) %>% 
      dplyr::pull()
    
    nClasses <- length(classes)
    
    # se nao existe nenhuma classe, nao faz leitura
    if(nClasses > 0){
      filtro <- iotools::dstrfw(I(dadosBrutos), col_types = c("character", "character"), widths = c(2L, 5L)) %>%
        dplyr::select(2) %>% 
        dplyr::pull() %>% 
        stringr::str_detect("^MEDIA")
      
      anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
      
      dadosBrutos <- dadosBrutos[filtro] %>% 
        stats::na.omit()
      
      # pega informacao de submercado no nome do arquivo
      inicioSubmercado <- stringr::str_locate(arquivo, "gtert") %>% {.[1,2] + 1} %>% unname()
      codSubmercado <- stringr::str_sub(arquivo, inicioSubmercado, inicioSubmercado+2) %>% as.integer()
      
      purrr::map_df(1:length(anos), function(andaAnos) {
        # posicoes e nomes das variaveis
        df.geracaoTermicaClassesMediaAnual <- readr::read_fwf(I(dadosBrutos[((andaAnos - 1)*nClasses+andaAnos):(andaAnos*nClasses+andaAnos-1)]),
                                                              col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                                c(18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108, 117),
                                                                # vetor com as posicoes finais de cada campo
                                                                c(25, 34, 43, 52, 61, 70, 79, 88, 97, 106, 115, 124),
                                                                # nome colunas
                                                                c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                                              col_types = "dddddddddddd") %>%
          dplyr::mutate(codUsina = classes)
        
        # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
        df.geracaoTermicaClassesMediaAnual <- df.geracaoTermicaClassesMediaAnual %>%
          tidyr::pivot_longer(cols = !codUsina, names_to = "mes", values_to = "geracao") %>%
          dplyr::mutate(ano = anos[andaAnos], codSubmercado = codSubmercado, anoMes = (ano * 100 + as.numeric(mes))) %>%
          dplyr::select(codSubmercado, codUsina, anoMes, geracao) 
        
        # concatena dados num data frame unico
        df.geracaoTermicaMediaClasses <- rbind(df.geracaoTermicaMediaClasses, df.geracaoTermicaClassesMediaAnual)
      })
    }
  })
  return(df.geracaoTermicaMediaClasses)
}

