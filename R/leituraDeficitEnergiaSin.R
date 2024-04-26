#' Leitor de dados de deficit de energia (Sistema Interligado Nacional)
#' 
#' Faz a leitura dos arquivos do NEWAVE com dados de deficit de energia por deficit no patamar do sistema interligado nacional   (defsinpXXX.*) e 
#' recupera esses valores por patamar com defcit, ano, mes, patamar e serie.
#' Nao retorna os valores de total, media, desvio e etc. do arquivo de origem. 
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com dados de deficit de energia do sistema interligado
#'
#' @return \code{df.deficitEnergiaSin} data frame com os valores de deficit de energia do sistema interligado
#' \itemize{
#' \item numero do patamar com deficit (\code{$patamarDeficit})
#' \item serie (\code{$serie})
#' \item numero do patamar de mercado (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor do deficit de energia [MWmes] (\code{$deficitEnergia})
#' }
#'
#' @examples
#' \dontrun{
#' leituraDeficitEnergiaSin("C:/PDE2027_Caso080")}
#'
#' @export
leituraDeficitEnergiaSin <- function(pasta) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # cria data frame de base 
  df.deficitEnergiaSin <- tidyr::tibble()
  
  # seleciona somente os arquivos defsin
  arquivos <- list.files(pasta, pattern = "^defsinp[0-9]")
  if (length(arquivos) == 0) {
    stop(paste0("N\u00E3o foram encontrados os arquivos defsinpXXX.out em ", pasta))
  }
  
  
  df.deficitEnergiaSin <- purrr::map_df(arquivos, function(arquivo) {
    
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    # filtra as linhas com totais
    localizaTotais <- stringr::str_detect(dadosBrutos, "^ +TOTAL")
    dadosBrutos <- dadosBrutos[!localizaTotais]
    
    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))
    
    #pega informacao patamar com deficit no nome do arquivo
    inicioPatamar <- stringr::str_locate(arquivo, "def") %>% {.[1,2] + 5} %>% unname()
    patamarDeficit <-  stringr::str_sub(arquivo, inicioPatamar, inicioPatamar+2) %>% as.integer()
    
    purrr::map_df(1:length(anos), function(andaAnos) {
      # posicoes e nomes das variaveis
      df.deficitEnergiaAnual <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
                                                col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                  c(3, 10, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112),
                                                  # vetor com as posicoes finais de cada campo
                                                  c(6, 11, 20, 29, 38, 47, 56, 65, 74, 83, 92, 101, 110, 119),
                                                  # nome colunas
                                                  c('serie', 'patamar', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                                col_types = "iidddddddddddd",
                                                skip = 2)
      
      # garante a sequencia correta na numeracao das series. Esse problema acontece na numeracao das series historicas. Assim troca-se o numero ou ano 
      # pelo valor dentro de uma sequencia para cada ano.
      numeroPatamar <- df.deficitEnergiaAnual %>% dplyr::distinct(patamar) %>% dplyr::pull() %>% max()
      series <- rep(1:(nrow(df.deficitEnergiaAnual) / numeroPatamar), each = numeroPatamar)
      df.deficitEnergiaAnual$serie <- series
      # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
      df.deficitEnergiaAnual <- df.deficitEnergiaAnual %>%
        tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "deficitEnergia") %>%
        dplyr::mutate(ano = anos[andaAnos], patamarDeficit = patamarDeficit, anoMes = (ano * 100 + as.numeric(mes))) %>%
        dplyr::select(patamarDeficit, serie, patamar, anoMes, deficitEnergia)
      # concatena dados num data frame unico
      df.deficitEnergiaSin <- rbind(df.deficitEnergiaSin, df.deficitEnergiaAnual)
    })
    
  })
  return(df.deficitEnergiaSin)
}
