#' Leitor de tabela Nwlistop COM patamar
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de inicio e fim da coluna 
#' Nao retorna os valores de media, desvio e etc. do arquivo de origem. 
#' Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. 
#' Esse "problema" acontece na numeracao das series historicas. 
#' Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
#'
#' @param pasta localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop
#' @param nomeTabela nome da tabela do Nwlistop ex (efiol)
#' @param passo tamanho do campo
#' @param colunaInicialJaneiro coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao do patamar)
#' @param pat TRUE retorna os dados por patamar, FALSE retorna o total dos patamares
#'
#' @return \code{df.dadosNwlistop} data frame com os dados lidos
#' \itemize{
#' \item codigo do REE (\code{$codREE})
#' \item serie (\code{$serie})
#' \item patamar (\code{$patamar})
#' \item valor de ano e mes (\code{$anoMes})
#' \item valor dos dados da tabela escolhida (\code{$dados})
#' }
#'
#' @examples
#' \dontrun{
#' leituraNwlistopGenericaComPatamaresTotal("C:/PDE2027_Caso080", "ghtot", 9, 12)}
#'
#' @export
#' @param pasta localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop
#' @param nomeTabela nome da tabela do Nwlistop ex (efiol)
#' @param passo tamanho do campo
#' @param colunaInicialJaneiro coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao do patamar)
#' @param pat TRUE retorna os dados por patamar, FALSE retorna por total
#' @export
leituraNwlistopGenericaComPatamaresTotal <- function (pasta, nomeTabela, passo, colunaInicialJaneiro, pat = FALSE){
  
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
  
  df.dadosNwlistop <- dplyr::tibble(codREE = numeric(), serie = numeric(), anoMes = numeric(), dados = numeric())
  tabela <- paste("^", nomeTabela, sep = "", "[0-9]*\\.",collapse = NULL)
  tabela_REE <- paste("^", nomeTabela, "[ms]", sep = "", collapse = NULL)
  arquivos <- dplyr::setdiff(list.files(pasta, pattern = tabela), list.files(pasta, pattern = tabela_REE))
  
  if (length(arquivos) == 0) {
    stop(paste0("Nao foram encontrados os arquivos ", nomeTabela, "XXX.out em ", pasta))
  }
  dados <- purrr::map_df(arquivos, function(arquivo) {
    
    # le o arquivo de entrada como um vetor de caracteres nx1
    dadosBrutos <- iotools::input.file(stringi::stri_enc_toutf8(paste(pasta, arquivo, sep = "/")), sep = "\n")
    # encontra os anos
    anos <- dadosBrutos[which(stringr::str_detect(dadosBrutos, "ANO:"))] %>% stringr::str_remove("ANO:") %>% as.integer()
    # localiza a posicao do inicio de dados
    inicioAnos <- which(stringr::str_detect(dadosBrutos, "ANO:"))
    # localiza a posicao do fim de dados pela informacao de desvio padrao
    fimAnos <- which(stringr::str_detect(dadosBrutos, "DPADRAO"))
    # pega informacao de ree no nome do arquivo
    inicioREE <- stringr::str_locate(arquivo,nomeTabela ) %>% {.[1,2] + 1} %>% unname()
    codREE <- stringr::str_sub(arquivo, inicioREE, inicioREE+2) %>% as.integer()
    # Encontra as colunas
    posicaoColunasInicio <- c(3, 10, seq.int(colunaInicialJaneiro, colunaInicialJaneiro + passo*11, passo))
    posicaoColunasFim <- c(6, 11, seq.int(colunaInicialJaneiro + passo -1, colunaInicialJaneiro + passo*12 -1, passo))
    
    purrr::map_df(1:length(anos), function(andaAnos) {
      suppressWarnings(
        df.dadosNwlistopAno <- readr::read_fwf(I(dadosBrutos[inicioAnos[andaAnos]:(fimAnos[andaAnos] - 2)]),
                                               col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                                 posicaoColunasInicio,
                                                 # vetor com as posicoes finais de cada campo
                                                 posicaoColunasFim,
                                                 # nome colunas
                                                 c('serie', 'patamar', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')),
                                               col_types = "iidddddddddddd",
                                               skip = 2)
      )
      
      
      if(pat) {
        df.dadosNwlistopAno <- df.dadosNwlistopAno[!is.na(df.dadosNwlistopAno$patamar),]
        numeroPatamar <- df.dadosNwlistopAno %>% 
          dplyr::distinct(patamar) %>%
          dplyr::pull() %>% 
          max()
        series <- rep(1:(nrow(df.dadosNwlistopAno)/numeroPatamar), each = numeroPatamar)
        df.dadosNwlistopAno <- df.dadosNwlistopAno %>% dplyr::mutate(serie = series)
        df.dadosNwlistopAno <- df.dadosNwlistopAno %>% tidyr::pivot_longer(cols = c(-serie, -patamar), names_to = "mes", values_to = "dados") %>%
          dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
          dplyr::select(codREE, serie, patamar, anoMes, dados)
      }else{
        df.dadosNwlistopAno <- df.dadosNwlistopAno[is.na(df.dadosNwlistopAno$patamar),]
        if (nrow(df.dadosNwlistopAno) > 0) {
          series <- rep(1:(nrow(df.dadosNwlistopAno)))
          df.dadosNwlistopAno <- df.dadosNwlistopAno %>% dplyr::mutate(serie = series) %>% 
            dplyr::select(-patamar) %>% 
            tidyr::pivot_longer(cols = -serie, names_to = "mes", values_to = "dados") %>%
            dplyr::mutate(ano = anos[andaAnos], codREE = codREE, anoMes = (ano * 100 + as.numeric(mes))) %>%
            dplyr::select(codREE, serie, anoMes, dados)
        }else{
          stop(paste0("Tabela ", nomeTabela, " nao possui coluna dados TOTAL"))
        }
      }
      df.dadosNwlistop <- rbind(df.dadosNwlistop, df.dadosNwlistopAno)
    })
  })
  return(dados)
}
