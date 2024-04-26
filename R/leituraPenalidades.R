#' Leitor de dados de penalidades
#'
#' Faz a leitura do arquivo do NEWAVE com dados de penalidades (penalid.dat).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo de planejamento da operacao de sistemas hidrotermicos interligados de longo e medio prazos do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 57
#' 
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{df.penalidades} data frame com dados de penalidades
#' \itemize{
#' \item tipo de penalidade: 1 para penalidade aplicada a utilizacao da variavel de folga (PENALID1); 2 para penalidade aplicada a utilizacao da variavel de folga no segundo patamar (PENALID2) (\code{$tipoPenalidade})
#' \item numero do REE (\code{$codREE})
#' \item numero do patamar de carga (\code{$patamar})
#' \item penalidade para variavel de folga relativa a outros usos da agua (\code{$desvio})
#' \item penalidade para variavel de folga relativa a intercambio minimo (\code{$intercambioMinimo})
#' \item penalidade para variavel de folga relativa a restricao de defluencia minima obrigatoria (\code{$defluenciaMinima})
#' \item penalidade para variavel de folga relativa a restricao de armazenamento minimo obrigatorio (\code{$volumeMinimo})
#' \item penalidade para variavel de folga relativa a geracao hidraulica minima (\code{$geracaoMinima})
#' 
#' }
#' 
#'
#' @examples
#' \dontrun{
#' leituraPenalidades("C:/PDE2027_Caso080")}
#'
#' @export
leituraPenalidades <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de mercado (penalid.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(29) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  
  # le o arquivo curva como um vetor de caracteres
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"), skip_empty_rows = T)
  df.penalidades <- tidyr::tibble()
  
  # data frame com posicoes e nomes das variaveis
  df.penalidades <- readr::read_fwf(I(dadosBrutos),
                                    col_positions = readr::fwf_positions(# vetor com as posicoes iniciais de cada campo
                                      c(2, 15, 25, 37, 43),
                                      # vetor com as posicoes finais de cada campo
                                      c(7, 22, 32, 39, 44),
                                      # nome colunas
                                      c('chave', 'penalidade1', 'penalidade2', 'codREE', 'patamar')),
                                    col_types = "cddii", skip = 2)
  
  # NAs na coluna codREE significa que valor e o mesmo para todos os REE
  df.ree <- leituraREE(pastaCaso) %>%  dplyr::mutate(aux = 1)  %>% dplyr::select(codREE, aux)
  #cria dataframe expandindo codREE para chaves sem codREE
  df.chaves <- df.penalidades[is.na(df.penalidades$codREE),]  %>%  dplyr::select(-codREE) %>%  dplyr::mutate(aux = 1)
  df.chaves <- dplyr::inner_join(df.chaves, df.ree, by = "aux") %>% dplyr::select(-aux) 
  
  
  df.penalidades <- rbind(df.penalidades,df.chaves) 
  df.penalidades <- df.penalidades[!is.na(df.penalidades$codREE),] %>% dplyr::arrange(codREE)
  
  #separando penalidade1 e penalidade2 
  df.penalidade1 <- dplyr::select(df.penalidades, -penalidade2)
  df.penalidade2 <- dplyr::select(df.penalidades, -penalidade1)
  
  df.penalidade1 <- df.penalidade1 %>% tidyr::pivot_wider(names_from = chave, values_from = penalidade1) %>% dplyr::mutate(tipoPenalidade = 1)
  df.penalidade2 <- df.penalidade2 %>% tidyr::pivot_wider(names_from = chave, values_from = penalidade2) %>% dplyr::mutate(tipoPenalidade = 2)
  
  df.penalidades <- rbind(df.penalidade1,df.penalidade2)
  names(df.penalidades) <- stringr::str_to_upper(names(df.penalidades))
  
  #inserindo chaves que faltam colunas vazias
  if (!("DESVIO") %in% names(df.penalidades)) {
    df.penalidades <- df.penalidades %>% dplyr::mutate(DESVIO = NA)
  }
  if (!"INTMIN" %in% names(df.penalidades)) {
    df.penalidades <- df.penalidades %>% dplyr::mutate(INTMIN = NA)
  }
  if (!"VAZMIN" %in% names(df.penalidades)) {
    df.penalidades <- df.penalidades %>% dplyr::mutate(VAZMIN = NA)
  }
  if (!"VOLMIN" %in% names(df.penalidades)) {
    df.penalidades <- df.penalidades %>% dplyr::mutate(VOLMIN = NA)
  }
  if (!"GHMIN" %in% names(df.penalidades)) {
    df.penalidades <- df.penalidades %>% dplyr::mutate(GHMIN = NA)
  }
  
  df.penalidades <- df.penalidades %>% dplyr::select('tipoPenalidade'= TIPOPENALIDADE, 
                                                     'codREE'= CODREE, 
                                                     'patamar'= PATAMAR, 
                                                     'desvio' = DESVIO, 
                                                     'intercambioMinimo'= INTMIN, 
                                                     'defluenciaMinima'= VAZMIN, 
                                                     'volumeMinimo' = VOLMIN, 
                                                     'geracaoMinima' = GHMIN)
  
  
  return(df.penalidades)
}
