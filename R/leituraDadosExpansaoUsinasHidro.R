#' Leitor de dados de expansao de usinas hidroeletricas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de expansao de usinas hidroeletricas (exph.*). Usa as funcoes internas \code{\link{leituraArquivos}}, 
#' \code{\link{definePeriodo}}.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#' Retorna as ateracoes no tempo (por todos os meses e anos do horizonte mesmo que o valor seja o mesmo) e sem os periodos de pre e pos
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE
#'
#' @return \code{lt.dadosExpansaoHidro} lista com data frames com dados de expansao hidroeletrica
#' \itemize{
#' \item \code{df.dadosVolumeMorto} data frame com dados de enchimento de volume morto
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item ano e mes de inicio do enchimento de volume morto (\code{$anoMes})
#' \item duracao em meses do enchimento de volume morto (\code{$duracaoEnchimentoVM})
#' \item \% do volume morto ja preenchido ate a data de inicio informada (\code{$volumeMortoCheio})
#' }
#' \item \code{df.dadosExpansaoHidro} data frame com dados de expansao hidroeletrica no "mesmo formato" do original
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item conjunto de maquinas (\code{$conjunto})
#' \item ano e mes de entrada em operacao da unidade hidroeletrica (\code{$anoMes})  
#' \item potencia unitaria da unidade adicionada (\code{$potenciaUnitaria})
#' \item numero da unidade a ser adicionada (\code{$unidadeAdicionada})
#' }
#' \item \code{df.dadosExpansaoHidroTempo} data frame com dados de expansao hidroeletrica no tempo
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item conjunto de maquinas (\code{$conjunto})
#' \item ano e mes de entrada em operacao da unidade hidroeletrica (\code{$anoMes})  
#' \item potencia unitaria da unidade adicionada (\code{$potenciaUnitaria})
#' \item numero de maquinas (\code{$numeroMaquinas})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraDadosExpansaoUsinasHidro("C:/PDE2027_Caso080")}
#'
#' @export
leituraDadosExpansaoUsinasHidro <- function(pastaCaso) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo de dados gerais de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% dplyr::slice(8) %>% dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"), skip = 3)
  dadosValidos <- stringr::str_sub(dadosBrutos, 1, 4) %>% {. != "9999"}
  dadosValidos <- dadosBrutos[dadosValidos]
  dadosTempo <- stringr::str_sub(dadosValidos, 45, 51) %>% stringr::str_detect("[0-9]+", negate = F)
  
  # verifica se o arquivo exph esta vazio e retorna os data frames vazios
  if (length(dadosValidos) == 0) {
    
    df.dadosVolumeMorto <- data.frame(codUsina = numeric(),
                                      nomeUsina = character(),
                                      anoMes = numeric(),
                                      duracaoEnchimentoVM = numeric(),
                                      volumeMortoCheio = numeric())
    
    df.dadosExpansaoHidro <- data.frame(codUsina = numeric(),
                                        nomeUsina = character(),
                                        conjunto = numeric(),
                                        anoMes = numeric(),
                                        potenciaUnitaria = numeric(),
                                        unidadeAdicionada = numeric())
    
    df.dadosExpansaoHidroTempo <- data.frame(codUsina = numeric(),
                                             nomeUsina = character(),
                                             conjunto = numeric(),
                                             anoMes = numeric(),
                                             potenciaUnitaria = numeric(),
                                             numeroMaquinas = numeric())
    
  } else {
    # se a variavel dadosValidos tiver somente uma linha, adiciona uma linha vazia para evitar erro na funcao read fwf
    if (length(dadosValidos) == 1){
      dadosValidos <- c(dadosValidos, "
                                                                                                                              ")
    }
    # para evitar erro de conversao e leitura, o arquivo e lido em duas etapas seguindo o manual do newave
    df.registrosTipo1 <- readr::read_fwf(I(dadosValidos), 
                                         col_positions = readr::fwf_positions(c(1, 6, 19, 22, 32, 38), # vetor com as posicoes iniciais de cada campo
                                                                              c(4, 17, 20, 25, 33, 42), # vetor com as posicoes finais de cada campo
                                                                              c("codUsina", "nomeUsina", "mesInicioEnchimentoVM", "anoInicioEnchimentoVM", 
                                                                                "duracaoEnchimentoVM", "volumeMortoCheio")),
                                         col_types = readr::cols(codUsina = readr::col_double(), nomeUsina = readr::col_character(), mesInicioEnchimentoVM = readr::col_integer(), 
                                                                 anoInicioEnchimentoVM = readr::col_integer(), duracaoEnchimentoVM = readr::col_integer(), volumeMortoCheio = readr::col_double()))
    # se a variavel dadosValidos tiver somente uma linha, filtra os valores NA
    if (length(dadosValidos) == 1){
      df.registrosTipo1 <- df.registrosTipo1 %>% dplyr::filter(!is.na(mesEntradaOperacao))
    }
    df.registrosTipo1 <- df.registrosTipo1 %>% dplyr::mutate(mesInicioEnchimentoVM=ifelse(!is.na(nomeUsina) & is.na(mesInicioEnchimentoVM),0,mesInicioEnchimentoVM),
                                                             anoInicioEnchimentoVM=ifelse(!is.na(nomeUsina) & is.na(anoInicioEnchimentoVM),0,anoInicioEnchimentoVM),
                                                             duracaoEnchimentoVM=ifelse(!is.na(nomeUsina) & is.na(duracaoEnchimentoVM),0,duracaoEnchimentoVM),
                                                             volumeMortoCheio=ifelse(!is.na(nomeUsina) & is.na(volumeMortoCheio),0,volumeMortoCheio)) %>% tidyr::fill(everything())
    df.dadosVolumeMorto <- df.registrosTipo1 %>% dplyr::distinct() %>% dplyr::mutate(anoMes = anoInicioEnchimentoVM * 100 + mesInicioEnchimentoVM) %>% 
      dplyr::select(-mesInicioEnchimentoVM, -anoInicioEnchimentoVM)
    
    # coloca o data frame de acordo com os dados do tipo 2
    df.dadosExpansaoHidro <- readr::read_fwf(I(dadosValidos[dadosTempo]), 
                                             col_positions = readr::fwf_positions(c(45, 48, 53, 61, 65), # vetor com as posicoes iniciais de cada campo
                                                                                  c(46, 51, 58, 62, 65), # vetor com as posicoes finais de cada campo
                                                                                  c("mesEntradaOperacao", "anoEntradaOperacao", 
                                                                                    "potenciaUnitaria", "unidadeAdicionada", "conjunto")),
                                             col_types = readr::cols(mesEntradaOperacao = readr::col_integer(), anoEntradaOperacao = readr::col_integer(), potenciaUnitaria = readr::col_double(),
                                                                     unidadeAdicionada = readr::col_integer(), conjunto = readr::col_integer()))
    # se a variavel dadosValidos tiver somente uma linha, filtra os valores NA
    if (length(dadosValidos) == 1){
      df.dadosExpansaoHidro <- df.dadosExpansaoHidro %>% dplyr::filter(!is.na(mesEntradaOperacao))
    }
    
    df.dadosExpansaoHidro <- cbind(df.registrosTipo1[dadosTempo, 1:2], df.dadosExpansaoHidro) %>% 
      dplyr::mutate(anoMesEntrada = anoEntradaOperacao * 100 + mesEntradaOperacao) %>% dplyr::select(-anoEntradaOperacao, -mesEntradaOperacao)
    
    # cria data frames para colocar a expansao de acordo com suas mudancas no horizonte do estudo
    df.periodo <- definePeriodo(pastaCaso)
    # coloca dados ao longo do tempo restringindo a existencia de dados anteriores as datas de modificacao
    df.dadosExpansaoHidroTempo <- dplyr::inner_join(dplyr::mutate(df.dadosExpansaoHidro, aux = 1), dplyr::mutate(df.periodo, aux = 1), by = c("aux"), relationship = "many-to-many") %>% 
      dplyr::select(-aux, -mes, -ano) %>% dplyr::mutate(vale = ifelse((anoMes >= anoMesEntrada),"s", "n")) %>% 
      dplyr::filter(vale != "n") %>% dplyr::select(-vale)
    
    # processo para colocar a expansao de acordo com suas mudancas no horizonte do estudo
    df.dadosExpansaoHidroTempoAux1 <- dplyr::select(df.dadosExpansaoHidroTempo, -unidadeAdicionada) %>% 
      dplyr::mutate(atributo = "potenciaUnitaria") %>% 
      dplyr::rename(valor = potenciaUnitaria)
    df.dadosExpansaoHidroTempoAux2 <- dplyr::select(df.dadosExpansaoHidroTempo, -potenciaUnitaria) %>% 
      dplyr::mutate(atributo = "numeroMaquinas") %>% 
      dplyr::rename(valor = unidadeAdicionada)
    # elimina os dados duplicados das modificacoes, ficando somente a correta modificao ao longo do tempo
    df.dadosExpansaoHidroTempo <- rbind(df.dadosExpansaoHidroTempoAux1, df.dadosExpansaoHidroTempoAux2) %>% 
      dplyr::arrange(desc(valor),desc(anoMesEntrada)) %>% 
      dplyr::distinct(codUsina, conjunto, atributo, anoMes, .keep_all = TRUE) %>% 
      dplyr::select(-anoMesEntrada) %>% 
      tidyr::pivot_wider(names_from = atributo, values_from = valor) %>% 
      dplyr::arrange(codUsina, conjunto, anoMes)
    
    # organiza dados de saida
    df.dadosExpansaoHidro <- df.dadosExpansaoHidro %>% dplyr::select(codUsina, nomeUsina, conjunto, anoMesEntrada, everything()) %>% 
      dplyr::rename(anoMes = anoMesEntrada) %>% 
      dplyr::arrange(codUsina, conjunto, anoMes)
    
    df.dadosVolumeMorto <- df.dadosVolumeMorto %>% dplyr::arrange(codUsina) %>% dplyr::select(codUsina, nomeUsina, anoMes, everything())
  }
  
  # cria lista com todos os data frames criados
  lt.dadosExpansaoHidro <- list(df.dadosVolumeMorto = dplyr::arrange(df.dadosVolumeMorto, codUsina),
                                df.dadosExpansaoHidro = df.dadosExpansaoHidro,
                                df.dadosExpansaoHidroTempo = df.dadosExpansaoHidroTempo)    
  
  return(lt.dadosExpansaoHidro)
}
