#' Leitor dos dados de classes termicas
#'
#' Faz a leitura do arquivo do NEWAVE com dados de classes termicas (clast.*).
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
#'
#' @param pastaCaso caracter com localizacao dos arquivos NEWAVE.
#'
#' @return \code{df.ClassesTermicas} data frame com dados de usinas termoeletricas
#' \itemize{
#' \item numero da usina termica (\code{$codUsinaTermica})
#' \item periodo (anoMes-AAAAMM) (\code{$anoMes})
#' \item tipo de combustivel da classe termica (\code{$tipoComb})
#' \item custo de operacao da classe termica ($/MWh) (\code{$CVU})
#' }
#'
#' @examples
#' \dontrun{
#' leituraClassesTermicas("C:/PDE2027_Caso080")}
#'
#' @export
leituraClassesTermicas <- function(pastaCaso){
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  
  # encontra o nome do arquivo com dados de usinas termoeletricas (clast.*) de acordo com a ordem informada no manual do NEWAVE para o arquivos.dat
  arquivo <- leituraArquivos(pastaCaso) %>% 
    dplyr::slice(7) %>% 
    dplyr::pull(arquivo)
  
  # verifica existencia do arquivo
  if (!file.exists(paste(pastaCaso, arquivo, sep = "/"))) {
    stop(paste0(arquivo, " n\u00E3o encontrado em ", pastaCaso))
  }
  
  # le o arquivo sistema como um vetor de caracteres
  clast_dados <- readr::read_lines(stringi::stri_enc_toutf8(paste(pastaCaso, arquivo, sep = "/")), locale = readr::locale(encoding = "latin1"), n_max = 500)
  
  # detecta o inicio e fim dos CVUs estrutural e conjuntural
  inicioClast <- which(stringr::str_detect(clast_dados, "NUM"))
  fimClast <- which(stringr::str_detect(clast_dados, "9999$"))
  
  # se nao identificar o CVU conjuntural, define o "fimClast" como o final do arquivo
  if (identical(fimClast, integer(0))) {fimClast <- end(clast_dados)[1]+1}
  
  # seleciona o conteudo do texto contendo as informacoes do CVU estrutural
  clast_estrutural <- clast_dados[(inicioClast[1] + 2):(fimClast - 1)]
  
  # cria a estrutura de dados contendo as informacoes iniciais do CVU estrutural
  df.ClassesTermicas <- data.frame(codUsinaTermica = as.integer(stringr::str_sub(clast_estrutural, 2, 5)),
                                   nomeUsina = stringr::str_sub(clast_estrutural, 7, 18), 
                                   tipoComb = stringr::str_sub(clast_estrutural, 20, 29))
  
  # cria a estrutura de dados contendo as informacoes do CUsto Variavel Unitario (CVU) ao longo dos anos do horizonte de simulacao
  df.ClassesTermicasSub <- paste0(stringr::str_sub(clast_estrutural, 2, 5), " ", stringr::str_sub(clast_estrutural, 31, 500)) %>% 
    stringr::str_squish() %>% 
    I() %>% 
    readr::read_delim(" ", col_names = FALSE, show_col_types = FALSE)
  
  # renomeia os campos do CVU com o ano correspondente
  df.dadosGerais <- leituraDadosGerais(pastaCaso)
  names(df.ClassesTermicasSub)[c(1:length(df.ClassesTermicasSub))] <- c("codUsinaTermica", seq(df.dadosGerais$anoInicio,(df.dadosGerais$anoInicio+length(df.ClassesTermicasSub)-2)))
  
  # mescla as informacoes das duas estruturas anteriores, para a mesma usina
  # tranforma colunas "Anos" em linhas, com a coluna "CVU" definindo o valor do CVU para cada ano
  # mescla o dataframe anterior com a estrutura que define o periodo de simulacao (anoMes - AAAAMM)  
  df.ClassesTermicas <- dplyr::inner_join(df.ClassesTermicas, df.ClassesTermicasSub, by = "codUsinaTermica", relationship = "many-to-many") %>% 
    tidyr::pivot_longer(-c(1, 2, 3), names_to = "ano", values_to = "CVU") %>% 
    dplyr::mutate(ano = as.numeric(ano)) %>% 
    dplyr::select(-c("nomeUsina")) %>% 
    dplyr::inner_join(definePeriodo(pastaCaso), by = "ano", relationship = "many-to-many") %>% 
    dplyr::filter(!is.na(codUsinaTermica)) %>% 
    dplyr::select(-c("ano", "mes"))
  
  # se a variavel "inicioClast" for maior que 1, existe um CVU Conjuntural que deve alterar o valor do CVU das usinas para os dois primeiros meses de simulacao
  if (length(inicioClast) > 1) {
    
    # seleciona o conteudo do texto contendo as informacoes do CVU conjuntural
    clast_conjuntural <- clast_dados[(inicioClast[2] + 2):(end(clast_dados)[1])]
    
    # cria a estrutura de dados contendo as infromacoes iniciais do CVU conjuntural
    df.ClassesTermicasConj <- data.frame(codUsinaTermica = as.integer(stringr::str_sub(clast_conjuntural, 2, 5)),
                                         valornew = as.numeric(stringr::str_sub(clast_conjuntural, 9, 15)),
                                         mi = as.integer(stringr::str_sub(clast_conjuntural, 18, 19)),
                                         anoi = as.integer(stringr::str_sub(clast_conjuntural, 21, 24)),
                                         mf = as.integer(stringr::str_sub(clast_conjuntural, 27, 28)),
                                         anof = as.integer(stringr::str_sub(clast_conjuntural, 30, 33)))
    
    # ajusta campos "mf-mes final" e "anof-ano final", substiutindo valores NA pelo ulimto periodo do horizonte de simulacao
    # cria colunas de data inicial (datainci) e data final (datafim) no formato AAAAMM
    # mescla o dataframe anterior com a estrutura que define o periodo de simulacao (anoMes - AAAAMM)
    # avalia se o valor do CVU COnjuntural e valido para cada mes. Se a um determinado mes for (maior ou igual) a data inicial e (menor ou igual) a data final, o valor fica vigente
    # filtro dos valores vigentes, considerando apenas aqueles diferentes de "nao"
    df.ClassesTermicasConj <- dplyr::mutate(df.ClassesTermicasConj, mf=ifelse(is.na(mf),12,mf), anof=ifelse(is.na(anof), df.dadosGerais$anoInicio+df.dadosGerais$duracaoEstudo-1, anof)) %>% 
      dplyr::mutate(datainc = as.numeric(anoi*100 + mi),datafim = as.numeric(anof*100 + mf),aux = 1) %>% 
      dplyr::filter(!is.na(codUsinaTermica)) %>% dplyr::select(-c("anoi","anof","mi","mf")) %>% 
      dplyr::inner_join(dplyr::mutate(definePeriodo(pastaCaso), aux = 1), by = "aux" , relationship = "many-to-many") %>% dplyr::select(-c("aux")) %>% 
      dplyr::mutate(validadeCVU = ifelse(((anoMes >= datainc) & (anoMes <= datafim)), "sim", "nao")) %>% 
      dplyr::filter(validadeCVU != "nao") %>% dplyr::select(-c("validadeCVU","datainc","datafim","ano","mes")) 
    
    # mescla as informacoes do CVU estrutural com o CVU COnjuntural, substituindo o primeiro pelo ultimo, caso esse nao seja nulo
    
    df.ClassesTermicas <- dplyr::left_join(df.ClassesTermicas,df.ClassesTermicasConj,by=c("codUsinaTermica","anoMes")) %>% 
      dplyr::mutate(CVU=ifelse(is.na(valornew),CVU,valornew)) %>% dplyr::select(codUsinaTermica,anoMes,tipoComb,CVU)
    
  }
  
  return(df.ClassesTermicas)
}
