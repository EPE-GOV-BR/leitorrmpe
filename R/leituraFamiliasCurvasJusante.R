#' Leitor de dados das familias de curvas de jusante
#'
#' Faz a leitura dos arquivos com dados do cadastro das familias de curvas de jusante e 
#' do cadastro dos coeficientes dos polinomios por partes das curvas de jusante.
#'
#' @param arquivo localizacao do arquivo polinjus.dat (ou equivalente)
#'
#' @return \code{lt.familiasCurvasJusante} lista com data frames com dados das familias de curvas de jusante
#' \itemize{
#' \item \code{df.familiasCurvasJusante} data frame com dados de cadastro das familias de curvas de jusante
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item indice da familia (sequencial) (\code{$indice})
#' \item nivel de montante da usina de jusante para referencia da familia (\code{$nivelMontanteReferencia})
#' \item quantidade de polinomios da familia (\code{$numPoliFamilia})
#' }
#' \item \code{df.coeficientesPolinomios} data frame com dados de cadastro dos coeficientes dos polinomios por partes das curvas de jusante
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item indice da familia (sequencial) (\code{$indice})
#' \item limite inferior de vazao de jusante (defluencia mais lateral) para janela de validade do polinomio (\code{$vazaoJusanteMinima})
#' \item limite superior de vazao de jusante (defluencia mais lateral) para janela de validade do polinomio (\code{$vazaoJusanteMaxima})
#' \item coeficiente A0 do polinomio de vazao de nivel de jusante (\code{$coeficienteA0})
#' \item coeficiente A1 do polinomio de vazao de nivel de jusante (\code{$coeficienteA1})
#' \item coeficiente A2 do polinomio de vazao de nivel de jusante (\code{$coeficienteA2})
#' \item coeficiente A3 do polinomio de vazao de nivel de jusante (\code{$coeficienteA3})
#' \item coeficiente A4 do polinomio de vazao de nivel de jusante (\code{$coeficienteA4})
#' }
#' }
#'
#' @examples
#' \dontrun{
#' leituraFamiliasCurvasJusante("C:/PDE2027_Caso080/polinjus.dat")}
#'
#' @export
leituraFamiliasCurvasJusante <- function(arquivo) {
  if (missing(arquivo)) {
    stop("favor indicar o arquivo com com o cadastro das familias de curvas de jusante")
  }
  
  if (!file.exists(arquivo)) {
    stop("N\u00E3o foi encontrado o arquivo de cadastro das familias de curvas de jusante.")
  }
  
  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- readr::read_lines(stringi::stri_enc_toutf8(arquivo), locale = readr::locale(encoding = "latin1"))
  
  # encontra os limites dos dados de cadastro das familias de curvas de jusante
  dadosValidos <- which(stringr::str_detect(dadosBrutos, "CURVAJUS"))
  
  df.familiasCurvasJusante <- readr::read_table(dadosBrutos[dadosValidos], 
                                                col_names = c("CURVAJUS", "codUsina", "indice", "nivelMontanteReferencia", "numPoliFamilia"),
                                                col_types = "ciidi") %>% 
    dplyr::select(-CURVAJUS)
  
  # encontra os limites dos dados de cadastro dos coeficientes dos polinomios por partes das curvas de jusante
  dadosValidos <- which(stringr::str_detect(dadosBrutos, "PPPJUS"))
  
  # trata notacao cientifica em portugues
  dadosValidos <- dadosBrutos[dadosValidos] %>% stringr::str_replace_all("D", "e")
  
  suppressWarnings(
    df.coeficientesPolinomios <- readr::read_table(dadosValidos,
                                                   col_names = c("PPPJUS", "codUsina", "indice", "vazaoJusanteMinima", "vazaoJusanteMaxima", 
                                                                 "coeficienteA0", "coeficienteA1", "coeficienteA2", "coeficienteA3", "coeficienteA4"),
                                                   col_types = "ciiddddddd") %>% 
      dplyr::select(-PPPJUS)
  )
  
  # cria lista com todos os data frames criados
  lt.familiasCurvasJusante <- list(df.familiasCurvasJusante = df.familiasCurvasJusante,
                                   df.coeficientesPolinomios = df.coeficientesPolinomios)
  
  return(lt.familiasCurvasJusante)
}
