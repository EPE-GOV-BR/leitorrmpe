#' Leitor de dados das planilha Editor do HIDR
#'
#' Faz a leitura do arquivo do .xlsm utilizado na EPE com dados das usinas 
#' hidroeletricas no formato utilizado no pacote leitorrmpe.
#'
#' @param planilha caracter com localizacao da planilha do Editor do HIDR
#' @param nRegistros numero de registros do arquivo hidr (320/600)
#' @param tamanhoRegistrosPoli tamanho dos registros dos dados de polinomio em
#' bytes 4/8
#'
#' @examples
#' \dontrun{
#' leituraEditorHidr("C:/PDE2027_Caso080/Editor do HIDR.xlsm", 600, 4)
#' }
#'
#' @export
leituraEditorHidr <- function(planilha, nRegistros = 600, tamanhoRegistrosPoli = 4) {

  nomeColunas <- c(
  "codUsina","nomeUsina","codSubsistema","codEmpresa","posto","postoBDH",                                     
  "codUsinaJusante","codUsinaDesvio","regulacao","volumeReferencia",
  "volumeMaximo","cotaMaxima","volumeMinimo","cotaMinima","volumeVertedouro",
  "volumeDesvio","poliCotaVolumeA0","poliCotaVolumeA1","poliCotaVolumeA2",
  "poliCotaVolumeA3","poliCotaVolumeA4","poliAreaCotaA0","poliAreaCotaA1",
  "poliAreaCotaA2","poliAreaCotaA3","poliAreaCotaA4","evap1","evap2","evap3",
  "evap4","evap5","evap6","evap7","evap8","evap9","evap10","evap11","evap12",
  "produtibilidade","TEIF","IP","tipoPerda",
  "perda","tipoTurbina","FCMax","FCMin","numUnidadesBase","numeroConjuntos",
  "representacaoConjunto",
  "numeroMaquinas1","potenciaUnitaria1","vazaoEfetiva1","quedaEfetiva1",
  "QHTA01","QHTA11","QHTA21","QHTA31","QHTA41",
  "QHGA01","QHGA11","QHGA21","QHGA31","QHGA41",
  "PHA01","PHA11","PHA21","PHA31","PHA41",
  "numeroMaquinas2","potenciaUnitaria2","vazaoEfetiva2","quedaEfetiva2",
  "QHTA02","QHTA12","QHTA22","QHTA32","QHTA42",
  "QHGA02","QHGA12","QHGA22","QHGA32","QHGA42",
  "PHA02","PHA12","PHA22","PHA32","PHA42",
  "numeroMaquinas3","potenciaUnitaria3","vazaoEfetiva3","quedaEfetiva3",
  "QHTA03","QHTA13","QHTA23","QHTA33","QHTA43",
  "QHGA03","QHGA13","QHGA23","QHGA33","QHGA43",
  "PHA03","PHA13","PHA23","PHA33","PHA43",
  "numeroMaquinas4","potenciaUnitaria4","vazaoEfetiva4","quedaEfetiva4",
  "QHTA04","QHTA14","QHTA24","QHTA34","QHTA44",
  "QHGA04","QHGA14","QHGA24","QHGA34","QHGA44",
  "PHA04","PHA14","PHA24","PHA34","PHA44",
  "numeroMaquinas5","potenciaUnitaria5","vazaoEfetiva5","quedaEfetiva5",
  "QHTA05","QHTA15","QHTA25","QHTA35","QHTA45",
  "QHGA05","QHGA15","QHGA25","QHGA35","QHGA45",
  "PHA05","PHA15","PHA25","PHA35","PHA45",
  "canalFugaMedio","influenciaVertimentoCanalFuga",
  "vazaoMinimaHistorico","numPoliVazaoNivelJusante",
  "PJA0_1","PJA1_1","PJA2_1","PJA3_1","PJA4_1","PJRM_1",
  "PJA0_2","PJA1_2","PJA2_2","PJA3_2","PJA4_2","PJRM_2",
  "PJA0_3","PJA1_3","PJA2_3","PJA3_3","PJA4_3","PJRM_3",
  "PJA0_4","PJA1_4","PJA2_4","PJA3_4","PJA4_4","PJRM_4",
  "PJA0_5","PJA1_5","PJA2_5","PJA3_5","PJA4_5","PJRM_5"
  )
  
  tipoColunas <- c(
    "numeric","text","numeric","numeric","numeric","text",                                     
    rep("numeric", 172)
  )
  
  dados <- readxl::read_xlsx(planilha, 
                             sheet = "Dados", 
                             range = readxl::cell_cols("A:FV"), 
                             col_names = nomeColunas,
                             col_types = tipoColunas,
                             ) %>% 
    dplyr::filter(!is.na(nomeUsina), nomeUsina != "Usina", nomeUsina != "0") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)),
                  dplyr::across(where(is.character), ~tidyr::replace_na(., "0")),
                  nomeUsina = iconv(nomeUsina, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  data = "01-01-00",
                  regulacao = ifelse(regulacao == 77, "M",
                                     ifelse(regulacao == 68, "D",
                                            ifelse(regulacao == 83, "S", regulacao))),
                  observacao = "NA",
                  codUsina = as.integer(codUsina),
                  posto = as.integer(posto),
                  codSubsistema = as.integer(codSubsistema),
                  codEmpresa = as.integer(codEmpresa),
                  codUsinaJusante = as.integer(codUsinaJusante),
                  codUsinaDesvio = as.integer(codUsinaDesvio),
                  numeroConjuntos = as.integer(numeroConjuntos),
                  numPoliVazaoNivelJusante = as.integer(numPoliVazaoNivelJusante),
                  influenciaVertimentoCanalFuga = as.integer(influenciaVertimentoCanalFuga),
                  vazaoMinimaHistorico = as.integer(vazaoMinimaHistorico),
                  numUnidadesBase = as.integer(numUnidadesBase),
                  tipoTurbina = as.integer(tipoTurbina),
                  representacaoConjunto = as.integer(representacaoConjunto),
                  tipoPerda = as.integer(tipoPerda)
    )
  
  df.dadosUsinasHidroeletricas <- dados %>% dplyr::select(
    codUsina,
    nomeUsina,
    posto,
    postoBDH,
    codSubsistema,
    codEmpresa,
    codUsinaJusante,
    codUsinaDesvio,
    volumeMinimo,
    volumeMaximo,
    volumeVertedouro,
    volumeDesvio,
    volumeReferencia,
    cotaMinima,
    cotaMaxima,
    poliCotaVolumeA0,
    poliCotaVolumeA1,
    poliCotaVolumeA2,
    poliCotaVolumeA3,
    poliCotaVolumeA4,
    poliAreaCotaA0,
    poliAreaCotaA1,
    poliAreaCotaA2,
    poliAreaCotaA3,
    poliAreaCotaA4,
    numeroConjuntos,
    produtibilidade,
    perda,
    numPoliVazaoNivelJusante,
    canalFugaMedio,
    influenciaVertimentoCanalFuga,
    vazaoMinimaHistorico,
    numUnidadesBase,
    tipoTurbina,
    representacaoConjunto,
    TEIF,
    IP,
    tipoPerda,
    data,
    observacao,
    regulacao
  )
  
  df.evaporacaoMensal <- dados %>% dplyr::select(
    codUsina,
    nomeUsina,
    codSubsistema,
    dplyr::starts_with("evap")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("evap"), 
                        names_to = "mes", 
                        values_to = "evaporacao") %>% 
    dplyr::mutate(mes = as.integer(stringr::str_remove(mes, "evap"))) %>% 
    dplyr::arrange(codUsina, mes)
  
  df.dadosConfiguracaoNMaq <- dados %>% dplyr::select(
    codUsina,
    nomeUsina,
    codSubsistema,
    numeroConjuntos,
    dplyr::starts_with("numeroMaquinas")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("numeroMaquinas"), 
                        names_to = "numeroMaquinasAux", 
                        values_to = "numeroMaquinas") %>% 
    dplyr::mutate(conjunto = as.integer(stringr::str_remove(numeroMaquinasAux, "numeroMaquinas"))) %>% 
    dplyr::filter(conjunto <= numeroConjuntos) %>% 
    dplyr::select(-numeroMaquinasAux, -numeroConjuntos)
  
  df.dadosConfiguracaoPot <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("potenciaUnitaria"),
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("potenciaUnitaria"), 
                        names_to = "potenciaUnitariaAux", 
                        values_to = "potenciaUnitaria") %>%
    dplyr::mutate(conjunto = as.integer(stringr::str_remove(potenciaUnitariaAux, "potenciaUnitaria"))) %>%
    dplyr::select(-potenciaUnitariaAux)
  
  df.dadosConfiguracaoQEf <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("vazaoEfetiva")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("vazaoEfetiva"), 
                        names_to = "vazaoEfetivaAux", 
                        values_to = "vazaoEfetiva") %>%
    dplyr::mutate(conjunto = as.integer(stringr::str_remove(vazaoEfetivaAux, "vazaoEfetiva"))) %>%
    dplyr::select(-vazaoEfetivaAux)
  
  df.dadosConfiguracaoQuedaEf <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("quedaEfetiva")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("quedaEfetiva"), 
                        names_to = "quedaEfetivaAux", 
                        values_to = "quedaEfetiva") %>%
    dplyr::mutate(conjunto = as.integer(stringr::str_remove(quedaEfetivaAux, "quedaEfetiva"))) %>%
    dplyr::select(-quedaEfetivaAux)
  
  df.dadosConfiguracao <- df.dadosConfiguracaoNMaq %>% 
    dplyr::left_join(df.dadosConfiguracaoPot, by = c("codUsina", "conjunto")) %>%
    dplyr::left_join(df.dadosConfiguracaoQuedaEf, by = c("codUsina", "conjunto")) %>% 
    dplyr::left_join(df.dadosConfiguracaoQEf, by = c("codUsina", "conjunto")) %>% 
    dplyr::arrange(codUsina, conjunto) %>% 
    dplyr::select(codUsina, nomeUsina, codSubsistema, conjunto, dplyr::everything())
  
  df.polinomiosVazaoNivelJusanteHRef <- dados %>% dplyr::select(
    codUsina,
    nomeUsina,
    codSubsistema,
    numPoliVazaoNivelJusante,
    dplyr::starts_with("PJRM")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJRM"), 
                        names_to = "numPoliAux", 
                        values_to = "alturaReferencia") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJRM_"))) %>% 
    dplyr::filter(polinomio <= numPoliVazaoNivelJusante) %>% 
    dplyr::select(-numPoliAux, -numPoliVazaoNivelJusante)
  
  df.polinomiosVazaoNivelJusanteA0 <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("PJA0_")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJA0_"), 
                        names_to = "numPoliAux", 
                        values_to = "coeficienteA0") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJA0_"))) %>% 
    dplyr::select(-numPoliAux)
  
  df.polinomiosVazaoNivelJusanteA1 <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("PJA1_")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJA1_"), 
                        names_to = "numPoliAux", 
                        values_to = "coeficienteA1") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJA1_"))) %>% 
    dplyr::select(-numPoliAux)
  
  df.polinomiosVazaoNivelJusanteA2 <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("PJA2_")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJA2_"), 
                        names_to = "numPoliAux", 
                        values_to = "coeficienteA2") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJA2_"))) %>% 
    dplyr::select(-numPoliAux)
  
  df.polinomiosVazaoNivelJusanteA3 <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("PJA3_")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJA3_"), 
                        names_to = "numPoliAux", 
                        values_to = "coeficienteA3") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJA3_"))) %>% 
    dplyr::select(-numPoliAux)
  
  df.polinomiosVazaoNivelJusanteA4 <- dados %>% dplyr::select(
    codUsina,
    dplyr::starts_with("PJA4_")
  ) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("PJA4_"), 
                        names_to = "numPoliAux", 
                        values_to = "coeficienteA4") %>% 
    dplyr::mutate(polinomio = as.integer(stringr::str_remove(numPoliAux, "PJA4_"))) %>% 
    dplyr::select(-numPoliAux)
  
  df.polinomiosVazaoNivelJusante <- df.polinomiosVazaoNivelJusanteHRef %>% 
    dplyr::left_join(df.polinomiosVazaoNivelJusanteA0, by = c("codUsina", "polinomio")) %>% 
    dplyr::left_join(df.polinomiosVazaoNivelJusanteA1, by = c("codUsina", "polinomio")) %>% 
    dplyr::left_join(df.polinomiosVazaoNivelJusanteA2, by = c("codUsina", "polinomio")) %>% 
    dplyr::left_join(df.polinomiosVazaoNivelJusanteA3, by = c("codUsina", "polinomio")) %>% 
    dplyr::left_join(df.polinomiosVazaoNivelJusanteA4, by = c("codUsina", "polinomio")) %>% 
    dplyr::arrange(codUsina, polinomio) %>% 
    dplyr::select(codUsina, nomeUsina, codSubsistema, polinomio, coeficienteA0, coeficienteA1, coeficienteA2, coeficienteA3, coeficienteA4, alturaReferencia)
  
  df.hidrInfo <- data.frame(nRegistros = nRegistros,
                            tamanhoRegistrosPoli = tamanhoRegistrosPoli)
  
  lt.dadosUsinasHidroeletricas <- list(
    df.dadosUsinasHidroeletricas = df.dadosUsinasHidroeletricas,
    df.evaporacaoMensal = df.evaporacaoMensal,
    df.dadosConfiguracao = df.dadosConfiguracao,
    df.polinomiosVazaoNivelJusante = df.polinomiosVazaoNivelJusante,
    df.hidrInfo = df.hidrInfo
  )
  
  return(lt.dadosUsinasHidroeletricas)
}
