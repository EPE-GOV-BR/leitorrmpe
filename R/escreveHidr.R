#' Escritor de dados das usinas hidroeletricas
#'
#' Faz a escrita do arquivo do NEWAVE com dados das usinas hidroeletricas 
#' (hidr.dat).
#' Arquivo de acesso direto, nao formatado não formatado, com 320 / 600 
#' postos, cada registro correspondendo a uma usina.
#'
#' @param lt.dadosUsinasHidroeletricas lista com os dados de usinas 
#' hidroeletricas no mesmo formato obtido na leitura
#' @param arquivo caracter com o nome do arquivo que sera criado
#'
#' @examples
#' \dontrun{
#' escreveHidr(lt.dadosUsinasHidroeletricas, "C:/PDE2027_Caso080/hidr.dat")
#' }
#'
#' @export
escreveHidr <- function(lt.dadosUsinasHidroeletricas, arquivo) {
  if (missing(lt.dadosUsinasHidroeletricas)) {
    stop("favor fornecer a lista contendo os dados")
  }
  if (missing(arquivo)) {
    stop("favor indicar o nome do arquivo a ser criado")
  }
  
  nRegistros <- lt.dadosUsinasHidroeletricas[["df.hidrInfo"]]$nRegistros
  tamanhoRegistrosPoli <- lt.dadosUsinasHidroeletricas[["df.hidrInfo"]]$tamanhoRegistrosPoli
  
  nBytesRegistro <- if(tamanhoRegistrosPoli == 4) 792 else 832
  
  con <- file(arquivo, "wb", encoding = "UTF-8")
  i <- 1
  for(cod in 1:nRegistros){
    if(cod %in% lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$codUsina){
      codUsina <- cod
  
      # dados unicos
      writeChar(stringr::str_pad(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$nomeUsina[i], width = 12, side = "right", pad = " "), con, eos = NULL)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$posto[i], con, size = 4)
      writeChar(stringr::str_pad(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$postoBDH[i], width = 8, side = "right", pad = " "), con, eos = NULL)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$codSubsistema[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$codEmpresa[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$codUsinaJusante[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$codUsinaDesvio[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$volumeMinimo[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$volumeMaximo[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$volumeVertedouro[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$volumeDesvio[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$cotaMinima[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$cotaMaxima[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliCotaVolumeA0[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliCotaVolumeA1[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliCotaVolumeA2[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliCotaVolumeA3[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliCotaVolumeA4[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliAreaCotaA0[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliAreaCotaA1[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliAreaCotaA2[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliAreaCotaA3[i], con, size = tamanhoRegistrosPoli)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$poliAreaCotaA4[i], con, size = tamanhoRegistrosPoli)
      writeBin(as.integer(lt.dadosUsinasHidroeletricas[["df.evaporacaoMensal"]][lt.dadosUsinasHidroeletricas[["df.evaporacaoMensal"]]$codUsina == codUsina, ]$evaporacao), con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$numeroConjuntos[i], con, size = 4)
      numeroMaquinas <- lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]][lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]]$codUsina == codUsina, ]$numeroMaquinas
      writeBin(as.integer(c(numeroMaquinas, rep(0, 5 - length(numeroMaquinas)))), con, size = 4)
      potenciaUnitaria <- lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]][lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]]$codUsina == codUsina, ]$potenciaUnitaria
      writeBin(as.numeric(c(potenciaUnitaria, rep(0, 5 - length(potenciaUnitaria)))), con, size = 4)
      writeBin(as.numeric(rep(0, 75)), con, size = 4) # qht, qhg e PHmaq
      quedaEfetiva <- lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]][lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]]$codUsina == codUsina, ]$quedaEfetiva
      writeBin(as.numeric(c(quedaEfetiva, rep(0, 5 - length(quedaEfetiva)))), con, size = 4)
      vazaoEfetiva <- lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]][lt.dadosUsinasHidroeletricas[["df.dadosConfiguracao"]]$codUsina == codUsina, ]$vazaoEfetiva
      writeBin(as.integer(c(vazaoEfetiva, rep(0, 5 - length(vazaoEfetiva)))), con, size = 4)    
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$produtibilidade[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$perda[i], con, size = 4)
      numPoliVazaoNivelJusante <- lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$numPoliVazaoNivelJusante[i]
      writeBin(numPoliVazaoNivelJusante, con, size = 4)
      if (numPoliVazaoNivelJusante > 0){
        polJusante <- c()
        for(pol in 1:lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$numPoliVazaoNivelJusante[i]){
          polJusante <- c(polJusante,
                          lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$coeficienteA0[pol],
                          lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$coeficienteA1[pol],
                          lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$coeficienteA2[pol],
                          lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$coeficienteA3[pol],
                          lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$coeficienteA4[pol]
                          )
        }
        writeBin(as.numeric(c(polJusante, rep(0, 30 - length(polJusante)))), con, size = 4)
      }else{
        writeBin(as.numeric(rep(0, 30)), con, size = 4)
      }
      alturaReferencia <- lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]][lt.dadosUsinasHidroeletricas[["df.polinomiosVazaoNivelJusante"]]$codUsina == codUsina, ]$alturaReferencia
      writeBin(as.numeric(c(alturaReferencia, rep(0, 6 - length(alturaReferencia)))), con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$canalFugaMedio[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$influenciaVertimentoCanalFuga[i], con, size = 4)
      writeBin(as.numeric(100), con, size = 4) # fator carga maximo
      writeBin(as.numeric(0), con, size = 4) # fator carga minimo
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$vazaoMinimaHistorico[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$numUnidadesBase[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$tipoTurbina[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$representacaoConjunto[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$TEIF[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$IP[i], con, size = 4)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$tipoPerda[i], con, size = 4)
      writeChar(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$data[i], con, nchar = 8, eos = NULL)
      writeChar(stringr::str_pad(iconv(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$observacao[i], from="latin1", to="ASCII//TRANSLIT"), width = 43, side = "right", pad = " "), con, eos = NULL)
      writeBin(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$volumeReferencia[i], con, size = 4)
      writeChar(lt.dadosUsinasHidroeletricas[["df.dadosUsinasHidroeletricas"]]$regulacao[i], con, eos = NULL)
      
      i <- i + 1
    }else{
      writeBin(raw(nBytesRegistro), con)
    }
  }
  
  close(con)
  
  return(paste0("Arquivo ", arquivo, " criado com sucesso!"))
}