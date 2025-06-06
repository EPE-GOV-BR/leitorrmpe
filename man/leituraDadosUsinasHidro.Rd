% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraDadosUsinasHidro.R
\name{leituraDadosUsinasHidro}
\alias{leituraDadosUsinasHidro}
\title{Leitor de dados das usinas hidroeletricas}
\usage{
leituraDadosUsinasHidro(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE}
}
\value{
\code{lt.dadosUsinasHidroeletricas} lista com data frames com dados das usinas hidroeletricas
\itemize{
\item \code{df.dadosUsinasHidroeletricas} data frame com dados gerais das usinas hidroeletricas
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item nome da usina (\code{$nomeUsina})
\item numero do posto de vazoes da usina (\code{$posto})
\item numero do posto BDH (\code{$postoBDH})
\item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
\item codigo da empresa da usina (\code{$codEmpresa})
\item codigo da usina jusante no cadastro de usinas hidroeletricas (\code{$codUsinaJusante})
\item codigo da usina de desvio no cadastro de usinas hidroeletricas (\code{$codUsinaDesvio})
\item volume Minimo [hm3] (\code{$volumeMinimo})
\item volume Maximo [hm3] (\code{$volumeMaximo})
\item volume Vertedouro [hm3] (\code{$volumeVertedouro})
\item volume Desvio [hm3] (\code{$volumeDesvio})
\item volume Referencia [hm3] (\code{$volumeReferencia})
\item cota Minima [m] (\code{$cotaMinima})
\item cota Maxima [m] (\code{$cotaMaxima})
\item coeficiente A0 do polinomio cota volume (\code{$poliCotaVolumeA0})
\item coeficiente A1 do polinomio cota volume (\code{$poliCotaVolumeA1})
\item coeficiente A2 do polinomio cota volume (\code{$poliCotaVolumeA2})
\item coeficiente A3 do polinomio cota volume (\code{$poliCotaVolumeA3})
\item coeficiente A4 do polinomio cota volume (\code{$poliCotaVolumeA4})
\item coeficiente A0 do polinomio area cota (\code{$poliAreaCotaA0})
\item coeficiente A1 do polinomio area cota (\code{$poliAreaCotaA1})
\item coeficiente A2 do polinomio area cota (\code{$poliAreaCotaA2})
\item coeficiente A3 do polinomio area cota (\code{$poliAreaCotaA3})
\item coeficiente A4 do polinomio area cota (\code{$poliAreaCotaA4})
\item numero de conjuntos de maquinas (\code{$numeroConjuntos})
\item produtibilidade especifica [MW/m3/s/m] (\code{$produtibilidade})
\item perdas (\code{$perda})
\item numero de polinomios de vazao de jusante (\code{$numPoliVazaoNivelJusante})
\item canal de fuga medio [m] (\code{$canalFugaMedio})
\item vazao minima do historico [m3/s] (\code{$vazaoMinimaHistorico})
\item numero de unidades base (\code{$numUnidadesBase})
\item tipo de turbina - 0:Nao Ha; 1:Francis; 2:Kaplan/Propeller; 3:Pelton (\code{$tipoTurbina})
\item representacao do conjunto (\code{$representacaoConjunto})
\item taxa esperada de indisponibilidade forcada (TEIF) [\%] (\code{$TEIF})
\item indisponibilidade programada (IP) [\%] (\code{$IP})
\item tipo de perda (\code{$tipoPerda})
\item data [dd/mm/aa] (\code{$data})
\item observacao (\code{$observacao})
\item regulacao - D:diaria; M:mensal (\code{$regulacao})
}
\item \code{df.evaporacaoMensal} data frame com dados de evaporacao mensal por usina
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item nome da usina (\code{$nomeUsina})
\item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
\item mes (\code{$mes})
\item evaporacao mensal [mm/mes] (\code{$evaporacao})
}
\item \code{df.dadosConfiguracao} data frame com dados de configuracao por usina
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item nome da usina (\code{$nomeUsina})
\item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
\item conjunto de maquinas (\code{$conjunto})
\item numero de maquinas (\code{$numeroMaquinas})
\item potencia unitaria por conjunto [MW] (\code{$potenciaUnitaria})
\item queda efetiva por conjunto [m] (\code{$quedaEfetiva})
\item vazao efetiva por conjunto [m3/s] (\code{$vazaoEfetiva})
}
\item \code{df.polinomiosVazaoNivelJusante} data frame com dados dos polinomios de vazao de nivel de jusante por usina
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item nome da usina (\code{$nomeUsina})
\item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
\item numero do polinomio de vazao de nivel de jusante. valor entre 1 e 6 (\code{$polinomio})
\item coeficiente A0 do polinomio de vazao de nivel de jusante (\code{$coeficienteA0})
\item coeficiente A1 do polinomio de vazao de nivel de jusante (\code{$coeficienteA1})
\item coeficiente A2 do polinomio de vazao de nivel de jusante (\code{$coeficienteA2})
\item coeficiente A3 do polinomio de vazao de nivel de jusante (\code{$coeficienteA3})
\item coeficiente A4 do polinomio de vazao de nivel de jusante (\code{$coeficienteA4})
\item altura de referencia (\code{$alturaReferencia})
}
}
}
\description{
Faz a leitura do arquivo do NEWAVE com dados das usinas hidroeletricas (hidr.dat).
O arquivo de dados das usinas hidroelrtricas corresponde ao arquivo de cadastro com os dados das usinas hidroeletricas.
Arquivo de acesso direto, nao formatado, com 320/600 registros, cada registro correspondendo a uma usina.
}
\examples{
\dontrun{
leituraDadosUsinasHidro("C:/PDE2027_Caso080")
}

}
