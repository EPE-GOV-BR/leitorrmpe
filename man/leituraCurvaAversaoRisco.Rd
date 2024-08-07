% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraCurvaAversaoRisco.R
\name{leituraCurvaAversaoRisco}
\alias{leituraCurvaAversaoRisco}
\title{Leitor dos dados da curva de aversao a risco}
\usage{
leituraCurvaAversaoRisco(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE.}
}
\value{
\code{lt.curvaAversaoRisco} lista com data frames com dados da curva de aversao a risco
\itemize{
\item \code{df.penalidade} data frame com dados de alteracao de configuracao hidroeletrica
\itemize{
\item numero do REE (\code{$codREE})
\item penalidade por violacao da curva de seguranca ou restricao de volume minimo operativo, por REE [$/MWh] (\code{$custo})
}

\item \code{df.curvaSeguranca} data frame com dados de alteracao de configuracao hidroeletrica
\itemize{
\item numero do REE (\code{$codREE})
\item ano e mes (\code{$anoMes})
\item percentual da energia armazenavel maxima (EM % DE EARMX) (\code{$energiaMaxima})
}
}
}
\description{
Faz a leitura do arquivo do NEWAVE com dados da curva de aversao a risco (curva.*).
Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 59
}
\examples{
\dontrun{
leituraCurvaAversaoRisco("C:/PDE2027_Caso080")
}

}
