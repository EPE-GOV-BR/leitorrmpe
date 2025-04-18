% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraSeriesHistoricasSimulacaoFinal.R
\name{leituraSeriesHistoricasSimulacaoFinal}
\alias{leituraSeriesHistoricasSimulacaoFinal}
\title{Leitor dos dados de series historicas para a simulacao final}
\usage{
leituraSeriesHistoricasSimulacaoFinal(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE}
}
\value{
\code{lt.seriesHistoriaSimulacaoFinal} lista com data frames com dados das series historicas para a simulacao final
\itemize{
\item \code{df.varredura} data frame com dados de varredura
\itemize{
\item simulacao com varredura da serie historica - 0: nao faz varredura; 1: faz (\code{$varredura})
\item ano inicio da varredura (a partir de 1932 inclusive) (\code{$anoInicio})
}
\item \code{df.seriesHistoricas} data frame com ano de inicio de simulacao das series historicas
\itemize{
\item ano historico de inicio da simulacao final (\code{$anoInicio})
}
}
}
\description{
Faz a leitura do arquivo do NEWAVE com informacao de series historicas para a simulacao final (shist.dat). Usa a funcao \code{\link{leituraArquivos}}
Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
}
\examples{
\dontrun{
leituraSeriesHistoricasSimulacaoFinal("C:/PDE2027_Caso080")
}

}
