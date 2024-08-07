% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraAntecipacaoGnl.R
\name{leituraAntecipacaoGnl}
\alias{leituraAntecipacaoGnl}
\title{Leitor dos dados de antecipação de despacho de usinas térmicas GNL}
\usage{
leituraAntecipacaoGnl(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE}
}
\value{
\code{df.antecipacaoDespachoGnl} data frame com os dados de antecipação de despacho de usinas térmicas GNL
\itemize{
\item numero da usina térmica (\code{$codUsina})
\item numero do mês para despacho antecipado (\code{$numMes})
\item patamar de carga (\code{$patamar})
\item despacho antecipado [MWmes] (\code{$despacho})
}
}
\description{
Faz a leitura do arquivo do NEWAVE com dados de antecipação de despacho de usinas térmicas GNL (adterm.dat)
Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
}
\examples{
\dontrun{
leituraAntecipacaoGnl("C:/PDE2027_Caso080")
}

}
