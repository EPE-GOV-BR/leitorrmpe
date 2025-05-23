% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consolidaTermeletricasnoHorizonte.R
\name{consolidaTermeletricasnoHorizonte}
\alias{consolidaTermeletricasnoHorizonte}
\title{Dados consolidados de termoeletricas ao longo do horizonte}
\usage{
consolidaTermeletricasnoHorizonte(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE.}
}
\value{
\code{df.consolidadoUTEnoHorizonte} data frame com dados consolidados de termoeletricas ao longo do horizonte
\itemize{
\item numero da usina termica (\code{$codUsinaTermica})
\item nome da usina (\code{$nomeUsina})
\item numero do subsistema/submercado a que pertence a usina (\code{$codSubsistema})
\item periodo (anoMes-AAAAMM) (\code{$anoMes})
\item capacidade instalada (MW) (\code{$capacidaInstalada})
\item fator de capacidade maximo (\%) (\code{$FCMaximo})
\item TEIF da usina termica (\%) (\code{$TEIF})
\item geracao termica minima (MWmes) (\code{$GTMin})
\item indisponibilidade programada (IP) da usina termica para os demais anos do estudo (\%) (\code{$IP})
\item potencia disponivel maxima (MW) (\code{$PDISP})
\item tipo de combustivel da classe termica (\code{$tipoComb})
\item custo de operacao da classe termica ($/MWh) (\code{$CVU})
}
}
\description{
Faz a consolidacao dos dados das termoeletricas ao longo do horizonte de estudo definido no NEWAVE.
Usa como referencia para a leitura dos arquivos as posicoes definidas no Manual do Usuario do
Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
}
\examples{
\dontrun{
consolidaTermeletricasnoHorizonte(pastaCaso)
}

}
