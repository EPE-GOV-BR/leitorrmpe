% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraEnergiaVertidaSubmercado.R
\name{leituraEnergiaVertidaSubmercado}
\alias{leituraEnergiaVertidaSubmercado}
\title{Leitor de dados de energia vertida por submercado}
\usage{
leituraEnergiaVertidaSubmercado(pasta)
}
\arguments{
\item{pasta}{localizacao dos arquivos do NEWAVE com dados de energia vertida}
}
\value{
\code{df.energiaVertidaSubmercado} data frame com os valores de energia vertida por submercado
\itemize{
\item codigo do submercado (\code{$codSubmercado})
\item serie (\code{$serie})
\item valor de ano e mes (\code{$anoMes})
\item valor de vertimento [MWmes] (\code{$vertimento})
}
}
\description{
Faz a leitura dos arquivos do NEWAVE com dados de energia vertida por submercado (evertmxxx.*) e recupera esses valores por ano, mes e serie.
Nao retorna os valores de media, desvio e etc. do arquivo de origem.
Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
}
\examples{
\dontrun{
leituraEnergiaVertidaSubmercado("C:/PDE2027_Caso080")
}

}
