% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraEnergiaVertida.R
\name{leituraEnergiaVertida}
\alias{leituraEnergiaVertida}
\title{Leitor de dados de energia vertida por REE}
\usage{
leituraEnergiaVertida(pasta)
}
\arguments{
\item{pasta}{localizacao dos arquivos do NEWAVE com dados de energia vertida}
}
\value{
\code{df.energiaVertida} data frame com os valores de energia vertida
\itemize{
\item codigo do REE (\code{$codREE})
\item serie (\code{$serie})
\item valor de ano e mes (\code{$anoMes})
\item valor de vertimento [MWmes] (\code{$vertimento})
}
}
\description{
Faz a leitura dos arquivos do NEWAVE com dados de energia vertida por REE (evertxxx.*) e recupera esses valores por ano, mes e serie.
Nao retorna os valores de media, desvio e etc. do arquivo de origem.
Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
}
\examples{
\dontrun{
leituraEnergiaVertida("C:/PDE2027_Caso080")
}

}
