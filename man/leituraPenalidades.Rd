% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraPenalidades.R
\name{leituraPenalidades}
\alias{leituraPenalidades}
\title{Leitor de dados de penalidades}
\usage{
leituraPenalidades(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE}
}
\value{
\code{df.penalidades} data frame com dados de penalidades
\itemize{
\item tipo de penalidade: 1 para penalidade aplicada a utilizacao da variavel de folga (PENALID1); 2 para penalidade aplicada a utilizacao da variavel de folga no segundo patamar (PENALID2) (\code{$tipoPenalidade})
\item numero do REE (\code{$codREE})
\item numero do patamar de carga (\code{$patamar})
\item penalidade para variavel de folga relativa a outros usos da agua (\code{$desvio})
\item penalidade para variavel de folga relativa a intercambio minimo (\code{$intercambioMinimo})
\item penalidade para variavel de folga relativa a restricao de defluencia minima obrigatoria (\code{$defluenciaMinima})
\item penalidade para variavel de folga relativa a restricao de armazenamento minimo obrigatorio (\code{$volumeMinimo})
\item penalidade para variavel de folga relativa a geracao hidraulica minima (\code{$geracaoMinima})

}
}
\description{
Faz a leitura do arquivo do NEWAVE com dados de penalidades (penalid.dat).
Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
Modelo de planejamento da operacao de sistemas hidrotermicos interligados de longo e medio prazos do Projeto NEWAVE versao 27 de dezembro/2019 - pagina 57
}
\examples{
\dontrun{
leituraPenalidades("C:/PDE2027_Caso080")
}

}
