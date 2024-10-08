% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraConfiguracaoHidro.R
\name{leituraConfiguracaoHidro}
\alias{leituraConfiguracaoHidro}
\title{Leitor de dados de configuracao hidroeletrica}
\usage{
leituraConfiguracaoHidro(pastaCaso)
}
\arguments{
\item{pastaCaso}{caracter com localizacao dos arquivos NEWAVE}
}
\value{
\code{df.dadosConfiguracaoHidro} data frame com dados de configuracao hidroeletrica
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item nome da usina (\code{$nomeUsina})
\item numero do posto de vazoes da usina (\code{$posto})
\item codigo da usina jusante no cadastro de usinas hidroeletricas (\code{$codUsinaJusante})
\item codigo do REE (\code{$codREE})
\item volume armazenado inicial em percentagem do volume util (\code{$volumeInical})
\item indicador de usina existente e/ou em expansao (EX existente; EE existente com expansao; NE nao existente; NC nao considerada)
(\code{$idUsinaExistente})
\item indice de modificacao de dados da usina (0 nao modifica, 1 modifica) (\code{$idModificacaoUsina})
\item primeiro ano do historico de vazoes do posto correspondente a usina (\code{$inicioHistorico})
\item ultimo ano do historico de vazoes do posto correspondente a usina (\code{$fimHistorico})
\item tecnologia da usina para efeito de calculo de emissoes de GEE (\code{$tecnologia})
}
}
\description{
Faz a leitura do arquivo do NEWAVE com dados de configuracao hidroeletrica (confhd.*). Usa a funcao \code{\link{leituraArquivos}}.
Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE
}
\examples{
\dontrun{
leituraConfiguracaoHidro("C:/PDE2027_Caso080")
}

}
