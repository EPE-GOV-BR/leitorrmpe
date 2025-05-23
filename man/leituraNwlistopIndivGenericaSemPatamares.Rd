% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraNwlistopIndivGenericaSemPatamares.R
\name{leituraNwlistopIndivGenericaSemPatamares}
\alias{leituraNwlistopIndivGenericaSemPatamares}
\title{Leitor de tabela Nwlistop Individualizado sem patamar}
\usage{
leituraNwlistopIndivGenericaSemPatamares(
  pasta,
  nomeTabela,
  passo,
  colunaInicialJaneiro
)
}
\arguments{
\item{pasta}{localizacao dos arquivos do NEWAVE com as tabelas do Nwlistop}

\item{nomeTabela}{Nome da tabela do Nwlistop ex (efiol)}

\item{passo}{tamanho do campo (opcional). Caso seja vazio ou NA, o passo será calculado pelo cabeçalho}

\item{colunaInicialJaneiro}{coluna em que inicia a impressao dos dados (coluna posterior ao final da impressao da serie)}
}
\value{
\code{df.dadosNwlistop} data frame com os dados lidos
\itemize{
\item codigo da usina (\code{$codUsina})
\item serie (\code{$serie})
\item valor de ano e mes (\code{$anoMes})
\item valor dos dados da tabela escolhida (\code{$dados})
}
}
\description{
Faz a leitura do arquivo do NEWAVE com informacao de inicio e fim da coluna
Nao retorna os valores de media, desvio e etc. do arquivo de origem.
Faz uma modificacao no numero da serie para garantir compatibilidade da sequencia. Esse "problema" acontece na numeracao das series historicas.
Assim troca-se o valor original para o campo serie (ano) pelo valor dentro de uma mesma sequencia para cada ano.
}
\examples{
\dontrun{
leituraNwlistopIndivGenericaSemPatamares("C:/PDE2027_Caso080", "hmont", 15, 8)
}

}
