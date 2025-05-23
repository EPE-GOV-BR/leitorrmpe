% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leituraFamiliasCurvasJusante.R
\name{leituraFamiliasCurvasJusante}
\alias{leituraFamiliasCurvasJusante}
\title{Leitor de dados das familias de curvas de jusante}
\usage{
leituraFamiliasCurvasJusante(arquivo)
}
\arguments{
\item{arquivo}{localizacao do arquivo polinjus.dat (ou equivalente)}
}
\value{
\code{lt.familiasCurvasJusante} lista com data frames com dados das familias de curvas de jusante
\itemize{
\item \code{df.familiasCurvasJusante} data frame com dados de cadastro das familias de curvas de jusante
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item indice da familia (sequencial) (\code{$indice})
\item nivel de montante da usina de jusante para referencia da familia (\code{$nivelMontanteReferencia})
\item quantidade de polinomios da familia (\code{$numPoliFamilia})
}
\item \code{df.coeficientesPolinomios} data frame com dados de cadastro dos coeficientes dos polinomios por partes das curvas de jusante
\itemize{
\item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
\item indice da familia (sequencial) (\code{$indice})
\item limite inferior de vazao de jusante (defluencia mais lateral) para janela de validade do polinomio (\code{$vazaoJusanteMinima})
\item limite superior de vazao de jusante (defluencia mais lateral) para janela de validade do polinomio (\code{$vazaoJusanteMaxima})
\item coeficiente A0 do polinomio de vazao de nivel de jusante (\code{$coeficienteA0})
\item coeficiente A1 do polinomio de vazao de nivel de jusante (\code{$coeficienteA1})
\item coeficiente A2 do polinomio de vazao de nivel de jusante (\code{$coeficienteA2})
\item coeficiente A3 do polinomio de vazao de nivel de jusante (\code{$coeficienteA3})
\item coeficiente A4 do polinomio de vazao de nivel de jusante (\code{$coeficienteA4})
}
}
}
\description{
Faz a leitura dos arquivos com dados do cadastro das familias de curvas de jusante e
do cadastro dos coeficientes dos polinomios por partes das curvas de jusante.
}
\examples{
\dontrun{
leituraFamiliasCurvasJusante("C:/PDE2027_Caso080/polinjus.dat")
}

}
