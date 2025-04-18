% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_tools.R
\name{delta_bimestre_sub_progetti}
\alias{delta_bimestre_sub_progetti}
\title{Workflow delta bimestre - Controllo per singoli interventi}
\usage{
delta_bimestre_sub_progetti(progetti, progetti_old)
}
\arguments{
\item{progetti}{File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)}

\item{progetti_old}{File di tipo progetti per un bimestre precedente da load_progetti(light=FALSE, visualizzati=FALSE)}
}
\value{
Messaggi e report di controllo.
}
\description{
Workflow di verifica della variazione dei dati tra due bimestri - Controllo del delta per singoli interventi
}
