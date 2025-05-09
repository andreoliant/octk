% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/psc_migrati.R
\name{update_clp_interventi_psc}
\alias{update_clp_interventi_psc}
\title{Integra CLP da attuazione in DB interventi programmati nei PSC}
\usage{
update_clp_interventi_psc(
  progetti_psc,
  interventi_psc,
  debug = TRUE,
  export = FALSE
)
}
\arguments{
\item{progetti_psc}{Dataset da load_progetti_psc_migrati()}

\item{interventi_psc}{Dataset da DBCOE per programmazione interventi PSC (ATTENZIONE: use_flt=FALSE altrimenti si perdono righe)}

\item{debug}{Vuoi esportare in TEMP le variazioni dei CLP?}

\item{export}{Vuoi esportare nel file interventi del DBCOE?}
}
\value{
Dataframe
}
\description{
Integra CLP da attuazione in DB interventi programmati nei PSC.
Integra CLP per match su CLP al netto di codice SIL e poi per CUP univoci.
Sostituisce CUP vuoti con CUP da attuazione.
}
