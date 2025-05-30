% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/programmazione.R
\name{load_db_dati}
\alias{load_db_dati}
\title{Carica un dataset "dati" dal database della programmazione}
\usage{
load_db_dati(DB, ciclo, ambito)
}
\arguments{
\item{DB}{Percorso al database generato con oc_init() o sovrascritto.}

\item{ciclo}{Ciclo di programmazione.}

\item{ambito}{Ambito di programmazione.}
}
\value{
Il dataset di programmazione per l'ambito e ciclo richiesto.
}
\description{
Carica il dataset "dati" richiesto dal database della programmazione.
}
