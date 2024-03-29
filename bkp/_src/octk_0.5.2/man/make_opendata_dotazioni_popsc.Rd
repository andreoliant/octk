% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/programmazione.R
\name{make_opendata_dotazioni_popsc}
\alias{make_opendata_dotazioni_popsc}
\title{Dotazioni programmi in PSC}
\usage{
make_opendata_dotazioni_popsc(
  programmi = NULL,
  progetti = NULL,
  export = TRUE,
  export_xls = TRUE
)
}
\arguments{
\item{programmi}{Dati di base da workflow_programmazione().}

\item{progetti}{Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)}

\item{export}{Vuoi salvare il file csv in TEMP?}

\item{export_xls}{Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?}
}
\value{
File opendata con le dotazioni per ambito e per i cicli 2007-2013 e 2014-2020.
}
\description{
Crea il file come quelli in opendata con le dotazioni dei programmi originari trasformati in PSC
}
\note{
Usa dati finanziari da "interventi" ma sovrascrivo le convenzioni da "workflow" presenti in "programmi" (interventi sono N:1 su programmi)
}
