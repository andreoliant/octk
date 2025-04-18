% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataiku.R
\name{make_report_bimestre_coesione_dataiku}
\alias{make_report_bimestre_coesione_dataiku}
\title{Crea report bimestre in modalita' "coesione"}
\usage{
make_report_bimestre_coesione_dataiku(
  programmi,
  usa_meuro = TRUE,
  export = TRUE
)
}
\arguments{
\item{programmi}{Dataset in formato "programmi" da make_report_programmi_coesione()}

\item{usa_meuro}{Vuoi i dati in Meuro? Di default sono come in 'programmi', vale solo se 'programmi' è in euro}
}
\value{
Il report bimestre.
}
\description{
Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
}
