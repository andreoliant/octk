% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/programmazione.R
\name{chk_variazione_risorse_programmi}
\alias{chk_variazione_risorse_programmi}
\title{Verifica variazione risorse per programma nei report per la pubblicazione}
\usage{
chk_variazione_risorse_programmi(
  programmi_new = NULL,
  programmi_old = NULL,
  path_to_new = NULL,
  path_to_old = NULL,
  encoding_old = "UTF-8",
  export = FALSE
)
}
\arguments{
\item{programmi_new}{Dataframe attuale da make_pagina_programmi()}

\item{programmi_old}{Dataframe precedente da make_pagina_programmi()}

\item{path_to_new}{Percorso ad attuale folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().}

\item{path_to_old}{Percorso a precedente folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().}

\item{encoding_old}{Cambia encoding del file old se è stato modificato da excel. Default su "UTF-8", diventa "latin3".}

\item{export}{vuoi salvare il file?}
}
\value{
Un dataframe per programma, ciclo e ambito.
}
\description{
Verifica variazione risorse per programma nei report per la pubblicazione. Confronta due dataframe risultanti da make_pagina_programmi() o i file csv da questa esportati.
}
