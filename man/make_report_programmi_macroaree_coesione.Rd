% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coesione.R
\name{make_report_programmi_macroaree_coesione}
\alias{make_report_programmi_macroaree_coesione}
\title{Esporta report per Programmi e Macroaree con dati coesione}
\usage{
make_report_programmi_macroaree_coesione(
  perimetro,
  perimetro_sie = NULL,
  usa_meuro = FALSE,
  use_713 = FALSE,
  use_eu = FALSE,
  use_flt = FALSE,
  use_po_psc = FALSE,
  use_cicli_psc = FALSE,
  use_fix_siepoc = FALSE,
  stime_fix_siepoc = FALSE,
  show_cp = FALSE,
  cut_no_risorse = FALSE,
  tipo_ciclo = "CICLO_STRATEGIA",
  focus = "report",
  export = FALSE,
  export_xls = FALSE,
  progetti = NULL,
  po_riclass = NULL,
  DB
)
}
\arguments{
\item{perimetro}{Dataset di classe operazioni}

\item{perimetro_sie}{Dataset con valori per macroaree dai livelli gerarchici}

\item{usa_meuro}{Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo}

\item{use_713}{Vuoi caricare anche i dati di programmaizone per il 2007-2013?}

\item{use_eu}{Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)}

\item{use_flt}{Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?}

\item{use_po_psc}{Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC? [FUNZIONALITA' DEPRECATA]}

\item{use_cicli_psc}{Vuoi usare i dati di programmazione per cicli dei PSC?}

\item{use_fix_siepoc}{Vuoi correggere i dati SIE e POC 1420 con le anticipazioni?}

\item{stime_fix_siepoc}{Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime?}

\item{show_cp}{Logico. Vuoi calcolare anche il costo pubblico (CP)?}

\item{cut_no_risorse}{Vuoi eliminare i programmi monitorati senza risorse lato DB?}

\item{tipo_ciclo}{Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?}

\item{focus}{nome per file.}

\item{export}{vuoi salvare il file?}

\item{export_xls}{Vuoi salvare i file xlsx in OUTPUT?}

\item{progetti}{dataset di tipo "progetti" da utilizzare per con add_totali == TRUE}

\item{po_riclass}{dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)}
}
\value{
Un file csv con apertura per programma e fase procedurale.
}
\description{
Report con apertura per programma e fase procedurale rispetto al focus selezionato.
E' costruito su operaizoni e dati coesione.
}
