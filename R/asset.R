# OC > Toolkit
# Asset

# Info
# https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html

#' Riclassifica i programmi
#'
#' Asset con la riclassificazione dei programmi per il miglioramento della reportistica.
#' Contiene correzioni puntuali forzate rispetto ai dati originali di OC.
#'
#'Le variabili sono:
#'\itemize{
#'   \item \strong{OC_CODICE_PROGRAMMA}: codice programma (per match con i dati OC)
#'   \item x_CICLO: ciclo
#'   \item x_FONDO: ...
#'   \item x_GRUPPO: ...
#'   \item x_AMBITO: ... (TODO!!!)
#'   \item x_PROGRAMMA: nome sintetico del programma
#' }
#'
#' @format A data frame
#' @source Team OC
"po_riclass"

#' Report su Monithon
#'
#' Riconciliazione tra progetti di OpenCoesione e report di monitoraggio civico su Monithon.
#'
#' @section Warning:
#' Dati aggiornati ad aprile 2018
#' @format A data frame
#' @source Team OC su dati \url{http://monithon.it}
"monithon_clp"


#' Obiettivi di Policy 2021-2027
#'
#' ...
#'
#' @format A data frame
#' @source Team OC su regolamenti in corso di negoziazione
"matrix_op"


#' Obiettivi Specifici 2021-2027
#'
#' ... OPOS
#'
#'Le variabili sono:
#'\itemize{
#'   \item FONDO: (CHK)
#'   \item OP: (CHK)
#'   \item \strong{OPOS}: codice obiettivo specifico (TODO: da riformulare)
#'   \item OS_DESCR_EN: ...
#'   \item OS_DESCR: ... (create con google translate)
#'   \item OS_DESCR_SHORT: nome sintetico del'OPOS
#' }
#'
#' @format A data frame
#' @source Team OC su regolamenti in corso di negoziazione
"matrix_opos"


#' Raccordo tra RA 2014-2020 e OPOS 2021-2027
#'
#' ... RA vs OPOS
#'
#'Le variabili sono:
#'\itemize{
#'   \item x_AMBITO: ... (CHK: solo per 14020!)
#'   \item \strong{COD_RA}: codice risultato atteso (TODO: da riformulare)
#'   \item RA_DESC: ...
#'   \item OP: ...
#'   \item \strong{OPOS}: codice obiettivo specifico 2021-2027 (TODO: da riformulare)
#'   \item NOTE: ...
#' }
#'
#' @format A data frame
#' @source Team OC su regolamenti in corso di negoziazione
"matrix_ra_opos"




#' Raccordo tra RA e temi prioritari FSC 2014-2020
#'
#' ... RA vs temi FSC
#'
#'Le variabili sono:
#'\itemize{
#'   \item \strong{COD_RA}: codice risultato atteso
#'   \item RA_ID: ...
#'   \item RA: ... (da rinominare in RA_DESC)
#'   \item \strong{COD_SETTORE_STRATEGICO}: area tematica FSC (denominazione BDU?)
#'   \item x_AREA_TEMATICA: denominazione area tematica FSC
#'   \item \strong{COD_ASSE_TEMATICO}: tema prioritario FSC (denominazione BDU?)
#'   \item x_TEMA_PRIORITARIO: denominazione tema prioritario FSC
#'   \item NOTE: ...
#' }
#'
#' @format A data frame
#' @source Team OC
"matrix_ra_temi_fsc"


#' Elenco settori, sotto-settori e categorie CUP
#'
#' Input per la funzione query_cup().
#'
#'Le variabili sono:
#'\itemize{
#'   \item \strong{CUP_COD_SETTORE}: codice settore CUP
#'   \item CUP_DESCR_SETTORE: ...
#'   \item \strong{CUP_COD_SOTTOSETTORE}: ...
#'   \item CUP_DESCR_SOTTOSETTORE: ...
#'   \item \strong{CUP_COD_CATEGORIA}: ...
#'   \item CUP_DESCR_CATEGORIA: ...
#'   \item QUERY: variabile guida per query (inserire 1, 2 o 0)
#'   \item NOTE: ...
#' }
#'
#' @format A data frame
#' @source Team OC
"categorie_cup"

#' Elenco programmi, linee e azioni
#'
#' Input per la funzione query_po().
#' Creata con make_matrix_po() in setup_data.R.
#' La logica segue quella in due livelli per articolazioni e sub-articolazioni in "progetti esteso".
#' Ad esempio: "Asse" e "Obiettivo operativo" per i PO FS 2007-2013.
#'
#'Le variabili sono:
#'\itemize{
#'   \item OC_COD_CICLO: ...
#'   \item OC_DESCR_CICLO: ...
#'   \item \strong{OC_CODICE_PROGRAMMA}: codice settore CUP
#'   \item OC_DESCRIZIONE_PROGRAMMA: ...
#'   \item OC_ARTICOLAZIONE_PROGRAMMA: ...
#'   \item \strong{OC_COD_ARTICOLAZ_PROGRAMMA}: ...
#'   \item OC_DESCR_ARTICOLAZ_PROGRAMMA: ...
#'   \item OC_SUBARTICOLAZIONE_PROGRAMMA: ...
#'   \item \strong{OC_COD_SUBARTICOLAZ_PROGRAMMA}: ...
#'   \item OC_DESCR_SUBARTICOLAZ_PROGRAMMA: ...
#'   \item QUERY: variabile guida per query (inserire 1, 2 o 0)
#'   \item NOTE: ...
#' }
#'
#' @section Warning:
#' Dati da aggiornare per ogni bimestre.
#' @format A data frame
#' @source Team OC su dati BDU
"po_linee_azioni"


#' Elenco campi di intervento UE
#'
#' Input per la funzione query_ue().
#' Creata con make_matrix_ue() in setup_data.R.
#' Comprende i temi prioritari 2007-2013 e i campi di intervento 2014-2020.
#'
#'Le variabili sono:
#'\itemize{
#'   \item OC_COD_CICLO: ...
#'   \item \strong{COD_TEMA_CAMPO}: codice per tema 2007-2013 o campo d'intervento 2014-2020 (con leading zero)
#'   \item DESCR_TEMA_CAMPO: ...
#'   \item QUERY: variabile guida per query (inserire 1, 2 o 0)
#'   \item NOTE: ...
#' }
#'
#'#' @section Warning:
#' Dati da aggiornare per ogni bimestre.
#' Comprende assegnazioni multiple possibili in BDU 2014-2020 (CHK?)
#' @format A data frame
#' @source Team OC su dati BDU
"categorie_ue"


#' Elenco risultati attesi AP 2014-2020
#'
#' Input per la funzione ... (TODO).
#'
#'Le variabili sono:
#'\itemize{
#'   \item OC_COD_CICLO: ...
#'   \item \strong{COD_RA}: codice del risultato atteso 2014-2020
#'   \item RA_DESC: ...
#'   \item QUERY: variabile guida per query (inserire 1, 2 o 0)
#'   \item NOTE: ...
#' }
#'
#'#' @section Warning:
#' Da integrare con RA rural (CHK?).
#' @format A data frame
#' @source Team OC
"ra"

#' Elenco aree tematiche e temi prioritari FSC 2014-2020
#'
#' Input per la funzione ... (TODO).
#'
#'Le variabili sono:
#'\itemize{
#'   \item x_CICLO: ...
#'   \item \strong{COD_SETTORE_STRATEGICO}: area tematica FSC (denominazione BDU?)
#'   \item DESCR_SETTORE_STRATEGICO_FSC: denominazione area tematica FSC (denominazione BDU?)
#'   \item x_AREA_TEMATICA: denominazione area tematica FSC
#'   \item \strong{COD_ASSE_TEMATICO}: tema prioritario FSC (denominazione BDU?)
#'   \item DESCR_ASSE_TEMATICO_FSC: denominazione tema prioritario FSC (denominazione BDU?)
#'   \item x_TEMA_PRIORITARIO: denominazione tema prioritario FSC
#'   \item QUERY: variabile guida per query (inserire 1, 2 o 0)
#'   \item NOTE: ...
#' }
#'
#' @section Warning:
#' Da integrare con RA rural (CHK?).
#' @format A data frame
#' @source Team OC
"aree_temi_fsc"

