# Funzioni per la gestione del PNRR

#' Carica dati di base per PNRR
#'
#' Carica dati di base per PNRR
#'
#' @param bimestre Bimestre di riferimento
#' @param versione_pnrr Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @return Dataframe
load_progetti_pnrr <- function(bimestre_pnrr, versione_pnrr) {
  
  progetti_pnrr <- read_csv2(file.path(DRIVE, "DATI", "PNRR", "progetti_pnrr", paste0("progetti_pnrr_", bimestre_pnrr, "_", versione_pnrr, ".csv")))
  
  return(progetti_pnrr)
}


