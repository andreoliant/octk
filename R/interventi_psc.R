# Funzioni per la gestione delle liste di interventi nella programmazione dei PSC


#' Carica lista interventi PSC
#'
#' Carica la lista di interventi delle sezioni ordinarie di PSC dal DBCOE in base alla variabile DB da oc_init().
#'
#' @param use_flt Vuoi caricare solo gli interventi monitorabili (con FLAG_MONITORAGGIO == 1)?
#' @details I progetti privi di OGV rientrano tra gli interventi monitorabili se la delibera di definanziamento non è ancora intervenuta a fronte di istruttoria OGV chiusa.
#' @return Dataframe
load_db_psc <- function(DB, use_flt=FALSE) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_PSC.xlsx"), 
                          col_types = c("text", "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text", "text", "text",
                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                        "numeric",
                                        "text", "text", "text", 
                                        "text", "text", "text", "text"))
  if (use_flt == TRUE) {
    interventi <- interventi %>% 
      filter(FLAG_MONITORAGGIO == 1)
  }
  
  return(interventi)
}

# TODO: integrare le altre funzioni del blocco

