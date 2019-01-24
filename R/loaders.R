# OC > Toolkit
# Loader

#' Carica il dataset progetti
#'
#' Carica il file progetti_esteso_$BIMESTRE.csv dal folder DATI.
#'
#' @param bimestre Stringa in formato "20180630" come da standard per le date in OC.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param debug Logico. Vuoi vedere i totali di progetti e costo pubblico per controllo sul portale OC?
#' @return Il dataset viene caricato come "progetti" nel Global Environment. Se "progetti" è gia presente compare una notifica.
load_progetti <- function(bimestre, visualizzati=TRUE, debug=FALSE) {

  if (exists("progetti")) {
    print("Progetti esteso è gia caricato")
    progetti <- progetti

  } else {

    # filename
    temp <- paste0("progetti_esteso_", bimestre, ".csv")

    # load progetti
    if (visualizzati == TRUE) {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 50000) %>%
        filter(OC_FLAG_VISUALIZZAZIONE == 0)
    } else {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 50000)
      # MEMO: qui prende anche non visualizzati
    }

    # Warning: 143229 parsing failures.
    # no trailing characters
    # number of columns of result is not a multiple of vector length
    # MEMO: risolto incrementando guess_max

    # analisi tipologia colonne
    # sapply(names(progetti), function(x) {print(paste0(x, " = ", class(progetti[[x]])))})

    # debug
    if (debug == TRUE) {
      msg <- progetti %>%
        summarise(N = n(),
                  CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
      message(paste0("Progetti esteso contiene ", format(msg$N, big.mark = ".", decimal.mark = ","),
                     " progetti per un costo pubblico totale di ",
                     format(round(msg$CP/1000000000, 1), big.mark = ".", decimal.mark = ","),
                     " miliardi di euro."))
    }
    return(progetti)
  }
}


#' Fix temporaneo per il dataset progetti in vestione preesteso
#'
#' Integra il dataset.
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_progetti <- function(progetti) {

  #-------- fix ---------#
  # fix temporaneo per matera
  progetti <- progetti %>%
    mutate(OC_CODICE_PROGRAMMA = case_when(is.na(OC_CODICE_PROGRAMMA) ~ "2018MATERAFSC",
                                           TRUE ~ OC_CODICE_PROGRAMMA),
           OC_DESCRIZIONE_PROGRAMMA = case_when(is.na(OC_DESCRIZIONE_PROGRAMMA) ~ "MATERA CAPITALE DELLA CULTURA 2019",
                                                TRUE ~ OC_DESCRIZIONE_PROGRAMMA))
  # progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))

  progetti <- progetti %>%
    mutate(DEN_REGIONE = case_when(COD_REGIONE == "002" ~ "VALLE D'AOSTA", # fix per denominazione bilingue
                                   COD_REGIONE == "004" ~ "TRENTINO-ALTO ADIGE",
                                   DEN_REGIONE == "EMILIA" ~ "EMILIA-ROMAGNA", # fix per denominazione doppia
                                   DEN_REGIONE == "FRIULI" ~ "FRIULI-VENEZIA GIULIA",
                                   TRUE ~ DEN_REGIONE))

  return(progetti)
}
