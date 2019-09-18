# OC > Toolkit
# Loader

#' Carica il dataset progetti
#'
#' Carica il file progetti_esteso_$BIMESTRE.csv dal folder DATI.
#'
#' @param bimestre Stringa in formato "20180630" come da standard per le date in OC.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param debug Logico. Vuoi vedere i totali di progetti e costo pubblico per controllo sul portale OC?
#' @param light Logico. Vuoi usare la versione light di "progetti.csv"?
#' @return Il dataset viene caricato come "progetti" nel Global Environment. Se "progetti" è gia presente compare una notifica.
load_progetti <- function(bimestre, data_path=NULL, visualizzati=TRUE, debug=FALSE, light=FALSE, refactor=FALSE)
{
  # if (exists("progetti", envir = .GlobalEnv)) {
  #   print("Progetti esteso è gia caricato")
  #   progetti <- progetti
  #
  # } else {

    # switch di filename per progetti_light
    if (light == TRUE) {
      temp <- paste0("progetti_light_", bimestre, ".csv")
    } else {
      if (as.numeric(bimestre) <= 20181231) {
        temp <- paste0("progetti_esteso_", bimestre, ".csv")
      } else {
        temp <- "PROGETTI_PREESTESO.csv"
      }
    }

    # switch
    if (!is.null(data_path)) {
      DATA <- data_path
      # MEMO: sovrascrive data_path a DATA
    }

    # load progetti
    if (visualizzati == TRUE) {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000) %>%
        filter(OC_FLAG_VISUALIZZAZIONE == 0)
    } else {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
      # MEMO: qui prende anche non visualizzati
    }

    # Warning: 143229 parsing failures.
    # no trailing characters
    # number of columns of result is not a multiple of vector length
    # MEMO: risolto incrementando guess_max

    # analisi tipologia colonne
    # sapply(names(progetti), function(x) {print(paste0(x, " = ", class(progetti[[x]])))})

    # refactor
    # MEMO: si applica solo a light
    if (light == TRUE & refactor == TRUE) {
      progetti <- refactor_progetti(progetti)
    }

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
  # }
}


#' Fix temporaneo per il dataset progetti in vestione preesteso
#'
#' Integra il dataset.
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_progetti <- function(progetti) {

  # fix temporaneo per matera
  # progetti <- progetti %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(is.na(OC_CODICE_PROGRAMMA) ~ "2018MATERAFSC",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA),
  #          OC_DESCRIZIONE_PROGRAMMA = case_when(is.na(OC_DESCRIZIONE_PROGRAMMA) ~ "MATERA CAPITALE DELLA CULTURA 2019",
  #                                               TRUE ~ OC_DESCRIZIONE_PROGRAMMA))
  # # progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))
  #
  # progetti <- progetti %>%
  #   mutate(DEN_REGIONE = case_when(COD_REGIONE == "002" ~ "VALLE D'AOSTA", # fix per denominazione bilingue
  #                                  COD_REGIONE == "004" ~ "TRENTINO-ALTO ADIGE",
  #                                  DEN_REGIONE == "EMILIA" ~ "EMILIA-ROMAGNA", # fix per denominazione doppia
  #                                  DEN_REGIONE == "FRIULI" ~ "FRIULI-VENEZIA GIULIA",
  #                                  TRUE ~ DEN_REGIONE))

  # progetti <- progetti %>%
  #   mutate(FONDO_COMUNITARIO = case_when(OC_CODICE_PROGRAMMA == "2014IT16M2OP006" & is.na(FONDO_COMUNITARIO) ~ "FESR",
  #                                        # MEMO: forzo su FESR ma c'è anche FSE
  #                                        TRUE ~ FONDO_COMUNITARIO))

  # fix temporaneo per YEI
  progetti <- progetti %>%
    mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI",
                                         TRUE ~ FONDO_COMUNITARIO))

  return(progetti)
}

#' Refactor per perimetro di progetti
#'
#' Integra un perimetro di progetti appena caricato con i factor per x_MACROAREA, x_AMBITO e OC_STATO_PROCEDURALE.
#'
#' @param perimetro Dataset in formato standard.
#' @return Il dataset integrato.
refactor_progetti <- function(perimetro) {

  perimetro <- perimetro %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
           x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "YEI", "SNAI", "FEASR", "FEAMP")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")),
           OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE, levels = c("Non avviato",
                                                                          "In avvio di progettazione",
                                                                          "In corso di progettazione",
                                                                          "In affidamento",
                                                                          "In esecuzione",
                                                                          "Eseguito",
                                                                          "Non determinabile")))
  return(perimetro)
}
