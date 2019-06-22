# Programmazione

#' Carica un dataset dal database della programmazione
#'
#' Carica il datase richiesto dal dataset della programmazione.
#'
#' @param ciclo Ciclo di programmazione.
#' @param ambito Ambito di programmazione..
#' @param simplify_loc Logico. Vuoi semplificare le localizzazioni per compatibilità con lo standard dei Report CD?
#' @return Il dataset di programmazione per l'ambito richiesto.
load_db <- function(ciclo, ambito, simplify_loc=FALSE){

  library("readxl")
  library("tidyverse")

  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FSC" | "FESR" | "FSE" | "POC" / "PAC" per "0713"

  if (ciclo == "2014-2020") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      TRUE ~ ambito)
    filename <- paste0(temp, "_1420.xlsx")
  } else {
    temp <- case_when(ambito == "FESR" ~ "FS",
                      ambito == "FSE" ~ "FS",
                      ambito == "POC" ~ "PAC",
                      TRUE ~ ambito)
    filename <- paste0(temp, "_0713.xlsx")
  }

  # appo <-  read_excel(file.path(DATA, "db", filename), guess_max = 5000) # MEMO: versione prima di GoogleDrive
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)

  if (ambito == "FESR" | ambito == "FSE") {
    appo <- appo %>%
      filter(OC_DESCR_FONTE == ambito)
  }

  appo <- appo %>%
    select(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, OC_TIPOLOGIA_PROGRAMMA,
           OC_DESCR_FONTE,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA, DEN_REGIONE) %>%
    mutate(OC_MACROAREA = toupper(OC_MACROAREA),
           DEN_REGIONE = toupper(DEN_REGIONE)) %>%
    mutate(x_MACROAREA = case_when(OC_MACROAREA == "NC" ~ "Nazionale", # MEMO: nel modello è presenta anche "Trasversale"
                                   is.na(OC_MACROAREA) ~ "Nazionale",
                                   OC_MACROAREA == "MEZZOGIORNO" ~ "Sud",
                                   OC_MACROAREA == "CENTRO NORD" ~ "Centro-Nord",
                                   OC_MACROAREA == "CENTRO-NORD" ~ "Centro-Nord",
                                   TRUE ~ OC_MACROAREA),
           x_REGIONE = case_when(DEN_REGIONE == "NC" ~ "ALTRO TERRITORIO",
                                 is.na(DEN_REGIONE) ~ "ALTRO TERRITORIO",
                                 DEN_REGIONE == "P.A. TRENTO" ~ "PA TRENTO",
                                 DEN_REGIONE == "P.A. BOLZANO" ~ "PA BOLZANO",
                                 DEN_REGIONE == "TRENTO" ~ "PA TRENTO",
                                 DEN_REGIONE == "BOLZANO" ~ "PA BOLZANO",
                                 DEN_REGIONE == "EMILIA ROMAGNA" ~ "EMILIA-ROMAGNA",
                                 DEN_REGIONE == "FRIULI VENEZIA GIULIA" ~ "FRIULI-VENEZIA GIULIA",
                                 DEN_REGIONE == "VALLE D’AOSTA" ~ "VALLE D'AOSTA",
                                 TRUE ~ DEN_REGIONE))

  if (simplify_loc == TRUE) {
    appo <- get_simply_non_loc(appo)
    # MEMO: restituisce x_MACROAREA e x_REGIONE in cormato compatibile con report CD
  }
  return(appo)
}





#' Carica un dataset dal database della programmazione
#'
#' Carica il dataset richesto della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param ciclo Ciclo di programmazione.
#' @param ambito Ambito di programmazione..
#' @param simplify_loc Logico. Vuoi semplificare le localizzazioni per compatibilità con lo standard dei Report CD?
#' @return Il dataset di programmazione per l'ambito richiesto.
load_db_temi <- function(ciclo, ambito, simplify_loc=FALSE){

  library("readxl")
  library("tidyverse")

  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FSC" | "FESR" | "FSE" | "POC" / "PAC" per "0713"

  # switch
  if (ciclo == "2014-2020") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "YEI" ~ "SIE",
                      TRUE ~ ambito)
    filename <- paste0(temp, "_1420.xlsx")
  } else {
    temp <- case_when(ambito == "FESR" ~ "FS",
                      ambito == "FSE" ~ "FS",
                      ambito == "POC" ~ "PAC",
                      TRUE ~ ambito)
    filename <- paste0(temp, "_0713.xlsx")
  }

  # load
  # appo <-  read_excel(file.path(DATA, "db", filename), guess_max = 5000) # MEMO: verisone prima di GoogleDrive
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)

  # filter
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI") {
    appo <- appo %>%
      filter(OC_DESCR_FONTE == ambito) %>%
      # MEMO: questo serve per integrare versione con Asse
      filter(OC_FLAG_ULTIMA_DECISIONE == "X")
  }

  appo <- appo %>%
    select(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, OC_TIPOLOGIA_PROGRAMMA,
           OC_DESCR_FONTE,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA, DEN_REGIONE,
           # MEMO: questa è per "_temi"
           DESCR_SETTORE_STRATEGICO_FSC,	DESCR_ASSE_TEMATICO_FSC,	COD_RA,	DESCR_RA,
           # NEW: inserito per match con SUS
           OC_COD_ARTICOLAZ_PROGRAMMA) %>%
    mutate(OC_MACROAREA = toupper(OC_MACROAREA),
           DEN_REGIONE = toupper(DEN_REGIONE)) %>%
    mutate(x_MACROAREA = case_when(OC_MACROAREA == "NC" ~ "Nazionale", # MEMO: nel modello è presenta anche "Trasversale"
                                   is.na(OC_MACROAREA) ~ "Nazionale",
                                   OC_MACROAREA == "MEZZOGIORNO" ~ "Sud",
                                   OC_MACROAREA == "CENTRO NORD" ~ "Centro-Nord",
                                   OC_MACROAREA == "CENTRO-NORD" ~ "Centro-Nord",
                                   TRUE ~ OC_MACROAREA),
           x_REGIONE = case_when(DEN_REGIONE == "NC" ~ "ALTRO TERRITORIO",
                                 is.na(DEN_REGIONE) ~ "ALTRO TERRITORIO",
                                 DEN_REGIONE == "P.A. TRENTO" ~ "PA TRENTO",
                                 DEN_REGIONE == "P.A. BOLZANO" ~ "PA BOLZANO",
                                 DEN_REGIONE == "TRENTO" ~ "PA TRENTO",
                                 DEN_REGIONE == "BOLZANO" ~ "PA BOLZANO",
                                 DEN_REGIONE == "EMILIA ROMAGNA" ~ "EMILIA-ROMAGNA",
                                 DEN_REGIONE == "FRIULI VENEZIA GIULIA" ~ "FRIULI-VENEZIA GIULIA",
                                 DEN_REGIONE == "VALLE D’AOSTA" ~ "VALLE D'AOSTA",
                                 TRUE ~ DEN_REGIONE)) %>%
    # MEMO: questa è per "_temi"
    mutate(temp = as.character(regmatches(DESCR_ASSE_TEMATICO_FSC,
                                          gregexpr("^([1-9]\\.?[A-z]?)\\.([0-9]{1,2})", DESCR_ASSE_TEMATICO_FSC))),
           COD_SETTORE_STRATEGICO_FSC = as.character(regmatches(temp, gregexpr("^([1-9])(\\.[A-z])?", temp))),
           COD_ASSE_TEMATICO_FSC = as.character(regmatches(temp, gregexpr("([0-9]{1,2})$", temp))),
           COD_RISULTATO_ATTESO = COD_RA) %>%
    select(-temp)

  if (simplify_loc == TRUE) {
    appo <- get_simply_non_loc(appo)
    # MEMO: restituisce x_MACROAREA e x_REGIONE in cormato compatibile con report CD
  }
  return(appo)
}
# MEMO: nasce da "load_db" in stato/draft_programmazione.R >>> DA SPOSTARE IN OC


#' Inizializza il databse della programmazione
#'
#' Carica il databse della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param ... ...
#' @return L'intero datbase dei programmazione.
init_programmazione <- function(usa_temi=FALSE, export=FALSE)
{

  library("tidyverse")

  # loads
  if (usa_temi == TRUE) {
    if (export == TRUE) {
      po_fsc <<- load_db_temi("2014-2020", "FSC", simplify_loc = TRUE)
      po_fesr <<- load_db_temi("2014-2020", "FESR", simplify_loc = TRUE)
      po_fse <<- load_db_temi("2014-2020", "FSE", simplify_loc = TRUE)
      po_poc <<- load_db_temi("2014-2020", "POC", simplify_loc = TRUE)
      message("Il db di programmazione è pronto in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'")

    } else {
      # po_fsc <- load_db_temi("2014-2020", "FSC", simplify_loc = TRUE)
      # po_fesr <- load_db_temi("2014-2020", "FESR", simplify_loc = TRUE)
      # po_fse <- load_db_temi("2014-2020", "FSE", simplify_loc = TRUE)
      # po_poc <- load_db_temi("2014-2020", "POC", simplify_loc = TRUE)
      message("Da implementare")
    }

  } else {
    if (export == TRUE) {
      po_fsc <<- load_db("2014-2020", "FSC", simplify_loc = TRUE)
      po_fesr <<- load_db("2014-2020", "FESR", simplify_loc = TRUE)
      po_fse <<- load_db("2014-2020", "FSE", simplify_loc = TRUE)
      po_poc <<- load_db("2014-2020", "POC", simplify_loc = TRUE)
      message("Il db di programmazione è pronto in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'")

    } else  {
      # po_fsc <- load_db("2014-2020", "FSC", simplify_loc = TRUE)
      # po_fesr <- load_db("2014-2020", "FESR", simplify_loc = TRUE)
      # po_fse <- load_db("2014-2020", "FSE", simplify_loc = TRUE)
      # po_poc <- load_db("2014-2020", "POC", simplify_loc = TRUE)
      message("Da implementare")
    }
  }
}


#' Quadro risorse programmate
#'
#' Crea una tabella di sintesi con le risorse programmate.
#'
#' @param usa_713 Vuoi anche le risorse 2007-2013?
#' @return Tabella di sintesi per ciclo, ambito e macroarea. Per il 2007-2013, le risorse vengono inserite in forma semplificata.
make_risorse <- function(usa_713=FALSE)
{

  init_programmazione(usa_temi=FALSE, export=TRUE)

  # bind risorse
  risorse <- po_fsc %>%
    mutate(x_CICLO = "2014-2020",
           x_AMBITO = "FSC") %>%
    bind_rows(po_poc %>%
                mutate(x_CICLO = "2014-2020",
                       x_AMBITO = "POC")) %>%
    bind_rows(po_fesr %>%
                mutate(x_CICLO = "2014-2020",
                       x_AMBITO = "FESR")) %>%
    bind_rows(po_fse %>%
                mutate(x_CICLO = "2014-2020",
                       x_AMBITO = "FSE")) %>%
    as.data.frame(.) %>%
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "SNAI")),
           x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Ambito nazionale"))) %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(RIS = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE))

  if (usa_713 == TRUE) {
    risorse_713 <- data_frame(x_CICLO = c("2007-2013", "2007-2013", "2007-2013",
                                   "2007-2013", "2007-2013",
                                   "2007-2013", "2007-2013",
                                   "2007-2013", "2007-2013"),
                       x_AMBITO = c("FSC", "FSC", "FSC",
                                    "POC", "POC",
                                    "FESR", "FESR",
                                    "FSE", "FSE"),
                       x_MACROAREA = c("Sud", "Centro-Nord", "Ambito nazionale",
                                       "Sud", "Centro-Nord",
                                       "Sud", "Centro-Nord",
                                       "Sud", "Centro-Nord"),
                       RIS = c(23179200000, 4843300000, 11624100000,
                               8875600000, 118400000,
                               24979900000, 7038500000,
                               7419400000, 6343600000)
                       # versione semplificata in Meuro
                       # RIS = c(23179.2, 4843.3, 11624.1,
                       #         8875.6, 118.4,
                       #         24979.9, 7038.5,
                       #         7419.4, 6343.6)
                       ) %>%
      mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "SNAI")),
             x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Ambito nazionale")))

    risorse <- risorse %>%
      bind_rows(risorse_713)
  }
  return(risorse)
}
