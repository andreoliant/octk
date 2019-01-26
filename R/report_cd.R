# report in formato cd

#' Esporta report in formato CD
#'
#' Report con apertura per regione e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param ciclo in formato x_CICLO.
#' @param ambito in formato x_AMBITO.
#' @param gruppo in formato x_GRUPPO.
#' @param po_ls vettore di PO in formato x_PROGRAMMA.
#' @param focus nome per file.
#' @param debug non implementato.
#' @return Un file csv con apertura per regione e fase procedurale.
report_cd_regioni <- function(perimetro, ciclo=NULL, ambito=NULL, gruppo=NULL, po_ls=NULL, focus="elab", debug=FALSE) {

  # DEV: decidere se tenere solo FSC

  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FSC"
  # gruppo <- "PATTI"
  # report_cd_regioni(perimetro, ciclo=ciclo, ambito=ambito, gruppo=gruppo, po_ls=NULL, focus="elab", debug=FALSE)

  # switch
  if (!is.null(po_ls)) {

    print("Report type: po")

    appo <- perimetro %>%
      filter(x_PROGRAMMA %in% po_ls)

    temp <- paste0(focus, "_custom.csv")
    print("Ricordati di rinominare il file XXX_custom.csv")

    # MEMO:
    # po_ls <- c("PS AREE METROPOLITANE ABRUZZO",
    #            "PS AREE METROPOLITANE EMILIA ROMAGNA",
    #            "PS AREE METROPOLITANE LIGURIA",
    #            "PS AREE METROPOLITANE LOMBARDIA",
    #            "PS AREE METROPOLITANE SARDEGNA",
    #            "PS AREE METROPOLITANE TOSCANA",
    #            "PS AREE METROPOLITANE VENETO")

  } else if (!is.null(gruppo)) {

    print("Report type: gruppo")

    appo <- perimetro %>%
      filter(x_CICLO == ciclo) %>%
      filter(x_AMBITO == ambito) %>%
      filter(x_GRUPPO == gruppo)

    temp <- paste0(focus, "_", ciclo, "_", ambito, "_", gruppo, ".csv")

  } else if (!is.null(ambito) & !is.null(ciclo)) {

    print("Report type: ambito e ciclo")

    appo <- perimetro %>%
      filter(x_CICLO == ciclo) %>%
      filter(x_AMBITO == ambito)

    temp <- paste0(focus, "_", ciclo, "_", ambito, ".csv")

  } else if (!is.null(ciclo) & is.null(ambito)) {

    print("Report type: ciclo")

    appo <- perimetro %>%
      filter(x_CICLO == ciclo)

    temp <- paste0(focus, "_", ciclo, ".csv")

  } else if (is.null(ciclo) & !is.null(ambito)) {

    print("Report type: ambito")

    appo <- perimetro %>%
      filter(x_AMBITO == ambito)

    temp <- paste0(focus, ambito, ".csv")

  } else {

    print("Report type: custom")

    appo <- perimetro

    temp <- paste0(focus, "_custom.csv")
    print("Ricordati di rinominare il file XXX_custom.csv")

  }

  # simply per non localizzati
  appo <- appo %>%
    mutate(x_REGIONE = as.character(x_REGIONE),
           x_MACROAREA = as.character(x_MACROAREA)) %>%
    mutate(x_REGIONE = case_when(x_REGIONE == "ALTRO TERRITORIO" & x_MACROAREA == "Centro-Nord" ~ "PLURI-LOCALIZZATI",
                                 x_REGIONE == "ALTRO TERRITORIO" & x_MACROAREA == "Sud" ~ "PLURI-LOCALIZZATI",
                                 x_REGIONE == "ALTRO TERRITORIO" ~ "AMBITO NAZIONALE",
                                 TRUE ~ x_REGIONE)) %>%
    mutate(x_REGIONE = factor(x_REGIONE, levels = c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                                                    "PA TRENTO", "PA BOLZANO",
                                                    "VENETO", "FRIULI-VENEZIA GIULIA",
                                                    "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO",
                                                    "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                                                    "CALABRIA", "SICILIA", "SARDEGNA",
                                                    "PLURI-LOCALIZZATI", "AMBITO NAZIONALE"))) %>%
    mutate(x_MACROAREA = case_when(x_MACROAREA == "Trasversale" ~ "Ambito nazionale",
                                   x_MACROAREA == "Nazionale" ~ "Ambito nazionale",
                                   x_MACROAREA == "Estero" ~ "Ambito nazionale",
                                   TRUE ~ x_MACROAREA)) %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Ambito nazionale")))

  # spalla per avere tutte le regioni
  spalla <- data_frame(x_MACROAREA = c("Centro-Nord", "Centro-Nord", "Centro-Nord", "Centro-Nord", "Centro-Nord",
                                       "Centro-Nord", "Centro-Nord", "Centro-Nord", "Centro-Nord", "Centro-Nord",
                                       "Centro-Nord", "Centro-Nord", "Centro-Nord", "Centro-Nord",
                                       "Sud", "Sud", "Sud", "Sud", "Sud", "Sud", "Sud", "Sud", "Sud",
                                       "Ambito nazionale"),
                       x_REGIONE = c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "PA TRENTO", "PA BOLZANO",
                                     "VENETO", "FRIULI-VENEZIA GIULIA", "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA",
                                      "UMBRIA", "MARCHE", "LAZIO",  "PLURI-LOCALIZZATI",
                                     "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                                     "CALABRIA", "SICILIA", "SARDEGNA", "PLURI-LOCALIZZATI",
                                     "AMBITO NAZIONALE")) %>%
    mutate(x_REGIONE = factor(x_REGIONE, levels = c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                                                    "PA TRENTO", "PA BOLZANO",
                                                    "VENETO", "FRIULI-VENEZIA GIULIA",
                                                    "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO",
                                                    "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                                                    "CALABRIA", "SICILIA", "SARDEGNA",
                                                    "PLURI-LOCALIZZATI", "AMBITO NAZIONALE"))) %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Ambito nazionale")))
  # MEMO: serve per tenere fermo ordine in tutti i report (comprese le righe vuote)
  # WARNING: usa la versione riscritta sopra di x_MACROAREA e di x_REGIONE

  # report
  out <- spalla %>%
    left_join(appo %>%
                group_by(x_MACROAREA, x_REGIONE) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(x_MACROAREA, x_REGIONE, OC_STATO_FASI) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            mutate(OC_STATO_FASI = factor(OC_STATO_FASI, levels = c("Non avviato",
                                                                                    "In avvio di progetta",
                                                                                    "In corso di progetta",
                                                                                    "In affidamento",
                                                                                    "In esecuzione",
                                                                                    "Eseguito",
                                                                                    "Non determinabile"))) %>%
                            spread(OC_STATO_FASI, COE, fill = 0),
                          by = c("x_MACROAREA", "x_REGIONE")),
              by = c("x_MACROAREA", "x_REGIONE")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  if (debug == TRUE) {
    write.csv2(out, file.path(OUTPUT, temp), row.names = FALSE)
  }
  return(out)
}



#' Esporta report CD in xlsx
#'
#' Esporta report CD in xlsx da template "stato_template_regioni.xlsx"
#'
#' @param report dataframe da report_cd_regioni().
#' @param titolo titolo interno al report (chr).
#' @param ambito ...
#' @param gruppo ...
#' @param ... ...
#' @param focus nome per file.
#' @return Un file xlsx con apertura per regione e fase procedurale.
export_report_cd_regioni <- function(report, titolo=NULL,
                                     # foglio=NULL, filename=NULL,
                                     ciclo=NULL, ambito=NULL, gruppo=NULL,
                                     focus="elab"
                                     # use_template=TRUE
                                     ) {

  # MEMO: per ora funziona con un report per volta (fare aggregazione a mano fuori da R)

  # TODO: implementare uso di focus

  # DEV: rendere dinamico il nome pagina (per input dato da lista di report )
  # DEV: creare versione per N tavole

  # wb3 <- copyWorkbook(wb) ## wrapper for wb$copy()

  # libs
  library("openxlsx")

  # clean data
  if (any(names(report) == "Non determinabile")) {
    report_data <- report %>%
      select(-x_MACROAREA, -x_REGIONE, -`Non determinabile`)
  } else {
    report_data <- report %>%
      select(-x_MACROAREA, -x_REGIONE)
  }
  # MEMO: toglie spalla sx (uguale a template per costruzione) e ultima colonna a dx (sempre vuota)

  # switch for naming
  if (is.null(titolo)) {
    if (!is.null(gruppo)) {
      temp_titolo <- paste("PROGRAMMAZIONE", ciclo, "- TOTALE PROGRAMMI", ambito, "- FOCUS", gruppo)
      temp_file <- paste0("regioni_", ciclo, "_", ambito, "_", gruppo, ".xlsx")
      temp_foglio <- paste0(ambito, " ", ciclo, "-", gruppo)
      print("Report type: gruppo")
      # MEMO: gruppo andrebbe rinominato per esteso

    } else if (!is.null(ciclo) & !is.null(ambito)) {
      temp_titolo <- paste("PROGRAMMAZIONE", ciclo, "- TOTALE PROGRAMMI", ambito)
      temp_file <- paste0("regioni_", ciclo, "_", ambito, ".xlsx")
      temp_foglio <- paste0(ambito, " ", ciclo)
      print("Report type: ciclo e ambito")

    } else if (!is.null(ciclo) & is.null(ambito)) {
      temp_titolo <- paste("PROGRAMMAZIONE", ciclo, "- TOTALE")
      temp_file <- paste0("regioni_", ciclo, ".xlsx")
      temp_foglio <- ciclo
      print("Report type: ciclo")

    } else if (is.null(ciclo) & !is.null(ambito)) {
      temp_titolo <- paste("PROGRAMMAZIONI 2007-2013 E 2014-2020", "- TOTALE PROGRAMMI", ambito)
      temp_file <- paste0("regioni_", ambito, ".xlsx")
      temp_foglio <- ambito
      print("Report type: ambito")

    } else {
      temp_titolo <- "TITOLO DA MODIFICARE"
      temp_file <- "regioni_custom.xlsx"
      temp_foglio <- "CUSTOM"
      print("Ricordati di rinominare il file regioni_custom.xlsx e il foglio CUSTOM")
      print("Report type: custom senza titolo")

    }
  } else {
    temp_titolo <- titolo
    temp_file <- "regioni_custom.xlsx"
    temp_foglio <- "CUSTOM"
    print("Ricordati di rinominare il file XXX_custom.xlsx e il foglio CUSTOM")
    print("Report type: custom con titolo")

  }
  titolo <- data_frame(titolo = c(temp_titolo))

  # load
  wb <- loadWorkbook(system.file("extdata", "stato_template_regioni.xlsx", package = "oc", mustWork = TRUE))

  # rename foglio
  names(wb) <- temp_foglio

  # scrive dati
  writeData(wb, sheet = temp_foglio, x = report_data, startCol = 4, startRow = 6, colNames = FALSE)

  # scrive titolo tabella
  writeData(wb, sheet = temp_foglio, x = titolo, startCol = 1, startRow = 2, colNames = FALSE)

  # salva
  saveWorkbook(wb, file = file.path(OUTPUT, temp_file), overwrite = TRUE)

}




#' Esporta report Patti in formato CD
#'
#' Report con apertura per patto e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param focus nome per file.
#' @param debug non implementato.
#' @return Un file csv con apertura per patto e fase procedurale.
report_cd_patti <- function(perimetro, focus="elab", debug=FALSE) {

  appo <- perimetro %>%
    mutate(x_PROGRAMMA = factor(x_PROGRAMMA, levels = c("PATTO EMILIA-ROMAGNA", "PATTO LOMBARDIA", "PATTO LAZIO",
                                                        "PATTO BOLOGNA", "PATTO FIRENZE", "PATTO GENOVA", "PATTO MILANO", "PATTO VENEZIA",
                                                        "PATTO ABRUZZO", "PATTO BASILICATA", "PATTO CALABRIA",  "PATTO CAMPANIA",
                                                        "PATTO MOLISE", "PATTO PUGLIA", "PATTO SARDEGNA",  "PATTO SICILIA",
                                                        "PATTO BARI", "PATTO CAGLIARI", "PATTO CATANIA",  "PATTO MESSINA",
                                                        "PATTO NAPOLI", "PATTO PALERMO", "PATTO REGGIO CALABRIA")))

  temp <- paste0("patti_", focus, ".csv")

  # spalla per avere tutte le regioni
  spalla <- data_frame(x_TIPO_PATTI = c("Regioni Centro-Nord", "Regioni Centro-Nord", "Regioni Centro-Nord",
                                        "Città Centro-Nord", "Città Centro-Nord", "Città Centro-Nord", "Città Centro-Nord", "Città Centro-Nord",
                                        "Regioni Sud", "Regioni Sud", "Regioni Sud", "Regioni Sud",
                                        "Regioni Sud", "Regioni Sud", "Regioni Sud", "Regioni Sud",
                                        "Città Sud", "Città Sud", "Città Sud", "Città Sud",
                                        "Città Sud", "Città Sud", "Città Sud"),
                       x_PROGRAMMA = c("PATTO EMILIA-ROMAGNA", "PATTO LOMBARDIA", "PATTO LAZIO",
                                       "PATTO BOLOGNA", "PATTO FIRENZE", "PATTO GENOVA", "PATTO MILANO", "PATTO VENEZIA",
                                       "PATTO ABRUZZO", "PATTO BASILICATA", "PATTO CALABRIA",  "PATTO CAMPANIA",
                                       "PATTO MOLISE", "PATTO PUGLIA", "PATTO SARDEGNA",  "PATTO SICILIA",
                                       "PATTO BARI", "PATTO CAGLIARI", "PATTO CATANIA",  "PATTO MESSINA",
                                       "PATTO NAPOLI", "PATTO PALERMO", "PATTO REGGIO CALABRIA")) %>%
    mutate(x_PROGRAMMA = factor(x_PROGRAMMA, levels = c("PATTO EMILIA-ROMAGNA", "PATTO LOMBARDIA", "PATTO LAZIO",
                                                        "PATTO BOLOGNA", "PATTO FIRENZE", "PATTO GENOVA", "PATTO MILANO", "PATTO VENEZIA",
                                                        "PATTO ABRUZZO", "PATTO BASILICATA", "PATTO CALABRIA",  "PATTO CAMPANIA",
                                                        "PATTO MOLISE", "PATTO PUGLIA", "PATTO SARDEGNA",  "PATTO SICILIA",
                                                        "PATTO BARI", "PATTO CAGLIARI", "PATTO CATANIA",  "PATTO MESSINA",
                                                        "PATTO NAPOLI", "PATTO PALERMO", "PATTO REGGIO CALABRIA"))) %>%
    mutate(x_TIPO_PATTI = factor(x_TIPO_PATTI, levels = c("Regioni Centro-Nord", "Città Centro-Nord",
                                                          "Regioni Sud", "Città Sud")))
  # MEMO: serve per tenere fermo ordine in tutti i report (comprese le righe vuote)

  # report
  out <- spalla %>%
    left_join(appo %>%
                group_by(x_PROGRAMMA) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(x_PROGRAMMA, OC_STATO_FASI) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            mutate(OC_STATO_FASI = factor(OC_STATO_FASI, levels = c("Non avviato",
                                                                                    "In avvio di progetta",
                                                                                    "In corso di progetta",
                                                                                    "In affidamento",
                                                                                    "In esecuzione",
                                                                                    "Eseguito",
                                                                                    "Non determinabile"))) %>%
                            spread(OC_STATO_FASI, COE, fill = 0),
                          by = "x_PROGRAMMA"),
              by = "x_PROGRAMMA") %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  if (debug == TRUE) {
    write.csv2(out, file.path(OUTPUT, temp), row.names = FALSE)
  }
  return(out)
}




#' Esporta report CD in xlsx
#'
#' Esporta report CD in xlsx da template "stato_template_regioni.xlsx"
#'
#' @param report dataframe da report_cd_regioni().
#' @param titolo titolo interno al report (chr).
#' @param ambito ...
#' @param gruppo ...
#' @param ... ...
#' @param focus nome per file.
#' @return Un file xlsx con apertura per patto e fase procedurale.
export_report_cd_patti <- function(report, titolo=NULL, focus=NULL) {

  # libs
  library("openxlsx")

  # clean data
  if (any(names(report) == "Non determinabile")) {
    report_data <- report %>%
      select(-x_TIPO_PATTI, -x_PROGRAMMA, -`Non determinabile`)
  } else {
    report_data <- report %>%
      select(-x_TIPO_PATTI, -x_PROGRAMMA)
  }
  # MEMO: toglie spalla sx (uguale a template per costruzione) e ultima colonna a dx (sempre vuota)

  # switch for naming
  if (is.null(titolo)) {
    if (!is.null(focus)) {
      temp_titolo <- paste("PROGRAMMAZIONE 2014-2020 - FSC - PATTI PER LO SVILUPPO (REGIONI E CITTA METROPOLITANE) - FOCUS", toupper(focus))
      temp_file <- paste0("patti_", focus, ".xlsx")
      temp_foglio <- paste0("PATTI-", toupper(focus))
    } else {
      temp_titolo <- paste("PROGRAMMAZIONE 2014-2020 - FSC - PATTI PER LO SVILUPPO (REGIONI E CITTA METROPOLITANE)")
      temp_file <- paste0("patti_custom.xlsx")
      temp_foglio <- "PATTI-CUSTOM"
      print("Ricordati di rinominare il file patti_custom.xlsx e il foglio PATTI-CUSTOM")
    }
  } else {
    temp_titolo <- titolo
    temp_file <- paste0("patti_custom.xlsx")
    temp_foglio <- "PATTI-CUSTOM"
    print("Ricordati di rinominare il file patti_custom.xlsx e il foglio PATTI-CUSTOM")
  }
  titolo <- data_frame(titolo = c(temp_titolo))

  # load
  wb <- loadWorkbook(system.file("extdata", "stato_template_patti.xlsx", package = "oc", mustWork = TRUE))

  # rename foglio
  names(wb) <- temp_foglio

  # scrive dati
  writeData(wb, sheet = temp_foglio, x = report_data, startCol = 4, startRow = 6, colNames = FALSE)

  # scrive titolo tabella
  writeData(wb, sheet = temp_foglio, x = titolo, startCol = 1, startRow = 2, colNames = FALSE)

  # salva
  saveWorkbook(wb, file = file.path(OUTPUT, temp_file), overwrite = TRUE)

}



#' Esporta report Piani nazionali FSC in formato CD
#'
#' Report con apertura per piano nazionale FSC e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param focus nome per file.
#' @param debug non implementato.
#' @return Un file csv con apertura per piano e fase procedurale.
report_cd_pianinaz <- function(perimetro, focus="elab", debug=FALSE) {


  # appo <- perimetro %>%
  appo <- pianinaz %>%
    mutate(x_PROGRAMMA = case_when(x_PROGRAMMA == "PATTO PER LO SVILUPPO REGIONE CAMPANIA:::PIANO OPERATIVO FSC IMPRESE E COMPETITIVITA'" ~ "PIANO OPERATIVO FSC IMPRESE E COMPETITIVITA'",
                                   grepl("^PS AREE ", x_PROGRAMMA) ~ "PIANO STRALCIO DISSESTO IDROGEOLOGICO AREE METROPOLITANE",
                                   TRUE ~ x_PROGRAMMA)) %>%
    mutate(x_PIANI = case_when(x_PROGRAMMA == "PIANO OPERATIVO FSC INFRASTRUTTURE" ~ "INFRASTRUTTURE",
                               x_PROGRAMMA == "PIANO OPERATIVO FSC AMBIENTE" ~ "AMBIENTE",
                               x_PROGRAMMA == "PIANO OPERATIVO FSC IMPRESE E COMPETITIVITA'" ~ "IMPRESE E COMPETITIVITA'",
                               x_PROGRAMMA == "PIANO OPERATIVO FSC AGRICOLTURA" ~ "AGRICOLTURA",
                               x_PROGRAMMA == "PIANO STRALCIO CULTURA E TURISMO" ~ "CULTURA E TURISMO",
                               x_PROGRAMMA == "PIANO STRALCIO DISSESTO IDROGEOLOGICO AREE METROPOLITANE" ~ "DISSESTO IDROG. AREE URBANE",
                               x_PROGRAMMA == "PIANO STRALCIO RICERCA E INNOVAZIONE" ~ "RICERCA E INNOVAZIONE")) %>%
    mutate(x_PIANI = factor(x_PIANI, levels = c("INFRASTRUTTURE", "AMBIENTE", "IMPRESE E COMPETITIVITA'", "AGRICOLTURA",
                                                "CULTURA E TURISMO", "DISSESTO IDROG. AREE URBANE", "RICERCA E INNOVAZIONE")))
  # TODO: queste riclassificazioni andrebbero gestite in po_linee_azioni o almeno in get_x_vars

  temp <- paste0("pianinaz_", focus, ".csv")

  # spalla per avere tutte le regioni
  spalla <- data_frame(x_TIPO_PIANI = c("PIANI OPERATIVI NAZIONALI", "PIANI OPERATIVI NAZIONALI", "PIANI OPERATIVI NAZIONALI",  "PIANI OPERATIVI NAZIONALI",
                                        "PIANI OPERATIVI NAZIONALI", "PIANI OPERATIVI NAZIONALI", "PIANI OPERATIVI NAZIONALI",  "PIANI OPERATIVI NAZIONALI",
                                        "ALTRI PIANI E PROGRAMMI", "ALTRI PIANI E PROGRAMMI"),
                       x_PIANI = c("INFRASTRUTTURE", "AMBIENTE", "IMPRESE E COMPETITIVITA'", "AGRICOLTURA",
                                   "CULTURA E TURISMO", "DISSESTO IDROG. AREE URBANE", "SALUTE", "SPORT E PERIFERIFERIE",
                                   "RICERCA E INNOVAZIONE", "BANDA ULTRA LARGA"),
                       x_PROGRAMMA = c("PIANO OPERATIVO FSC INFRASTRUTTURE",
                                       "PIANO OPERATIVO FSC AMBIENTE",
                                       "PIANO OPERATIVO FSC IMPRESE E COMPETITIVITA'",
                                       "PIANO OPERATIVO FSC AGRICOLTURA",
                                       "PIANO STRALCIO CULTURA E TURISMO",
                                       "PIANO STRALCIO DISSESTO IDROGEOLOGICO AREE METROPOLITANE",
                                       "x_SALUTE",
                                       "x_SPORT E PERIFERIFERIE",
                                       "PIANO STRALCIO RICERCA E INNOVAZIONE",
                                       "x_BANDA ULTRA LARGA")) %>%
    mutate(x_PIANI = factor(x_PIANI, levels = c("INFRASTRUTTURE", "AMBIENTE", "IMPRESE E COMPETITIVITA'", "AGRICOLTURA",
                                                "CULTURA E TURISMO", "DISSESTO IDROG. AREE URBANE", "SALUTE", "SPORT E PERIFERIFERIE",
                                                "RICERCA E INNOVAZIONE", "BANDA ULTRA LARGA"))) %>%
    mutate(x_TIPO_PIANI = factor(x_TIPO_PIANI, levels = c("PIANI OPERATIVI NAZIONALI", "ALTRI PIANI E PROGRAMMI")))
  # MEMO: serve per tenere fermo ordine in tutti i report (comprese le righe vuote)

  # report
  out <- spalla %>%
    left_join(appo %>%
                group_by(x_PROGRAMMA) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(x_PROGRAMMA, OC_STATO_FASI) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            mutate(OC_STATO_FASI = factor(OC_STATO_FASI, levels = c("Non avviato",
                                                                                    "In avvio di progetta",
                                                                                    "In corso di progetta",
                                                                                    "In affidamento",
                                                                                    "In esecuzione",
                                                                                    "Eseguito",
                                                                                    "Non determinabile"))) %>%
                            spread(OC_STATO_FASI, COE, fill = 0),
                          by = "x_PROGRAMMA"),
              by = "x_PROGRAMMA") %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    select(-x_PROGRAMMA)

  if (debug == TRUE) {
    write.csv2(out, file.path(OUTPUT, temp), row.names = FALSE)
  }
  return(out)
}


#' Esporta report CD in xlsx
#'
#' Esporta report CD in xlsx da template "stato_template_pianinaz.xlsx"
#'
#' @param report dataframe da report_cd_pianinaz().
#' @param titolo titolo interno al report (chr).
#' @param ambito ...
#' @param gruppo ...
#' @param ... ...
#' @param focus nome per file.
#' @return Un file xlsx con apertura per piano e fase procedurale.
export_report_cd_pianinaz <- function(report, titolo=NULL, focus=NULL) {

  # libs
  library("openxlsx")

  # clean data
  if (any(names(report) == "Non determinabile")) {
    report_data <- report %>%
      select(-x_TIPO_PIANI, -x_PIANI, -`Non determinabile`)
  } else {
    report_data <- report %>%
      select(-x_TIPO_PIANI, -x_PIANI)
  }
  # MEMO: toglie spalla sx (uguale a template per costruzione) e ultima colonna a dx (sempre vuota)

  # switch for naming
  if (is.null(titolo)) {
    if (!is.null(focus)) {
      temp_titolo <- paste("PROGRAMMAZIONE 2014-2020 - FSC - PIANI OPERATIVI NAZIONALI - FOCUS", toupper(focus))
      temp_file <- paste0("piani_", focus, ".xlsx")
      temp_foglio <- paste0("PIANI-", toupper(focus))
    } else {
      temp_titolo <- paste("PROGRAMMAZIONE 2014-2020 - FSC - PIANI OPERATIVI NAZIONALI")
      temp_file <- paste0("piani_custom.xlsx")
      temp_foglio <- "PIANI-CUSTOM"
      print("Ricordati di rinominare il file patti_custom.xlsx e il foglio PIANI-CUSTOM")
    }
  } else {
    temp_titolo <- titolo
    temp_file <- paste0("piani_custom.xlsx")
    temp_foglio <- "PIANI-CUSTOM"
    print("Ricordati di rinominare il file patti_custom.xlsx e il foglio PIANI-CUSTOM")
  }
  titolo <- data_frame(titolo = c(temp_titolo))

  # load
  wb <- loadWorkbook(system.file("extdata", "stato_template_piani.xlsx", package = "oc", mustWork = TRUE))

  # rename foglio
  names(wb) <- temp_foglio

  # scrive dati
  writeData(wb, sheet = temp_foglio, x = report_data, startCol = 4, startRow = 6, colNames = FALSE)

  # scrive titolo tabella
  writeData(wb, sheet = temp_foglio, x = titolo, startCol = 1, startRow = 2, colNames = FALSE)

  # salva
  saveWorkbook(wb, file = file.path(OUTPUT, temp_file), overwrite = TRUE)

}
