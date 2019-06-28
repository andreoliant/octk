# OC > Toolkit
# Report regionali

# ----------------------------------------------------------------------------------- #
# MEMO:
# Il report "regioni" richiede "progetti_light" perché non calcola x_MACROAREA e simili.
#
# ----------------------------------------------------------------------------------- #

#' Crea report regionale
#'
#' Crea report per regione con tutti i progetti dei programmi regionali e i progetti dei programmi nazionali \
#' localizzati nel territorio della regione.
#'
#' @param regione Regione di interesse.
#'
#' @param debug Vuoi esportare i dataset intermedi di programmazione e attuazione?
#' @return Il dataset con il report regionale.
report_regione <- function(regione, progetti, debug = TRUE) {

  # debug
  # regione <- "PUGLIA"
  if (missing(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  }

  # log
  test <- unique(progetti$x_REGNAZ)
  if (!(toupper(regione) %in% test)) {
    message("Verifica come ha scritto la regione...!")

  } else {
    # load attuazione
    progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)

    # filter
    perimetro <- progetti %>%
      mutate(FLT = case_when(x_REGNAZ == toupper(regione) ~ "REG",
                             x_REGNAZ == "NAZ" & x_REGIONE == toupper(regione) ~ "NAZ",
                             TRUE ~ "")) %>%
      mutate(FLT = factor(FLT, levels = c("REG", "NAZ"))) %>%
      filter(!is.na(FLT))



    # switch variabile COE
    perimetro <- perimetro %>%
      # MEMO: COSTO_RENDICONTABILE_UE e OC_FINANZ_STATO_FSC_NETTO contengono NA quindi converto numerici in 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
             COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                             x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                             x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                             TRUE ~ OC_FINANZ_TOT_PUB_NETTO))

    # # switch variabile COE (VERSIONE TEMPORANEA)
    # perimetro <- perimetro %>%
    #   # MEMO: COSTO_RENDICONTABILE_UE e OC_FINANZ_STATO_FSC_NETTO contengono NA quindi converto numerici in 0
    #   mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    #   mutate(CP = OC_FINANZ_TOT_PUB_NETTO)
    # # COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
    # #                 x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
    # #                 x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
    # #                 TRUE ~ OC_FINANZ_TOT_PUB_NETTO))


    # meuro
    perimetro <- perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)

    # meuro (VERSIONE TEMPORANEA)
    # perimetro <- perimetro %>%
    #   mutate(CP = CP / 1000000,
    #          # COE = COE / 1000000,
    #          IMP = IMPEGNI / 1000000,
    #          PAG = TOT_PAGAMENTI / 1000000)

    # patch
    # perimetro <- perimetro %>%
    #   mutate(x_PROGRAMMA = case_when(
    #     OC_CODICE_PROGRAMMA == "2014IT16M2OP005:::2017FSCRICERCA" ~ "PON FESR FSE RICERCA E INNOVAZIONE",
    #     TRUE ~ x_PROGRAMMA)) %>%
    #   mutate(OC_CODICE_PROGRAMMA = case_when(
    #     OC_CODICE_PROGRAMMA == "2014IT16M2OP005:::2017FSCRICERCA" ~ "2014IT16M2OP005",
    #     TRUE ~ OC_CODICE_PROGRAMMA))


    # -------------------------------------------- #
    # programmazione

    # loads
    init_programmazione(usa_temi=FALSE, export=TRUE)

    # binds
    programmi <- po_fsc %>%
      mutate(x_CICLO = "2014-2020",
             x_AMBITO = "FSC") %>%
      bind_rows(po_fesr %>%
                  mutate(x_CICLO = "2014-2020",
                         x_AMBITO = "FESR")) %>%
      bind_rows(po_fse %>%
                  mutate(x_CICLO = "2014-2020",
                         x_AMBITO = "FSE")) %>%
      bind_rows(po_poc %>%
                  mutate(x_CICLO = "2014-2020",
                         x_AMBITO = "POC"))

    # rm(po_fesr, po_fse, po_fsc, po_poc)

    # integra con FLT per REG/NAZ
    programmi <- programmi %>%
      left_join(po_riclass %>%
                  select(OC_CODICE_PROGRAMMA, x_REGNAZ),
                by = "OC_CODICE_PROGRAMMA")

    # filter Regione
    perimetro_po <- programmi %>%
      mutate(FLT = case_when(x_REGNAZ == toupper(regione) ~ "REG",
                             x_REGIONE == toupper(regione) & x_REGNAZ == "NAZ" ~ "NAZ",
                             TRUE ~ "")) %>%
      filter(FLT != "") %>%
      mutate(FLT = factor(FLT, levels = c("REG", "NAZ")),
             x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "SNAI")))



    # -------------------------------------------- #
    # report

    # report
    report <- perimetro_po %>%
      group_by(x_CICLO, FLT, x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise(RIS = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
      mutate(RIS = RIS / 1000000) %>%
      full_join(perimetro %>%
                  group_by(x_CICLO, FLT, x_AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
                  summarise(N = n(),
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE),
                            COE = sum(COE, na.rm = TRUE)) %>%
                  left_join(perimetro %>%
                              group_by(OC_CODICE_PROGRAMMA, OC_STATO_PROCEDURALE) %>%
                              summarise(COE = sum(COE, na.rm = TRUE)) %>%
                              spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                            by = "OC_CODICE_PROGRAMMA"),
                by = c("x_CICLO", "FLT", "x_AMBITO", "OC_CODICE_PROGRAMMA")) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      arrange(FLT, x_AMBITO, OC_CODICE_PROGRAMMA)

    # recupera x_PROGRAMMA
    # MEMO: serve perché x_PROGRAMMA in report viene solo da progetti

    # patch per x_PROGRAMMA
    report <- report %>%
      left_join(po_riclass %>%
                  select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA),
                by = "OC_CODICE_PROGRAMMA") %>%
      mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA)) %>%
      select(-x_PROGRAMMA_2)

    # riordina variabili e record
    temp <- names(report)

    report <- report %>%
      arrange(FLT, x_AMBITO, desc(RIS)) %>%
      select(c("x_CICLO",	"x_AMBITO", "x_PROGRAMMA", "RIS", temp[7:17], "FLT"))


    # -------------------------------------------- #
    # export

    if (debug == TRUE) {
      # dati attuazione
      export <- perimetro %>%
        select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
               FLT,
               OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA,
               x_MACROAREA, x_REGIONE,
               OC_STATO_PROCEDURALE, # MEMO: assente per dati di giugno
               OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO, COSTO_RENDICONTABILE_UE, IMPEGNI, TOT_PAGAMENTI)

      write.csv2(export, file.path(TEMP, "dati.csv"), row.names = FALSE)

      # dati programmazione
      write.csv2(perimetro_po, file.path(TEMP, "db.csv"), row.names = FALSE)

      # report
      write.csv2(report, file.path(TEMP, "report.csv"), row.names = FALSE)
    }

    return(report)

  }
}


#' Esporta report regionale in Excel
#'
#' Esporta report regionale in Excel
#'
#' @param report Dataset prodotto con report_regione().
#' @param regione Regione di interesse.
#' @param bimestre Bimestre di riferimento in formato OC.
#' @return Il dataset con il report regionale in Excel.
export_report_regione <- function(report, regione, bimestre=bimestre, OUTPUT=OUTPUT) {

  # libs
  require("openxlsx")

  # preps
  regio <- toupper(regione) %>%
    sub("-", "", .) %>%
    sub(" ", "", .) %>%
    sub("'", "", .)

  reg_block <- report %>%
    filter(FLT == "REG") %>%
    ungroup() %>%
    select(-FLT)

  naz_block <- report %>%
    filter(FLT == "NAZ") %>%
    ungroup() %>%
    select(-FLT)

  temp <- str_glue("REGIONE {regio} – QUADRO REGIONALE PROGRAMMAZIONE 2014-2020")
  titolo <- data_frame(titolo = c(temp))
  nome_file <- str_glue("REPORT_{regio}_{bimestre}.xlsx")

  # load
  wb <- loadWorkbook(system.file("extdata", "report_regione.xlsx", package = "octk", mustWork = TRUE))

  # rename foglio
  names(wb) <- regio

  # scrive titolo tabella
  writeData(wb, sheet = regio, x = titolo, startCol = 1, startRow = 2, colNames = FALSE)

  # scrive dati per REG
  writeData(wb, sheet = regio, x = reg_block, startCol = 1, startRow = 6, colNames = FALSE)

  # scrive dati per NAZ
  writeData(wb, sheet = regio, x = naz_block, startCol = 1, startRow = 16, colNames = FALSE)

  # salva
  saveWorkbook(wb, file = file.path(OUTPUT, nome_file), overwrite = TRUE)

}

