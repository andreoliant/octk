# OC > Toolkit
# Report bimestrale e controlli su anomalie nelle variazioni


# ----------------------------------------------------------------------------------- #

prep_perimetro_bimestre <- function(bimestre, usa_meuro=TRUE) {

  # TODO: inserire switch per ciclo con calcolo di CP/COE direttamente su operazioni per 1420 (ora si può fare solo per impegni e pagamenti)

  # loads
  progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)

  # switch variabile COE
  # MEMO: COSTO_RENDICONTABILE_UE e OC_FINANZ_STATO_FSC_NETTO contengono NA quindi converto numerici in 0
  perimetro <- progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))

  # meuro
  if (usa_meuro == TRUE) {
    perimetro <- perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    perimetro <- perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }

  # simply
  perimetro <- get_simply_non_loc(perimetro)

  return(perimetro)
}

make_report_bimestre <- function(bimestre, perimetro, last_bimestre, last_data_path,
                                 usa_macroaree=FALSE, usa_meuro=TRUE, export=FALSE) {

  # last_bimestre <- "20181231"
  # last_data_path = "/Users/aa/dati/oc/20181231"

  # progetti
  if (missing(perimetro)) {
    perimetro <- prep_perimetro_bimestre(bimestre = bimestre, usa_meuro=usa_meuro)
  }

  # risorse
  risorse <- make_risorse(usa_713=TRUE, usa_macroaree=usa_macroaree) %>%
    mutate(RIS = round(RIS / 1000000, 1))


  # ----------------- #
  # report base

  # prep
  if (usa_macroaree == FALSE) {
    report <- risorse %>%
      full_join(perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO) %>%
                  summarise(N = n(),
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE),
                            COE = sum(COE, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO")) %>%
      arrange(desc(x_CICLO), x_AMBITO) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  } else {
    report <- risorse %>%
      full_join(perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
                  summarise(N = n(),
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE),
                            COE = sum(COE, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
      arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  }

  # ----------------- #
  # bimestre precedente

  # setup
  # rm(progetti)
  # last_bimestre <- "20181231"
  # last_data_path = "/Users/aa/dati/oc/20181231"
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }

  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)

  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))

  # meuro
  last_perimetro <- last_perimetro %>%
    mutate(CP = CP / 1000000,
           COE = COE / 1000000,
           IMP = IMPEGNI / 1000000,
           PAG = TOT_PAGAMENTI / 1000000)

  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)


  # ----------------- #
  # report def

  if (usa_macroaree == FALSE) {
    report_last <- report %>%
      full_join(last_perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO) %>%
                  summarise(COE_LAST = sum(COE, na.rm = TRUE),
                            PAG_LAST = sum(PAG, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO")) %>%
      arrange(desc(x_CICLO), x_AMBITO) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # mutate(COE_DELTA = (COE - COE_LAST) / COE,
      #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
      mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
             PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
      mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
             PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
      mutate(COE_DELTA = if_else(COE_DELTA > 1, 1, COE_DELTA),
             PAG_DELTA = if_else(PAG_DELTA > 1, 1, PAG_DELTA)) %>%
      select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)

  } else {
    report_last <- report %>%
      full_join(last_perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
                  summarise(COE_LAST = sum(COE, na.rm = TRUE),
                            PAG_LAST = sum(PAG, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
      arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      mutate(COE_DELTA = (COE - COE_LAST) / COE,
             PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
      mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
             PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
      mutate(COE_DELTA = if_else(COE_DELTA > 1, 1, COE_DELTA),
             PAG_DELTA = if_else(PAG_DELTA > 1, 1, PAG_DELTA)) %>%
      select(x_CICLO,	x_AMBITO,	x_MACROAREA, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)
  }

  if (export == TRUE) {
    if (usa_macroaree == FALSE) {
      write.csv2(report_last, file.path(TEMP, "report.csv"), row.names = FALSE)
    } else {
      write.csv2(report_last, file.path(TEMP, "report_macoaree.csv"), row.names = FALSE)
    }
  }

  return(report_last)
}






#' Esporta report per Programmi
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param add_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi <- function(perimetro, usa_meuro=FALSE, add_713=FALSE, focus="elab", export=FALSE) {

  # TODO:
  # correggere "FESR-FSE"
  # inserire verifica bimestre precedente

  # appo <- perimetro %>%
  #   mutate(x_PROGRAMMA = factor(x_PROGRAMMA, levels = c("PATTO EMILIA-ROMAGNA", "PATTO LOMBARDIA", "PATTO LAZIO",
  #                                                       "PATTO BOLOGNA", "PATTO FIRENZE", "PATTO GENOVA", "PATTO MILANO", "PATTO VENEZIA",
  #                                                       "PATTO ABRUZZO", "PATTO BASILICATA", "PATTO CALABRIA",  "PATTO CAMPANIA",
  #                                                       "PATTO MOLISE", "PATTO PUGLIA", "PATTO SARDEGNA",  "PATTO SICILIA",
  #                                                       "PATTO BARI", "PATTO CAGLIARI", "PATTO CATANIA",  "PATTO MESSINA",
  #                                                       "PATTO NAPOLI", "PATTO PALERMO", "PATTO REGGIO CALABRIA")))

  appo <- perimetro

  temp <- paste0(focus, "_programmi.csv")

  programmi <- init_programmazione(usa_temi=FALSE, add_713=add_713, export=FALSE)

  if (usa_meuro == TRUE) {
    programmi<- programmi %>%
      mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  }

  # spalla con risorse (per tutti i programmi)
  # spalla <- programmi %>%
  #   group_by(OC_CODICE_PROGRAMMA) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, RISORSE)

  # OLD:
  # spalla <- octk::po_riclass %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR" #,
  #          # x_AMBITO != "FEAMP"
  #          ) %>%
  #   left_join(programmi %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))

  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2014-2020")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2014-2020")
  programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")

  spalla <- programmi %>%
    mutate(x_GRUPPO = case_when(
      x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "POCN",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "POCN",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "POCR",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "POCR",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Piani nazionali" ~ "PIANI",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Patti per lo sviluppo" ~ "PATTI",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altre assegnazioni CIPE" ~ "ALTRI CIPE",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altro" ~ "ALTRO",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Cofinanziamento SIE" ~ "ALTRO",
      x_CICLO == "2014-2020" & x_AMBITO == "FEAMP" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "YEI" ~ "YEI",
      x_CICLO == "2014-2020" & x_AMBITO == "SNAI" ~ "SNAI",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POIN" ~ "POIN",
      x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Nazionale" ~ "PACN",
      x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Regionale" ~ "PACR",
      # MEMO: per il momento il blocco FSC resta come si trova (e dovrebbe essere coerente con po_riclass)
      TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>%
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, FINANZ_TOTALE_PUBBLICO) %>%
    filter(x_CICLO != "2000-2006",
           x_AMBITO != "CTE",
           x_AMBITO != "FEASR" #,
           # x_AMBITO != "FEAMP"
    ) %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA")


  # report
  out <- spalla %>%
    # TODO: inserire filtro sopra!!! Non posso usare solo quello con right_join su attuazione
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(OC_CODICE_PROGRAMMA, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  out <- out %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA", suffix = c("", ".x")) %>%
    as_tibble(.) %>%
    mutate(x_CICLO = as.character(x_CICLO),
           x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_CICLO = if_else(is.na(x_CICLO), x_CICLO.x, x_CICLO),
           x_AMBITO = if_else(is.na(x_AMBITO), x_AMBITO.x, x_AMBITO),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO.x, x_GRUPPO),
           x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA.x, x_PROGRAMMA)) %>%
    select(-x_CICLO.x, -x_AMBITO.x, -x_GRUPPO.x, -x_PROGRAMMA.x) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, CP, IMP, PAG, COE,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)

  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}



#' Integra report per Programmi con delta da un bimestre precedente
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
add_delta_programmi <- function(bimestre, programmi, last_bimestre, last_data_path,
                                 usa_meuro=FALSE, focus="delta", export=FALSE) {

  # if (missing(programmi)) {
  #
  #   programmi <- make_report_programmi(perimetro, focus="chk", usa_meuro=usa_meuro, export=FALSE)
  # }
  # DEV: eliminato perché non è definito "perimetro"

  # ----------------- #
  # bimestre precedente

  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }

  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)

  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))

  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }

  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)

  appo <- last_perimetro

  # temp <- paste0(focus, "_programmi.csv")
  #
  # programmi <- init_programmazione(usa_temi=FALSE, export=FALSE)
  #
  # if (usa_meuro == TRUE) {
  #   programmi<- programmi  %>%
  #     mutate(RISORSE = round(RISORSE / 1000000, 1))
  # }

  # spalla con risorse (per tutti i programmi)
  # spalla <- programmi %>%
  #   group_by(OC_CODICE_PROGRAMMA) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, RISORSE)

  # spalla <- octk::po_riclass %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR",
  #          x_AMBITO != "FEAMP")
  # %>%
  #   left_join(programmi %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))

  # MEMO: non serve ripassare per "report_cd_programmi()" perché ho già risorse

  # report
  out <- programmi %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  # %>%
  #   select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)

  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}


#' Esporta report per Programmi (NEW)
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param add_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_2 <- function(perimetro, usa_meuro=FALSE, add_713=FALSE, focus="elab", export=FALSE) {

  appo <- perimetro

  temp <- paste0(focus, "_programmi.csv")

  # DEBUG: add_713 <- TRUE
  programmi <- init_programmazione(usa_temi=FALSE, add_713=add_713, export=FALSE)

  if (usa_meuro == TRUE) {
    programmi<- programmi %>%
      mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  }

  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")

  spalla <- octk::po_riclass %>%
    filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
           x_CICLO != "2000-2006",
           x_AMBITO != "CTE",
           x_AMBITO != "FEASR") %>%
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
    separate_rows(x_AMBITO, sep="-") %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    # OLD:
    # separate(x_AMBITO, c("F1", "F2"), sep = "-", remove=FALSE, fill = "right") %>%
    # gather("x_AMBITO", )
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    left_join(programmi %>%
                # MEMO: patch per factor di x_AMBITO
                mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                              "FEAD", "FAMI", "CTE"))) %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))

  # spalla <- programmi %>%
  #   mutate(x_GRUPPO = case_when(
  #     x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "POCN",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "POCN",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "POCR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "POCR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Piani nazionali" ~ "PIANI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Patti per lo sviluppo" ~ "PATTI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altre assegnazioni CIPE" ~ "ALTRI CIPE",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altro" ~ "ALTRO",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Cofinanziamento SIE" ~ "ALTRO",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FEAMP" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "YEI" ~ "YEI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "SNAI" ~ "SNAI",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POIN" ~ "POIN",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Nazionale" ~ "PACN",
  #     x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Regionale" ~ "PACR",
  #     # MEMO: per il momento il blocco FSC resta come si trova (e dovrebbe essere coerente con po_riclass)
  #     TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, FINANZ_TOTALE_PUBBLICO) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR" #,
  #          # x_AMBITO != "FEAMP"
  #   ) %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA")

  # spalla %>% count(x_CICLO, x_AMBITO, x_GRUPPO)

  # report
  out <- spalla %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
                # MEMO: patch per factor di x_AMBITO
                mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                              "FEAD", "FAMI", "CTE"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  out <- out %>%
    # left_join(octk::po_riclass %>%
    #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
    #           by = "OC_CODICE_PROGRAMMA", suffix = c("", ".x")) %>%
    as_tibble(.) %>%
    mutate(x_CICLO = as.character(x_CICLO),
           x_AMBITO = as.character(x_AMBITO)) %>%
    # mutate(x_CICLO = if_else(is.na(x_CICLO), x_CICLO.x, x_CICLO),
    #        x_AMBITO = if_else(is.na(x_AMBITO), x_AMBITO.x, x_AMBITO),
    #        x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO.x, x_GRUPPO),
    #        x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA.x, x_PROGRAMMA)) %>%
    # select(-x_CICLO.x, -x_AMBITO.x, -x_GRUPPO.x, -x_PROGRAMMA.x) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, CP, IMP, PAG, COE,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)

  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}


#' Integra report per Programmi con delta da un bimestre precedente (NEW)
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
add_delta_programmi_2 <- function(bimestre, programmi, last_bimestre, last_data_path,
                                usa_meuro=FALSE, focus="delta", export=FALSE) {

  # bimestre precedente
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }

  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)

  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))

  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }

  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)

  appo <- last_perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))

  # report
  out <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  # %>%
  #   select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)

  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}
