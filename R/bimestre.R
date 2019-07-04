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
      mutate(COE_DELTA = (COE - COE_LAST) / COE,
             PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
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




delta_programmi <- function(bimestre, programmi, last_bimestre, last_data_path,
                            usa_meuro=FALSE, focus="delta", export=FALSE) {

  if (missing(programmi)) {
    programmi <- report_cd_programmi(perimetro, focus="chk", usa_meuro=usa_meuro, export=FALSE)
  }

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

  # report
  out <- programmi %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE,
           PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1, PAG_DELTA))
  # %>%
  #   select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)

  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}
