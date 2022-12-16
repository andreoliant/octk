

# ----------------------------------------------------------------------------------- #
# cicli_temi

report_cicli_temi <- function(perimetro, debug=FALSE) {
  
  cicli_temi <- perimetro %>%
    group_by(x_CICLO, x_TEMA) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  # %>%
  # refactor_ambito(.) %>%
  # refactor_ciclo(.)
  
  if (debug == TRUE) {
    cicli_temi %>%
      write.csv2(file.path(TEMP, "cicli_temi.csv"), na = "", row.names = FALSE)
  }
  
  return(cicli_temi)
  
}



# ----------------------------------------------------------------------------------- #
# cicli_ambiti

report_cicli_ambiti <- function(perimetro, debug=FALSE) {
  
  cicli_ambiti <- perimetro %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  # %>%
  #   refactor_ambito(.) %>%
  #   refactor_ciclo(.)
  
  if (debug == TRUE) {
    cicli_ambiti %>%
      write.csv2(file.path(TEMP, "cicli_ambiti.csv"), na = "", row.names = FALSE)
  }
  
  return(cicli_ambiti)
  
}



# ----------------------------------------------------------------------------------- #
# regioni

report_regioni <- function(perimetro, debug=FALSE) {
  
  
  
  # # OLD:
  # # # defactor
  # # perimetro <- perimetro %>%
  # #   mutate(x_TEMA = as.character(x_TEMA))
  # # # MEMO: evita warning quando aggiungo "Totale"
  # #
  # # # semplifica non-regioni
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  # 
  # # NEW BLOCK
  # if (!any(names(perimetro) == "COD_REGIONE")) {
  #   perimetro <- perimetro %>%
  #     left_join(progetti %>%
  #                 select(COD_LOCALE_PROGETTO, COD_REGIONE, DEN_REGIONE),
  #               by = "COD_LOCALE_PROGETTO")
  # }
  # 
  # # regioni
  # appo <- perimetro %>%
  #   mutate(DEN_REGIONE = ifelse(COD_REGIONE %in% c(reg_cn, reg_sud), DEN_REGIONE, "ALTRO TERRITORIO"))
  # # MEMO: semplifica DEN_REGIONE diversi da vera Regione in "ALTRO TERRITORIO"
  
  appo <- perimetro
  
  regioni <- appo %>%
    group_by(x_MACROAREA, x_REGIONE) %>%
    # group_by(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_MACROAREA, x_REGIONE) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/macroarea/regione
    # bind_rows(appo %>%
    #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
    #                       PAG = sum(PAG, na.rm = TRUE))) %>%
    # totali per ciclo + classe/macroarea/regione
    # bind_rows(appo %>%
    #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/macroarea/regione
  # bind_rows(appo %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per macroarea/regione
  # bind_rows(appo %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", x_MACROAREA, x_REGIONE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  as.data.frame() %>%
    # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
    # mutate(x_MACROAREA = factor(x_MACROAREA, levels=c("Sud", "Centro-Nord", "Nazionale", "Trasversale", "Estero"))) %>%
    refactor_macroarea(.) %>%
    mutate(x_REGIONE = factor(x_REGIONE, levels = c(names(reg_cn), names(reg_sud), "ALTRO TERRITORIO"))) %>%
    # arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_MACROAREA, x_REGIONE)
    arrange(x_MACROAREA, x_REGIONE)
  
  
  if (debug == TRUE) {
    regioni %>%
      write.csv2(file.path(TEMP, "regioni.csv"), na = "", row.names = FALSE)
  }
  
  return(regioni)
  
}



# ----------------------------------------------------------------------------------- #
# dimensioni

report_dimensioni <- function(perimetro, debug=FALSE) {
  
  # dimensioni
  dimensioni <- perimetro %>%
    group_by(x_DIM_FIN) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) # %>%
  # totali per ciclo/fondo + classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per ciclo + classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # as.data.frame() %>%
  # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
  # mutate(x_DIM_FIN = factor(x_TEMA_FIN, levels = c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty"))) %>%
  # arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_TEMA_FIN)
  
  if (debug == TRUE) {
    dimensioni %>%
      write.csv2(file.path(TEMP, "dimensioni.csv"), na = "", row.names = FALSE)
  }
  
  return(dimensioni)
  
  
}


# ----------------------------------------------------------------------------------- #
# stati procedurali
# MEMO: nuova variabile usata per IDRICO e DISSESTO


report_stati <- function(perimetro, debug=FALSE) {
  
  # stato per CP
  stati <- perimetro %>%
    group_by(OC_STATO_PROCEDURALE) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/stato
    # bind_rows(perimetro %>%
    #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
    #                       PAG = sum(PAG, na.rm = TRUE))) %>%
    # totali per ciclo + classe/stato
    # bind_rows(perimetro %>%
    #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/stato
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per stato
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", OC_STATO_PROCEDURALE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # as.data.frame() %>%
  # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
  mutate(OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE,
                                       levels =  c("Non avviato",
                                                   "In avvio di progettazione",
                                                   "In corso di progettazione",
                                                   "In affidamento",
                                                   "In esecuzione",
                                                   "Eseguito",
                                                   "Non determinabile"))) %>%
    arrange(OC_STATO_PROCEDURALE)
  
  
  if (debug == TRUE) {
    stati %>%
      write.csv2(file.path(TEMP, "stati.csv"), na = "", row.names = FALSE)
  }
  
  return(stati)
  
  
  
}


# ----------------------------------------------------------------------------------------- #
#workflow


workflow_report <- function(clp_csv, report_ls=NULL, progetti=NULL, use_coe=TRUE, operazioni=NULL, 
                            tema=NULL, livelli_tema=NULL, nome_file=NULL, use_template=FALSE, debug=FALSE) {
  
  # DEBUG:
  # livelli_tema <- c("Cultura", "Natura", "Turismo")
  
  if (is.null(report_ls)) {
    report_ls <- c("report_cicli_temi", "report_cicli_ambiti", "report_regioni", "report_dimensioni", "report_stati")
  }
  
  # switch per variabili finanziarie
  if (use_coe == TRUE) {
    if (is.null(operazioni)) {
      operazioni <- load_operazioni(bimestre) 
    }
    
    appo <- clp_csv %>%
      left_join(operazioni %>%
                  select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
                         x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ, x_MACROAREA, x_REGIONE,
                         COE, COE_IMP, COE_PAG,
                         OC_STATO_PROCEDURALE,
                         OC_COD_TEMA_SINTETICO),
                by = "COD_LOCALE_PROGETTO") %>%
      rename(CP = COE, 
             IMP = COE_IMP, 
             PAG = COE_PAG)
  } else {
    
    
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, light=TRUE)
    }
    
    appo <- clp_csv %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
                         x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ, x_MACROAREA, x_REGIONE,
                         OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI,
                         OC_STATO_PROCEDURALE,
                         # DEV: aggiungere beneficiario
                         OC_COD_TEMA_SINTETICO),
                by = "COD_LOCALE_PROGETTO") %>%
      rename(CP = OC_FINANZ_TOT_PUB_NETTO, 
             IMP = IMPEGNI, 
             PAG = TOT_PAGAMENTI)
  }
  
  # DEV: qui potrebbe essere necessario aggiungere campi da "progetti" a "operazioni", quindi "progetti" andrebbe importato prima di if
  
  # switch per tema
  if (is.null(tema)) {
    appo1 <- appo %>%
      rename(x_TEMA = OC_COD_TEMA_SINTETICO)
    
    if (is.null(livelli_tema)) {
      livelli_tema <- unique(appo$OC_COD_TEMA_SINTETICO)
    }
    
    # TODO: qui va ricodificato codice con descrizione
    # TODO: qui va iserito factor
  } else if (tema == "CLASSE") {
    
    if (is.null(livelli_tema)) {
      livelli_tema <- unique(appo$CLASSE)
    }
    
    appo1 <- appo %>%
      rename(x_TEMA = CLASSE) %>%
      mutate(x_TEMA = factor(x_TEMA, levels = livelli_tema))
  } 
  
  # dimensione finanziaria
  appo2 <- get_dim_fin(df = appo1, debug_mode=FALSE) 
  
  
  # export
  export_report_edit(perimetro = appo2, report_ls = report_ls, nome_file, debug, use_template) 
  
}












#' Wrapper per report con lista di contenuti editabile
#'
#' Wrapper per report con lista di contenuti editabile.
#'
#' @param perimetro Dataset da workflow_report
#' @param report_ls Elenco dei report da generare
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_[1], QUERY_[2], QUERY_[N] e TIPO_QUERY.
export_report_edit <- function(perimetro, report_ls, nome_file=NULL, debug=FALSE, use_template=FALSE) {
  
  # report_ls <- c("report_cicli_temi", "report_cicli_ambiti", "report_regioni", "report_dimensioni", "report_stati")
  tab_list <- list()
  
  appo <- perimetro
  # appo <- appo2
  
  # query
  for (q in report_ls) {
    print(q)
    tab_list[[q]] <- do.call(q, list(appo, debug))
  }
  
  # libs
  library("openxlsx")
  
  if (is.null(nome_file)) {
    nome_file <- "elaborazione.xlsx"
  }
  
  if (use_template == FALSE) {
    # write all tables
    # MEMO: usato al primo giro per creare template (poi integrato a mano)
    write.xlsx(tab_list, file = file.path(OUTPUT, nome_file), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
    # CHK: verificare numero righe con formato...
  } else {
    
    # edit template
    # wb <- loadWorkbook(file.path(src_path, "elab_template.xlsx"))
    # wb <- loadWorkbook(system.file("extdata", "elab_template.xlsx", package = "oc", mustWork = TRUE))
    wb <- loadWorkbook(system.file("extdata", "elab_template.xlsx", package = "octk", mustWork = TRUE))
    for (i in seq_along(tab_list)) {
      print(names(tab_list)[i])
      removeTable(wb = wb, sheet = names(tab_list)[i], table = getTables(wb, sheet = names(tab_list)[i]))
      writeDataTable(wb, sheet = names(tab_list)[i], x = tab_list[[i]], stack = TRUE)
    }
    saveWorkbook(wb, file = file.path(OUTPUT, nome_file), overwrite = TRUE)
    
    # DEV: inserire formati
    
  }
}
