


#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param progetti dataset di tipo "progetti" che serve per integrare CP, impegni e pagamenti totali.
#' @return Un file csv con apertura per programma, con RISORSE, COE, COE_IMPe, COE_PAG e con COE per fase procedurale.
make_report_programmi_coesione <- function(perimetro, usa_meuro=FALSE, show_cp=FALSE, use_eu=FALSE, use_flt=TRUE, 
                                                   use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                   export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  po <- octk::po_riclass
  
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  message("programmi caricato")
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)%>%
      mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
  }
  
  # patch YEI su programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI su attuazione
  perimetro <- perimetro %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # programmazione
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  
  # attuazione
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  # chk programmi con attuazione e risorse 0
  chk <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`) %>%
    filter(RISORSE == 0 | is.na(RISORSE)) 
  write.xlsx(chk, file.path(TEMP, "chk_programmi_risorse_0.xlsx"))
  
  if (dim(chk)[1]>0){
    message("WARNING: sono presenti righe con risore 0 e attuazione valorizzata!")
  }
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`) %>%
    filter(RISORSE > 0)
  
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    out <- out %>% 
      select(-RISORSE_UE)
  } 
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_cp2.xlsx"))
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi.xlsx"))
    }
  }
  
  return(out)
}




#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx in OUTPUT?
#' @param progetti dataset di tipo "progetti" da utilizzare per show_cp == TRUE
#' @return Un file csv con apertura per programma e fase procedurale.
#' @note Nel report restano per definizione righe con risorse 0, che derivano da errate imputazioni di macroaree.
make_report_programmi_macroaree_coesione <- function(perimetro, usa_meuro=FALSE, use_eu=FALSE,
                                                             use_flt=FALSE, use_cicli_psc=FALSE,
                                                             use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                             show_cp=FALSE, export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  # OLD:
  # po <- octk::po_riclass
  # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
  
  # NEW:
  po <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))%>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
  
  # programmazione
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)%>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
    
  }
  
  # patch YEI programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # refactor macroarea
  programmi <- refactor_macroarea(programmi)
  perimetro <- refactor_macroarea(perimetro)
  
  # patch YEI attuazione 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  # DEV: qui raddoppia per ogni progetto su più macroaree
  
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea(.)%>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, 
           RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP,IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`)
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  }
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree_cp2.xlsx"), rowNames = FALSE)
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree.xlsx"), rowNames = FALSE)
    }
  }
  
  return(out)
}

#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono come in 'programmi', vale solo se 'programmi' è in euro
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre
  
  # fix
  programmi <- programmi %>%
    mutate(N_CLP = N) #,
  # IMP = 0,
  # PAG = 0)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
    
  } else {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
  }
  
  
  
  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }
  
  return(report)
}


#' Esporta report per ciclo, ambito e macroarea con dati coesione
#'
#' Report con apertura ciclo, ambito e macroarea.
#' E' costruito su operazioni e dati coesione.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_macroaree_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo, ambto e macroarea.
make_report_macroaree_coesione <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEBUG:
  # programmi <- programmi_macroaree
  
  report <- programmi  %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    arrange(x_CICLO, x_AMBITO, x_MACROAREA)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N) 
      
    }
  } else {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N) 
      
    }
  }
  
  
  
  
  out <- report %>% 
    filter(# x_AMBITO != "FEAMP", 
      x_AMBITO != "FEASR") %>% 
    arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA)
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "report_macroaree.csv"))
  }
  
  return(out)
}



#' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_programmi_coesione <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                      use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO", "x_PROGRAMMA"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}


' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_sintesi_coesione <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                    use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(x_CICLO, x_AMBITO)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_sintesi_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}

