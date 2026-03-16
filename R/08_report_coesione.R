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
make_report_programmi_coesione_evo_macro <- function(perimetro, usa_meuro=FALSE, show_cp=FALSE, use_eu=FALSE, use_flt=TRUE, 
                                                     use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                     export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- operazioni
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
      filter(FLAG_MONITORAGGIO == 1)
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10), OC_FLAG_AGGREGATO == 0)
  }
  
  # patch YEI su programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) 
  
  # patch YEI su attuazione
  perimetro <- perimetro %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO))
  
  # filtro programmazione
  programmi <- programmi %>%
    filter(x_AMBITO != "FEASR", x_AMBITO != "CTE")
  
  # integra sezione programmazione
  programmi <- programmi %>%
    mutate(x_SEZIONE = case_when(x_GRUPPO == "PSC" & COD_LIVELLO_1 == "ORD" ~ "SO",
                                 x_GRUPPO == "PSC" & COD_LIVELLO_1 == "SEZ_SPEC_1_COVID" ~ "SS_1",
                                 x_GRUPPO == "PSC" & COD_LIVELLO_1 == "SEZ_SPEC_2_FS" ~ "SS_2",
                                 x_GRUPPO == "PSC" & COD_LIVELLO_1 == "CIS" ~ "SO_CIS",
                                 x_GRUPPO == "ACCORDI" & grepl("Anticipazioni", x_PROGRAMMA) ~ "ANT",
                                 x_GRUPPO == "ACCORDI" & grepl("FDR", x_PROGRAMMA) ~ "COMP",
                                 x_GRUPPO == "ACCORDI" & grepl("Ordinario", x_PROGRAMMA) ~ "ORD",
                                 x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCSTRCAMPANIA" ~ "STRAL2",
                                 x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCBAGNCAMPANIA" ~ "STRAL3",
                                 TRUE ~ NA_character_))
  
  # filtro attuazione
  perimetro <- perimetro %>%
    filter(x_AMBITO != "FEASR", x_AMBITO != "CTE") 
  
  # integra sezione attuazione
  # perimetro %>% filter(x_GRUPPO == "ACCORDI") %>% count(x_CICLO, x_GRUPPO, x_LIVELLO_0)
  # perimetro %>% filter(x_GRUPPO == "PSC") %>% count(x_CICLO, x_GRUPPO, x_LIVELLO_0)
  if (!("X_SEZIONE" %in% names(perimetro))) {
    perimetro <- perimetro%>% 
      mutate(x_SEZIONE = case_when(x_GRUPPO == "PSC" & grepl("SOCIS", x_LIVELLO_0) ~ "SO_CIS",
                                   x_GRUPPO == "PSC" & grepl("SO", x_LIVELLO_0) & x_CICLO == "2021-2027" ~ "ANT",
                                   x_GRUPPO == "PSC" & grepl("SO", x_LIVELLO_0) ~ "SO",
                                   x_GRUPPO == "PSC" & grepl("SS_1", x_LIVELLO_0) ~ "SS_1",
                                   x_GRUPPO == "PSC" & grepl("SS_2", x_LIVELLO_0) ~ "SS_2",
                                   x_GRUPPO == "ACCORDI" & grepl("Comp", x_LIVELLO_0) ~ "COMP",
                                   x_GRUPPO == "ACCORDI" & grepl("Ord", x_LIVELLO_0) ~ "ORD",
                                   x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCSTRCAMPANIA" ~ "STRAL2",
                                   x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCBAGNCAMPANIA" ~ "STRAL3",
                                   TRUE ~ NA_character_))
  }
  
  # programmazione
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_SEZIONE) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>%
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  
  # attuazione
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_SEZIONE) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  appo <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_SEZIONE, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_SEZIONE")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_SEZIONE")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, replace_na, replace=0) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_SEZIONE, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`)
  
  out <- appo %>%
    filter(RISORSE > 0)
  
  chk <- appo %>%
    filter(RISORSE <= 0 | is.na(RISORSE))
  
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
  
  write.xlsx(chk, file.path(TEMP, "chk_risorse_0_report_programmi.xlsx"))
  
  return(out)
}


#' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param programmi Report da make_report_programmi_coesione_evo_macro()
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export Vuoi salvare il file?
#' @param export_sum Vuoi salvare il file con la sintesi per ciclo/ambito?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_programmi_coesione_evo_macro <- function(programmi=NULL, dati_new=NULL, dbcoe_new=NULL, dati_old, dbcoe_old, 
                                                        usa_meuro = FALSE, show_cp = FALSE, use_eu = FALSE,
                                                        use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE, export_sum=FALSE){
  
  # DEBUG:
  # dati_new = "20251031"
  # dbcoe_new="20251031.00"
  # dati_old = "20250831"
  # dbcoe_old="20250831.00"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # use_flt=TRUE
  # show_cp=FALSE
  # usa_meuro=FALSE
  # use_eu=FALSE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  if (is.null(programmi)) {
    DATA1 <- file.path(dirname(DATA), dati_new)
    macroaree1 <- load_operazioni_evo_macro(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
    DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
    programmi_new <- make_report_programmi_coesione_evo_macro(macroaree1, usa_meuro=usa_meuro, use_eu=use_eu, use_flt=use_flt, show_cp=show_cp, 
                                                              use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc,
                                                              export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  } else {
    programmi_new <- programmi
  }
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_operazioni_evo_macro(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_evo_macro(macroaree2, usa_meuro=usa_meuro, use_eu=use_eu, use_flt=use_flt, show_cp=show_cp, 
                                                            use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc,
                                                            export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_SEZIONE) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_SEZIONE)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO", "x_PROGRAMMA", "x_SEZIONE"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  if (export_sum==TRUE) {
    chk <- programmi_new %>%
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
    
    write.xlsx(chk, file.path(TEMP, paste0("chk_delta_coesione_summary_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
    
  }
  
  return(out)
  
}


