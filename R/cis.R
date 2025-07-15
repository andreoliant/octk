# Funzioni del blocco CIS


#' Carica dati interventi CIS
#'
#' Carica dati interventi CIS
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_cis <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_CIS.xlsx"), guess_max=100000)
  return(interventi)
}


#' Workflow per analisi CIS
#'
#' Crea dataset di base per analisi programmazione vs attuazione CIS
#' 
#' @param bimestre Bimestre di OpenCoesione
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param operazioni Dataset da operazioni_light
#' @param progetti Dataset da progetti_light
#' @param ritardi Dataset da setup_ritardi()
#' @param debug Vuoi verificare il DB e il mappging con l'attuazione?
#' @param export Vuoi esportare in TEMP in xlsx?
#' @return Dataset di base per analisi programmazione vs attuazione CIS
workflow_cis <- function(bimestre, interventi, operazioni, ritardi=NULL, debug=FALSE, export=FALSE) {
  
  # chk
  # interventi %>% 
  #   group_by(AMBITO) %>% 
  #   summarise(FINANZ_TOT = sum(FINANZ_TOT, na.rm = TRUE),  
  #             FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),  
  #             FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),  
  #             FINANZ_UE = sum(FINANZ_UE, na.rm = TRUE),  
  #             FINANZ_PNRR = sum(FINANZ_PNRR, na.rm = TRUE),  
  #             FINANZ_ALTRO = sum(FINANZ_ALTRO, na.rm = TRUE))

  # mapping
  cis <- interventi %>% 
    mutate(RISORSE_COE = case_when(AMBITO == "FSC" ~ FINANZ_FSC,
                                   AMBITO == "FESR" ~ FINANZ_UE,
                                   AMBITO == "PAC" ~ FINANZ_FDR,
                                   AMBITO == "PNRR" ~ FINANZ_PNRR,
                                   TRUE ~ 0),
           RISORSE_TOT = FINANZ_TOT) %>% 
    select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA=DESCRIZIONE_PROGRAMMA, TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE, 
           RISORSE_COE, RISORSE_TOT) %>%
    left_join(operazioni %>% 
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COE, COE_IMP, COE_PAG, CP, IMP, PAG),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  
  # integra ritardi
  if (!is.null(ritardi)) {
    cis <- cis %>% 
      left_join(ritardi %>% 
                  select(COD_LOCALE_PROGETTO, 
                         FASE_CORRENTE, FASE_SEGUENTE, FASE_SEGUENTE_DATA, FASE_SEGUENTE_DATA_OLD, DELTA_DATA_SEGUENTE, 
                         CHK_DATA, FASE_SEGUENTE_RITARDO, CHK_RITARDO, SUM_RITARDO, 
                         x_STATO_PROCEDURALE,
                         DATA_INIZIO_PREV_STUDIO_FATT, DATA_INIZIO_EFF_STUDIO_FATT, DATA_FINE_PREV_STUDIO_FATT, DATA_FINE_EFF_STUDIO_FATT, 
                         DATA_INIZIO_PREV_PROG_PREL, DATA_INIZIO_EFF_PROG_PREL, DATA_FINE_PREV_PROG_PREL, DATA_FINE_EFF_PROG_PREL,
                         DATA_INIZIO_PREV_PROG_DEF, DATA_INIZIO_EFF_PROG_DEF, DATA_FINE_PREV_PROG_DEF, DATA_FINE_EFF_PROG_DEF, 
                         DATA_INIZIO_PREV_PROG_ESEC, DATA_INIZIO_EFF_PROG_ESEC, DATA_FINE_PREV_PROG_ESEC, DATA_FINE_EFF_PROG_ESEC, 
                         DATA_INIZIO_PREV_STIP_ATTRIB, DATA_INIZIO_EFF_STIP_ATTRIB, DATA_FINE_PREV_STIP_ATTRIB, DATA_FINE_EFF_STIP_ATTRIB, 
                         DATA_INIZIO_PREV_ESECUZIONE, DATA_INIZIO_EFF_ESECUZIONE, DATA_FINE_PREV_ESECUZIONE, DATA_FINE_EFF_ESECUZIONE, 
                         DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO
                         ),
                by = "COD_LOCALE_PROGETTO") %>% 
      # integra casistica nei factor per "non monitorati" (righe provenienti da programmazione)
      mutate(x_STATO_PROCEDURALE = as.character(x_STATO_PROCEDURALE)) %>% 
      mutate(x_STATO_PROCEDURALE = if_else(is.na(x_STATO_PROCEDURALE), "Non monitorato", x_STATO_PROCEDURALE)) %>% 
      mutate(x_STATO_PROCEDURALE = factor(x_STATO_PROCEDURALE, levels = c("Non monitorato", "Non avviato", "In avvio di progettazione", 
                                                                          "In corso di progettazione", "In affidamento", "In esecuzione", "Eseguito"))) %>% 
      mutate(CHK_RITARDO = as.character(CHK_RITARDO)) %>% 
      mutate(CHK_RITARDO = if_else(is.na(CHK_RITARDO), "non monitorato", CHK_RITARDO)) %>% 
      # mutate(CHK_RITARDO = factor(CHK_RITARDO, levels = c("concluso", "regolare", "in scadenza", "scaduti", "ritardo", "ritardo grave", "non monitorato"))) %>% 
      mutate(CHK_RITARDO = factor(CHK_RITARDO, levels = c("non monitorato",  "ritardo grave",  "ritardo", "scaduti",  "in scadenza",  "regolare", "concluso"))) %>% 
      mutate(SUM_RITARDO = as.character(SUM_RITARDO)) %>% 
      mutate(SUM_RITARDO = if_else(is.na(SUM_RITARDO), "non monitorato", SUM_RITARDO)) %>% 
      mutate(SUM_RITARDO = factor(SUM_RITARDO, levels = c("non monitorato", "ritardo", "regolare")))
      # mutate(SUM_RITARDO = factor(SUM_RITARDO, levels = c("regolare", "ritardo", "non monitorato")))
      
  }
  
  
  # debug
  if (debug == TRUE) {
    # verifica db
    message("Inizio verifica DB")
    
    # TODO
  }
  
  if (export == TRUE) {
    write.xlsx(cis, file.path(TEMP, "interventi_operazioni_cis.xlsx"))
  }
  
  return(cis)
  
}


#' Report CIS
#' 
#' Report per ogni CIS con diversi livelli di aggregazione
#' 
#' @param bimestre Bimestre di OpenCoesione
#' @param analisi Analisi interventi CIS con programmazione, attuazione e ritardi da workflow_cis()
#' @return Per ogni CIS, un dossier excel in OUTPUT/dossier
make_report_cis_bkp01 <- function(bimestre, analisi) {
  
  # DEBUG:
  # cis <- "CIS VOLARE"
  
  header <- read_xlsx(file.path(INPUT, "header.xlsx"))

  if (!dir.exists(file.path(OUTPUT, "dossier"))) {
    dir.create(file.path(OUTPUT, "dossier"))
  }
  
  lista_cis <- analisi %>% count(CIS) %>% .$CIS
  
  for (cis in lista_cis) {
    print(cis)
    
    # filtro
    appo <- analisi %>% 
      filter(CIS == cis) %>% 
      ungroup()
    
    # sintesi per programma
    programmi <- appo %>% 
      group_by(AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
                COE = sum(COE, na.rm = TRUE), 
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE), 
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>% 
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
      select(AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, 
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) %>% 
      left_join(appo %>% 
                  group_by(x_STATO_PROCEDURALE, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
                  summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                  pivot_wider(id_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "x_STATO_PROCEDURALE", 
                              names_expand = TRUE,
                              values_from = "RISORSE_COE", values_fill = 0),
                by = c("OC_CODICE_PROGRAMMA", "AMBITO", "x_PROGRAMMA"))%>% 
      left_join(appo %>% 
                  group_by(CHK_RITARDO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
                  summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                  pivot_wider(id_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "CHK_RITARDO", 
                              names_expand = TRUE,
                              values_from = "RISORSE_COE", values_fill = 0),
                by = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"))
    
    # sintesi per beneficiario
    beneficiari <- appo %>% 
      group_by(AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE) %>% 
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
                COE = sum(COE, na.rm = TRUE), 
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE), 
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>% 
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
      select(AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE, 
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) %>% 
      left_join(appo %>% 
                  group_by(x_STATO_PROCEDURALE, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE) %>% 
                  summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                  pivot_wider(id_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"), names_from = "x_STATO_PROCEDURALE", 
                              names_expand = TRUE,
                              values_from = "RISORSE_COE", values_fill = 0),
                by = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"))%>% 
      left_join(appo %>% 
                  group_by(CHK_RITARDO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE) %>% 
                  summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                  pivot_wider(id_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"), names_from = "CHK_RITARDO", 
                              names_expand = TRUE,
                              values_from = "RISORSE_COE", values_fill = 0),
                by = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"))
    
    # sintesi per fase procedurale
    iter <-  appo %>% 
      group_by(x_STATO_PROCEDURALE) %>% 
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
                COE = sum(COE, na.rm = TRUE), 
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE), 
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>% 
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
      select(x_STATO_PROCEDURALE, 
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) %>% 
      left_join(appo %>% 
                  group_by(CHK_RITARDO, x_STATO_PROCEDURALE) %>% 
                  summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                  pivot_wider(id_cols = "x_STATO_PROCEDURALE", names_from = "CHK_RITARDO", 
                              names_expand = TRUE,
                              values_from = "RISORSE_COE", values_fill = 0) %>% 
                  arrange(x_STATO_PROCEDURALE),
                by = "x_STATO_PROCEDURALE")
      
    interventi <- appo %>% 
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
      select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, 
             TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE,
             RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG,
             x_STATO_PROCEDURALE,
             FASE_CORRENTE, FASE_SEGUENTE, FASE_SEGUENTE_DATA, FASE_SEGUENTE_DATA_OLD, DELTA_DATA_SEGUENTE, CHK_DATA,
             FASE_SEGUENTE_RITARDO, CHK_RITARDO, 
             # SUM_RITARDO,
             # iter procedurale
             DATA_INIZIO_PREV_STUDIO_FATT, DATA_INIZIO_EFF_STUDIO_FATT, DATA_FINE_PREV_STUDIO_FATT, DATA_FINE_EFF_STUDIO_FATT, 
             DATA_INIZIO_PREV_PROG_PREL, DATA_INIZIO_EFF_PROG_PREL, DATA_FINE_PREV_PROG_PREL, DATA_FINE_EFF_PROG_PREL,
             DATA_INIZIO_PREV_PROG_DEF, DATA_INIZIO_EFF_PROG_DEF, DATA_FINE_PREV_PROG_DEF, DATA_FINE_EFF_PROG_DEF, 
             DATA_INIZIO_PREV_PROG_ESEC, DATA_INIZIO_EFF_PROG_ESEC, DATA_FINE_PREV_PROG_ESEC, DATA_FINE_EFF_PROG_ESEC, 
             DATA_INIZIO_PREV_STIP_ATTRIB, DATA_INIZIO_EFF_STIP_ATTRIB, DATA_FINE_PREV_STIP_ATTRIB, DATA_FINE_EFF_STIP_ATTRIB, 
             DATA_INIZIO_PREV_ESECUZIONE, DATA_INIZIO_EFF_ESECUZIONE, DATA_FINE_PREV_ESECUZIONE, DATA_FINE_EFF_ESECUZIONE, 
             DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO
             ) %>% 
      # converte in data tutte le colonne dell'iter procedurale
      mutate(
        across(
          matches("^DATA_"),       # seleziona tutte le colonne che iniziano con "DATA_"
          ~ ymd(.x)                # applica ymd() a ciascuna di esse
        )
      ) %>% 
      mutate(FASE_SEGUENTE_DATA = ymd(FASE_SEGUENTE_DATA),
             FASE_SEGUENTE_DATA_OLD = ymd(FASE_SEGUENTE_DATA_OLD),
             DELTA_DATA_SEGUENTE = as.integer(DELTA_DATA_SEGUENTE),
             FASE_SEGUENTE_RITARDO = as.integer(FASE_SEGUENTE_RITARDO),
             FASE_CORRENTE = case_when(FASE_CORRENTE == "END" ~ "Fine progetto",
                                       FASE_CORRENTE == "COL_END" ~ "Avvio fine progetto",
                                       FASE_CORRENTE == "COL" ~ "Fine collaudo",
                                       FASE_CORRENTE == "ESEC_COL" ~ "Avvio collaudo",
                                       FASE_CORRENTE == "ESEC" ~ "Fine esecuzione",
                                       FASE_CORRENTE == "STIP_ESEC" ~ "Avvio esecuzione",
                                       FASE_CORRENTE == "STIP" ~ "Fine gara/attribuzione finanziamento",
                                       FASE_CORRENTE == "PE_STIP" ~ "Avvio gara/attribuzione finanziamento",
                                       FASE_CORRENTE == "PE" ~ "Fine progettazione esecutiva",
                                       FASE_CORRENTE == "PD_PE" ~ "Avvio progettazione esecutiva",
                                       FASE_CORRENTE == "PD" ~ "Fine progettazione definitiva",
                                       FASE_CORRENTE == "PP_PD" ~ "Avvio progettazione definitiva",
                                       FASE_CORRENTE == "PP" ~ "Fine progettazione preliminare",
                                       FASE_CORRENTE == "SDF_PP" ~ "Avvio progettazione preliminare",
                                       FASE_CORRENTE == "SDF" ~ "Fine studio di fattibilità",
                                       FASE_CORRENTE == "START" ~ "Avvio progetto"),
             FASE_SEGUENTE = case_when(FASE_SEGUENTE == "END" ~ "Fine progetto",
                                       FASE_SEGUENTE == "COL_END" ~ "Avvio fine progetto",
                                       FASE_SEGUENTE == "COL" ~ "Fine collaudo",
                                       FASE_SEGUENTE == "ESEC_COL" ~ "Avvio collaudo",
                                       FASE_SEGUENTE == "ESEC" ~ "Fine esecuzione",
                                       FASE_SEGUENTE == "STIP_ESEC" ~ "Avvio esecuzione",
                                       FASE_SEGUENTE == "STIP" ~ "Fine gara/attribuzione finanziamento",
                                       FASE_SEGUENTE == "PE_STIP" ~ "Avvio gara/attribuzione finanziamento",
                                       FASE_SEGUENTE == "PE" ~ "Fine progettazione esecutiva",
                                       FASE_SEGUENTE == "PD_PE" ~ "Avvio progettazione esecutiva",
                                       FASE_SEGUENTE == "PD" ~ "Fine progettazione definitiva",
                                       FASE_SEGUENTE == "PP_PD" ~ "Avvio progettazione definitiva",
                                       FASE_SEGUENTE == "PP" ~ "Fine progettazione preliminare",
                                       FASE_SEGUENTE == "SDF_PP" ~ "Avvio progettazione preliminare",
                                       FASE_SEGUENTE == "SDF" ~ "Fine studio di fattibilità",
                                       FASE_SEGUENTE == "START" ~ "Avvio progetto")) %>% 
      mutate(CHK_RITARDO = case_when(CHK_RITARDO == "concluso" ~ "Cronoprogramma concluso",
                                     CHK_RITARDO == "regolare" ~ "Cronoprogramma regolare",
                                     CHK_RITARDO == "in scadenza" ~ "Cronoprogramma in scadenza",
                                     CHK_RITARDO == "scaduti" ~ "Cronoprogramma scaduto (< 60 giorni)",
                                     CHK_RITARDO == "ritardo" ~ "Cronoprogramma in ritardo (> 60 giorni)",
                                     CHK_RITARDO == "ritardo grave" ~ "Cronoprogramma in ritardo grave (> 365 giorni)",
                                     CHK_RITARDO == "non monitorato" ~ "Intervento non monitorato")) %>% 
      mutate(CHK_RITARDO = factor(CHK_RITARDO, levels = c("Intervento non monitorato",
                                                          "Cronoprogramma in ritardo grave (> 365 giorni)", "Cronoprogramma in ritardo (> 60 giorni)", 
                                                          "Cronoprogramma scaduto (< 60 giorni)", 
                                                          "Cronoprogramma in scadenza", "Cronoprogramma regolare", "Cronoprogramma concluso"))) 
      
    
    
    
      

    # NEW: con auto format
    # export
    file_name <- paste0(str_replace(cis, " ", "_"), "_", bimestre,".xlsx")
    data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
    titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
    wb <- createWorkbook()

    write_table_to_wb(
      wb          = wb,
      df          = programmi,
      title       = titolo_base,
      subtitle    = "Sintesi per ambito di programmazione e programma",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      note        = "Nota: ...",
      sheet_name  = "programmi",
      start_row   = 4,
      header_df   = header
    )
    
    write_table_to_wb(
      wb          = wb,
      df          = beneficiari,
      title       = titolo_base,
      subtitle    = "Sintesi per beneficiario e localizzazione",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      note        = "Nota: ...",
      sheet_name  = "beneficiari",
      start_row   = 4,
      header_df   = header
    )
    
    write_table_to_wb(
      wb          = wb,
      df          = iter,
      title       = titolo_base,
      subtitle    = "Sintesi per fase dell'iter procedurale",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      note        = "Nota: ...",
      sheet_name  = "iter",
      start_row   = 4,
      header_df   = header
    )
    
    write_table_to_wb(
      wb          = wb,
      df          = interventi,
      title       = titolo_base,
      subtitle    = "Dettaglio interventi con analisi ritardi su prossima fase dell'iter procedurale",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      note        = "Nota: ...",
      sheet_name  = "interventi",
      start_row   = 4,
      header_df   = header
    )
    
    saveWorkbook(wb, file = file.path(OUTPUT, "dossier", file_name), overwrite = TRUE)
    
    # OLD:
    # # export
    # file_name <- paste0(str_replace(cis, " ", "_"), "_", bimestre,".xlsx")
    # data_bimestre <- format(ymd(bimestre), "%d/%m/%Y") 
    # titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
    # wb <- createWorkbook()
    # 
    # # programmi
    # addWorksheet(wb, "programmi")
    # titolo <- paste0(titolo_base, " - ", "Sintesi per programma")
    # writeData(wb, sheet = "programmi", x = titolo, startCol = 1, startRow = 1)
    # writeData(wb, sheet = "programmi", x = programmi, startCol = 1, startRow = 3, colNames = TRUE)
    # 
    # # beneficiari
    # addWorksheet(wb, "beneficiari")
    # titolo <- paste0(titolo_base, " - ", "Sintesi per beneficiario")
    # writeData(wb, sheet = "beneficiari", x = titolo, startCol = 1, startRow = 1)
    # writeData(wb, sheet = "beneficiari", x = beneficiari, startCol = 1, startRow = 3, colNames = TRUE)
    # 
    # # fasi
    # addWorksheet(wb, "iter")
    # titolo <- paste0(titolo_base, " - ", "Sintesi per fase dell'iter procedurale")
    # writeData(wb, sheet = "iter", x = titolo, startCol = 1, startRow = 1)
    # writeData(wb, sheet = "iter", x = iter, startCol = 1, startRow = 3, colNames = TRUE)
    # 
    # # interventi
    # addWorksheet(wb, "interventi")
    # titolo <- paste0(titolo_base, " - ", "Dettaglio interventi con analisi ritardi")
    # writeData(wb, sheet = "interventi", x = titolo, startCol = 1, startRow = 1)
    # writeData(wb, sheet = "interventi", x = interventi, startCol = 1, startRow = 3, colNames = TRUE)
    # 
    # saveWorkbook(wb, file = file.path(OUTPUT, "dossier", file_name), overwrite = TRUE)

  
  }
  
  # sintesi
  print("Sintesi per tutti i CIS")
  
  # sintesi per cis
  sintesi_cis <- analisi %>% 
    group_by(CIS) %>% 
    summarise(N = n(),
              RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
              COE = sum(COE, na.rm = TRUE), 
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE), 
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>% 
    mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
           p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
           p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
           p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
           p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
           p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
    select(CIS, 
           N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
           RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) %>% 
    left_join(analisi %>% 
                group_by(x_STATO_PROCEDURALE, CIS) %>% 
                summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                pivot_wider(id_cols = "CIS", names_from = "x_STATO_PROCEDURALE", 
                            names_expand = TRUE,
                            values_from = "RISORSE_COE", values_fill = 0),
              by = c("CIS"))%>% 
    left_join(analisi %>% 
                group_by(CHK_RITARDO, CIS) %>% 
                summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                pivot_wider(id_cols = "CIS", names_from = "CHK_RITARDO", 
                            names_expand = TRUE,
                            values_from = "RISORSE_COE", values_fill = 0),
              by = "CIS")
  
  
  # sintesi per programma
  programmi_cis <- analisi %>% 
    group_by(CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(N = n(),
              RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
              COE = sum(COE, na.rm = TRUE), 
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE), 
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>% 
    mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
           p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
           p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
           p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
           p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
           p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
    select(CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, 
           N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
           RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) %>% 
    left_join(analisi %>% 
                group_by(x_STATO_PROCEDURALE, CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
                summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                pivot_wider(id_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "x_STATO_PROCEDURALE", 
                            names_expand = TRUE,
                            values_from = "RISORSE_COE", values_fill = 0),
              by = c("CIS", "OC_CODICE_PROGRAMMA", "AMBITO", "x_PROGRAMMA"))%>% 
    left_join(analisi %>% 
                group_by(CHK_RITARDO, CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
                summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
                pivot_wider(id_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "CHK_RITARDO", 
                            names_expand = TRUE,
                            values_from = "RISORSE_COE", values_fill = 0),
              by = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"))
  
  # export sintesi
  file_name <- paste0("sintesi_cis_", bimestre,".xlsx")
  data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
  titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
  wb <- createWorkbook()
  
  write_table_to_wb(
    wb          = wb,
    df          = sintesi_cis,
    title       = titolo_base,
    subtitle    = "Sintesi per CIS",
    source      = "Fonte: Elaborazione Dipcoes-NUPC",
    note        = "Nota: ...",
    sheet_name  = "cis",
    start_row   = 4,
    header_df   = header
  )
  
  write_table_to_wb(
    wb          = wb,
    df          = programmi_cis,
    title       = titolo_base,
    subtitle    = "Sintesi per CIS e programma",
    source      = "Fonte: Elaborazione Dipcoes-NUPC",
    note        = "Nota: ...",
    sheet_name  = "programmi",
    start_row   = 4,
    header_df   = header
  )
  
  saveWorkbook(wb, file = file.path(OUTPUT, "dossier", file_name), overwrite = TRUE)
  
}


#' Report CIS
#' 
#' Report per ogni CIS con diversi livelli di aggregazione
#' 
#' @param bimestre Bimestre di OpenCoesione
#' @param analisi Analisi interventi CIS con programmazione, attuazione e ritardi da workflow_cis()
#' @param setup_risk Vuoi esportare anche i report per la verifia dei rischi di attuazione a partire dall'analisi dei ritardi?
#' @return Per ogni CIS, un dossier excel in OUTPUT/dossier
make_report_cis <- function(bimestre, analisi, setup_risk=FALSE) {
  
  # DEBUG:
  # cis <- "CIS VOLARE"
  
  header <- read_xlsx(file.path(INPUT, "header.xlsx"))
  
  if (!dir.exists(file.path(OUTPUT, "dossier"))) {
    dir.create(file.path(OUTPUT, "dossier"))
  }
  
  if (setup_risk == TRUE) {
    if (!dir.exists(file.path(OUTPUT, "risk"))) {
      dir.create(file.path(OUTPUT, "risk"))
    }
  }

  lista_cis <- analisi %>% count(CIS) %>% .$CIS
  
  for (cis in lista_cis) {
    print(cis)
    
    # filtro
    appo <- analisi %>% 
      filter(CIS == cis) %>% 
      ungroup()
    
    # sintesi per programma
    programmi_1 <- appo %>% 
      group_by(AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
                COE = sum(COE, na.rm = TRUE), 
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE), 
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>% 
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
      select(AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, 
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG)
    
    
    # sintesi per programma
    programmi_2 <- appo %>% 
      group_by(x_STATO_PROCEDURALE, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
      summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
      pivot_wider(id_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "x_STATO_PROCEDURALE", 
                  names_expand = TRUE,
                  values_from = "RISORSE_COE", values_fill = 0)
    
    # sintesi per beneficiario
    beneficiari_1 <- appo %>%
      group_by(AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE) %>%
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
      select(AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE,
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG)
    
    
    # sintesi per beneficiario
    beneficiari_2 <- appo %>%
      group_by(x_STATO_PROCEDURALE, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE) %>%
      summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>%
      pivot_wider(id_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"), names_from = "x_STATO_PROCEDURALE",
                  names_expand = TRUE,
                  values_from = "RISORSE_COE", values_fill = 0)
    
    # sintesi per fase procedurale
    iter_1 <-  appo %>%
      group_by(x_STATO_PROCEDURALE) %>%
      summarise(N = n(),
                RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
      select(x_STATO_PROCEDURALE,
             N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG)
    
    
    interventi_1 <- appo %>%
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
      select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA,
             TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE,
             RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG,
             x_STATO_PROCEDURALE)
    
    interventi_2 <- appo %>%
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
      select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA,
             TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE,
             RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
             RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG,
             x_STATO_PROCEDURALE,
             # iter procedurale
             DATA_INIZIO_PREV_STUDIO_FATT, DATA_INIZIO_EFF_STUDIO_FATT, DATA_FINE_PREV_STUDIO_FATT, DATA_FINE_EFF_STUDIO_FATT,
             DATA_INIZIO_PREV_PROG_PREL, DATA_INIZIO_EFF_PROG_PREL, DATA_FINE_PREV_PROG_PREL, DATA_FINE_EFF_PROG_PREL,
             DATA_INIZIO_PREV_PROG_DEF, DATA_INIZIO_EFF_PROG_DEF, DATA_FINE_PREV_PROG_DEF, DATA_FINE_EFF_PROG_DEF,
             DATA_INIZIO_PREV_PROG_ESEC, DATA_INIZIO_EFF_PROG_ESEC, DATA_FINE_PREV_PROG_ESEC, DATA_FINE_EFF_PROG_ESEC,
             DATA_INIZIO_PREV_STIP_ATTRIB, DATA_INIZIO_EFF_STIP_ATTRIB, DATA_FINE_PREV_STIP_ATTRIB, DATA_FINE_EFF_STIP_ATTRIB,
             DATA_INIZIO_PREV_ESECUZIONE, DATA_INIZIO_EFF_ESECUZIONE, DATA_FINE_PREV_ESECUZIONE, DATA_FINE_EFF_ESECUZIONE,
             DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO
      ) %>%
      # converte in data tutte le colonne dell'iter procedurale
      mutate(
        across(
          matches("^DATA_"),       # seleziona tutte le colonne che iniziano con "DATA_"
          ~ ymd(.x)                # applica ymd() a ciascuna di esse
        )
      )
    
    
    
    # NEW: con auto format
    # export
    file_name <- paste0(str_replace(cis, " ", "_"), "_", bimestre,".xlsx")
    data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
    titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
    wb <- createWorkbook()

    write_tables_to_wb(
      wb          = wb,
      tables      = list(programmi_1, programmi_2),
      title       = titolo_base,
      subtitles   = c("Avanzamento finanziario per ambito di programmazione e programma", 
                      "Avanzamento procedurale per ambito di programmazione e programma - Risorse coesione programmate per stato di avanzamento procedurale"),
      source      = c("Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "programmi",
      start_row   = 4,
      header_df   = header
    )
    
    
    write_tables_to_wb(
      wb          = wb,
      tables      = list(beneficiari_1, beneficiari_2),
      title       = titolo_base,
      subtitles   = c("Avanzamento finanziario per beneficiario e localizzazione", 
                      "Avanzamento procedurale per beneficiario e localizzazione - Risorse coesione programmate per stato di avanzamento procedurale"),
      source      = c("Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "beneficiari",
      start_row   = 4,
      header_df   = header
    )
    
    write_table_to_wb(
      wb          = wb,
      df          = iter_1,
      title       = titolo_base,
      subtitle    = "Avanzamento finanziario per stato di avanzamento procedurale",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      # note        = "Nota: ...",
      sheet_name  = "iter",
      start_row   = 4,
      header_df   = header
    )
    
    write_table_to_wb(
      wb          = wb,
      df          = interventi_1,
      title       = titolo_base,
      subtitle    = "Dettaglio interventi",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      # note        = "Nota: ...",
      sheet_name  = "interventi",
      start_row   = 4,
      header_df   = header
    )
    
    
    write_table_to_wb(
      wb          = wb,
      df          = interventi_2,
      title       = titolo_base,
      subtitle    = "Dettaglio interventi con iter procedurale completo",
      source      = "Fonte: Elaborazione Dipcoes-NUPC",
      # note        = "Nota: ...",
      sheet_name  = "interventi-dettagli",
      start_row   = 4,
      header_df   = header
    )
    
    saveWorkbook(wb, file = file.path(OUTPUT, "dossier", file_name), overwrite = TRUE)
    
    
    
    if (setup_risk == TRUE) {
      
      # sintesi per fase procedurale
      iter_risk <-  appo %>%
        group_by(CHK_RITARDO, x_STATO_PROCEDURALE) %>%
        summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>%
        pivot_wider(id_cols = "x_STATO_PROCEDURALE", names_from = "CHK_RITARDO",
                    names_expand = TRUE,
                    values_from = "RISORSE_COE", values_fill = 0) %>%
        arrange(x_STATO_PROCEDURALE)
      
      interventi_risk <- appo %>%
        mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
               p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
               p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
               p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
               p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
               p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
        select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA,
               TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE,
               RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
               RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG,
               x_STATO_PROCEDURALE,
               FASE_CORRENTE, FASE_SEGUENTE, FASE_SEGUENTE_DATA, FASE_SEGUENTE_DATA_OLD, DELTA_DATA_SEGUENTE, CHK_DATA,
               FASE_SEGUENTE_RITARDO, CHK_RITARDO,
               # SUM_RITARDO,
               # iter procedurale
               DATA_INIZIO_PREV_STUDIO_FATT, DATA_INIZIO_EFF_STUDIO_FATT, DATA_FINE_PREV_STUDIO_FATT, DATA_FINE_EFF_STUDIO_FATT,
               DATA_INIZIO_PREV_PROG_PREL, DATA_INIZIO_EFF_PROG_PREL, DATA_FINE_PREV_PROG_PREL, DATA_FINE_EFF_PROG_PREL,
               DATA_INIZIO_PREV_PROG_DEF, DATA_INIZIO_EFF_PROG_DEF, DATA_FINE_PREV_PROG_DEF, DATA_FINE_EFF_PROG_DEF,
               DATA_INIZIO_PREV_PROG_ESEC, DATA_INIZIO_EFF_PROG_ESEC, DATA_FINE_PREV_PROG_ESEC, DATA_FINE_EFF_PROG_ESEC,
               DATA_INIZIO_PREV_STIP_ATTRIB, DATA_INIZIO_EFF_STIP_ATTRIB, DATA_FINE_PREV_STIP_ATTRIB, DATA_FINE_EFF_STIP_ATTRIB,
               DATA_INIZIO_PREV_ESECUZIONE, DATA_INIZIO_EFF_ESECUZIONE, DATA_FINE_PREV_ESECUZIONE, DATA_FINE_EFF_ESECUZIONE,
               DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO
        ) %>%
        # converte in data tutte le colonne dell'iter procedurale
        mutate(
          across(
            matches("^DATA_"),       # seleziona tutte le colonne che iniziano con "DATA_"
            ~ ymd(.x)                # applica ymd() a ciascuna di esse
          )
        ) %>%
        mutate(FASE_SEGUENTE_DATA = ymd(FASE_SEGUENTE_DATA),
               FASE_SEGUENTE_DATA_OLD = ymd(FASE_SEGUENTE_DATA_OLD),
               DELTA_DATA_SEGUENTE = as.integer(DELTA_DATA_SEGUENTE),
               FASE_SEGUENTE_RITARDO = as.integer(FASE_SEGUENTE_RITARDO),
               FASE_CORRENTE = case_when(FASE_CORRENTE == "END" ~ "Fine progetto",
                                         FASE_CORRENTE == "COL_END" ~ "Avvio fine progetto",
                                         FASE_CORRENTE == "COL" ~ "Fine collaudo",
                                         FASE_CORRENTE == "ESEC_COL" ~ "Avvio collaudo",
                                         FASE_CORRENTE == "ESEC" ~ "Fine esecuzione",
                                         FASE_CORRENTE == "STIP_ESEC" ~ "Avvio esecuzione",
                                         FASE_CORRENTE == "STIP" ~ "Fine gara/attribuzione finanziamento",
                                         FASE_CORRENTE == "PE_STIP" ~ "Avvio gara/attribuzione finanziamento",
                                         FASE_CORRENTE == "PE" ~ "Fine progettazione esecutiva",
                                         FASE_CORRENTE == "PD_PE" ~ "Avvio progettazione esecutiva",
                                         FASE_CORRENTE == "PD" ~ "Fine progettazione definitiva",
                                         FASE_CORRENTE == "PP_PD" ~ "Avvio progettazione definitiva",
                                         FASE_CORRENTE == "PP" ~ "Fine progettazione preliminare",
                                         FASE_CORRENTE == "SDF_PP" ~ "Avvio progettazione preliminare",
                                         FASE_CORRENTE == "SDF" ~ "Fine studio di fattibilità",
                                         FASE_CORRENTE == "START" ~ "Avvio progetto"),
               FASE_SEGUENTE = case_when(FASE_SEGUENTE == "END" ~ "Fine progetto",
                                         FASE_SEGUENTE == "COL_END" ~ "Avvio fine progetto",
                                         FASE_SEGUENTE == "COL" ~ "Fine collaudo",
                                         FASE_SEGUENTE == "ESEC_COL" ~ "Avvio collaudo",
                                         FASE_SEGUENTE == "ESEC" ~ "Fine esecuzione",
                                         FASE_SEGUENTE == "STIP_ESEC" ~ "Avvio esecuzione",
                                         FASE_SEGUENTE == "STIP" ~ "Fine gara/attribuzione finanziamento",
                                         FASE_SEGUENTE == "PE_STIP" ~ "Avvio gara/attribuzione finanziamento",
                                         FASE_SEGUENTE == "PE" ~ "Fine progettazione esecutiva",
                                         FASE_SEGUENTE == "PD_PE" ~ "Avvio progettazione esecutiva",
                                         FASE_SEGUENTE == "PD" ~ "Fine progettazione definitiva",
                                         FASE_SEGUENTE == "PP_PD" ~ "Avvio progettazione definitiva",
                                         FASE_SEGUENTE == "PP" ~ "Fine progettazione preliminare",
                                         FASE_SEGUENTE == "SDF_PP" ~ "Avvio progettazione preliminare",
                                         FASE_SEGUENTE == "SDF" ~ "Fine studio di fattibilità",
                                         FASE_SEGUENTE == "START" ~ "Avvio progetto")) %>%
        mutate(CHK_RITARDO = case_when(CHK_RITARDO == "concluso" ~ "Cronoprogramma concluso",
                                       CHK_RITARDO == "regolare" ~ "Cronoprogramma regolare",
                                       CHK_RITARDO == "in scadenza" ~ "Cronoprogramma in scadenza",
                                       CHK_RITARDO == "scaduti" ~ "Cronoprogramma scaduto (< 60 giorni)",
                                       CHK_RITARDO == "ritardo" ~ "Cronoprogramma in ritardo (> 60 giorni)",
                                       CHK_RITARDO == "ritardo grave" ~ "Cronoprogramma in ritardo grave (> 365 giorni)",
                                       CHK_RITARDO == "non monitorato" ~ "Intervento non monitorato")) %>%
        mutate(CHK_RITARDO = factor(CHK_RITARDO, levels = c("Intervento non monitorato",
                                                            "Cronoprogramma in ritardo grave (> 365 giorni)", "Cronoprogramma in ritardo (> 60 giorni)",
                                                            "Cronoprogramma scaduto (< 60 giorni)",
                                                            "Cronoprogramma in scadenza", "Cronoprogramma regolare", "Cronoprogramma concluso")))
      
      
      # export analisi rischi
      file_name <- paste0(str_replace(cis, " ", "_"), "_", bimestre,".xlsx")
      data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
      titolo_base <- paste0("Analisi rischi CIS - Aggiornamento al ", data_bimestre)
      
      wb <- createWorkbook()

      write_table_to_wb(
        wb          = wb,
        df          = iter_risk,
        title       = titolo_base,
        subtitle    = "Analisi ritardi per stato di avanzamento procedurale - Risorse coesione programmate per casistiche analisi ritardi",
        source      = "Fonte: Elaborazione Dipcoes-NUPC",
        # note        = "Nota: ...",
        sheet_name  = "sintesi",
        start_row   = 4,
        header_df   = header
      )
      
      write_table_to_wb(
        wb          = wb,
        df          = interventi_risk,
        title       = titolo_base,
        subtitle    = "Dettaglio interventi con analisi ritardi su prossima fase dell'iter procedurale",
        source      = "Fonte: Elaborazione Dipcoes-NUPC",
        # note        = "Nota: ...",
        sheet_name  = "interventi",
        start_row   = 4,
        header_df   = header
      )
      
      saveWorkbook(wb, file = file.path(OUTPUT, "risk", file_name), overwrite = TRUE)
    }
  }
  
  # sintesi
  print("Sintesi per tutti i CIS")
  
  
  # sintesi per cis
  sintesi_cis_1 <- analisi %>% 
    group_by(CIS) %>% 
    summarise(N = n(),
              RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
              COE = sum(COE, na.rm = TRUE), 
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE), 
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>% 
    mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
           p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
           p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
           p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
           p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
           p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
    select(CIS, 
           N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
           RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG) 
  

  sintesi_cis_2 <- analisi %>% 
    group_by(x_STATO_PROCEDURALE, CIS) %>% 
    summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = "CIS", names_from = "x_STATO_PROCEDURALE", 
                names_expand = TRUE,
                values_from = "RISORSE_COE", values_fill = 0)
  
  
  sintesi_cis_3 <- analisi %>% 
    group_by(CHK_RITARDO, CIS) %>% 
    summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = "CIS", names_from = "CHK_RITARDO", 
                names_expand = TRUE,
                values_from = "RISORSE_COE", values_fill = 0)
  
  
  # sintesi per programma
  programmi_cis_1 <- analisi %>% 
    group_by(CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(N = n(),
              RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE), 
              COE = sum(COE, na.rm = TRUE), 
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              RISORSE_TOT = sum(RISORSE_TOT, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE), 
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>% 
    mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
           p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
           p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
           p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
           p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
           p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>% 
    select(CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, 
           N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG,
           RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG)
  
  programmi_cis_2 <- analisi %>% 
    group_by(x_STATO_PROCEDURALE, CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "x_STATO_PROCEDURALE", 
                names_expand = TRUE,
                values_from = "RISORSE_COE", values_fill = 0)
  
  programmi_cis_3 <- analisi %>% 
    group_by(CHK_RITARDO, CIS, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(RISORSE_COE = sum(RISORSE_COE, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"), names_from = "CHK_RITARDO", 
                names_expand = TRUE,
                values_from = "RISORSE_COE", values_fill = 0)
  
  # export sintesi
  file_name <- paste0("sintesi_cis_", bimestre,".xlsx")
  data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
  titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
  wb <- createWorkbook()
  
  write_tables_to_wb(
    wb          = wb,
    tables      = list(sintesi_cis_1, sintesi_cis_2, sintesi_cis_3),
    title       = titolo_base,
    subtitles   = c("Avanzamento finanziario per CIS", 
                    "Avanzamento procedurale per CIS - Risorse coesione programmate per stato di avanzamento procedurale",
                    "Analisi ritardi per per CIS - Risorse coesione programmate per casistiche analisi ritardi"),
    source      = c("Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC", 
                    "Elaborazione Dipcoes-NUPC"),
    # note        = "Nota: ...",
    sheet_name  = "cis",
    start_row   = 4,
    header_df   = header
  )
  
  
  write_tables_to_wb(
    wb          = wb,
    tables      = list(sintesi_cis_1, sintesi_cis_2, sintesi_cis_3),
    title       = titolo_base,
    subtitles   = c("Avanzamento finanziario per CIS e programma", 
                    "Avanzamento procedurale per CIS e programma - Risorse coesione programmate per stato di avanzamento procedurale",
                    "Analisi ritardi per CIS e programma - Risorse coesione programmate per casistiche analisi ritardi"),
    source      = c("Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC", 
                    "Elaborazione Dipcoes-NUPC"),
    # note        = "Nota: ...",
    sheet_name  = "programmi",
    start_row   = 4,
    header_df   = header
  )

  saveWorkbook(wb, file = file.path(OUTPUT, "dossier", file_name), overwrite = TRUE)
  
}