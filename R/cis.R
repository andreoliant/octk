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
                                   # AMBITO == "PNRR" ~ FINANZ_PNRR,
                                   TRUE ~ 0),
           RISORSE_TOT = FINANZ_TOT) %>% 
    select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA=DESCRIZIONE_PROGRAMMA, 
           TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE, NATURA, DELIBERA_CIS,
           RISORSE_COE, RISORSE_TOT) %>%
    left_join(operazioni %>% 
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COE, COE_IMP, COE_PAG, CP, IMP, PAG),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  
  # integra ritardi
  if (!is.null(ritardi)) {
    cis <- cis %>% 
      left_join(ritardi %>% 
                  select(COD_LOCALE_PROGETTO, 
                         FASE_CORRENTE, FASE_CORRENTE_DATA, FASE_SEGUENTE, FASE_SEGUENTE_DATA, FASE_SEGUENTE_DATA_OLD, DELTA_DATA_SEGUENTE, 
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
make_report_cis <- function(bimestre, analisi) {
  
  # DEBUG:
  #  "CIS AREE SISMA"
  
  header <- read_xlsx(file.path(INPUT, "header.xlsx"))
  
  if (!dir.exists(file.path(OUTPUT, "report"))) {
    dir.create(file.path(OUTPUT, "report"))
  }
  
  lista_cis <- analisi %>% count(CIS) %>% .$CIS
  
  for (cis in lista_cis) {
    print(cis)
    
    # filtro
    appo <- analisi %>% 
      filter(CIS == cis) %>% 
      ungroup()
    
    # sintesi per programma
    programmi_1 <- report_summarise_coe(
      df = appo,
      group_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
      total_label_col = "AMBITO"
    )
    
    programmi_3 <- report_summarise_cp(
      df = appo,
      group_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
      total_label_col = "AMBITO"
    )
    
    programmi_2 <- report_pivot_stato(
      df = appo,
      group_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
      total_label_col = "AMBITO"
    )

    programmi_4 <- report_pivot_ritardo(
      df = appo,
      group_cols = c("AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
      total_label_col = "AMBITO"
    )
    
    # sintesi per beneficiario
    beneficiari_1 <- report_summarise_coe(
      df = appo,
      group_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"),
      total_label_col = "AMMINISTRAZIONE_BENEFICIARIA"
    )
    
    beneficiari_2 <- report_pivot_stato(
      df = appo,
      group_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"),
      total_label_col = "AMMINISTRAZIONE_BENEFICIARIA"
    )

    beneficiari_4 <- report_pivot_ritardo(
      df = appo,
      group_cols = c("AMMINISTRAZIONE_BENEFICIARIA", "LOCALIZZAZIONE"),
      total_label_col = "AMMINISTRAZIONE_BENEFICIARIA"
    )
    
    # sintesi per fase procedurale
    iter_1 <- report_summarise_coe(
      df = appo,
      group_cols = c("x_STATO_PROCEDURALE"),
      total_label_col = "x_STATO_PROCEDURALE"
    )
    
    iter_2 <- report_summarise_cp(
      df = appo,
      group_cols = c("x_STATO_PROCEDURALE"),
      total_label_col = "x_STATO_PROCEDURALE"
    )
    
    iter_4 <- report_pivot_ritardo(
      df = appo,
      group_cols = c("x_STATO_PROCEDURALE"),
      total_label_col = "x_STATO_PROCEDURALE"
    )
    
    # interventi
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
    
    # interventi con dettagli
    interventi_2 <- appo %>%
      mutate(p_COE = if_else(RISORSE_COE == 0, 0, round(COE/RISORSE_COE, 2)),
             p_COE_IMP = if_else(RISORSE_COE == 0, 0, round(COE_IMP/RISORSE_COE, 2)),
             p_COE_PAG = if_else(RISORSE_COE == 0, 0, round(COE_PAG/RISORSE_COE, 2)),
             p_CP = if_else(RISORSE_TOT == 0, 0, round(CP/RISORSE_TOT, 2)),
             p_IMP = if_else(RISORSE_TOT == 0, 0, round(IMP/RISORSE_TOT, 2)),
             p_PAG = if_else(RISORSE_TOT == 0, 0, round(PAG/RISORSE_TOT, 2))) %>%
      select(ID, CIS, TIPO_CIS, CUP, COD_LOCALE_PROGETTO, AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA,
             TITOLO_PROGETTO, AMMINISTRAZIONE_BENEFICIARIA, LOCALIZZAZIONE, NATURA, DELIBERA_CIS,
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
             DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO,
             FASE_CORRENTE, FASE_CORRENTE_DATA, 
             FASE_SEGUENTE, FASE_SEGUENTE_DATA, FASE_SEGUENTE_DATA_OLD, DELTA_DATA_SEGUENTE, FASE_SEGUENTE_RITARDO, CHK_RITARDO,
      ) %>%
      # converte in data tutte le colonne dell'iter procedurale
      mutate(
        across(
          matches("^DATA_"),       # seleziona tutte le colonne che iniziano con "DATA_"
          ~ ymd(.x)                # applica ymd() a ciascuna di esse
        )
      ) %>% 
      mutate(FASE_CORRENTE_DATA = ymd(FASE_CORRENTE_DATA),
             FASE_SEGUENTE_DATA = ymd(FASE_SEGUENTE_DATA),
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

    # export
    file_name <- paste0("report_", str_replace(cis, " ", "_"), "_", bimestre,".xlsx")
    data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
    titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
    wb <- createWorkbook()
    
    write_tables_to_wb(
      wb          = wb,
      tables      = list(programmi_1, programmi_3, programmi_2, programmi_4),
      title       = titolo_base,
      subtitles   = c("Avanzamento finanziario per ambito di programmazione e programma- Risorse coesione",
                      "Avanzamento finanziario per ambito di programmazione e programma - Risorse coesione e cofinanziamenti",
                      "Avanzamento procedurale per ambito di programmazione e programma - Risorse coesione programmate per stato dei progetti",
                      "Avanzamento procedurale per ambito di programmazione e programma - Risorse coesione programmate per casistiche analisi ritardi"),
      source      = c("Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "programmi",
      start_row   = 4,
      header_df   = header
    )
    
    write_tables_to_wb(
      wb          = wb,
      tables      = list(beneficiari_1, beneficiari_2),
      title       = titolo_base,
      subtitles   = c("Avanzamento finanziario per beneficiario e localizzazione - Risorse coesione", 
                      "Avanzamento procedurale per beneficiario e localizzazione - Risorse coesione programmate per stato dei progetti",
                      "Avanzamento procedurale per beneficiario e localizzazione - Risorse coesione programmate per casistiche analisi ritardi"),
      source      = c("Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "beneficiari",
      start_row   = 4,
      header_df   = header
    )
    
    write_tables_to_wb(
      wb          = wb,
      tables          = list(iter_1, iter_2, iter_4),
      title       = titolo_base,
      subtitles   = c("Avanzamento finanziario per stato di avanzamento procedurale - Risorse coesione",
                      "Avanzamento finanziario per stato di avanzamento procedurale - Risorse coesione e cofinanzimenti",
                      "Avanzamento finanziario per stato di avanzamento procedurale  - Risorse coesione programmate per casistiche analisi ritardi"),
      source      = c("Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC", "Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "iter",
      start_row   = 4,
      header_df   = header
    )
    
    write_tables_to_wb(
      wb          = wb,
      tables      = list(interventi_1),
      title       = titolo_base,
      subtitles   = c("Dettaglio interventi"),
      source      = c("Fonte: Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "interventi",
      start_row   = 4,
      header_df   = header
    )

    write_tables_to_wb(
      wb          = wb,
      tables      = list(interventi_2),
      title       = titolo_base,
      subtitles   = c("Dettaglio interventi con iter procedurale completo e analisi ritardi"),
      source      = c("Fonte: Elaborazione Dipcoes-NUPC"),
      # note        = "Nota: ...",
      sheet_name  = "interventi-dettagli",
      start_row   = 4,
      header_df   = header
    )
    
    saveWorkbook(wb, file = file.path(OUTPUT, "report", file_name), overwrite = TRUE)
    }
}


#' Report CIS
#' 
#' Report per ogni CIS con diversi livelli di aggregazione
#' 
#' @param bimestre Bimestre di OpenCoesione
#' @param analisi Analisi interventi CIS con programmazione, attuazione e ritardi da workflow_cis()
#' @return Per ogni CIS, un dossier excel in OUTPUT/dossier
make_report_cis_sintesi <- function(bimestre, analisi) {
  
  header <- read_xlsx(file.path(INPUT, "header.xlsx"))
  
  sintesi_cis_1 <- report_summarise_coe(
    df = analisi,
    group_cols = c("CIS"),
    total_label_col = "CIS"
  )
  
  sintesi_cis_4 <- report_summarise_cp(
    df = analisi,
    group_cols = c("CIS"),
    total_label_col = "CIS"
  )
  
  sintesi_cis_2 <- report_pivot_stato(
    df = analisi,
    group_cols = c("CIS"),
    total_label_col = "CIS"
  )
  
  sintesi_cis_3 <- report_pivot_ritardo(
    df = analisi,
    group_cols = c("CIS"),
    total_label_col = "CIS"
  )
  
  programmi_cis_1 <- report_summarise_coe(
    df = analisi,
    group_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
    total_label_col = "CIS"
  )
  
  programmi_cis_4 <- report_summarise_cp(
    df = analisi,
    group_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
    total_label_col = "CIS"
  )
  
  programmi_cis_2 <- report_pivot_stato(
    df = analisi,
    group_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
    total_label_col = "CIS"
  )
  
  programmi_cis_3 <- report_pivot_ritardo(
    df = analisi,
    group_cols = c("CIS", "AMBITO", "OC_CODICE_PROGRAMMA", "x_PROGRAMMA"),
    total_label_col = "CIS"
  )
  
  # export sintesi
  file_name <- paste0("report_sintesi_cis_", bimestre,".xlsx")
  data_bimestre <- format(ymd(bimestre), "%d/%m/%Y")
  titolo_base <- paste0("Analisi attuazione CIS - Aggiornamento al ", data_bimestre)
  wb <- createWorkbook()
  
  write_tables_to_wb(
    wb          = wb,
    tables      = list(sintesi_cis_1, sintesi_cis_4, sintesi_cis_2, sintesi_cis_3),
    title       = titolo_base,
    subtitles   = c("Avanzamento finanziario per CIS - Risorse coesione", 
                    "Avanzamento finanziario per CIS - Risorse coesione e cofinanziamenti", 
                    "Avanzamento procedurale per CIS - Risorse coesione programmate per stato dei progetti",
                    "Analisi ritardi per per CIS - Risorse coesione programmate per casistiche analisi ritardi"),
    source      = c("Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC", 
                    "Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC"),
    # note        = "Nota: ...",
    sheet_name  = "cis",
    start_row   = 4,
    header_df   = header
  )
  
  write_tables_to_wb(
    wb          = wb,
    tables      = list(programmi_cis_1, programmi_cis_4, programmi_cis_2, programmi_cis_3),
    title       = titolo_base,
    subtitles   = c("Avanzamento finanziario per CIS e programma - Risorse coesione", 
                    "Avanzamento finanziario per CIS e programma - Risorse coesione e cofinanziamenti", 
                    "Avanzamento procedurale per CIS e programma - Risorse coesione programmate per stato dei progetti",
                    "Analisi ritardi per CIS e programma - Risorse coesione programmate per casistiche analisi ritardi"),
    source      = c("Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC", 
                    "Elaborazione Dipcoes-NUPC",
                    "Elaborazione Dipcoes-NUPC"),
    # note        = "Nota: ...",
    sheet_name  = "programmi",
    start_row   = 4,
    header_df   = header
  )
  
  saveWorkbook(wb, file = file.path(OUTPUT, file_name), overwrite = TRUE)
}