# Funzioni per la gestione dei PSC (nuova versione post 20241031 fine migrazione)

# DEV:
# per distinguere le funzioni da versioni ante migrazioni ora hanno suffisso "_psc_migrati"


#' Carica dati di base per PSC
#'
#' Carica dati di base per PSC
#'
#' @param bimestre Bimestre di riferimento
#' @param versione_psc Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @return Dataframe
load_progetti_psc_migrati <- function(bimestre, versione_psc) {
  
  progetti_psc <- read_csv2(file.path(DRIVE, "DATI", "PSC", "psc", paste0("dati_psc_", bimestre, "_", versione_psc, ".csv")))
  
  return(progetti_psc)
}


#' Dati FSC per elaborazioni PSC
#'
#' Crea il file con operazioni FSC compatibile con PSC
#' 
#' @param bimestre Bimestre di riferimento, nella versione specifica per i dati PSC (es. 20211231.01 oppure 20211231), che viene ricondotto automaticamente al bimestre OC
#' @param versione_psc Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param operazioni Dataset progetti da load_operazioni(visualizzati = FALSE)
#' @param progetti Dataset progetti da load_progetti(visualizzati = FALSE, light = FALSE)
#' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' @param export Vuoi esportare i dati nel repository PSC in DATI?
#' @return File "dati_psc_BIMESTRE_VERSIONE.csv" in OUTPUT 
#' @note ...
prep_dati_psc_migrati_bimestre <- function(bimestre, versione_psc, operazioni, progetti, chk_today, export=TRUE) {

  # DEBUG:
  # chk_today = "2023-08-31"
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  bimestre_oc <- str_sub(bimestre, 1, 8) # 
  DATA <- file.path(dirname(DATA), bimestre_oc)
  
  
  # ------------------ load dati ------------------ #
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati = FALSE, light = FALSE)
  }
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, visualizzati = FALSE)
  }
  
  
  # ------------------ elab ------------------ #
  
  # Filtra PSC e unisce info
  appo0 <- operazioni %>% 
    inner_join(octk::info_psc, by = "OC_CODICE_PROGRAMMA")
  # TODO: qui probabilmente ancora qualcosa va ripescato
  
  # chk temi multipli
  chk <- appo0 %>% 
    select(COD_LOCALE_PROGETTO, 
           ID_PSC,
           x_COD_LIVELLO_1,
           x_COD_LIVELLO_2,
           COE) %>% 
    filter(grepl(":::", x_COD_LIVELLO_1) | grepl(":::", x_COD_LIVELLO_2)) %>% 
    arrange(ID_PSC)
  write.xlsx(chk, file.path(TEMP, "chk_temi_multipli.xlsx"))
  
  # clean temi
  appo1 <- appo0 %>% 
    # DEBUG:
    # select(COD_LOCALE_PROGETTO, 
    #        ID_PSC,
    #        x_COD_LIVELLO_1,
    #        x_COD_LIVELLO_2,
    #        COE) %>% 
    # fix per anomalie con 2 temi per 1 settore
    mutate(x_COD_LIVELLO_1 = case_when(nchar(x_COD_LIVELLO_2) == 2 & nchar(x_COD_LIVELLO_1) > 2 ~ str_sub(x_COD_LIVELLO_1, 1, 2),
                                       TRUE ~ x_COD_LIVELLO_1)) %>% 
    # DEV: qui andrebbero separate le righe
    # codifica temi con fix per temi e settori multipli
    mutate(COD_AREA_TEMATICA = case_when(nchar(x_COD_LIVELLO_1) > 2 & ID_PSC == "PSC_MIC" ~ "06",
                                         nchar(x_COD_LIVELLO_1) > 2 & COD_LOCALE_PROGETTO == "9CA23034CP000000001" ~ "07",
                                         nchar(x_COD_LIVELLO_1) > 2 & COD_LOCALE_PROGETTO == "1MISE24" ~ "06",
                                         nchar(x_COD_LIVELLO_1) > 2 ~ NA_character_,
                                         TRUE ~ x_COD_LIVELLO_1),
           COD_SETTORE_INTERVENTO = case_when(nchar(x_COD_LIVELLO_2) > 2 & ID_PSC == "PSC_MIC" ~ "01",
                                              nchar(x_COD_LIVELLO_2) > 2 & COD_LOCALE_PROGETTO == "9CA23034CP000000001" ~ "05",
                                              nchar(x_COD_LIVELLO_2) > 2 & COD_LOCALE_PROGETTO == "1MISE24" ~ "01",
                                              nchar(x_COD_LIVELLO_2) > 2 ~ NA_character_,
                                              TRUE ~ x_COD_LIVELLO_2)) %>% 
    left_join(octk::info_psc_matrix_temi %>% 
                select(COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO, AREA_TEMATICA, SETTORE_INTERVENTO), 
              by = c("COD_AREA_TEMATICA", "COD_SETTORE_INTERVENTO")) 
  
  appo1 %>% filter(is.na(COD_AREA_TEMATICA)) 
  appo1 %>% filter(is.na(COD_SETTORE_INTERVENTO))
  appo1 %>% count(AREA_TEMATICA)
  
  
  # chk sezione
  chk <- appo1 %>% 
    select(COD_LOCALE_PROGETTO, 
           ID_PSC,
           x_COD_LIVELLO_0,
           COE) %>% 
    count(ID_PSC, x_COD_LIVELLO_0) %>% 
    filter(!x_COD_LIVELLO_0 %in% c("SO", "SS_1", "SS_2")) %>% 
    arrange(ID_PSC)
  write.xlsx(chk, file.path(TEMP, "chk_sezioni.xlsx"))
  
  # clean sezione
  appo2 <- appo1 %>% 
    mutate(SEZIONE = case_when(x_COD_LIVELLO_0 == "SO" ~ "ORD",
                               # x_COD_LIVELLO_0 == "SO:::" ~ "ORD",
                               # x_COD_LIVELLO_0 == "SO:::SOCIS_RC" ~ "ORD",
                               x_COD_LIVELLO_0 == "SOCIS" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_CO" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_NA" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_PAL" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_RC" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_SA" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCIS_VENT" ~ "CIS",
                               x_COD_LIVELLO_0 == "SOCISTA" ~ "CIS",
                               x_COD_LIVELLO_0 == "SS_1" ~ "SS_1",
                               x_COD_LIVELLO_0 == "SS_2" ~ "SS_2",
                               x_COD_LIVELLO_0 == "SS_2:" ~ "SS_2",
                               is.na(x_COD_LIVELLO_0) ~ "CHK",
                               x_COD_LIVELLO_0 == "" ~ "CHK",
                               TRUE ~ "CHK"))
  
  appo2 %>% 
    count(ID_PSC, SEZIONE) %>% 
    filter(!SEZIONE %in% c("SO", "SS_1", "SS_2"))
  
  
  # dati finanziari da progetti
  appo3 <- appo2 %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, 
                       FINANZ_STATO_FSC, OC_FINANZ_STATO_FSC_NETTO,
                       FINANZ_TOTALE_PUBBLICO, FINANZ_PRIVATO, FINANZ_DA_REPERIRE, ECONOMIE_TOTALI,
                       OC_FINANZ_TOT_PUB_NETTO, DB),
              by = "COD_LOCALE_PROGETTO") %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(FINANZ_TOT = FINANZ_TOTALE_PUBBLICO + FINANZ_PRIVATO + FINANZ_DA_REPERIRE - ECONOMIE_TOTALI,
           x = COE/FINANZ_TOT,
           COE_CR = COSTO_REALIZZATO * x,
           COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO,
           CP = OC_FINANZ_TOT_PUB_NETTO,
           CPP = FINANZ_TOTALE_PUBBLICO + FINANZ_PRIVATO)
  
  # stato procedurale
  appo_stato <- progetti %>% # OLD: progetti_pub %>% 
    semi_join(appo3, by = "COD_LOCALE_PROGETTO") %>%
    get_stato_attuazione(., chk_today = chk_today) %>% 
    select(COD_LOCALE_PROGETTO, OC_STATO_PROCEDURALE = STATO_PROCED)
  # message("Se ci sono 12 waring su clean_data va bene perché è il numero delle variabili e il warning indica date NA in input")
  
  appo4 <- appo3 %>% 
    rename(OC_STATO_PROCEDURALE_OLD = OC_STATO_PROCEDURALE) %>%
    left_join(appo_stato, by = "COD_LOCALE_PROGETTO")
  
  # DEV: parti da verificare e reintegrare se serve
  # # fix CIS Taranto in PRA Puglia
  # appo3 <- fix_ciclo_cis_taranto_pra_puglia(progetti_psc = appo2)
  # 
  # # fix visualizzati PRA Campania
  # appo4 <- fix_visualizzati_pra_campania(progetti_psc = appo3)
  # 
  # # fix assegnazioni di legge  PRA Campania
  # appo4bis <- fix_assegnazioni_legge_pra_campania(progetti_psc = appo4, progetti = progetti)
  # 
  # # fix visualizzati APQ MARI Marche
  # appo5 <- fix_visualizzati_apq_mari_marche(progetti_psc = appo4bis)
  # 
  # # fix macroarea
  # appo8 <- fix_macroarea_progetti_psc(progetti_psc = appo7)
  
  # clean per export
  out <- appo4 %>%
    select(COD_LOCALE_PROGETTO, 
           CUP, 
           OC_TITOLO_PROGETTO,
           x_CICLO,
           x_PROGRAMMA,
           OC_CODICE_PROGRAMMA,
           x_MACROAREA,
           SEZIONE,
           AREA_TEMATICA, 
           SETTORE_INTERVENTO,
           OC_STATO_PROCEDURALE,
           COE, 
           COE_IMP,
           COE_CR,
           COE_PAG,
           COE_ECO,
           CP,
           CPP,
           ID_PSC,
           OC_FLAG_VISUALIZZAZIONE,
           DB)
  
  
  # export
  write.csv2(out, file.path(OUTPUT, paste0("dati_psc_", bimestre, "_", versione_psc, ".csv")), row.names = FALSE)
  
  if (export == TRUE) {
    write.csv2(out, file.path(PSC, "psc", paste0("dati_psc_", bimestre, "_", versione_psc, ".csv")), row.names = FALSE)
  }
}


#' Verifica variazioni dati FSC per elaborazioni PSC
#'
#' Verifica variazioni dati FSC per elaborazioni PSC rispetto a un bimestre e versione precedente
#' 
#' @param bimestre Bimestre di riferimento
#' @param versione_psc Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param bimestre_old Bimestre di confronto
#' @param versione_psc_old Versione di confronto dei dati  (sono possibili più versioni per lo stesso bimestre)
#' @return File "dati_psc_BIMESTRE_VERSIONE.csv" in OUTPUT 
#' @note ...
chk_delta_psc_migrati_bimestre <- function(bimestre, versione_psc, bimestre_old, versione_psc_old) {
  
  # loads
  progetti_psc <- load_progetti_psc_migrati(bimestre, versione = versione_psc)
  progetti_psc_old <- load_progetti_psc_migrati(bimestre_old, versione = versione_psc_old)
  
  # elab psc
  chk <- progetti_psc %>%
    group_by(ID_PSC) %>% 
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE)) %>% 
    full_join(progetti_psc_old %>%
                group_by(ID_PSC) %>% 
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE)),
              by = "ID_PSC", suffix = c(".new", ".old")) %>% 
    mutate(DELTA_N = N.new - N.old,
           DELTA_COE = COE.new - COE.old)
  
  file_name <- paste0("chk_delta_psc_", bimestre, "-", versione_psc, "_",  bimestre_old, "-", versione_psc_old,".xlsx")
  write.xlsx(chk, file.path(TEMP, file_name))
  
  # elab psc e ciclo
  CHK_CICLO <- progetti_psc %>%
    group_by(ID_PSC, x_CICLO) %>% 
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE)) %>% 
    full_join(progetti_psc_old %>%
                group_by(ID_PSC, x_CICLO) %>% 
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE)),
              by = c("ID_PSC", "x_CICLO"), suffix = c(".new", ".old")) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(DELTA_N = N.new - N.old,
           DELTA_COE = COE.new - COE.old)
  
  file_name <- paste0("chk_delta_psc_", bimestre, "-", versione_psc, "_",  bimestre_old, "-", versione_psc_old,"_ciclo.xlsx")
  write.xlsx(CHK_CICLO, file.path(TEMP, file_name))
  
  chk_flag <- progetti_psc %>%
    group_by(ID_PSC, x_CICLO, OC_FLAG_VISUALIZZAZIONE) %>% 
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE)) %>% 
    full_join(progetti_psc_old %>%
                group_by(ID_PSC, x_CICLO, OC_FLAG_VISUALIZZAZIONE) %>% 
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE)),
              by = c("ID_PSC", "x_CICLO", "OC_FLAG_VISUALIZZAZIONE"), suffix = c(".new", ".old")) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(DELTA_N = N.new - N.old,
           DELTA_COE = COE.new - COE.old)
  
  file_name <- paste0("chk_delta_psc_", bimestre, "-", versione_psc, "_",  bimestre_old, "-", versione_psc_old,"_flag.xlsx")
  write.xlsx(chk_flag, file.path(TEMP, file_name))
  
  out <- chk %>% 
    filter(abs(DELTA_COE) > 10000000)
  print(out)
}


#' Report PSC per temi
#'
#' Crea report di confronto programmazione attuazione per PSC e tema
#' 
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_temi_psc_migrati <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
  # DEBUG:
  # programmazione = NULL
  # visualizzati = TRUE
  # show_cp = FALSE
  # usa_meuro = TRUE
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2") %>% 
    mutate(x_CICLO = "2014-2020",
           SEZIONE = "ORD+CIS") #MEMO: valore forzato
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- load_db_psc(DB, use_flt=TRUE) %>% 
      rename(TIPOLOGIA_AMMINISTRAZIONE = TIPO_AR,
             x_CICLO = CICLO_PROGRAMMAZIONE) %>% 
      mutate(x_CICLO = "2014-2020",
             SEZIONE = "ORD+CIS") #MEMO: valore forzato
  }
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }
  
  if (show_cp == TRUE) {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE)) %>% 
      select(ID_PSC, OC_CODICE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n()) %>% 
      select(ID_PSC, OC_CODICE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE,
           SEZIONE,
           AREA_TEMATICA,
           SETTORE_INTERVENTO) %>% 
    group_by(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2,
              by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) 
  
  # fix label vuoti da full_join
  report <- report %>% 
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  if (show_cp == TRUE) {
    report <- report %>% 
      # clean
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO)
  } else {
    report <- report %>% 
      # clean
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO)
  }
  
  if (usa_meuro == TRUE) {
    if (show_cp == TRUE) {
      report <- report %>% 
        mutate(RISORSE = RISORSE/1000000, 
               COE = COE/1000000, 
               COE_IMP = COE_IMP/1000000, 
               COE_CR = COE_CR/1000000, 
               COE_PAG = COE_PAG/1000000,
               CP = CP/1000000)
    } else {
      report <- report %>% 
        mutate(RISORSE = RISORSE/1000000, 
               COE = COE/1000000, 
               COE_IMP = COE_IMP/1000000, 
               COE_CR = COE_CR/1000000, 
               COE_PAG = COE_PAG/1000000)
    }
  }
  
  if (visualizzati == TRUE){
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_temi_psc_nodupli.xlsx"))
      
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_temi_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}


#' Report PSC per temi con sezioni speciali 
#'
#' Crea report di confronto programmazione attuazione per PSC e tema su sezione speciale 1
#' 
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_sezioni_psc_migrati <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE == "SS_1" | SEZIONE == "SS_2") %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SS_1" ~ "SEZ_SPEC_1_COVID",
                               SEZIONE == "SS_2" ~ "SEZ_SPEC_2_FS")) %>% 
    mutate(x_CICLO = "2014-2020")
  
  matrix_psc <- data.frame(
    ID_PSC = c("PSC_ABRUZZO", "PSC_BARI", "PSC_BASILICATA", "PSC_BOLOGNA", "PSC_CAGLIARI", "PSC_CALABRIA", 
               "PSC_CAMPANIA", "PSC_CATANIA", "PSC_EMILIA-ROMA", "PSC_FIRENZE", "PSC_FRIULI-VENE", 
               "PSC_GENOVA", "PSC_LAZIO", "PSC_LIGURIA", "PSC_LOMBARDIA", "PSC_MARCHE", "PSC_MATTM", 
               "PSC_MESSINA", "PSC_MI", "PSC_MIC", "PSC_MILANO", "PSC_MIPAAF", "PSC_MISALUTE", "PSC_MISE", 
               "PSC_MIT", "PSC_MOLISE", "PSC_MTUR", "PSC_MUR", "PSC_NAPOLI", "PSC_PA_BOLZANO", 
               "PSC_PA_TRENTO", "PSC_PALERMO", "PSC_PCM-SPORT", "PSC_PIEMONTE", "PSC_PUGLIA", 
               "PSC_REGGIO_CALA", "PSC_SARDEGNA", "PSC_SICILIA", "PSC_TOSCANA", "PSC_UMBRIA", 
               "PSC_VALLE_D_AOS", "PSC_VENETO", "PSC_VENEZIA"),
    OC_CODICE_PROGRAMMA = c("PSCABRUZZO", "PSCCITMETBARI", "PSCBASILICATA", "PSCCIMETBOLOGNA", 
                            "PSCCIMTCAGLIARI", "PSCCALABRIA", "PSCCAMPANIA", "PSCCIMETCATANIA", 
                            "PSCEMILROMAGNA", "PSCCIMETFIRENZE", "PSCFRIULI", "PSCCIMETGENOVA", 
                            "PSCLAZIO", "PSCLIGURIA", "PSCLOMBARDIA", "PSCMARCHE", "PSCTRANSECOLOG", 
                            "PSCCIMETMESSINA", "PSCISTRUZIONE", "PSCCULTURA", "PSCCITMETMILANO", 
                            "PSCAGRICOLTURA", "PSCSALUTE", "PSCSVILECONOM", "PSCINFRASTRUT", "PSCMOLISE", 
                            "PSCTURISMO", "PSCUNIVRICERCA", "PSCCITMETNAPOLI", "PSCBOLZANO", "PSCTRENTO", 
                            "PSCCIMETPALERMO", "PSCPCMSPORT", "PSCPIEMONTE", "PSCPUGLIA", "PSCCITMETREGCAL", 
                            "PSCSARDEGNA", "PSCSICILIA", "PSCTOSCANA", "PSCUMBRIA", "PSCVALLEAOSTA", 
                            "PSCVENETO", "PSCCIMETVENEZIA"))
  
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati(DB=DB) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(SEZIONE = DESCR_LIVELLO_1) %>% 
      filter(SEZIONE == "SEZ_SPEC_1_COVID" | SEZIONE == "SEZ_SPEC_2_FS") %>% 
      left_join(matrix_psc, by = "OC_CODICE_PROGRAMMA")
  }
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }
  
  if (show_cp == TRUE) {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE)) %>% 
      select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n()) %>% 
      select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  # NEW:
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           SEZIONE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO = DESCR_SETTORE_INTERVENTO_PSC) %>% 
    # toglie NA da settore di intervento
    mutate(SETTORE_INTERVENTO = case_when(is.na(SETTORE_INTERVENTO) ~ "",
                                          TRUE ~ SETTORE_INTERVENTO)) %>% 
    group_by(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2,
              by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) 
  
  
  # fix label vuoti da full_join
  report <- report %>% 
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  
  if (show_cp == TRUE) {
    report <- report %>% 
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO)
  } else {
    report <- report %>% 
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO)
  }
  
  report <- report %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SEZ_ORD" ~ "Sez. ordinaria",
                               SEZIONE == "SEZ_SPEC_1_COVID" ~ "Sez. speciale 1-Covid",
                               SEZIONE == "SEZ_SPEC_2_FS" ~ "Sez. speciale 2-FS",
                               SEZIONE == "CSR" ~ "Da programmare",
                               TRUE ~ "CHK"))
  if (usa_meuro == TRUE) {
    if (show_cp == TRUE) {
      report <- report %>% 
        mutate(RISORSE = RISORSE/1000000, 
               COE = COE/1000000, 
               COE_IMP = COE_IMP/1000000, 
               COE_CR = COE_CR/1000000, 
               COE_PAG = COE_PAG/1000000,
               CP = CP/1000000)
    } else {
      report <- report %>% 
        mutate(RISORSE = RISORSE/1000000, 
               COE = COE/1000000, 
               COE_IMP = COE_IMP/1000000, 
               COE_CR = COE_CR/1000000, 
               COE_PAG = COE_PAG/1000000)
    }
  }
  
  if (visualizzati == TRUE){
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_sezioni_speciali_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_sezioni_speciali_psc_nodupli.xlsx"))
      
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_sezioni_speciali_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_sezioni_speciali_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}


#' Report PSC e PO d'origine
#'
#' Crea report di confronto programmazione attuazione per PSC e PO d'origine
#' 
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO d'origine. Le risorse delle sezioni speciali, nuove o da riprogrammazione, sono in righe separate.
#' @note Contiene patch per incorporare patti città metro anche se non sono PSC in DBCOE
make_report_cicli_psc_migrati <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE))
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- load_db_psc(DB, use_flt=TRUE) %>% 
      rename(TIPOLOGIA_AMMINISTRAZIONE = TIPO_AR,
             x_CICLO = CICLO_PROGRAMMAZIONE)
  }
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
  }
  
  appo2 <- appo1 %>%
    group_by(ID_PSC, OC_CODICE_PROGRAMMA, x_CICLO) %>%
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              COE_CR = sum(COE_CR, na.rm = TRUE),
              N = n())
  
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE) %>% 
    group_by(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    full_join(appo2 %>%
                select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, COE, COE_IMP, COE_CR, COE_PAG, N),
              by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0)))
  
  # fix label vuoti da full_join
  report <- report %>% 
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
    select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, 
           RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  if (usa_meuro == TRUE) {
    report <- report %>% 
      mutate(RISORSE = RISORSE/1000000, 
             COE = COE/1000000, 
             COE_IMP = COE_IMP/1000000, 
             COE_CR = COE_CR/1000000, 
             COE_PAG = COE_PAG/1000000)
  }
  
  if (visualizzati == TRUE){
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_cicli_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_cicli_psc_nodupli.xlsx"))
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_cicli_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_cicli_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}



#' Report PSC per temi con macroarea
#'
#' Crea report di confronto programmazione attuazione per temi con apertura per macroarea
#' 
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param operazioni Dataset da load_operazioni()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti.
#' @note ...
make_report_temi_macroaree_psc_migrati <- function(progetti_psc, operazioni=NULL, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE, DB) {
  
  # Isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>%
    mutate(SEZIONE = "ORD+CIS") %>% 
    mutate(x_CICLO = "2014-2020") %>% 
    mutate(COE_CR = 0) # Forzo a 0 per coerenza con macroarea
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, visualizzati = FALSE)
  }
  
  # Gestione macroaree con setup specifico
  progetti_psc_migrati <- setup_macroaree_psc_migrati(progetti_psc, operazioni)
  
  # sum(progetti_psc$COE, na.rm = TRUE)
  # sum(progetti_psc_migrati$COE, na.rm = TRUE)
  
  appo <- progetti_psc %>% 
    anti_join(progetti_psc_migrati, 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO")) %>% 
    bind_rows(progetti_psc_migrati)
  
  progetti_psc <- appo
  
  if (is.null(programmazione)) {
    programmazione <- load_db_psc(DB, use_flt=TRUE) %>% 
      filter(SEZIONE != "SEZ_SPEC_1_COVID", SEZIONE != "SEZ_SPEC_2_FS") %>%
      mutate(SEZIONE = "ORD+CIS") %>% 
      mutate(x_CICLO = "2014-2020") %>% 
      rename(TIPOLOGIA_AMMINISTRAZIONE = TIPO_AR) 
  }
  
  if (visualizzati == TRUE) {
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    appo1 <- progetti_psc %>% filter(!OC_FLAG_VISUALIZZAZIONE %in% c(4, 5))
  }
  
  if (show_cp == TRUE) {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO, x_MACROAREA) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE))
  } else {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, OC_CODICE_PROGRAMMA, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO, x_MACROAREA) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n())
  }
  
  # converte in formato long
  programmazione2 <- programmazione %>% 
    group_by(ID_PSC, OC_CODICE_PROGRAMMA, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE_SUD = sum(RISORSE_SUD, na.rm = TRUE),
              RISORSE_CN = sum(RISORSE_CN, na.rm = TRUE)) %>% 
    pivot_longer(cols = c("RISORSE_SUD", "RISORSE_CN"), 
                 names_to = "x_MACROAREA", values_to = "RISORSE") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "RISORSE_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "RISORSE_CN" ~ "Centro-Nord",
                                   TRUE ~ "CHK"))
  
  # chk
  sum(programmazione$RISORSE_CN, na.rm = TRUE) + sum(programmazione$RISORSE_SUD, na.rm = TRUE) - sum(programmazione2$RISORSE, na.rm = TRUE)
  
  # crea report
  report <- programmazione2 %>%
    full_join(appo2, by = c("ID_PSC", "OC_CODICE_PROGRAMMA", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO", "x_MACROAREA")) %>%
    mutate_if(is.numeric, replace_na, 0) %>% 
    # elimina righe vuote
    filter(!(RISORSE == 0 & COE == 0))
  
  # fix label vuoti da full_join
  report <- report %>% 
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  if (show_cp == TRUE) {
    report <- report %>% 
      # clean
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE,
             AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, x_CICLO) 
  } else {
    report <- report %>% 
      # clean
      select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE,
             AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N) %>% 
      arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, x_CICLO) 
  }

  if (usa_meuro == TRUE) {
    if (show_cp == TRUE) {
      report <- report %>% mutate(across(c(RISORSE, COE, COE_IMP, COE_CR, COE_PAG, CP), ~./1e6))
    } else {
      report <- report %>% mutate(across(c(RISORSE, COE, COE_IMP, COE_CR, COE_PAG), ~./1e6))
    }
  }
  
  if (visualizzati == TRUE) {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_macroaree_psc_nodupli.csv"))
    }
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_temi_macroaree_psc_nodupli.xlsx"))
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_macroaree_psc_dupli.csv"))
    }
    if (export_xls == TRUE) {
      write.xlsx(report, file.path(OUTPUT, "report_temi_macroaree_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}



#' Crea file di base per macroaree
#'
#' Crea file di base per macroaree, da operazioni, per psc migrati.
#'
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param operazioni Dataset da load_operazioni(), con visualizzati=FALSE
#' @param export vuoi salvare il file?
#' @return Dataset macroaree per il report make_report_temi_macroaree_psc_migrati
setup_macroaree_psc_migrati <- function(progetti_psc, operazioni, export=FALSE) {
  
  # MEMO:
  # progetti_psc non contiene x_GRUPPO e variabili COE_SUD, COE_IMP_SUD, ecc., che devo riprendere da operazioni
  # vengono trattati solo i progetti monitorati nei PSC migrati
  
  # WARNING: c'è un progetto 4VAPSC0010209XX00001INV in PSCVALLEAOSTA con OC_FLAG_VISUALIZZAZIONE == 1 
  
  progetti_psc <- progetti_psc %>% 
    filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) 
  
  if (is.null(operazioni)) {
    operazioni_1420_raw <- load_operazioni(bimestre, visualizzati=FALSE) %>%
      # filter(x_GRUPPO == "PSC") %>%
      filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
      # elimina sezione speciale se assente in progetti (serve per controlli dopo)
      semi_join(progetti_psc, 
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO"))
    
  } else {
    operazioni_1420_raw <- operazioni %>%
      # filter(x_GRUPPO == "PSC") %>%
      filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
      # elimina sezione speciale se assente in progetti (serve per controlli dopo)
      semi_join(progetti_psc, 
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO"))
  }
  
  # 
  # %>% 
  #   filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>% 
  #   select(-SEZIONE)
  
  nrow(operazioni_1420_raw) - nrow(progetti_psc)
  # CHK:
  # -1
  
  # chk
  # progetti_psc %>% 
  #   count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO) %>% 
  #   filter(n>1)
  # 
  # chk <- progetti_psc %>% 
  #   anti_join(operazioni_1420_raw, 
  #             by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO"))
  
  # fix temporaneo su imp e pag
  appo <- operazioni_1420_raw %>% 
    mutate(QUOTA_SUD = COE_SUD/COE) %>% 
    mutate(COE_IMP_SUD = QUOTA_SUD * COE_IMP,
           COE_IMP_CN = COE_IMP - COE_IMP_SUD,
           COE_PAG_SUD = QUOTA_SUD * COE_PAG,
           COE_PAG_CN = COE_PAG - COE_PAG_SUD)
  
  # chk fix
  appo %>% 
    summarise(COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              COE_IMP_SUD = sum(COE_IMP_SUD, na.rm = TRUE),
              COE_IMP_CN = sum(COE_IMP_CN, na.rm = TRUE),
              COE_PAG_SUD = sum(COE_PAG_SUD, na.rm = TRUE),
              COE_PAG_CN = sum(COE_PAG_CN, na.rm = TRUE)) %>% 
    mutate(CHK_IMP = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           CHK_PAG = COE_PAG - COE_PAG_SUD - COE_PAG_CN)
  # COE_IMP     COE_PAG COE_IMP_SUD  COE_IMP_CN COE_PAG_SUD  COE_PAG_CN     CHK_IMP      CHK_PAG
  # <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>        <dbl>
  # 13720805269. 4075468994. 9318101036. 4402704233. 2421109661. 1654359333. 0.000000954 -0.000000238
  
  # chk quota fix
  appo %>% filter(QUOTA_SUD > 1)
  # 0
  
  # integra variabili finanziarie per macroaree
  operazioni_1420 <- progetti_psc %>% 
    mutate(x_AMBITO = "FSC") %>% 
    left_join(appo %>% 
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO,
                       COE_SUD, COE_CN, COE_IMP_SUD, COE_IMP_CN, COE_PAG_SUD, COE_PAG_CN),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO"))
  
  pivo <- workflow_pivot_macroaree(operazioni_1420)
  
  out <- pivo %>% 
    # select(-QUOTA_SUD) %>% 
    left_join(operazioni_1420 %>% 
                select(-COE, -COE_IMP, -COE_PAG, -x_MACROAREA), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) 
  
  nrow(pivo) - nrow(out)
  sum(pivo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  sum(operazioni_1420$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  sum(operazioni_1420_raw$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  # CHK:
  # 0
  
  # pulisce dupli per macroaree vuote
  out <- out %>%
    filter(COE != 0)
  # MEMO: vanno escluse per non duplicare le altre variabili
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "progetti_psc_macroaree.csv"))
  }
  
  return(out)
}


#' Verifica coerenza tra file dati FSC e file interventi PSC
#'
#' Verifica coerenza tra file dati FSC e file interventi PSC
#'
#' @param bimestre Bimestre di riferimento
#' @param versione_psc Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @return Dataframe
chk_dati_dbcoe_psc_migrati <- function() {
  
  programmazione <- load_db_psc(DB, use_flt=TRUE) 
  
  po_fsc <- load_db("2014-2020", "FSC", simplify_loc = TRUE, use_articolaz = TRUE) %>% 
    filter(TIPOLOGIA_PROGRAMMA == "PSC" & 
             COD_LIVELLO_1 %in% c("SEZ_CIS", "SEZ_ORD"))
  
  chk <- programmazione %>% 
    group_by(OC_CODICE_PROGRAMMA) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    full_join(po_fsc %>% 
                group_by(OC_CODICE_PROGRAMMA) %>% 
                summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA", suffix = c(".int", ".dati")) %>% 
    mutate(CHK = RISORSE.int - RISORSE.dati)
  
  if (export == TRUE) {
    write.xlsx(chk, file.path(TEMP, "chk_psc_dati_vs_interventi.xlsx"))
  }
  
  return(chk)
}


#' Integra CLP da attuazione in DB interventi programmati nei PSC
#'
#' Integra CLP da attuazione in DB interventi programmati nei PSC.
#' Integra CLP per match su CLP al netto di codice SIL e poi per CUP univoci.
#' Sostituisce CUP vuoti con CUP da attuazione.
#'
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param interventi_psc Dataset da DBCOE per programmazione interventi PSC (ATTENZIONE: use_flt=FALSE altrimenti si perdono righe)
#' @param debug Vuoi esportare in TEMP le variazioni dei CLP?
#' @param export Vuoi esportare nel file interventi del DBCOE?
#' @return Dataframe
update_clp_interventi_psc <- function(progetti_psc, interventi_psc, debug=TRUE, export=FALSE) {
  
  # DEBUG:
  # debug=TRUE
  
  # join by clp
  
  clean_clp <- function(x, sil) {
    for (l in sil$sil) {
      # DEBUG:
      # x <- "1MISE10067"
      # l <- "1MISE"
      r_l <- paste0("^", l)
      # print(r_l)
      x <- str_replace_all(x, r_l, "")
    }
    return(x)
  }
  
  appo_int <- interventi_psc %>% 
    anti_join(progetti_psc, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(COD_LOCALE_PROGETTO_OGV = COD_LOCALE_PROGETTO,
           CUP_OGV = CUP) %>% 
    mutate(TEMP_CLP = clean_clp(COD_LOCALE_PROGETTO, octk::sil))
  
  appo_pro <- progetti_psc %>% 
    anti_join(interventi_psc, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(TEMP_CLP = clean_clp(COD_LOCALE_PROGETTO, octk::sil)) %>% 
    select(TEMP_CLP, ID_PSC, COD_LOCALE_PROGETTO, CUP)
  
  appo <- appo_int %>% 
    inner_join(appo_pro, 
               by = c("TEMP_CLP", "ID_PSC"),
               suffix = c(".int", ".pro")) 
  
  if (debug == TRUE) {
    chk <- appo %>%
      select(TEMP_CLP, ID_PSC, COD_LOCALE_PROGETTO.int, COD_LOCALE_PROGETTO.pro, CUP.int, CUP.pro)
    write.xlsx(chk, file.path(TEMP, "chk_match_interventi_by_clp.xlsx"))
    message("Correzioni da match su CLP senza codice SIL per ", dim(chk)[1], " interventi")
  }
  
  appo1 <- appo %>% 
    mutate(CUP = if_else(is.na(CUP.int), CUP.pro, CUP.int), #MEMO: integra solo CUP vuoti lato programmazione, non modifica CUP diversi con attuazione (che si controllano in data quality)
           COD_LOCALE_PROGETTO = COD_LOCALE_PROGETTO.pro) 
  
  interventi_psc_2 <- appo1 %>% 
    select(-TEMP_CLP, -COD_LOCALE_PROGETTO.int, -COD_LOCALE_PROGETTO.pro, -CUP.int, -CUP.pro) %>% 
    # bind_rows(interventi_psc %>% 
    #             anti_join(appo1 %>% 
    #                         select(COD_LOCALE_PROGETTO = COD_LOCALE_PROGETTO.int), # MEMO: il join va fatto sul codice originale!
    #                       by = "COD_LOCALE_PROGETTO")) %>% 
    bind_rows(interventi_psc %>% 
                anti_join(appo1 %>% 
                            select(ID),
                          by = "ID")) %>% 
    select(names(interventi_psc))
  
  dim(interventi_psc_2)[1] == dim(interventi_psc)[1]    
  
  
  # join by cup 
  n_cup_int <- interventi_psc_2 %>% 
    filter(!is.na(CUP)) %>% 
    filter(CUP != "no") %>% 
    filter(CUP != "-") %>% 
    filter(nchar(CUP) == 15) %>% 
    # count(CUP) %>%
    count(CUP, ID_PSC) %>%
    filter(n == 1)
  
  n_cup_pro <- progetti_psc %>% 
    # count(CUP) %>%
    count(CUP, ID_PSC) %>%
    filter(n == 1)
  
  appo_int_2 <- interventi_psc_2 %>% 
    semi_join(n_cup_int, by = c("CUP", "ID_PSC"))
  
  appo_pro_2 <- progetti_psc %>% 
    semi_join(n_cup_pro, by = c("CUP", "ID_PSC")) %>% 
    anti_join(interventi_psc_2, by = "COD_LOCALE_PROGETTO") %>% 
    select(ID_PSC, CUP, COD_LOCALE_PROGETTO)
  
  appo2 <- appo_int_2 %>% 
    inner_join(appo_pro_2, 
               by = c("CUP", "ID_PSC"),
               suffix = c(".int", ".pro")) 
  
  if (debug == TRUE) {
    chk1 <- appo2 %>%
      select(ID_PSC, CUP, COD_LOCALE_PROGETTO.int, COD_LOCALE_PROGETTO.pro)
    write.xlsx(chk1, file.path(TEMP, "chk_match_interventi_by_cup.xlsx"))
    message("Correzioni da match su CUP per ", dim(chk1)[1], " interventi")
    
    # # chk su cup duplicati
    # n_cup_int_dupli <- interventi_psc_2 %>%
    #   filter(!is.na(CUP)) %>%
    #   filter(CUP != "no") %>%
    #   filter(CUP != "-") %>%
    #   filter(nchar(CUP) == 15) %>%
    #   count(CUP, ID_PSC) %>%
    #   filter(n > 1)
    # 
    # n_cup_pro_dupli <- progetti_psc %>%
    #   count(CUP, ID_PSC) %>%
    #   filter(n > 1)
    # 
    # appo_int_2_dupli <- interventi_psc_2 %>%
    #   semi_join(n_cup_int_dupli, by = c("CUP", "ID_PSC"))
    # 
    # appo_pro_2_dupli <- progetti_psc %>%
    #   semi_join(n_cup_pro_dupli, by = c("CUP", "ID_PSC")) %>%
    #   anti_join(interventi_psc_2, by = "COD_LOCALE_PROGETTO") %>%
    #   select(ID_PSC, CUP, COD_LOCALE_PROGETTO, TITOLO_PROGETTO=OC_TITOLO_PROGETTO)
    # 
    # appo2_dupli <- appo_int_2_dupli %>%
    #   inner_join(appo_pro_2_dupli,
    #              by = c("CUP", "ID_PSC"))
    # 
    # chk2 <- appo2_dupli %>%
    #   select(ID_PSC, CUP, COD_LOCALE_PROGETTO.x, TITOLO_PROGETTO.x, COD_LOCALE_PROGETTO.y, TITOLO_PROGETTO.y)
    # 
    # 
    # write.xlsx(chk2, file.path(TEMP, "chk_match_interventi_by_cup_duplicati.xlsx"))
    # message("Duplicati da match su CUP per ", dim(chk2)[1], " interventi/progetti. Analizza il file chk_match_interventi_by_cup_duplicati.xlsx e correggi a mano nel DBCOE")
  }
  
  appo3 <- appo2 %>% 
    mutate(COD_LOCALE_PROGETTO = COD_LOCALE_PROGETTO.pro) 
  
  interventi_psc_3 <- appo3 %>% 
    select(-COD_LOCALE_PROGETTO.int, -COD_LOCALE_PROGETTO.pro) %>% 
    # bind_rows(interventi_psc_2 %>% 
    #             anti_join(appo3 %>% 
    #                         select(COD_LOCALE_PROGETTO = COD_LOCALE_PROGETTO.int), # MEMO: il join va fatto sul codice originale!
    #                       by = "COD_LOCALE_PROGETTO")) %>% 
    bind_rows(interventi_psc_2 %>% 
                anti_join(appo3 %>% 
                            select(ID),
                          by = "ID")) %>% 
    select(names(interventi_psc))

  dim(interventi_psc_3)[1] == dim(interventi_psc_2)[1] 
  
  if (export == TRUE) {
    write.xlsx(interventi_psc_3, file.path(DB, "Interventi_DBCOE_PSC.xlsx"))
  }
  
  if (debug == TRUE) {
    write.xlsx(interventi_psc_3, file.path(TEMP, "Interventi_DBCOE_PSC.xlsx"))
  }
  
  return(interventi_psc_3)
}



#' Analisi qualità dati PSC
#'
#' Analisi qualità dati PSC
#'
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param interventi_psc Dataset da DBCOE per programmazione interventi PSC
#' @param progetti Dataset progetti da load_progetti(visualizzati = FALSE, light = TRUE)
#' @param export Vuoi esportare il file in OUTPUT?
#' @return Dataframe
analisi_data_quality_psc <- function(progetti_psc, interventi_psc, progetti, export=FALSE) {
  
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  # ---- programmazione ----
  
  interventi_psc_2 <- interventi_psc %>% 
    rename(CICLO = CICLO_PROGRAMMAZIONE,
           PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  
  
  # ---- attuazione ----
  
  progetti_psc_2 <- progetti_psc %>% 
    filter(SEZIONE %in% c("ORD", "CIS")) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, IMPEGNI, CUP_DESCR_NATURA),
              by = "COD_LOCALE_PROGETTO") %>% 
    rename(TITOLO_PROGETTO = OC_TITOLO_PROGETTO,
           CICLO = x_CICLO,
           PROGRAMMA = x_PROGRAMMA)
  
  
  # ---- join ----
  
  appo <- progetti_psc_2 %>% 
    full_join(interventi_psc_2, 
              by = c("COD_LOCALE_PROGETTO", "ID_PSC")) %>% 
    mutate(
      CUP = if_else(is.na(CUP.y), CUP.x, CUP.y),
      TITOLO_PROGETTO = if_else(is.na(TITOLO_PROGETTO.y), TITOLO_PROGETTO.x, TITOLO_PROGETTO.y),
      # CICLO = if_else(is.na(CICLO.y), CICLO.x, CICLO.y),
      PROGRAMMA = if_else(is.na(PROGRAMMA.y), PROGRAMMA.x, PROGRAMMA.y),
      OC_CODICE_PROGRAMMA = if_else(is.na(OC_CODICE_PROGRAMMA.y), OC_CODICE_PROGRAMMA.x, OC_CODICE_PROGRAMMA.y),
      SEZIONE = if_else(is.na(SEZIONE.y), SEZIONE.x, SEZIONE.y),
      # AREA_TEMATICA = if_else(is.na(AREA_TEMATICA.y), AREA_TEMATICA.x, AREA_TEMATICA.y),
      # SETTORE_INTERVENTO = if_else(is.na(SETTORE_INTERVENTO.y), SETTORE_INTERVENTO.x, SETTORE_INTERVENTO.y)
      AREA_TEMATICA = AREA_TEMATICA.y,
      AREA_TEMATICA_MONIT = AREA_TEMATICA.x,
      SETTORE_INTERVENTO = SETTORE_INTERVENTO.y,
      SETTORE_INTERVENTO_MONIT = SETTORE_INTERVENTO.x) %>% 
    rename() %>% 
    # TODO: scegliere i casi da mostrare nel dossier e commentare per lasciare differenze in chiaro
    # fix varibili vuote ma di cui va occupata posizione
    mutate(MACROAREA = "ND",
           RISORSE_COE = RISORSE,
           RISORSE_ECO = 0) %>% 
    select(ID,
           COD_LOCALE_PROGETTO,
           CUP,
           TITOLO_PROGETTO,
           CICLO_ATTUAZIONE = CICLO.x, 
           CICLO_PROGRAMMAZIONE = CICLO.y,
           ID_PSC,
           PROGRAMMA,
           OC_CODICE_PROGRAMMA,
           MACROAREA,
           SEZIONE,
           AREA_TEMATICA,
           SETTORE_INTERVENTO,
           AREA_TEMATICA_MONIT,
           SETTORE_INTERVENTO_MONIT,
           CUP_DESCR_NATURA,
           CASI_OGV,
           RISORSE,
           RISORSE_COE,
           RISORSE_ECO,
           COE,
           COE_IMP,
           COE_CR,
           COE_PAG,
           COE_ECO,
           CP,
           CPP,
           IMPEGNI,
           OC_STATO_PROCEDURALE,
           OC_FLAG_VISUALIZZAZIONE,
           DB) %>% 
    # fix flag
    mutate(OC_FLAG_VISUALIZZAZIONE = if_else(is.na(OC_FLAG_VISUALIZZAZIONE), 0, OC_FLAG_VISUALIZZAZIONE)) %>% 
    # variabili per analisi match
    mutate(CHK_LISTE = case_when(is.na(CASI_OGV) ~ "no psc",
                                 CASI_OGV == "economie programmazione" ~ "economie",
                                 CASI_OGV == "solo economie" ~ "economie",
                                 is.na(COE) ~ "no monit",
                                 TRUE ~ "match")) %>% 
    mutate(DELTA = RISORSE - COE) %>%
    # mutate(DELTA = RISORSE_COE - COE) %>% 
    mutate(CHK_RISORSE = case_when(# DELTA > 0 ~ "da monitorare",
                                   # DELTA == 0 ~ "invariati",
                                   # DELTA < 0 ~ "overbooking",
                                   DELTA > 1 ~ "da monitorare", #MEMO: introduco tolleranza di un euro
                                   DELTA >= -1 & DELTA < 1 ~ "invariati",
                                   DELTA < -1 ~ "overbooking",
                                   # is.na(RISORSE_COE) ~ "overbooking",
                                   is.na(RISORSE) ~ "overbooking",
                                   is.na(COE) ~ "da monitorare",
                                   TRUE ~ "chk")) %>% 
    mutate(COE_FIX = case_when(CHK_LISTE == "no monit" ~ RISORSE,
                               CHK_LISTE == "economie" ~ RISORSE,
                               CHK_LISTE == "match" ~ RISORSE,
                               TRUE ~ COE)) %>% # variabile ibrida per controlli
    mutate(OGV = case_when(CASI_OGV == "dichiarazione ogv da acquisire" ~ "OGV",
                           CASI_OGV == "economie programmazione" ~ "OGV",
                           CASI_OGV == "esoneri" ~ "OGV",
                           CASI_OGV == "ogv conseguita dic22" ~ "OGV",
                           CASI_OGV == "ogv conseguita giu22" ~ "OGV",
                           CASI_OGV == "ogv conseguita per dichiarazioni" ~ "OGV",
                           CASI_OGV == "ogv conseguita su salvaguardia 200M" ~ "OGV",
                           CASI_OGV == "ogv conseguita su salvaguardia 25M" ~ "OGV",
                           CASI_OGV == "ogv non conseguita" ~ "NO OGV",
                           CASI_OGV == "ogv non conseguita con art. 53" ~ "NO OGV",
                           CASI_OGV == "ogv non conseguita per verifica desk" ~ "NO OGV",
                           CASI_OGV == "ogv non conseguita su salvaguardia 200M" ~ "NO OGV",
                           CASI_OGV == "ogv non conseguita su salvaguardia 25M" ~ "NO OGV",
                           CASI_OGV == "salvaguardia CIS" ~ "OGV",
                           CASI_OGV == "salvaguardia COMM" ~ "OGV",
                           CASI_OGV == "salvaguardia pnrr" ~ "OGV",
                           TRUE ~ "ND"))
  
  analisi <- appo %>%
    mutate(VERIFICA_PROCEDURALE = case_when(OC_STATO_PROCEDURALE == "In esecuzione" ~ "IN ESECUZIONE",
                                            OC_STATO_PROCEDURALE == "Eseguito" ~ "IN ESECUZIONE",
                                            is.na(OC_STATO_PROCEDURALE) ~ "NON DEFINIBILE",
                                            TRUE ~ "NON IN ESECUZIONE")) %>% 
    mutate(IMPEGNI = ifelse(is.na(IMPEGNI), 0, IMPEGNI))%>%
    mutate(VERIFICA_IMPEGNI = case_when (is.na(CP) ~ "NON DEFINIBILE",
                                         # IMPEGNI/CP < 0.6 ~ "IMPEGNI INFERIORI AL 60%",
                                         COE_IMP/COE < 0.6 ~ "IMPEGNI INFERIORI AL 60%",
                                         TRUE ~ "IMPEGNI OK"))%>%
    mutate(CHK_CICLO = case_when(CICLO_PROGRAMMAZIONE == CICLO_ATTUAZIONE ~ "CICLO CORRETTO",
                                 CICLO_PROGRAMMAZIONE != CICLO_ATTUAZIONE ~ "CICLO NON COERENTE",
                                 is.na(CICLO_PROGRAMMAZIONE) ~ "PROGETTO NON NEL PSC",
                                 is.na(CICLO_ATTUAZIONE) ~ "PROGETTO NON MONITORATO")) %>%
    mutate(CHK_LISTE_2 = case_when(CHK_LISTE == "match" & CHK_RISORSE %in% c("da monitorare", "overbooking") ~ "psc ma variati",
                                   CHK_LISTE == "match" & CHK_RISORSE %in% c("invariati") ~ "psc ok",
                                   TRUE ~ CHK_LISTE)) %>% 
    mutate(CHK_TEMI = case_when(AREA_TEMATICA == AREA_TEMATICA_MONIT ~ "tema ok",
                                is.na(AREA_TEMATICA) ~ "PROGETTO NON NEL PSC",
                                is.na(AREA_TEMATICA_MONIT) ~ "PROGETTO NON MONITORATO",
                                TRUE ~ "tema no")) %>% 
    mutate(CHK_SETTORI = case_when(SETTORE_INTERVENTO == SETTORE_INTERVENTO_MONIT ~ "settore ok",
                                   is.na(SETTORE_INTERVENTO) ~ "PROGETTO NON NEL PSC",
                                   is.na(SETTORE_INTERVENTO_MONIT) ~ "PROGETTO NON MONITORATO",
                                   TRUE ~ "settore no"))
  
  
  # ---- chk clp dupli ----
  
  check_dupli <- analisi%>%
    filter(!is.na(COD_LOCALE_PROGETTO))%>%
    filter(COD_LOCALE_PROGETTO %!in% c("ND", "Residuo_Ducato Estense", "XXX12", "XXX3_3", "XXX5", "XXX8"))%>%
    group_by(COD_LOCALE_PROGETTO, ID_PSC)%>%
    summarise(N= sum(n()))%>%
    filter(N>1)
  
  if ((dim(check_dupli)[1]==0) == FALSE){
    print("ATTENZIONE!!!! SONO PRESENTI PROGETTI DUPLICATI")
  } else {
    print("OK! NESSUN PROGETTO DUPLICATO")
  }
  
  if (export == TRUE) {
    metadati <- read_xlsx(file.path(INPUT, "metadati_psc.xlsx"))
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName="dati")
    addWorksheet(wb, sheetName="metadati")
    
    writeData(wb, sheet = "dati", x = appo, colNames = TRUE)
    writeData(wb, sheet = "metadati", x = metadati, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, paste0("interventi_data_quality_psc_", bimestre, ".xlsx")), overwrite = TRUE)  
  }
  
  return(analisi)
}


#' Dossier qualità dati PSC
#'
#' Dossier qualità dati PSC
#'
#' @param analisi Dataset di analisi qualità dati da analisi_data_quality_psc()
#' @return Nessuno, salva file in Drive
dossier_data_quality_psc <- function(analisi) {
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  # ---- liste dossier ----
  
  #aggiungere in analisi programmazione altri campi di interesse (Verifica CUP, ritardi, salvaguardie)
  
  
  #Progetti non presenti nel monitoraggio
  no_monit <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 == "no monit") %>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  
  #Progetti da eliminare dal monitoraggio (al netto di NO OGV)
  no_lista <- analisi %>%
    filter(OGV == "ND") %>% #MEMO: questi progetti sono privi di CASI_OGV per definizione 
    filter(CHK_LISTE_2 =="no psc") %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  #Progetti da eliminare dal monitoraggio perché NO OGV
  no_ogv <- analisi %>%
    filter(OGV == "NO OGV") %>% 
    # filter(CHK_LISTE_2 =="no psc") %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)

  #Progetti con COE diverso
  CHK_coe <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 =="psc ma variati")%>%
    filter(RISORSE > 0) %>%
    filter(DELTA != 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  
  # Progetti con IMPEGNI Inferiori al 60%
  CHK_impegni <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(VERIFICA_IMPEGNI =="IMPEGNI INFERIORI AL 60%") %>%
    filter(CASI_OGV %in% c("ogv conseguita per dichiarazioni", 
                           "ogv conseguita dic22",
                           "ogv conseguita giu22",
                           "ogv conseguita su salvaguardia 25M", 
                           "ogv conseguita su salvaguardia 200M")) %>%
    filter(CUP_DESCR_NATURA %in% c("ACQUISTO DI BENI", 
                                   "ACQUISTO O REALIZZAZIONE DI SERVIZI", 
                                   "REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)")) %>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  # Progetti con PROCEDURALE non eseguito
  CHK_iter <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(VERIFICA_PROCEDURALE =="NON IN ESECUZIONE") %>%
    filter(CASI_OGV %in% c("ogv conseguita per dichiarazioni", 
                           "ogv conseguita dic22",
                           "ogv conseguita giu22",
                           "ogv conseguita su salvaguardia 25M", 
                           "ogv conseguita su salvaguardia 200M")) %>%
    filter(CUP_DESCR_NATURA %in% c("ACQUISTO DI BENI", 
                                   "ACQUISTO O REALIZZAZIONE DI SERVIZI", 
                                   "REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)")) %>%
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  #Progetti da migrare
  # da_migrare <- analisi %>%
  #   filter(OGV == "OGV") %>% 
  #   mutate(da_migrare = case_when(DB !="PUC" ~ "da migrare",
  #                                 DB =="PUC" & !grepl("^PSC", OC_CODICE_PROGRAMMA) ~ "da migrare",
  #                                 TRUE ~ "ok"))%>%
  #   mutate(PROGRAMMA = ifelse(DB =="SGP" & grepl("^PSC", OC_CODICE_PROGRAMMA), paste0(PROGRAMMA, " (da validare)"), PROGRAMMA))%>%
  #   filter(da_migrare =="da migrare")%>%
  #   filter(RISORSE > 0) %>%
  #   select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX , -da_migrare, -CHK_LISTE_2, -OGV)
  
  #Progetti con ciclo non coerente
  CHK_ciclo <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_CICLO =="CICLO NON COERENTE")%>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  #Progetti con tema non coerente
  CHK_tema <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_TEMI =="tema no")%>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  #Progetti con settore non coerente
  CHK_settore <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_SETTORI =="settore no")%>%
    filter(RISORSE > 0) %>%
    select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -CHK_LISTE_2, -OGV)
  
  # ---- export dossier ----
  # stili
  # style_number <<- createStyle(numFmt = "#,##0.0", border = c("top", "bottom", "left", "right"), fontColour = "#000000")
  # style_number3 <<- createStyle(numFmt = "#,##0", halign = "center", border = c("top", "bottom", "left", "right"), fontColour = "#000000")
  # style_number4 <<- createStyle(numFmt = "#,##0", halign = "center", fontColour = "#000000")
  # style_date <<- createStyle(numFmt = "DATE", border = c("top", "bottom", "left", "right"))
  # style_date2 <<- createStyle(numFmt = "DATE")
  # style_centered <<- createStyle(halign = "center", border = c("top", "bottom", "left", "right"))
  # style_centered2 <<- createStyle(halign = "center")
  # style_border <<- createStyle(border = c("top", "bottom", "left", "right"))
  # style_bold_centered <<- createStyle(textDecoration = "bold", halign = "center", border = c("top", "bottom", "left", "right"))
  # style_bold_number <<- createStyle(textDecoration = "bold", numFmt = "#,##0.00", border = c("top", "bottom", "left", "right"))
  # style_bold <<- createStyle(textDecoration = "bold", border = c("top", "bottom", "left", "right"))
  # style_number2 <<- createStyle(numFmt = "#,##0.00", halign = "right", fontColour = "#000000")
  # style_none <<- createStyle(border = NULL)
  # style_white <<- createStyle(border = NULL, fontColour = "#FFFFFF")
  # style_percentage <<- createStyle(numFmt = "PERCENTAGE", border = c("top", "bottom", "left", "right"), fontColour = "#000000")
  # 
  
  psc_all <- analisi%>%
    distinct(ID_PSC) 
  psc <- analisi%>%
    distinct(ID_PSC) %>% .$ID_PSC
  
  
  for (i in psc) {
    print(i)
    
    # DEBUG:
    # i <- "PSC_MARCHE"  
    
    data_riferimento <- data.frame(bimestre)%>%
      mutate(anno = substr(bimestre, start = 1, stop= 4))%>%
      mutate(mese = substr(bimestre, start = 5, stop= 6))%>%
      mutate(giorno = substr(bimestre, start = 7, stop= 8))%>%
      mutate(data = paste0(giorno, "/", mese, "/", anno))%>%
      mutate(as.Date(data))%>%
      select(data)
    
    lista <- analisi %>%
      filter(OGV == "OGV") %>%
      filter(ID_PSC == i) %>%
      select(-OC_FLAG_VISUALIZZAZIONE, -COE_FIX, -OGV)
    
    
    no_monit_psc <- no_monit %>%
      filter(ID_PSC == i)
    
    no_lista_psc <- no_lista %>%
      filter(ID_PSC == i)
    
    no_ogv_psc <- no_ogv %>%
      filter(ID_PSC == i)
    
    CHK_impegni_psc <- CHK_impegni %>%
      filter(ID_PSC == i)
    
    CHK_coe_psc <- CHK_coe %>%
      filter(ID_PSC == i)
    
    CHK_iter_psc <- CHK_iter %>%
      filter(ID_PSC == i)
    
    # da_migrare_psc <- da_migrare %>%
    #   filter(ID_PSC == i)
    
    CHK_ciclo_psc <- CHK_ciclo %>%
      filter(ID_PSC == i)
    
    CHK_tema_psc <- CHK_tema %>%
      filter(ID_PSC == i)
    
    CHK_settore_psc <- CHK_settore %>%
      filter(ID_PSC == i)
    
    nessun_dato <- "NESSUN DATO"
    
    # filename per export
    temp_file <- paste0("Dossier ",i, "_verifica_monitoraggio dati al ", bimestre, ".xlsx")
    
    # load
    wb <- loadWorkbook(file.path(INPUT, "template_quality.xlsx"))
    
    # scrive dati
    writeData(wb, sheet = "Copertina", x = i, startCol = 10, startRow = 25, colNames = FALSE)
    writeData(wb, sheet = "Copertina", x = data_riferimento, startCol = 10, startRow = 26, colNames = FALSE)
    addStyle(wb, sheet = "Copertina", style_date2, rows = 26:26, cols = 10 , gridExpand = TRUE, stack = TRUE)
    
    writeData(wb, sheet = "lista_dati", x = lista, startCol = 1, startRow = 2, colNames = FALSE)
    addStyle(wb, sheet = "lista_dati", style_border, rows = 1:nrow(lista)+1, cols = 1:39 , gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sheet = "lista_dati", style_number, rows = 1:nrow(lista)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
    addStyle(wb, sheet = "lista_dati", style_number, rows = 1:nrow(lista)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, "lista_dati", cols = 1:39, widths = 15)
    
    if ((dim(no_monit_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_no_monit", x = no_monit_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_no_monit", style_border, rows = 1:nrow(no_monit_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_monit", style_number, rows = 1:nrow(no_monit_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_monit", style_number, rows = 1:nrow(no_monit_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_no_monit", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_no_monit", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(no_lista_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_no_psc", x = no_lista_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_no_psc", style_border, rows = 1:nrow(no_lista_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_psc", style_number, rows = 1:nrow(no_lista_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_psc", style_number, rows = 1:nrow(no_lista_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_no_psc", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_no_psc", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(no_ogv_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_no_ogv", x = no_ogv_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_no_ogv", style_border, rows = 1:nrow(no_ogv_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_ogv", style_number, rows = 1:nrow(no_ogv_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_no_ogv", style_number, rows = 1:nrow(no_ogv_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_no_ogv", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_no_ogv", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(CHK_coe_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_variazione_costo", x = CHK_coe_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_variazione_costo", style_border, rows = 1:nrow(CHK_coe_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_variazione_costo", style_number, rows = 1:nrow(CHK_coe_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_variazione_costo", style_number, rows = 1:nrow(CHK_coe_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_variazione_costo", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_variazione_costo", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(CHK_impegni_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_anomalie_impegni", x = CHK_impegni_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_anomalie_impegni", style_border, rows = 1:nrow(CHK_impegni_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_impegni", style_number, rows = 1:nrow(CHK_impegni_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_impegni", style_number, rows = 1:nrow(CHK_impegni_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_anomalie_impegni", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_anomalie_impegni", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(CHK_iter_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_anomalie_procedurale", x = CHK_iter_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_anomalie_procedurale", style_border, rows = 1:nrow(CHK_iter_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_procedurale", style_number, rows = 1:nrow(CHK_iter_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_procedurale", style_number, rows = 1:nrow(CHK_iter_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_anomalie_procedurale", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_anomalie_procedurale", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    # if ( (dim(da_migrare_psc)[1]==0) == FALSE)
    # { 
    #   writeData(wb, sheet = "progetti_da_migrare", x = da_migrare_psc, startCol = 1, startRow = 2, colNames = FALSE)
    #   addStyle(wb, sheet = "progetti_da_migrare", style_border, rows = 1:nrow(da_migrare_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
    #   addStyle(wb, sheet = "progetti_da_migrare", style_number, rows = 1:nrow(da_migrare_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
    #   addStyle(wb, sheet = "progetti_da_migrare", style_number, rows = 1:nrow(da_migrare_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
    #   setColWidths(wb, "progetti_da_migrare", cols = 1:38, widths = 15)
    # } else 
    # { 
    #   writeData(wb, sheet = "progetti_da_migrare", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    # }
    
    
    if ( (dim(CHK_ciclo_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_anomalie_ciclo", x = CHK_ciclo_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_anomalie_ciclo", style_border, rows = 1:nrow(CHK_ciclo_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_ciclo", style_number, rows = 1:nrow(CHK_ciclo_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_ciclo", style_number, rows = 1:nrow(CHK_ciclo_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_anomalie_ciclo", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_anomalie_ciclo", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(CHK_tema_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_anomalie_tema", x = CHK_tema_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_anomalie_tema", style_border, rows = 1:nrow(CHK_tema_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_tema", style_number, rows = 1:nrow(CHK_tema_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_tema", style_number, rows = 1:nrow(CHK_tema_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_anomalie_tema", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_anomalie_tema", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    if ( (dim(CHK_settore_psc)[1]==0) == FALSE)
    { 
      writeData(wb, sheet = "progetti_anomalie_settore", x = CHK_settore_psc, startCol = 1, startRow = 2, colNames = FALSE)
      addStyle(wb, sheet = "progetti_anomalie_settore", style_border, rows = 1:nrow(CHK_settore_psc)+1, cols = 1:38 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_settore", style_number, rows = 1:nrow(CHK_settore_psc)+1, cols = 17:27 , gridExpand = TRUE, stack = TRUE)
      addStyle(wb, sheet = "progetti_anomalie_settore", style_number, rows = 1:nrow(CHK_settore_psc)+1, cols = 31:31 , gridExpand = TRUE, stack = TRUE)
      setColWidths(wb, "progetti_anomalie_settore", cols = 1:38, widths = 15)
    } else 
    { 
      writeData(wb, sheet = "progetti_anomalie_settore", x = nessun_dato, startCol = 1, startRow = 2, colNames = FALSE)
    }
    
    # salva
    saveWorkbook(wb, file = file.path(OUTPUT, "Dossier", temp_file), overwrite = TRUE)
  }
}


#' Report qualità dati PSC
#'
#' Report qualità dati PSC
#'
#' @param analisi Dataset di analisi qualità dati da analisi_data_quality_psc()
#' @param progetti_psc Dataset da load_progetti_psc_migrati()
#' @param export Vuoi esportare il file in OUTPUT?
#' @return Dataframe
report_data_quality_psc <- function(analisi, progetti_psc, export=TRUE) {
  
  # TODO: 
  # rivedere metadati
  
  # carica metadati
  metadati <- read_xlsx(file.path(INPUT, "metadati_quality.xlsx"))
  
  
  # ---- liste report ----
  
  # Tutti gli interventi
  lista <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(RISORSE > 0) %>% 
    filter(CASI_OGV != "economie programmazione") %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "TOTALE")
  
  # Base con risorse di riferimento
  risorse <- lista %>%
    select(ID_PSC, RISORSE)
  
  # Economie
  economie <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(RISORSE > 0) %>% 
    filter(CASI_OGV == "economie programmazione") %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "ECONOMIE")
  
  # No OGV da disattivare
  no_ogv <- analisi %>%
    filter(OGV == "NO OGV") %>% 
    # filter(RISORSE > 0) %>% 
    filter(COE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              # RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
              RISORSE = sum(COE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO OGV")
  
  # Interventi presenti nel monitoraggio
  monit_ok <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 == "psc ok") %>%
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "MONIT OK")
  
  # Interventi non presenti nel monitoraggio
  no_monit <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 == "no monit") %>%
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO MONIT")
  
  # Progetti da eliminare dal monitoraggio (altri casi)
  no_lista <- analisi %>%
    filter(OGV == "ND") %>% 
    filter(CHK_LISTE_2 =="no psc") %>% #MEMO: i due filtri corrispondono
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(COE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO PSC")
  
  # Interventi con COE diverso
  CHK_coe <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 =="psc ma variati") %>%
    filter(RISORSE > 0) %>% 
    filter(DELTA != 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(DELTA, na.rm=TRUE)) %>% 
    mutate(KPI = "DELTA COE")
  
  # Progetti con IMPEGNI Inferiori al 60%
  CHK_impegni <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(VERIFICA_IMPEGNI =="IMPEGNI INFERIORI AL 60%") %>%
    filter(CASI_OGV %in% c("ogv conseguita per dichiarazioni", 
                           "ogv conseguita dic22",
                           "ogv conseguita giu22",
                           "ogv conseguita su salvaguardia 25M", 
                           "ogv conseguita su salvaguardia 200M")) %>%
    filter(CUP_DESCR_NATURA %in% c("ACQUISTO DI BENI", 
                                   "ACQUISTO O REALIZZAZIONE DI SERVIZI", 
                                   "REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)")) %>%
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO IMPEGNI")
  
  # Progetti con PROCEDURALE non almeno in esecuzione
  CHK_iter <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(VERIFICA_PROCEDURALE =="NON IN ESECUZIONE") %>%
    filter(CASI_OGV %in% c("ogv conseguita per dichiarazioni", 
                           "ogv conseguita dic22",
                           "ogv conseguita giu22",
                           "ogv conseguita su salvaguardia 25M", 
                           "ogv conseguita su salvaguardia 200M")) %>%
    filter(CUP_DESCR_NATURA %in% c("ACQUISTO DI BENI", 
                                   "ACQUISTO O REALIZZAZIONE DI SERVIZI", 
                                   "REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)"))%>%
    filter(CHK_LISTE_2 != "no monit") %>%
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO ITER")
  
  # Progetti con ciclo non coerente
  CHK_ciclo <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_CICLO == "CICLO NON COERENTE") %>% 
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO CICLO")
  
  # Progetti con tema non coerente
  CHK_tema <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_TEMI == "tema no") %>% 
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO TEMA")
  
  # Progetti con settore non coerente
  CHK_settore <- analisi %>%
    filter(OGV == "OGV") %>% 
    filter(CHK_LISTE_2 !="no monit") %>%
    filter(CHK_SETTORI == "settore no") %>% 
    filter(RISORSE > 0) %>% 
    group_by(ID_PSC) %>% 
    summarise(N = n(), 
              RISORSE = sum(RISORSE, na.rm=TRUE)) %>% 
    mutate(KPI = "NO SETTORE")
  
  
  # unione delle liste
  temp <- lista %>%
    bind_rows(economie) %>%
    bind_rows(no_ogv) %>%
    bind_rows(monit_ok) %>% 
    bind_rows(no_monit) %>% 
    bind_rows(no_lista) %>% 
    bind_rows(CHK_coe) %>% 
    bind_rows(CHK_impegni) %>% 
    bind_rows(CHK_iter) %>% 
    bind_rows(CHK_ciclo) %>% 
    bind_rows(CHK_tema) %>% 
    bind_rows(CHK_settore) %>% 
    rename(RISORSE_MONIT = RISORSE)
  
  # integra risorse e calcola peso
  appo <- risorse %>% 
    left_join(temp, by = "ID_PSC") %>% 
    mutate(PESO = round(RISORSE_MONIT / RISORSE, 2))
  
  # integra descrizione kpi
  appo1 <- appo %>% 
    mutate(KPI_DESCR = case_when(KPI == "TOTALE" ~ "Tutti gli interventi programmati",
                                 KPI == "ECONOMIE" ~ "Economie di programmazione",
                                 KPI == "NO OGV" ~ "Interventi privi di OGV in programmazione per memoria",
                                 KPI == "MONIT OK" ~ "Interventi programmati e monitorati con importo identico",
                                 KPI == "NO MONIT" ~ "Interventi programmati non monitorati",
                                 KPI == "NO PSC" ~ "Progetti monitorati non previsti tra interventi programmati",
                                 KPI == "DELTA COE" ~ "Interventi programmati monitorati con importo diverso",
                                 KPI == "NO IMPEGNI" ~ "Interventi programmati monitorati con impegni non coerenti con OGV",
                                 KPI == "NO ITER" ~ "Interventi programmati monitorati con iter procedurale non coerente con OGV",
                                 KPI == "NO CICLO" ~ "Interventi programmati monitorati con ciclo diverso",
                                 KPI == "NO TEMA" ~ "Interventi programmati monitorati con tema diverso",
                                 KPI == "NO SETTORE" ~ "Interventi programmati monitorati con settore diverso",
                                 TRUE ~ "CHK")) 
  
  # editing
  report <- appo1 %>% 
    left_join(octk::info_psc %>% 
                select(ID_PSC, TIPO_AR),
              by = "ID_PSC") %>% 
    select(ID_PSC, TIPO_AR, KPI, KPI_DESCR, N, RISORSE, RISORSE_MONIT, PESO)
  
  if (export == TRUE) {
    # OLD:
    # write.xlsx(report, file.path(OUTPUT, "report_data_quality_psc.xlsx"))
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName="report")
    addWorksheet(wb, sheetName="metadati")
    
    writeData(wb, sheet = "report", x = report, colNames = TRUE)
    writeData(wb, sheet = "metadati", x = metadati, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, paste0("report_data_quality_psc_", bimestre, ".xlsx")), overwrite = TRUE)
  }
  return(report)
}



#' Ranking qualità dati PSC
#'
#' Ranking qualità dati PSC
#'
#' @param report Report di analisi qualità dati da report_data_quality_psc()
#' @param export Vuoi esportare il file in OUTPUT?
#' @return Dataframe
ranking_data_quality_psc <- function(report, export=TRUE) {
  
  # TODO: 
  # rivedere metadati
  
  # carica metadati
  metadati <- read_xlsx(file.path(INPUT, "metadati_quality.xlsx"))
  
  temp <- report %>% 
    pivot_wider(id_cols = c("ID_PSC", "TIPO_AR"), names_from = "KPI", values_from = "PESO", values_fill = 0)
  
  appo0 <- report %>% 
    filter(KPI == "TOTALE") %>% 
    select(ID_PSC, RISORSE) %>% 
    left_join(temp, by = "ID_PSC")
  
  # fix colonne mancanti
  if (!("NO PSC" %in% names(appo0))) {
    appo0 <- appo0 %>% 
      mutate(`NO PSC` = 0)
  }
  
  appo1 <- appo0 %>% 
    mutate(x_match = `MONIT OK`,
           x_nolista = 1-`NO PSC`, 
           x_imp = 1-`NO IMPEGNI`,
           x_iter = 1-`NO ITER`,
           x_ciclo = 1-`NO CICLO`,
           x_tema = 1-`NO TEMA`) %>% 
    mutate(quality_index = 0.70*x_match + 0.20*x_nolista + 0.025*x_imp + 0.025*x_iter + 0.025*x_ciclo + 0.025*x_tema) %>%
    select(ID_PSC, TIPO_AR, RISORSE, x_match, x_nolista, x_imp, x_iter, x_ciclo, x_tema, quality_index) %>% 
    arrange(desc(quality_index))
  
  ranking <- appo1
  
  # export
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, sheetName="report")
    addWorksheet(wb, sheetName="metadati")
    
    writeData(wb, sheet = "report", x = ranking, colNames = TRUE)
    writeData(wb, sheet = "metadati", x = metadati, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, paste0("ranking_data_quality_psc_", bimestre, ".xlsx")), overwrite = TRUE)
  }
  
  return(ranking)
}

