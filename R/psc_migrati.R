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
  chk <- operazioni_psc %>% 
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
    programmazione <- load_db_psc(use_flt=TRUE) %>% 
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
    programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE, use_articolaz = TRUE) %>% 
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
    programmazione <- load_db_psc(use_flt=TRUE) %>% 
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
make_report_temi_macroaree_psc_migrati <- function(progetti_psc, operazioni=NULL, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
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
    programmazione <- load_db_psc(use_flt=TRUE) %>% 
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
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
    # clean
    select(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, SEZIONE,
           AREA_TEMATICA, SETTORE_INTERVENTO,
           RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N) %>% 
    arrange(TIPOLOGIA_AMMINISTRAZIONE, ID_PSC, x_CICLO)
  
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