# OC > Toolkit
# Utility per aggiornare gli oc_asset in data/

# MEMO: funzione eseguite in "dev.R" per inizializzare i complementi al package per il bimestre
# WARNING: richiede oc_init


# ----------------------------------------------------------------------------------- #
# setup bimestre


#' Copia i dati di OC da GoogleDrive
#'
#' Copia i dati di OC da GoogleDrive.
#'
#' @param bimestre Versione dei dati di attuazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param data_path Percorso allla fonte dati (senza folder del bimestre).
#' @return I file di "progetti_preesteso" sono copiati nel folder DATA.
oc_init_data <- function(bimestre, data_path=NULL) {
  
  # finanziamenti_preesteso.sas7bdat
  # indicatori_pucok.sas7bdat
  # operazioni_pucok.sas7bdat
  # PROGETTI_PREESTESO.csv
  # PROGETTI_PREESTESO.zip
  
  ROOT <- "/Volumes/GoogleDrive/Drive condivisi"
  
  # wizard dati attuazione
  if (is.null(data_path)) {
    appo <- readline("Quale path per la fonte dati? ")
    data_path <- gsub("\\\"", "", appo)
  }
  
  DATA <- file.path(data_path, bimestre)
  
  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "PROGETTI_PREESTESO.zip"),
            to = file.path(DATA, "PROGETTI_PREESTESO.zip"))
  unzip(zipfile = file.path(DATA, "PROGETTI_PREESTESO.zip"),
        exdir = file.path(DATA))
  
  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "finanziamenti_preesteso.sas7bdat"),
            to = file.path(DATA, "finanziamenti_preesteso.sas7bdat"))
  
  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "indicatori_pucok.sas7bdat"),
            to = file.path(DATA, "indicatori_pucok.sas7bdat"))
  
  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "operazioni_pucok.sas7bdat"),
            to = file.path(DATA, "operazioni_pucok.sas7bdat"))
  
}


#' Carica operazioni 1420
#'
#' Carica operazioni 1420 da dataiku
#'
#' @return Dataset operazinoi da dataiuku.
load_operazioni_1420 <- function() {
  
  col_types = cols(
    db = col_character(),
    cod_locale_progetto = col_character(),
    oc_cod_programma = col_character(),
    oc_descrizione_programma = col_character(),
    CODICE_TIPOLOGIA_PROGRAMMA = col_character(),
    oc_costo_coesione = col_double(),
    oc_impegni_coesione = col_double(),
    oc_tot_pagamenti_coesione = col_double(),
    OC_COD_CICLO = col_double(),
    oc_ambito = col_character(),
    oc_cod_fonte = col_character(),
    oc_descr_fonte = col_character(),
    ue_categ_regione = col_character(),
    ue_descr_categ_regione = col_character(),
    ue_fondo = col_character(),
    ue_descr_fondo = col_character(),
    ue_asse_prioritario = col_character(),
    ue_descr_asse_prioritario = col_character(),
    ue_ob_specifico = col_character(),
    ue_descr_ob_specifico = col_character(),
    fsc_settore_strategico = col_character(),
    fsc_descr_settore_strategico = col_character(),
    fsc_asse_tematico = col_character(),
    fsc_descr_asse_tematico = col_character(),
    pac_asse_tematico = col_character(),
    pac_descr_asse_tematico = col_character(),
    pac_lineazione = col_character(),
    pac_descr_lineazione = col_character(),
    psc_macroarea = col_character(),
    psc_descr_macroarea = col_character(),
    psc_sezione = col_character(),
    psc_descr_sezione = col_character(),
    psc_area_tematica = col_character(),
    psc_descr_area_tematica = col_character(),
    psc_sett_interv = col_character(),
    psc_descr_sett_interv = col_character(),
    cod_tipoint_feasr = col_character(),
    descr_tipoint_feasr = col_character(),
    cod_misura_feasr = col_character(),
    descr_misura_feasr = col_character(),
    cod_submisura_feasr = col_character(),
    descr_submisura_feasr = col_character(),
    cod_farea_feasr = col_character(),
    descr_farea_feasr = col_character(),
    cod_priorita_feasr = col_character(),
    descr_priorita_feasr = col_character(),
    cod_proced_attivazione = col_character(),
    descr_proced_attivazione = col_character(),
    cod_tipo_proced_attivazione = col_character(),
    descr_tipo_proced_attivazione = col_character(),
    data_effettiva_inizio_proc_attiv = col_integer(),
    data_effettiva_fine_proc_attiv = col_integer(),
    COD_RISULTATO_ATTESO = col_character(),
    DESCR_RISULTATO_ATTESO = col_character(),
    COD_STRUMENTO = col_character(),
    DESCR_STRUMENTO = col_character(),
    COD_TIPO_STRUMENTO = col_character(),
    DESCR_TIPO_STRUMENTO = col_character(),
    DATA_APPROV_STRUMENTO = col_double(),
    costo_rendicontabile_UE = col_double(),
    oc_tot_pagamenti_rendicontab_ue = col_double(),
    costo_ammesso_MZ = col_double(),
    costo_ammesso_CN = col_double(),
    imp_ammesso_MZ = col_double(),
    imp_ammesso_CN = col_double(),
    imp_trasf_ammesso_MZ = col_double(),
    imp_trasf_ammesso_CN = col_double(),
    pag_ammesso_MZ = col_double(),
    pag_ammesso_CN = col_double(),
    pag_trasf_ammesso_MZ = col_double(),
    pag_trasf_ammesso_CN = col_double(),
    oc_spesa_certificata_pubblica = col_double(),
    oc_spesa_certificata_totale = col_double(),
    STATO = col_double(),
    dps_flag_pac = col_double(),
    OC_FLAG_TR = col_double(),
    OC_PROGETTO_AGGREGATO = col_character()
  )
  
  operazioni_1420_raw <- read_csv2(file.path(DATA, "oper_pucok_preesteso.csv"), col_types = col_types) 
  return(operazioni_1420_raw)
}
  
  
#' Carica operazioni 713
#'
#' Carica operazioni 713 da dataiku
#'
#' @return Dataset operazinoi da dataiuku.
load_operazioni_713 <- function() {
  
  col_types = cols(
    cod_locale_progetto = col_character(),
    oc_cod_programma = col_character(),
    oc_descrizione_programma = col_character(),
    oc_ambito = col_character(),
    oc_costo_coesione = col_double(),
    oc_impegni_coesione = col_double(),
    oc_tot_pagamenti_coesione = col_double(),
    QSN_AREA_OBIETTIVO_UE = col_character(),
    QSN_FONDO_COMUNITARIO = col_character(),
    qsn_cod_priorita = col_double(),
    qsn_descrizione_priorita = col_character(),
    qsn_cod_obiettivo_generale = col_number(),
    qsn_descr_obiettivo_generale = col_character(),
    qsn_codice_obiettivo_specifico = col_number(),
    qsn_descr_obiettivo_specifico = col_character(),
    qsn_cod_tema_prioritario_ue = col_character(),
    qsn_descr_tema_prioritario_ue = col_character(),
    OC_COD_FONTE = col_character(),
    OC_DESCR_FONTE = col_character(),
    PO_CODICE_ASSE = col_character(),
    PO_DENOMINAZIONE_ASSE = col_character(),
    PO_COD_OBIETTIVO_OPERATIVO = col_character(),
    PO_OBIETTIVO_OPERATIVO = col_character(),
    COD_LINEA = col_character(),
    descr_linea = col_character(),
    COD_AZIONE = col_character(),
    descr_azione = col_character(),
    cod_strumento = col_character(),
    descr_strumento = col_character(),
    descr_tipo_strumento = col_character(),
    data_approv_strumento = col_integer(),
    COSTO_RENDICONTABILE_UE = col_double(),
    OC_TOT_PAGAMENTI_RENDICONTAB_UE = col_double(),
    OC_TOT_PAGAMENTI_FSC = col_double(),
    OC_TOT_PAGAMENTI_PAC = col_double(),
    cod_proced_attivazione = col_character(),
    descr_proced_attivazione = col_character(),
    cod_tipo_proced_attivazione = col_double(),
    descr_tipo_proced_attivazione = col_character(),
    data_prevista_bando_proc_attiv = col_integer(),
    data_effettiva_bando_proc_attiv = col_integer(),
    data_prevista_fine_proc_attiv = col_integer(),
    data_effettiva_fine_proc_attiv = col_integer(),
    OC_FLAG_PAC = col_double(),
    STATO = col_double(),
    status = col_double(),
    attivo_coe = col_double(),
    TOT_PAGAMENTI = col_double(),
    finanz_stato_pac = col_double(),
    finanz_stato_fsc = col_double(),
    impegni = col_double(),
    finanz_totale_pubblico = col_double(),
    finecon_stato_FSC = col_double(),
    finecon_stato_PAC = col_double(),
    finecon_totale_pubblico = col_double(),
    OC_PROGETTO_AGGREGATO = col_character()
  )
  
  operazioni_713_raw <- read_csv2(file.path(DATA, "oper_fltok_preesteso.csv"), col_types = col_types) 
  return(operazioni_713_raw)
}
  
#' Workflow chk bimestre
#'
#' Workflow di controllo dei dati del nuovo bimestre
#'
#' @param progetti File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param export Vuoi esportare i report di controllo in TEMP?
#' @return Messaggi e report di controllo.
workflow_chk_bimestre <- function(progetti, export=FALSE) {
  
  chk_bimestre_sub_fondo_comunitario(progetti, export=export)
  
  chk_bimestre_sub_po_riclass(progetti, export=export) 
  
  chk_bimestre_sub_dbcoe(progetti, export=export)
  
  chk_bimestre_sub_x_vars(progetti, export=export)
  
}



#' Workflow chk bimestre - Controllo fondo comunitario
#'
#' Workflow di controllo dei dati del nuovo bimestre - Controllo fondo comunitario con focus su YEI (che storicamente ha problemi).
#'
#' @param progetti File di tipo progetti da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param export Vuoi esportare i report di controllo in TEMP?
#' @return Messaggi e report di controllo.
chk_bimestre_sub_fondo_comunitario <- function(progetti, export=FALSE) {
  
  message("Controlli su fondo comunitario (in particolare per YEI")
  
  print("Tassonomia generale per fondo comunitario:")
  chk <- progetti %>% count(FONDO_COMUNITARIO)
  print(chk)
  
  print("Fondo comunitario per progetti YEI:")
  chk <- progetti %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>% count(FONDO_COMUNITARIO)
  print(chk)
  
  print("Fondo comunitario per progetti YEI post correzione in fix_progetti():")
  chk <- progetti %>% fix_progetti(.) %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>% count(FONDO_COMUNITARIO)
  print(chk)
  
  if (export==TRUE){
    # DO SOMETHING...
  }
  
  message("Se serve integra correzione in fix_progetti(), poi rilancia")
  
}
  
  
#' Workflow chk bimestre - Controllo x_vars
#'
#' Workflow di controllo dei dati del nuovo bimestre - Controllo su po_riclass.csv e x_vars
#'
#' @param progetti File di tipo progetti da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param export Vuoi esportare i report di controllo in TEMP?
#' @return Messaggi e report di controllo.
chk_bimestre_sub_po_riclass <- function(progetti, export=FALSE) {
  
  # MEMO: parte dalla versione di sviluppo di "octk::po_riclass" che si assume aggiornata al bimestre precedente
  
  # TIPO
  # 0: programma normale
  # 1: programma misto con ":::"
  # 2: programma duplicato lato IGRUE (2 programmi con CCI diversi ma uno è vuoto)
  # 3: progetti/programma da accorpare (e programma/unione post accorpamento fino a 0.2.5)
  # 4: programma censito lato programmazione e ancora da caricare in BDU
  # 5: programma/unione post accorpamento (sostituisce casi con 3 da 0.2.6)
  # 6: programma fittizio per completamenti
  # 8: programma da verificare
  # 9: programma disattivato in BDU
  
  # load da DB programmazione
  po <- octk::po_riclass %>%
    filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disattivati
           # x_AMBITO != "FEASR",
           x_CICLO != "2000-2006") %>%
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))

  # chk mismatch con po_riclass (trova nuovi programmi unici)
  chk1 <- progetti %>% 
    count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>%
    anti_join(octk::po_riclass, by = "OC_CODICE_PROGRAMMA") %>%
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))

  # chk mismatch con po_riclass (trova nuovi programmi misti)
  chk2 <- progetti %>% 
    count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>%
    anti_join(octk::po_riclass, by = "OC_CODICE_PROGRAMMA") %>%
    filter(grepl(":::", OC_CODICE_PROGRAMMA))
  
  # chk anomalie da match con get_x_vars (diverse da nuovi programmi)
  chk3 <- progetti %>%
    fix_progetti(.) %>%
    get_x_vars(., progetti=progetti) %>%
    filter(is.na(x_CICLO) | is.na(x_AMBITO)) %>%
    anti_join(chk1, by = "OC_CODICE_PROGRAMMA") %>%
    anti_join(chk2, by = "OC_CODICE_PROGRAMMA") %>%
    count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO, FONDO_COMUNITARIO)
  
  # chk duplicati da applicazione x_vars
  chk4 <- progetti %>%
    get_x_vars(., progetti=progetti) %>%
    semi_join(progetti %>%
                fix_progetti(.) %>%
                get_x_vars(., progetti=progetti) %>%
                count(COD_LOCALE_PROGETTO) %>%
                filter(n > 1),
              by = "COD_LOCALE_PROGETTO")
  # MEMO: uso semi_join per visualizzare contesto
  # write.xlsx(chk2, file.path(TEMP, "progetti_clp_duplo.xlsx"))

  # log
  message(paste0("Rilevati ", dim(chk1)[1], " nuovi programmi unici:"))
  print(chk1)

  message(paste0("Rilevati ", dim(chk2)[1], " nuovi programmi misti:"))
  print(chk2)

  message(paste0("Rilevate ", dim(chk3)[1], " anomalie nell'applicazione delle x_vars:"))
  print(chk3)
  
  message(paste0("Rilevate ", dim(chk4)[1], " duplicazioni nell'applicazione delle x_vars:"))
  print(chk4)

  if (export==TRUE) {
    write_csv2(chk1, file.path(TEMP, "chk_mismatch_po_riclass_unici.csv"))
    write_csv2(chk2, file.path(TEMP, "chk_mismatch_po_riclass_misti.csv"))
    write_csv2(chk3, file.path(TEMP, "chk_mismatch_po_riclass_anomalie.csv"))
    write_csv2(chk4, file.path(TEMP, "chk_mismatch_po_riclass_duplicazioni.csv"))
  }
  
  message("Integra po_riclass.csv (vedi file 'chk_mismatch_po_riclass_xxxxx.csv').")
  
}
# TODO: rigenerare po_riclass direttamente dal DB


#' Workflow chk bimestre - Controllo su DBCOE
#'
#' Workflow di controllo dei dati del nuovo bimestre - Controllo di coerenza con DBCOE
#'
#' @param progetti File di tipo progetti da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param export Vuoi esportare i report di controllo in TEMP?
#' @return Messaggi e report di controllo.
chk_bimestre_sub_dbcoe <- function(progetti, export=FALSE) {

  po_psc <- read_csv2(file.path(DRIVE, "DATI", "PSC", "info", "matrix_po_psc.csv"))
  
  programmi <- init_programmazione_dati(DB=DB) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA) %>%
    count(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
  
  chk <- progetti %>% 
    count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, X_CICLO, X_AMBITO, OC_FLAG_VISUALIZZAZIONE) %>%
    # scarta programmi nel DBCOE
    anti_join(programmi, by = "OC_CODICE_PROGRAMMA") %>%
    # scarta programmi confluiti nei PSC
    anti_join(po_psc, by = "OC_CODICE_PROGRAMMA") %>%
    # scarta programmi misti
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>% 
    # scarta programmi non visualizzati
    filter(!(OC_FLAG_VISUALIZZAZIONE %in% c(5, 7)))
  
  # log
  message(paste0("Rilevati ", dim(chk)[1], " nuovi programmi assenti in DBCOE:"))
  print(chk)
  if (export==TRUE) {
    write_csv2(chk, file.path(TEMP, "chk_mismatch_dboce.csv"))
  }
  
  message("Integra il DBCOE in caso di codifiche assenti (vedi file 'chk_mismatch_dboce.csv').")

}




#' Workflow chk bimestre - Controllo su x_vars
#'
#' Workflow di controllo dei dati del nuovo bimestre - Controllo di coerenza tra x_vars applicate da octk e dati originali da sas
#'
#' @param progetti File di tipo progetti da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param export Vuoi esportare i report di controllo in TEMP?
#' @return Messaggi e report di controllo.
chk_bimestre_sub_x_vars <- function(progetti, export=FALSE) {
  
  appo <- progetti %>%
    # rename(X_REGNAZ = x_regnaz) %>% 
    fix_progetti(.) %>%
    get_x_vars(., progetti=progetti) %>%
    get_macroarea(progetti=progetti, real_reg=TRUE) %>%
    get_regione_simply(progetti=progetti, real_reg=TRUE) 
  
  # divergenze ciclo e ambito
  chk1 <- appo %>%
    count(x_CICLO, X_CICLO, x_AMBITO, X_AMBITO) %>% 
    filter(x_CICLO != X_CICLO | x_AMBITO != X_AMBITO)
  
  # chk contributi comuni snai (risorse ordinarie)
  chk2 <- appo %>%
    filter(x_AMBITO == "SNAI", X_AMBITO == "FSC") %>% 
    count(x_CICLO, X_CICLO, x_AMBITO, X_AMBITO, OC_DESCRIZIONE_PROGRAMMA)

  # chk regnaz
  chk3 <- appo %>%
    filter(x_REGNAZ != X_REGNAZ) %>% 
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>% 
    count(x_CICLO, x_AMBITO, x_REGNAZ, X_REGNAZ, OC_DESCRIZIONE_PROGRAMMA)
  
  # chk regnaz vs regione
  chk4 <- appo %>%
    filter(x_REGNAZ != "NAZ") %>% 
    filter(x_REGNAZ != x_REGIONE) %>% 
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>% 
    count(x_CICLO, x_AMBITO, x_REGNAZ, x_REGIONE, DEN_REGIONE, DEN_PROVINCIA, OC_DESCRIZIONE_PROGRAMMA)
  
  
  # chk macroarea
  chk5 <- appo %>% 
    mutate(OC_MACROAREA = case_when(OC_MACROAREA == "Ambito Nazionale" ~ "Ambito nazionale",
                                    OC_MACROAREA ==	"Non definibile" ~ "Trasversale",
                                    TRUE ~ OC_MACROAREA)) %>%
    filter(x_MACROAREA != OC_MACROAREA) %>% 
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>% 
    count(x_CICLO, x_AMBITO, x_REGIONE, x_MACROAREA, OC_MACROAREA, OC_DESCRIZIONE_PROGRAMMA)
  
  
  reg_cn <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
              "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO", "PA TRENTO", "PA BOLZANO")
  reg_sud <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  chk6 <- appo %>% 
    mutate(TEMP = case_when(x_REGIONE %in% reg_cn ~ "Centro-Nord",
                            x_REGIONE %in% reg_sud ~ "Mezzogiorno",
                            TRUE ~ as.character(x_MACROAREA))) %>%
    filter(x_MACROAREA != TEMP) %>% 
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>% 
    count(x_CICLO, x_AMBITO, x_REGIONE, x_MACROAREA, OC_DESCRIZIONE_PROGRAMMA)


  # log
  message(paste0("Rilevate ", dim(chk1)[1], " divergenze per x_CICLO e X_CICLO oppure x_AMBITO e X_AMBITO:"))
  print(chk1)

  message(paste0("di cui relative al programma per contributi ai comuni delle aree internne (risorse ordinarie):"))
  print(chk2)
  
  message(paste0("Rilevate ", dim(chk3)[1], " divergenze per x_REGNAZ e X_REGNAZ:"))
  print(chk3)
  
  message(paste0("Rilevate ", dim(chk4)[1], " anomalie tra x_REGIONE e x_REGNAZ:"))
  print(chk4)
  
  message(paste0("Rilevate ", dim(chk5)[1], " anomalie tra x_MACROAREA e OC_MACROAREA:"))
  print(chk5)
  if (export==TRUE) {
    write_csv2(chk5, file.path(TEMP, "chk_mismatch_x_vars_macroaree.csv"))
  }
  
  message(paste0("Rilevate ", dim(chk6)[1], " anomalie tra x_MACROAREA e x_REGIONE:"))
  print(chk6)
  if (export==TRUE) {
    write_csv2(chk6, file.path(TEMP, "chk_mismatch_x_vars_macroaree_regioni.csv"))
  }
}



#' Workflow delta bimestre
#'
#' Workflow di verifica della variazione dei dati tra due bimestri 
#'
#' @param progetti File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param progetti_old File di tipo progetti per un bimestre precedente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @return Messaggi e report di controllo.
workflow_delta_bimestre <- function(progetti, progetti_old) {
  
  # DEV: è separato da workflow_chk_bimestre() perché presuppone l'assestamento di po_riclass prima fi girare
  
  delta_bimestre_sub_sintesi(progetti, progetti_old)
  
  delta_bimestre_sub_progetti(progetti, progetti_old)
  
}


#' Workflow delta bimestre - Controllo per ciclo, ambito e visualizzazione
#'
#' Workflow di verifica della variazione dei dati tra due bimestri - Controllo del delta su bimestre precedente, per ciclo, ambito e casistiche di visualizzazione
#'
#' @param progetti File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param progetti_old File di tipo progetti per un bimestre precedente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @return Messaggi e report di controllo.
delta_bimestre_sub_sintesi <- function(progetti, progetti_old) {
  
  chk1 <- progetti_old %>%
    fix_progetti(.) %>%
    get_x_vars(., progetti=progetti_old) %>%
    group_by(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(OC_COSTO_COESIONE, na.rm = TRUE),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)) %>%
    full_join(progetti %>%
                fix_progetti(.) %>%
                get_x_vars(., progetti=progetti) %>%
                group_by(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
                summarise(N = n(),
                          COE = sum(OC_COSTO_COESIONE, na.rm = TRUE),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)),
              by = c("OC_FLAG_VISUALIZZAZIONE", "x_CICLO", "x_AMBITO"), suffix = c(".old", ".new")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(N.chk = N.new - N.old,
           COE.chk = COE.new - COE.old,
           CP.chk = CP.new - CP.old) %>%
    select(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO,
           N.old, N.new, N.chk,
           COE.old, COE.new, COE.chk,
           CP.old, CP.new, CP.chk)
  
  # log
  temp <- chk1 %>% filter(abs(COE.chk) > 500000000) %>% select(-CP.old, -CP.new, -CP.chk)
  print(paste0("Rilevate ", dim(temp)[1], " variazioni di COE superiori a 500 Meuro:"))
  print(temp)
  
  message("Controlla le variazioni tra bimestri per ciclo, ambito e visualizzazione nel file (vedi 'chk_delta_BIMESTRE.xlsx' in TEMP).")
  write.xlsx(chk1, file.path(TEMP, paste0("chk_delta_", bimestre, ".xlsx")))
  
}





#' Workflow delta bimestre - Controllo per singoli interventi
#'
#'Workflow di verifica della variazione dei dati tra due bimestri - Controllo del delta per singoli interventi
#'
#' @param progetti File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param progetti_old File di tipo progetti per un bimestre precedente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @return Messaggi e report di controllo.
delta_bimestre_sub_progetti <- function(progetti, progetti_old) {
  
  # singoli progetti (sopra 1 Meuro)
  chk2 <- progetti_old %>%
    fix_progetti(.) %>%
    get_x_vars(., progetti=progetti) %>%
    select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO, CP = OC_FINANZ_TOT_PUB_NETTO) %>%
    full_join(progetti %>%
                fix_progetti(.) %>%
                get_x_vars(progetti=progetti) %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO, CP = OC_FINANZ_TOT_PUB_NETTO),
              by = "COD_LOCALE_PROGETTO", suffix = c(".old", ".new")) %>%
    mutate(CP.chk = CP.new - CP.old) %>% 
    filter(abs(CP.chk) > 1000000)
  
  # log
  temp <- chk2 %>% filter(CP.new > 0, (CP.old == 0 | is.na(CP.old)))
  message(paste0("Rilevati ", dim(temp)[1], " nuovi progetti."))
  
  temp <- chk2 %>% filter((CP.new == 0 | is.na(CP.new)), CP.old > 0)
  message(paste0("Rilevati ", dim(temp)[1], " progetti eliminati."))
  
  message("Controlla le variazioni tra bimestri per progetti nel file (vedi 'chk_delta_BIMESTRE.xlsx' in TEMP con variazioni sopra 1 Meuro).")
  write.xlsx(chk2, file.path(TEMP, paste0("chk_delta_", bimestre, "_progetti.xlsx")), rowNames = FALSE)
  
}


# ----------------------------------------------------------------------------------- #
# raw data


#' Workflow integrazione raw data
#'
#' Workflow integrazione raw data per nuovo bimestre. Aggirona i file csv nel repository in TOOLS > OCTK > _dat.
#'
#' @param progetti File di tipo progetti per il bimestre corrente da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param RAWDATA Percorso al repository (da file.path(DRIVE, "DATI", "OCTK", "_dat"))
#' @return File nel repository in TOOLS > OCTK > _dat.
workflow_update_rawdata <- function(progetti, RAWDATA) {
  
  progetti <- fix_progetti(progetti)
  
  # po_linee_azioni.csv
  make_matrix_po(bimestre, progetti)

  # strum_att.csv
  make_matrix_strum(bimestre, progetti)
  
  # delib_cipe.csv
  make_matrix_cipe(bimestre)
  
  # prog_comp.csv
  make_prog_comp(bimestre, progetti)
  
  # patt.csv
  make_patt(bimestre, progetti)
  
  # comuni
  make_comuni()
  
  # ra
  # progetti %>%
  #   count(COD_RISULTATO_ATTESO, DESCR_RISULTATO_ATTESO) %>%
  #   filter(!(grepl(":::", COD_RISULTATO_ATTESO))) %>%
  #   rename(COD_RIS_ATTESO = COD_RISULTATO_ATTESO) %>% 
  #   full_join(octk::ra, by = "COD_RIS_ATTESO") %>% 
  #   filter(is.na(QUERY)) %>% 
  #   write_csv2(file.path(TEMP, "ra.csv"))
  # HAND: estendere lista RA
  
}


#' Workflow integrazione raw data
#'
#' Workflow integrazione raw data per nuovo bimestre. Rende disponibili nel package in formato .Rdata i file nel repository in TOOLS > OCTK > _dat.
#'
#' @param RAWDATA Percorso al repository (da file.path(DRIVE, "DATI", "OCTK", "_dat"))
#' @return Messaggi e report di controllo.
workflow_setup_rawdata <- function(RAWDATA) {
  # po_riclass
  po_riclass <- read_csv2(file.path(RAWDATA, "po_riclass.csv")) %>%
    # MEMO: raw contiene NA per i programmi POC 2014-2020 > fanno casino perché join con progetti è sempre su OC_COD_PROGRAMMA
    filter(!is.na(OC_CODICE_PROGRAMMA))
  usethis::use_data(po_riclass, overwrite = TRUE)
  
  # po_riclass
  # po_riclass_ext <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_riclass_ext.csv"))
  # usethis::use_data(po_riclass_ext, overwrite = TRUE)
  
  # matrix_comuni
  comuni <- read_csv2(file.path(RAWDATA, "matrix_comuni.csv"))
  usethis::use_data(comuni, overwrite = TRUE)
  # MEMO: questo viene workflow sas di Luca
  
  # matrix_op
  matrix_op <- read_csv2(file.path(RAWDATA, "matrix_op.csv"))
  usethis::use_data(matrix_op, overwrite = TRUE)
  
  # matrix_opos
  matrix_opos <- read_csv2(file.path(RAWDATA, "matrix_opos.csv"))
  usethis::use_data(matrix_opos, overwrite = TRUE)
  
  # matrix_ra_opos
  matrix_ra_opos <- read_csv2(file.path(RAWDATA, "matrix_ra_opos.csv"))
  usethis::use_data(matrix_ra_opos, overwrite = TRUE)
  
  # matrix_ra_temi_fsc
  matrix_ra_temi_fsc <- read_csv2(file.path(RAWDATA, "matrix_ra_temi_fsc.csv"))
  usethis::use_data(matrix_ra_temi_fsc, overwrite = TRUE)
  
  # categorie_cup
  categorie_cup <- read_csv2(file.path(RAWDATA, "categorie_cup.csv"))
  usethis::use_data(categorie_cup, overwrite = TRUE)
  
  # po_linee_azioni
  po_linee_azioni <- read_csv2(file.path(RAWDATA, "po_linee_azioni.csv"))
  usethis::use_data(po_linee_azioni, overwrite = TRUE)
  
  # categorie_ue
  # make_matrix_ue()
  # MEMO: va creato staticamente sulla base delle tabelle di contesto
  categorie_ue <- read_csv2(file.path(RAWDATA, "categorie_ue.csv"))
  usethis::use_data(categorie_ue, overwrite = TRUE)
  
  # ra
  ra <- read_csv2(file.path(RAWDATA, "ra.csv"))
  usethis::use_data(ra, overwrite = TRUE)
  
  # aree_temi_fsc
  aree_temi_fsc <- read_csv2(file.path(RAWDATA, "aree_temi_fsc.csv"))
  usethis::use_data(aree_temi_fsc, overwrite = TRUE)
  
  # aree_temi_psc
  aree_temi_psc <- read_csv2(file.path(RAWDATA, "aree_temi_psc.csv"))
  usethis::use_data(aree_temi_psc, overwrite = TRUE)
  
  
  # strum_att
  strum_att <- read_csv2(file.path(RAWDATA, "strum_att.csv"))
  usethis::use_data(strum_att, overwrite = TRUE)
  
  # prog_comp
  prog_comp <- read_csv2(file.path(RAWDATA, "prog_comp.csv"))
  usethis::use_data(prog_comp, overwrite = TRUE)
  
  # delib_cipe
  delib_cipe <- read_csv2(file.path(RAWDATA, "delib_cipe.csv"))
  usethis::use_data(delib_cipe, overwrite = TRUE)
  
  # patt
  patt <- read_csv2(file.path(RAWDATA, "patt.csv"))
  usethis::use_data(patt, overwrite = TRUE)
  
  # qsn
  qsn <- read_csv2(file.path(RAWDATA, "qsn.csv"), col_types = "ccic")
  usethis::use_data(qsn, overwrite = TRUE)
  
  # tipologie_cup
  tipologie_cup <- read_csv2(file.path(RAWDATA, "tipologie_cup.csv"))
  usethis::use_data(tipologie_cup, overwrite = TRUE)
  
  # flag_beniconf
  flag_beniconf <- read_csv2(file.path(RAWDATA, "flag_beniconf.csv"))
  usethis::use_data(flag_beniconf, overwrite = TRUE)
  
  # stoplist
  stoplist <- read_csv2(file.path(RAWDATA, "template_stoplist.csv"))
  usethis::use_data(stoplist, overwrite = TRUE)
  
  # safelist
  safelist <- read_csv2(file.path(RAWDATA, "template_safelist.csv"))
  usethis::use_data(safelist, overwrite = TRUE)
  
  # fixlist
  fixlist <- read_csv2(file.path(RAWDATA, "template_fixlist.csv"))
  usethis::use_data(fixlist, overwrite = TRUE)
  
  # moniton_clp
  monithon_clp <- read_csv2(file.path(RAWDATA, "monithon_clp.csv"))
  usethis::use_data(monithon_clp, overwrite = TRUE)
  
  # fixlist
  keyword <- read_csv2(file.path(RAWDATA, "template_query_keyword.csv"))
  usethis::use_data(keyword, overwrite = TRUE)
  
  # forma_giuridica_soggetti
  forma_giuridica_soggetti <- read_csv2(file.path(RAWDATA, "forma_giuridica_soggetti.csv"), col_types = "ccccl")
  usethis::use_data(forma_giuridica_soggetti, overwrite = TRUE)
  
  
  # info_psc
  info_psc <- read_csv2(file.path(RAWDATA, "info_psc.csv"), col_types = "ccccc")
  usethis::use_data(info_psc, overwrite = TRUE)
  
  # info_psc_matrix_temi
  info_psc_matrix_temi <- read_csv2(file.path(RAWDATA, "info_psc_matrix_temi.csv"), col_types = "ccccc")
  usethis::use_data(info_psc_matrix_temi, overwrite = TRUE)
  
  
  # sil
  sil <- read_csv2(file.path(RAWDATA, "sil.csv"), col_types = "c")
  usethis::use_data(sil, overwrite = TRUE)
}


# ----------------------------------------------------------------------------------- #
# progetti_light

#' Crea progetti_light.csv
#'
#' Crea un file progetti da progetti_esteso.csv, con le sole variabili fondamentali. Aggiunge blocco x_var e x_MACROAREA.
#'
#' @param bimestre Bimestre di riferimento.
#' @param progetti File di tipo progetti da load_progetti(light=FALSE, visualizzati=FALSE)
#' @param operazioni_713 File di tipo operazioni da flusso sas/dataiku.
#' @return Il dataset viene salvato in DATA e può essere caricato con load_progetti(light = TRUE).
setup_progetti_light <- function(bimestre, progetti, operazioni_713, fix = FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {
    # loads
    # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = FALSE)
    # progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE, light = FALSE) # MEMO: versione con SNAI-FEASR

    message("Avvio creazione di progetti_light")
    
    # clean
    progetti_light <- progetti %>%
      select(COD_LOCALE_PROGETTO,
             CUP,
             OC_TITOLO_PROGETTO,
             OC_SINTESI_PROGETTO,
             # OC_LINK,
             # OC_COD_CICLO,
             # OC_DESCR_CICLO,
             OC_COD_TEMA_SINTETICO,
             # OC_TEMA_SINTETICO,
             # COD_GRANDE_PROGETTO,
             # DESCRIZIONE_GRANDE_PROGETTO,
             OC_COD_FONTE, # inseria per funzionamento di fix_progetti
             # OC_DESCR_FONTE,
             FONDO_COMUNITARIO,
             OC_CODICE_PROGRAMMA,
             OC_DESCRIZIONE_PROGRAMMA,
             # COD_OB_TEMATICO,
             # DESCR_OB_TEMATICO,
             # COD_PRIORITA_INVEST,
             # DESCR_PRIORITA_INVEST,
             COD_RISULTATO_ATTESO,
             DESCR_RISULTATO_ATTESO,
             OC_COD_CATEGORIA_SPESA,
             OC_DESCR_CATEGORIA_SPESA,
             # OC_ARTICOLAZIONE_PROGRAMMA,
             # OC_SUBARTICOLAZIONE_PROGRAMMA,
             OC_COD_ARTICOLAZ_PROGRAMMA,
             OC_DESCR_ARTICOLAZ_PROGRAMMA,
             OC_COD_SUBARTICOLAZ_PROGRAMMA,
             OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             COD_STRUMENTO,
             DESCR_STRUMENTO,
             DESCR_TIPO_STRUMENTO,

             COD_PROGETTO_COMPLESSO,
             DESCRIZIONE_PROGETTO_COMPLESSO,
             COD_TIPO_COMPLESSITA,
             DESCR_TIPO_COMPLESSITA,

             CUP_COD_NATURA,
             CUP_DESCR_NATURA,
             CUP_COD_TIPOLOGIA,
             CUP_DESCR_TIPOLOGIA,
             CUP_COD_SETTORE,
             CUP_DESCR_SETTORE,
             CUP_COD_SOTTOSETTORE,
             CUP_DESCR_SOTTOSETTORE,
             CUP_COD_CATEGORIA,
             CUP_DESCR_CATEGORIA,
             # COD_ATECO,
             # DESCRIZIONE_ATECO,
             # OC_COD_TIPO_AIUTO,
             # OC_DESCR_TIPO_AIUTO,
             COD_REGIONE,
             DEN_REGIONE,
             COD_PROVINCIA,
             DEN_PROVINCIA,
             COD_COMUNE,
             DEN_COMUNE,
             # OC_COD_SLL,
             # OC_DENOMINAZIONE_SLL,
             # FINANZ_UE,
             # FINANZ_UE_FESR,
             # FINANZ_UE_FSE,
             # FINANZ_UE_FEASR,
             # FINANZ_UE_FEAMP,
             # FINANZ_UE_IOG,
             # FINANZ_STATO_FONDO_DI_ROTAZIONE,
             # FINANZ_STATO_FSC,
             # FINANZ_STATO_PAC,
             # FINANZ_STATO_COMPLETAMENTI,
             # FINANZ_STATO_ALTRI_PROVVEDIMENTI,
             # FINANZ_REGIONE,
             # FINANZ_PROVINCIA,
             # FINANZ_COMUNE,
             # FINANZ_RISORSE_LIBERATE,
             # FINANZ_ALTRO_PUBBLICO,
             # FINANZ_STATO_ESTERO,
             # FINANZ_PRIVATO,
             # FINANZ_DA_REPERIRE,
             # FINANZ_TOTALE_PUBBLICO,
             # ECONOMIE_TOTALI,
             # ECONOMIE_TOTALI_PUBBLICHE,
             OC_FINANZ_UE_NETTO, # DA RIPRISTINARE
             # OC_FINANZ_UE_FESR_NETTO,
             # OC_FINANZ_UE_FSE_NETTO,
             # OC_FINANZ_UE_FEASR_NETTO,
             # OC_FINANZ_UE_FEAMP_NETTO,
             # OC_FINANZ_UE_IOG_NETTO,
             OC_FINANZ_STATO_FONDO_ROT_NETTO = OC_FINANZ_Stato_Fondo_Rot_NETTO,
             OC_FINANZ_STATO_FSC_NETTO = OC_FINANZ_Stato_FSC_NETTO,
             OC_FINANZ_STATO_PAC_NETTO = OC_FINANZ_Stato_PAC_NETTO,
             # OC_FINANZ_STATO_COMPL_NETTO,
             # OC_FINANZ_STATO_ALTRI_PROV_NETTO,
             # OC_FINANZ_REGIONE_NETTO,
             # OC_FINANZ_PROVINCIA_NETTO,
             # OC_FINANZ_COMUNE_NETTO,
             # OC_FINANZ_RISORSE_LIBERATE_NETTO,
             # OC_FINANZ_ALTRO_PUBBLICO_NETTO,
             # OC_FINANZ_STATO_ESTERO_NETTO,
             # OC_FINANZ_PRIVATO_NETTO,
             OC_FINANZ_TOT_PUB_NETTO,
             IMPEGNI,
             TOT_PAGAMENTI,
             
             COSTO_REALIZZATO,
             
             # COSTO_RENDICONTABILE_UE,
             # OC_TOT_PAGAMENTI_RENDICONTAB_UE,
             # OC_TOT_PAGAMENTI_FSC,
             # OC_TOT_PAGAMENTI_PAC,
             
             # variabili coesione (versione da progetti che non distingue ambito)
             OC_COSTO_COESIONE,
             OC_IMPEGNI_COESIONE,
             OC_PAGAMENTI_COESIONE,
             
             # OC_DATA_INIZIO_PROGETTO,
             # OC_DATA_FINE_PROGETTO_PREVISTA,
             # OC_DATA_FINE_PROGETTO_EFFETTIVA,
             # DATA_INIZIO_PREV_STUDIO_FATT,
             # DATA_INIZIO_EFF_STUDIO_FATT,
             # DATA_FINE_PREV_STUDIO_FATT,
             # DATA_FINE_EFF_STUDIO_FATT,
             # DATA_INIZIO_PREV_PROG_PREL,
             # DATA_INIZIO_EFF_PROG_PREL,
             # DATA_FINE_PREV_PROG_PREL,
             # DATA_FINE_EFF_PROG_PREL,
             # DATA_INIZIO_PREV_PROG_DEF,
             # DATA_INIZIO_EFF_PROG_DEF,
             # DATA_FINE_PREV_PROG_DEF,
             # DATA_FINE_EFF_PROG_DEF,
             # DATA_INIZIO_PREV_PROG_ESEC,
             # DATA_INIZIO_EFF_PROG_ESEC,
             # DATA_FINE_PREV_PROG_ESEC,
             # DATA_FINE_EFF_PROG_ESEC,
             # DATA_INIZIO_PREV_AGG_BANDO,
             # DATA_INIZIO_EFF_AGG_BANDO,
             # DATA_FINE_PREV_AGG_BANDO,
             # DATA_FINE_EFF_AGG_BANDO,
             # DATA_INIZIO_PREV_STIP_ATTRIB,
             # DATA_INIZIO_EFF_STIP_ATTRIB,
             # DATA_FINE_PREV_STIP_ATTRIB,
             # DATA_FINE_EFF_STIP_ATTRIB,
             # DATA_INIZIO_PREV_ESECUZIONE,
             # DATA_INIZIO_EFF_ESECUZIONE,
             # DATA_FINE_PREV_ESECUZIONE,
             # DATA_FINE_EFF_ESECUZIONE,
             # DATA_INIZIO_PREV_COLLAUDO,
             # DATA_INIZIO_EFF_COLLAUDO,
             # DATA_FINE_PREV_COLLAUDO,
             # DATA_FINE_EFF_COLLAUDO,
             # OC_STATO_FINANZIARIO,
             OC_STATO_PROGETTO,
             OC_STATO_PROCEDURALE,
             OC_COD_FASE_CORRENTE,
             OC_DESCR_FASE_CORRENTE,
             COD_PROCED_ATTIVAZIONE,
             DESCR_PROCED_ATTIVAZIONE,
             # COD_TIPO_PROCED_ATTIVAZIONE,
             # DESCR_TIPO_PROCED_ATTIVAZIONE,
             # OC_CODFISC_PROGRAMMATORE,
             # OC_DENOM_PROGRAMMATORE,
             # OC_COD_FORMA_GIU_PROGRAMMATORE,
             # OC_DESCR_FORMA_GIU_PROGRAMMATORE,
             # OC_TOTALE_PROGRAMMATORI,
             # OC_CODFISC_ATTUATORE,
             # OC_DENOM_ATTUATORE,
             # OC_COD_FORMA_GIU_ATTUATORE,
             # OC_DESCR_FORMA_GIU_ATTUATORE,
             # OC_TOTALE_ATTUATORI,
             OC_CODFISC_BENEFICIARIO,
             OC_DENOM_BENEFICIARIO,
             OC_COD_FORMA_GIU_BENEFICIARIO,  # DA RIPRISTINARE
             # OC_DESCR_FORMA_GIU_BENEFICIARIO,  # DA RIPRISTINARE
             # OC_TOTALE_BENEFICIARI,
             # OC_CODFISC_REALIZZATORE,
             # OC_DENOM_REALIZZATORE,
             # OC_COD_FORMA_GIU_REALIZZATORE,
             # OC_DESCR_FORMA_GIU_REALIZZATORE,
             # OC_TOTALE_REALIZZATORI,
             # OC_TOTALE_INDICATORI,
             # COD_INDICATORE_1,
             # DESCR_INDICATORE_1,
             # UNITA_MISURA_INDICATORE_1,
             # PROGRAMMATO_INDICATORE_1,
             # REALIZZATO_INDICATORE_1,
             # COD_INDICATORE_2,
             # DESCR_INDICATORE_2,
             # UNITA_MISURA_INDICATORE_2,
             # PROGRAMMATO_INDICATORE_2,
             # REALIZZATO_INDICATORE_2,
             # COD_INDICATORE_3,
             # DESCR_INDICATORE_3,
             # UNITA_MISURA_INDICATORE_3,
             # PROGRAMMATO_INDICATORE_3,
             # REALIZZATO_INDICATORE_3,
             # COD_INDICATORE_4,
             # DESCR_INDICATORE_4,
             # UNITA_MISURA_INDICATORE_4,
             # PROGRAMMATO_INDICATORE_4,
             # REALIZZATO_INDICATORE_4,
             # OC_FLAG_REGIONE_UNICA,
             OC_FLAG_VISUALIZZAZIONE,
             # OC_FLAG_PAC,
             # DATA_AGGIORNAMENTO,
             # OC_FOCUS
             OC_FLAG_BENICONF = OC_FLAG_TAG_BENICONF,
             OC_FLAG_COVID = COVID,
             OC_MACROAREA,
             SNAI,
             # COD_AREA_INT, # MEMO: queste entrano dopo
             # AREA_INTERNA
             IMPORTO_AGGIUDICATO,
             IMPORTO_AGGIUDICATO_NODATA,
             IMPORTO_AGGIUDICATO_BANDITO,
             
             COD_AREA_INT = OC_COD_AI, 
             AREA_INTERNA = OC_DENOM_AI
      )

    # add QSN
    message("...integro variabili qsn")
    progetti_light <- progetti_light %>%
      left_join(operazioni_713_raw %>%
                  distinct(COD_LOCALE_PROGETTO = cod_locale_progetto,
                           QSN_CODICE_OBIETTIVO_SPECIFICO = qsn_codice_obiettivo_specifico,
                           QSN_DESCR_OBIETTIVO_SPECIFICO = qsn_descr_obiettivo_specifico), 
                by = "COD_LOCALE_PROGETTO")

    # # NEW:
    # progetti_light <- progetti_light %>%
    #   left_join(progetti %>%
    #               select(COD_LOCALE_PROGETTO, OC_COD_CICLO),
    #             by = "COD_LOCALE_PROGETTO") %>%
    #   mutate(x_CICLO = case_when(x_AMBITO == "FSC" & OC_COD_CICLO == 9 ~ "2000-2006",
    #                              x_AMBITO == "FSC" & OC_COD_CICLO == 1 ~ "2007-2013",
    #                              x_AMBITO == "FSC" & OC_COD_CICLO == 2 ~ "2014-2020",
    #                              x_AMBITO == "FSC" & OC_COD_CICLO == 3 ~ "2021-2027",
    #                              TRUE ~ x_CICLO))

    # clean & fix
    if (fix == TRUE) {
      message("...applico fix")
      progetti_light <- fix_progetti(progetti_light)
    }
    message("...applico x_vars")
    progetti_light <- get_x_vars(progetti_light, progetti=progetti)
    
    message("...calcolo macroarea")
    # progetti_light <- get_macroarea(progetti_light, progetti_light, real_reg=TRUE)
    progetti_light <- get_macroarea_oc(progetti_light, progetti=progetti)
    
    message("...calcolo regione")
    progetti_light <- get_regione_simply(progetti_light, progetti=progetti)

    # export
    message("...salvataggio")
    write.csv2(progetti_light, file.path(DATA, paste0("progetti_light_", bimestre, ".csv")), row.names = FALSE)

  } else {
    message("Non hai definito il folder DATA. Carica 'oc' ed inizializza 'oc_init()'.")
  }
}


# ----------------------------------------------------------------------------------- #
# analisi nuovi po
# WARNING: richiede load di progetti_esteso


#' Crea nuovo po_riclass.csv
#'
#' DO SOMETHING.
#'
#' @param bimestre Bimestre di riferimento.
#' @param progetti Dataset completo da progetti_esteso.csv.
#' @return Il dataset viene salvato in DATA e può essere caricato con load_progetti(light = TRUE).
make_po_riclass <- function(bimestre, progetti=NULL) {

  # load
  if (missing(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }

  # filter
  programmi <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)

  # load from package
  out <- octk::po_riclass %>%
    bind_rows(programmi %>%
                anti_join(po_riclass, by = "OC_CODICE_PROGRAMMA"))

  file.rename(file.path(RAWDATA, "po_riclass.csv"),
              file.path(RAWDATA, "po_riclass_OLD.csv"))

  write_delim(out, file.path("data-raw", "po_riclass_NEW.csv"), delim = ";", na = "")
}



# ----------------------------------------------------------------------------------- #
# query

# TODO:
# queste dovrebbero popolare in automatico il file "query_template.xlsx" (ora in "inst" con altri template xls)
# creare wrapper che riepie direttamente il template

# crea nuuovo matrix programmi_linee_azioni
make_matrix_po <- function(bimestre, progetti) {

  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  # }

  require("readr")

  out <- progetti %>%
    distinct(OC_COD_CICLO, OC_DESCR_CICLO,
             OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA,
             OC_ARTICOLAZIONE_PROGRAMMA, OC_COD_ARTICOLAZ_PROGRAMMA, OC_DESCR_ARTICOLAZ_PROGRAMMA,
             OC_SUBARTICOLAZIONE_PROGRAMMA, OC_COD_SUBARTICOLAZ_PROGRAMMA, OC_DESCR_SUBARTICOLAZ_PROGRAMMA) %>%
    select(OC_COD_CICLO, OC_DESCR_CICLO,
           OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA,
           OC_ARTICOLAZIONE_PROGRAMMA, OC_COD_ARTICOLAZ_PROGRAMMA, OC_DESCR_ARTICOLAZ_PROGRAMMA,
           OC_SUBARTICOLAZIONE_PROGRAMMA, OC_COD_SUBARTICOLAZ_PROGRAMMA, OC_DESCR_SUBARTICOLAZ_PROGRAMMA) %>%
    mutate(QUERY = 0,
           NOTE = NA)

  file.rename(file.path(RAWDATA, "po_linee_azioni.csv"),
              file.path(RAWDATA, "po_linee_azioni_OLD.csv"))

  # write_delim(out, file.path(RAWDATA, "po_linee_azioni_NEW.csv"), delim = ";", na = "")
  # write_delim(out, file.path(RAWDATA, "po_linee_azioni.csv"), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, "po_linee_azioni.csv"), delim = ";", na = "")
  
  
}


# crea matrix strumenti attuativi
make_matrix_strum <- function(bimestre, progetti, file_name="strum_att.csv") {

  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  # }

  out <- progetti %>%
    distinct(COD_STRUMENTO, DESCR_STRUMENTO, DESCR_TIPO_STRUMENTO, OC_CODICE_PROGRAMMA) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(QUERY = 0,
           NOTE = NA) %>% 
    filter(!is.na(COD_STRUMENTO))

  # write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")
  
}


# crea matrix delibere cipe
make_matrix_cipe <- function(bimestre, file_name="delib_cipe.csv") {

  # load finanziamenti
  # temp <- paste0("finanziamenti_esteso_", bimestre, ".csv")
  # delibere <- read_csv2(file.path(DATA, temp), guess_max = 5000)
  # temp <- file.path(DATA, "finanziamenti_preesteso.sas7bdat")
  # delibere <- read_sas(temp)
  delibere <- read_csv2(file.path(DATA, "finanziamenti_preesteso.csv"))

  out <- delibere %>%
    # distinct(COD_DEL_CIPE, NUMERO_DEL_CIPE, ANNO_DEL_CIPE, TIPO_QUOTA, DESCRIZIONE_QUOTA, IMPORTO) %>%
    # distinct(NUMERO_DEL_CIPE, ANNO_DEL_CIPE, DESCRIZIONE_QUOTA) %>%
    rename(NUMERO_DEL_CIPE = numero_del_cipe,
           ANNO_DEL_CIPE = anno_del_cipe) %>%
    distinct(NUMERO_DEL_CIPE, ANNO_DEL_CIPE, DESCRIZIONE_QUOTA) %>%
    mutate(QUERY = 0,
           NOTE = NA)

  # write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")
  
}


# crea matrix progetti complessi
make_prog_comp <- function(bimestre, progetti, file_name="prog_comp.csv") {

  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  # }

  out <- progetti %>%
    distinct(COD_PROGETTO_COMPLESSO, DESCRIZIONE_PROGETTO_COMPLESSO, COD_TIPO_COMPLESSITA, DESCR_TIPO_COMPLESSITA,
             OC_CODICE_PROGRAMMA) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(QUERY = 0,
           NOTE = NA) %>% 
    filter(!is.na(COD_PROGETTO_COMPLESSO))

  # write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")

}


# crea matrix patt
make_patt <- function(bimestre, progetti, file_name="patt.csv") {

  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  # }

  appo <- progetti %>%
    distinct(COD_PROCED_ATTIVAZIONE, DESCR_PROCED_ATTIVAZIONE, COD_TIPO_PROCED_ATTIVAZIONE, DESCR_TIPO_PROCED_ATTIVAZIONE,
             OC_CODICE_PROGRAMMA) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(QUERY = 0,
           NOTE = NA) 
  
  # lista caratteri anomali
  unicode_list <- c("\\u0000","\\u0001","\\u0002","\\u0003","\\u0004","\\u0005","\\u0006","\\u0007", 
                    "\\u0008","\\u0009","\\u000A","\\u000B","\\u000C","\\u000D","\\u000E","\\u000F", 
                    "\\u0010","\\u0011","\\u0012","\\u0013","\\u0014","\\u0015","\\u0016","\\u0017", 
                    "\\u0018","\\u0019","\\u001A","\\u001B","\\u001C","\\u001D","\\u001E","\\u001F", 
                    "\\u007F","\\u0080","\\u0081","\\u0082","\\u0083","\\u0084","\\u0085","\\u0086", 
                    "\\u0087","\\u0088","\\u0089","\\u008A","\\u008B","\\u008C","\\u008D","\\u008E", 
                    "\\u008F","\\u0090","\\u0091","\\u0092","\\u0093","\\u0094","\\u0095","\\u0096", 
                    "\\u0097","\\u0098","\\u0099","\\u009A","\\u009B","\\u009C","\\u009D","\\u009E", 
                    "\\u009F")
  # https://en.wikipedia.org/wiki/List_of_Unicode_characters
  
  # clean unicode
  out <- appo
  for (i in seq_along(unicode_list)) {
      x <- unicode_list[i] 
      
      out <- out %>% 
        mutate(DESCR_PROCED_ATTIVAZIONE = str_remove(DESCR_PROCED_ATTIVAZIONE, x))
  }
  
  # CHK
  for (i in seq_along(unicode_list)) {
    x <- unicode_list[i] 
    # x <- "\\u0080"
    
    chk <- appo %>% 
      mutate(CHK = str_detect(DESCR_PROCED_ATTIVAZIONE, x)) %>%
      filter(CHK)
    
    # debug
    # print(paste0(x, ": ", nrow(chk)))
    
    # DEBUG:
    # cat(chk$DESCR_PROCED_ATTIVAZIONE)
    
    # DEBUG:
    # fileConn <- file(file.path(TEMP, "chk.txt"))
    # writeLines(chk$DESCR_PROCED_ATTIVAZIONE, con = fileConn, sep = "\n\r")
    # close(fileConn)
    
  }

  # export
  # write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")

}


# crea matrix comuni
make_comuni <- function(file_name="matrix_comuni.csv") {
  
  # library("haven")
  
  message("Ricodati di chiedere a Luca se ha cambiato il file!")
  message("Ricodati di convertire a mano il csv di Luca in UTF8")
  
  
  # out <- read_csv2(file.path(getwd(), "setup", "data-raw", "variazioni_comuni.csv"))  %>%
  out <- read_csv2(file.path(RAWDATA, "variazioni_comuni.csv"))  %>%
    rename(ANNO_VARIAZIONE = anno,
           TIPO_VARIAZIONE = tipo,
           COD_COMUNE_OLD = id_old,
           DEN_COMUNE_OLD = name_old,
           COD_COMUNE = id_new, 
           DEN_COMUNE = name_new) %>% 
    mutate(QUERY = 0,
           AMBITO = NA,
           AMBITO_SUB = NA)

  # write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
  write_delim(out, file.path(RAWDATA, file_name), delim = ";", na = "")
}



#' Fix temporaneo per il dataset progetti in vestione preesteso
#'
#' Integra il dataset.
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_progetti <- function(progetti) {
  
  # fix temporaneo per IOG>YEI
  progetti <- progetti %>%
    mutate(FONDO_COMUNITARIO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & is.na(FONDO_COMUNITARIO) ~ "YEI",
                                         OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & FONDO_COMUNITARIO == "IOG::" ~ "YEI",
                                         OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & FONDO_COMUNITARIO == "FSE:::IOG" ~ "YEI",
                                         OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & FONDO_COMUNITARIO == "IOG" ~ "YEI",
                                         TRUE ~ FONDO_COMUNITARIO))
  
  # # fix snai
  # if (!(is.null(path_snai))) {
  #   progetti <- fix_snai(progetti, path_snai)
  # }
  
  return(progetti)
}



#' Fix variabili SNAI
#'
#' Fix temporaneo per integrare le variabili SNAI (COD_AREA_INT e AREA_INTERNA) dal file di Andrea
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_snai <- function(progetti, path_snai) {
  
  message("------------enter fix snai")
  # path_snai <- "ELAB/20211031/SNAI/snai/V.01/output/perimetro_snai.xlsx"
  snai <- read_xlsx(file.path(DRIVE, path_snai)) %>% 
    # select(COD_LOCALE_PROGETTO, SNAI_OC, COD_AREA_INT, AREA_INTERNA) %>% 
    distinct(COD_LOCALE_PROGETTO, SNAI_OC, COD_AREA_INT, AREA_INTERNA) %>%
    filter(SNAI_OC == 1) %>% 
    select(-SNAI_OC)
  
  # fix temporaneo
  progetti <- progetti %>%
    left_join(snai, by = "COD_LOCALE_PROGETTO")
  
  return(progetti)
}


#' Controlla integrità file light
#'
#' Controlla integrità file light
#'
#' @param progetti Dataset in formato standard da load_progetti().
#' @param operazioni Dataset in formato standard da load_progetti().
#' @return Messaggi e report di controllo.
chk_bimestre_light <- function(progetti, operazioni) {
  
  # chk vuoti
  chk1 <- progetti %>% filter(is.na(x_AMBITO)) %>% count(x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA)
  chk2 <- progetti %>% filter(is.na(x_CICLO)) %>% count(x_AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA)
  chk3 <- operazioni %>% filter(is.na(x_AMBITO)) %>% count(x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA)
  chk4 <- operazioni %>% filter(is.na(x_CICLO)) %>% count(x_AMBITO, OC_CODICE_PROGRAMMA, x_PROGRAMMA)
  
  # chk mismatch progetti vs operazioni
  chk5 <- progetti %>%
    select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO) %>%
    full_join(operazioni %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>%
    mutate(CHK = x_AMBITO.x == x_AMBITO.y) %>%
    filter(CHK == FALSE)

  # log
  message(paste0("Rilevati ", dim(chk1)[1], " programmi con progetti con x_AMBITO nullo:"))
  print(chk1)
  message(paste0("Rilevati ", dim(chk2)[1], " programmi con progetti con x_CICLO nullo:"))
  print(chk2)
  message(paste0("Rilevati ", dim(chk3)[1], " programmi con operazioni con x_AMBITO nullo:"))
  print(chk3)
  message(paste0("Rilevati ", dim(chk4)[1], " programmi con operazioni con x_CICLO nullo:"))
  print(chk4)

  message(paste0("Rilevati ", dim(chk5)[1], " progetti con mismatch su x_AMBITO tra progetti e operazioni:"))
  print(chk5 %>% count(OC_CODICE_PROGRAMMA, x_AMBITO.x, x_AMBITO.y))
  write_csv2(chk5, file.path(TEMP, "chk_mismatch_ambito_progetti_operazioni.csv"))

}