#' Carica operazioni generato da workflow macroaree
#'
#' Carica operazioni generato da workflow macroaree
#'
#' @param perimetro Dataset di classe operazioni
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param use_pqt Vuoi leggere progetti_light da file parquet?
#' @param DATA Path to DATA
load_operazioni <- function(bimestre, visualizzati=TRUE, use_pqt=FALSE, DATA) {
  
  col_types <- cols(
    COD_LOCALE_PROGETTO = col_character(),
    OC_CODICE_PROGRAMMA = col_character(),
    x_PROGRAMMA = col_character(),
    x_GRUPPO = col_character(),
    x_AMBITO = col_character(),
    x_CICLO = col_character(),
    x_MACROAREA = col_character(),
    x_CATREG = col_character(),
    x_REGIONE = col_character(),
    x_REGNAZ = col_character(),
    x_LIVELLO_0 = col_character(),
    x_LIVELLO_1 = col_character(),
    x_LIVELLO_2 = col_character(),
    COE = col_double(),
    COE_IMP = col_double(),
    COE_PAG = col_double(),
    CUP = col_character(),
    OC_TITOLO_PROGETTO = col_character(),
    OC_COD_TEMA_SINTETICO = col_character(),
    OC_DESCR_ARTICOLAZ_PROGRAMMA = col_character(),
    OC_DESCR_SUBARTICOLAZ_PROGRAMMA = col_character(),
    CUP_COD_NATURA = col_character(),
    CUP_DESCR_NATURA = col_character(),
    CUP_COD_TIPOLOGIA = col_character(),
    CUP_DESCR_TIPOLOGIA = col_character(),
    CUP_COD_SETTORE = col_character(),
    CUP_DESCR_SETTORE = col_character(),
    CUP_COD_SOTTOSETTORE = col_character(),
    CUP_DESCR_SOTTOSETTORE = col_character(),
    CUP_COD_CATEGORIA = col_character(),
    CUP_DESCR_CATEGORIA = col_character(),
    COD_REGIONE = col_character(),
    DEN_REGIONE = col_character(),
    COD_PROVINCIA = col_character(),
    DEN_PROVINCIA = col_character(),
    COD_COMUNE = col_character(),
    DEN_COMUNE = col_character(),
    OC_STATO_PROGETTO = col_character(),
    OC_STATO_PROCEDURALE = col_character(),
    OC_COD_FASE_CORRENTE = col_character(),
    OC_DESCR_FASE_CORRENTE = col_character(),
    COD_PROCED_ATTIVAZIONE = col_character(),
    DESCR_PROCED_ATTIVAZIONE = col_character(),
    OC_CODFISC_BENEFICIARIO = col_character(),
    OC_DENOM_BENEFICIARIO = col_character(),
    OC_FLAG_VISUALIZZAZIONE = col_integer(),
    OC_FLAG_AGGREGATO = col_integer()
  )
  
  # loads
  # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  # perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)
  DATA <- file.path(dirname(DATA), bimestre)
  # perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), col_types = col_types)
  
  
  if (use_pqt == TRUE) {
    perimetro <- read_parquet(file.path(DATA, paste0("operazioni_light_", bimestre, ".parquet")))
    
  } else {
    perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), col_types = col_types, locale = readr::locale(encoding = "UTF-8"))
    
  }

  # fix per dissesto
  # TODO: da spostare a monte nel workflow di operazioni
  # perimetro <- perimetro %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
  #                                          OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  
  # viz
  if (visualizzati == TRUE) {
    perimetro <- perimetro %>%
      # filter(OC_FLAG_VISUALIZZAZIONE == 0)
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9 | OC_FLAG_VISUALIZZAZIONE == 10) # include progetti FEASR per SNAI
  }
  
  # meuro
  # if (usa_meuro == TRUE) {
  #   perimetro <- perimetro %>%
  #     mutate(COE = COE / 1000000,
  #            COE_IMP = COE_IMP / 1000000,
  #            COE_PAG = COE_PAG / 1000000)
  # } else {
  #   perimetro <- perimetro %>%
  #     mutate(COE = COE,
  #            COE_IMP = COE_IMP,
  #            COE_PAG = COE_PAG)
  # }
  
  # simply
  perimetro <- get_simply_non_loc(perimetro)
  
  # refactor
  perimetro <- refactor_ambito(perimetro)
  perimetro <- refactor_ciclo(perimetro)
  
  return(perimetro)
}


#' Carica operazioni 1420
#'
#' Carica operazioni 1420 da dataiku
#'
#' @return Dataset operazinoi da dataiuku.
load_operazioni_extra <- function() {
  
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
  
  operazioni_extra_raw <- read_csv2(file.path(DATA, "oper_extok_preesteso.csv"), col_types = col_types) 
  return(operazioni_extra_raw)
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
    qsn_cod_priorita = col_character(),
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


#' Setup di operazioni da workflow macroaree
#'
#' Setup di operazioni da workflow macroaree
#'
#' @param bimestre Bimestre di rifeirimento da oc_init().
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @param operazioni_713_raw File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_1420_raw File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_extra_raw File di tipo operazioni da flusso sas/dataiku.
#' @param export Vuoi esportare il file in formato operazioni?
#' @param export_pqt Vuoi esportare il file in formato operazioni in formato parquet?
#' @param debug_mode Vuoi esportare i file di debug?
#' @return Il dataset operazioni.
setup_operazioni_evo_macro <- function(bimestre, progetti, 
                                       operazioni_713_raw, operazioni_1420_raw, operazioni_extra_raw, 
                                       export=TRUE, export_pqt=FALSE, debug=FALSE) {
  
  out <- workflow_macroaree(bimestre, progetti, operazioni_713=operazioni_713_raw, 
                             operazioni_1420=operazioni_1420_raw, operazioni_extra=operazioni_extra_raw, 
                             debug=debug)
  
  out <- out %>% 
    # fix ambito fdr
    mutate(x_AMBITO = case_when(x_AMBITO == "FDR" ~ "POC",
                                TRUE ~ x_AMBITO)) %>% 
    # crea sezione
    mutate(x_SEZIONE = case_when(x_GRUPPO == "PSC" & grepl("SOCIS", x_LIVELLO_0) ~ "SO_CIS",
                                 x_GRUPPO == "PSC" & grepl("SO", x_LIVELLO_0) & x_CICLO == "2021-2027" ~ "ANT",
                                 x_GRUPPO == "PSC" & grepl("SO", x_LIVELLO_0) ~ "SO",
                                 x_GRUPPO == "PSC" & grepl("SS_1", x_LIVELLO_0) ~ "SS_1",
                                 x_GRUPPO == "PSC" & grepl("SS_2", x_LIVELLO_0) ~ "SS_2",
                                 x_GRUPPO == "ACCORDI" & grepl("Comp", x_LIVELLO_0) ~ "COMP",
                                 x_GRUPPO == "ACCORDI" & grepl("Ord", x_LIVELLO_0) ~ "ORD",
                                 x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCSTRCAMPANIA" ~ "STRAL2",
                                 x_GRUPPO == "ACCORDI" & OC_CODICE_PROGRAMMA == "ACCBAGNCAMPANIA" ~ "STRAL3",
                                 TRUE ~ NA_character_)) %>% 
    # crea stato
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, 
                       IMPEGNI,
                       TOT_PAGAMENTI,
                       OC_FINANZ_TOT_PUB_NETTO, 
                       DATA_FINE_EFF_COLLAUDO,
                       DATA_INIZIO_EFF_COLLAUDO,
                       DATA_FINE_EFF_ESECUZIONE,
                       DATA_INIZIO_EFF_ESECUZIONE,
                       DATA_FINE_EFF_STIP_ATTRIB,
                       DATA_INIZIO_EFF_STIP_ATTRIB,
                       DATA_FINE_EFF_PROG_ESEC,
                       DATA_INIZIO_EFF_PROG_ESEC, 
                       DATA_FINE_EFF_PROG_DEF,
                       DATA_INIZIO_EFF_PROG_DEF,
                       DATA_FINE_EFF_PROG_PREL, 
                       DATA_INIZIO_EFF_PROG_PREL, 
                       DATA_FINE_EFF_STUDIO_FATT, 
                       DATA_INIZIO_EFF_STUDIO_FATT),
              by = "COD_LOCALE_PROGETTO") %>% 
    get_x_stato(., data_scarico=bimestre) %>% 
    select(-IMPEGNI,
           -TOT_PAGAMENTI,
           -OC_FINANZ_TOT_PUB_NETTO, 
           -DATA_FINE_EFF_COLLAUDO,
           -DATA_INIZIO_EFF_COLLAUDO,
           -DATA_FINE_EFF_ESECUZIONE,
           -DATA_INIZIO_EFF_ESECUZIONE,
           -DATA_FINE_EFF_STIP_ATTRIB,
           -DATA_INIZIO_EFF_STIP_ATTRIB,
           -DATA_FINE_EFF_PROG_ESEC,
           -DATA_INIZIO_EFF_PROG_ESEC, 
           -DATA_FINE_EFF_PROG_DEF,
           -DATA_INIZIO_EFF_PROG_DEF,
           -DATA_FINE_EFF_PROG_PREL, 
           -DATA_INIZIO_EFF_PROG_PREL, 
           -DATA_FINE_EFF_STUDIO_FATT, 
           -DATA_INIZIO_EFF_STUDIO_FATT)
  
  
  if (export == TRUE) {
    write.csv2(out, file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)
  }
  
  if (export_pqt == TRUE) {
    arrow::write_parquet(out, file.path(DATA, paste0("operazioni_light_", bimestre, ".parquet")))
  }
  
  # return(out)
  
  # memory mgm
  rm(out)
  gc()
}


#' Workflow per creare il dataset macroaree
#'
#' Workflow per creare il dataset macroaree
#'
#' @param bimestre Bimestre di rifeirimento da oc_init().
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @param operazioni_713_raw File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_1420_raw File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_extra_raw File di tipo operazioni da flusso sas/dataiku.
#' @param export Vuoi esportare il file in formato operazioni?
#' @param debug_mode Vuoi esportare i file di debug?
#' @return Il dataset operazioni.
workflow_macroaree <- function(bimestre, progetti, 
                               operazioni_713_raw, operazioni_1420_raw, operazioni_extra_raw, 
                               debug=FALSE) {
  
  # ----------------------------------------------------------------------------------- #
  # loads----
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass
  
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni extra----
  
  
  message("Preparazione dati extra...")
  
  # DEBUG:
  # operazioni_extra %>% 
  #   count(oc_cod_fonte, ue_descr_fondo, CODICE_TIPOLOGIA_PROGRAMMA, oc_ambito)
  # chk <- operazioni_extra %>%
  #   count(x_AMBITO, psc_sezione, ue_asse_prioritario, psc_area_tematica, fsc_settore_strategico, pac_asse_tematico, cod_misura_feasr)
  # chk <- operazioni_extra %>%
  #   count(x_AMBITO, ue_ob_specifico, psc_sett_interv, fsc_asse_tematico, pac_lineazione, cod_submisura_feasr)
  
  # clean
  operazioni_extra_0 <- operazioni_extra_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_extra_2127(.) %>%
    # creo ambito
    mutate(x_AMBITO = case_when(oc_cod_fonte == "FS2127" ~ oc_ambito,
                                oc_cod_fonte == "FDR2127" ~ oc_ambito,
                                oc_cod_fonte == "FSC2127" ~ oc_ambito,
                                oc_cod_fonte == "FS1420" &  oc_ambito == "FEASR" ~ oc_ambito,
                                oc_cod_fonte == "FS1420" &  oc_ambito == "FSE" ~ oc_ambito)) %>%
    # articolazioni
    mutate(x_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sezione,
                                   x_AMBITO == "FDR" & !is.na(psc_sezione) ~ psc_sezione,
                                   TRUE ~ NA_character_),
           # x_DES_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sezione,
           #                             x_AMBITO == "FDR" & !is.na(psc_sezione) ~ psc_descr_sezione,
           #                             TRUE ~ NA_character_),
           x_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_asse_prioritario,
                                   x_AMBITO == "FSE" ~ ue_asse_prioritario,
                                   x_AMBITO == "CTE" ~ ue_asse_prioritario,
                                   x_AMBITO == "JTF" ~ ue_asse_prioritario,
                                   x_AMBITO == "FSC" ~ psc_area_tematica,
                                   x_AMBITO == "FDR" ~ psc_area_tematica),
           # x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "FSE" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "YEI" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "CTE" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "ENI" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_area_tematica,
           #                             x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_area_tematica,
           #                             x_AMBITO == "FSC" ~ fsc_descr_settore_strategico,
           #                             x_AMBITO == "POC" ~ pac_descr_asse_tematico,
           #                             x_AMBITO == "SNAI" ~ pac_descr_asse_tematico,
           #                             x_AMBITO == "FEASR" ~ descr_misura_feasr,
           #                             x_AMBITO == "FDR" ~ psc_descr_area_tematica,
           #                             x_AMBITO == "JTF" ~ ue_descr_asse_prioritario),
           x_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_ob_specifico,
                                   x_AMBITO == "FSE" ~ ue_ob_specifico,
                                   x_AMBITO == "CTE" ~ ue_ob_specifico,
                                   x_AMBITO == "JTF" ~ ue_ob_specifico,
                                   x_AMBITO == "FSC" ~ psc_sett_interv,
                                   x_AMBITO == "FDR" ~ psc_sett_interv) #,
           # x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "FSE" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "YEI" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "CTE" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "ENI" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sett_interv,
           #                             x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_sett_interv,
           #                             x_AMBITO == "FSC" ~ fsc_descr_asse_tematico,
           #                             x_AMBITO == "POC" ~ pac_descr_lineazione,
           #                             x_AMBITO == "SNAI" ~ pac_descr_lineazione,
           #                             x_AMBITO == "FEASR" ~ descr_submisura_feasr,
           #                             x_AMBITO == "FDR" ~ psc_descr_sett_interv,
           #                             x_AMBITO == "JTF" ~ ue_descr_ob_specifico,
    ) %>%
    # variabili coesione
    rename(COE = oc_costo_coesione,
           COE_IMP = oc_impegni_coesione,
           COE_PAG = oc_tot_pagamenti_coesione) %>% 
    # risolve trasferimenti
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, CUP_COD_NATURA),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE_SUD = costo_ammesso_MZ,
           COE_CN = costo_ammesso_CN,
           COE_IMP_SUD = case_when(CUP_COD_NATURA == "08" ~ imp_trasf_ammesso_MZ,
                                   TRUE ~ imp_ammesso_MZ),
           COE_IMP_CN = case_when(CUP_COD_NATURA == "08" ~ imp_trasf_ammesso_CN,
                                  TRUE ~ imp_ammesso_CN),
           COE_PAG_SUD = case_when(CUP_COD_NATURA == "08" ~ pag_trasf_ammesso_MZ,
                                   TRUE ~ pag_ammesso_MZ),
           COE_PAG_CN = case_when(CUP_COD_NATURA == "08" ~ pag_trasf_ammesso_CN,
                                  TRUE ~ pag_ammesso_CN)) %>%
    # integra ciclo serve in workflow_macroaree_sub_programmazione() per x_CATREG)
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, x_CICLO=X_CICLO),
    #           by = "COD_LOCALE_PROGETTO") %>%
    # clean
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           # x_CICLO,
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE,
           COE_SUD,
           COE_CN,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN)
  
  # chk
  # operazioni_extra %>%
  #   group_by(x_AMBITO) %>%
  #   summarise_if(is.numeric, sum, na.rm=TRUE)
  
  # integra ciclo
  operazioni_extra <- workflow_macroaree_sub_ciclo(operazioni_extra_0, po)
  # MEMO: anticipato perché serve in workflow_macroaree_sub_programmazione() per x_CATREG
  
  
  # ----------------------------------------------------------------------------------- #
  # macroaree extra----
  
  # integra territori
  operazioni_extra_1 <- workflow_macroaree_sub_programmazione(operazioni_extra, operazioni_extra_raw, progetti)
  
  # DEBUG:
  # sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_1$COE, na.rm = TRUE) #CHK
  # dim(operazioni_extra)[1] - dim(operazioni_extra_1)[1]
  # operazioni_extra %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  # operazioni_extra_1 %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  
  # chk <- operazioni_extra_1 %>%
  #   count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
  # write.xlsx(chk, file.path(TEMP, "chk_macroarea_caterg_regione_extra.xlsx"))
  
  # studio mapping
  # chk <- workflow_macroaree_sub_studio(operazioni_1420_1, debug=FALSE)
  
  # mapping variabili
  operazioni_extra_2 <- workflow_macroaree_sub_mapping(operazioni_extra_1)
  
  # DEBUG:
  # sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_2$COE, na.rm = TRUE)
  # sum(operazioni_extra_1$COE, na.rm = TRUE) - sum(operazioni_extra_2$COE, na.rm = TRUE)
  
  # chk
  # operazioni_extra_2 %>%
  #   group_by(x_AMBITO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_SUD = sum(COE_SUD, na.rm = TRUE),
  #             COE_CN = sum(COE_CN, na.rm = TRUE),
  #             COE_ND = sum(COE_ND, na.rm = TRUE)) %>%
  #   mutate(tot2 = COE_SUD + COE_CN + COE_ND,
  #          chk2 = COE - tot2)
  
  # pivot
  operazioni_extra_3 <- workflow_macroaree_sub_pivot(operazioni_extra_2) 
  
  # DEBUG:
  # sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_3$COE, na.rm = TRUE)
  # sum(operazioni_extra_2$COE, na.rm = TRUE) - sum(operazioni_extra_3$COE, na.rm = TRUE) #CHK
  
  # fix
  operazioni_extra_4 <- workflow_macroaree_sub_fixing(operazioni_extra_3)
  
  # DEBUG:
  # sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_4$COE, na.rm = TRUE)
  # sum(operazioni_extra_3$COE, na.rm = TRUE) - sum(operazioni_extra_4$COE, na.rm = TRUE)
  
  # chk
  sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_4$COE, na.rm = TRUE)
  sum(operazioni_extra$COE_IMP, na.rm = TRUE) - sum(operazioni_extra_4$COE_IMP, na.rm = TRUE)
  sum(operazioni_extra$COE_PAG, na.rm = TRUE) - sum(operazioni_extra_4$COE_PAG, na.rm = TRUE)
  # 0
  
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni 1420----
  
  
  message("Preparazione dati 1420...")
  
  # clean
  operazioni_1420_0 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_macroaree_1420(.) %>%
    # creo ambito
    mutate(x_AMBITO = case_when(oc_cod_fonte == "FSC1420"& ue_descr_fondo == "PAC" ~ "SNAI", # AREEINTVVFF: STRATEGIA AREE INTERNE INCENDI BOSCHIVI
                                oc_cod_fonte == "NAZORD" & ue_descr_fondo == "PAC" ~ "SNAI", # programmi SNAI LdS
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI", # 2020PCDPCINA001: CONTRIBUTI AI COMUNI DELLE AREE INTERNE
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "IPA" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
                                oc_cod_fonte == "FSC1420" ~ "FSC",
                                oc_cod_fonte == "PAC1420" ~ "POC",
                                oc_cod_fonte == "FSC2127" ~ "FSC",
                                oc_cod_fonte == "FSC0713" ~ "FSC",
                                oc_cod_fonte == "NAZORD" ~ "SNAI")) %>% 
    # articolazioni
    mutate(x_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sezione,
                                   TRUE ~ NA_character_),
           # x_DES_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sezione,
           #                             TRUE ~ NA_character_),
           x_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_asse_prioritario,
                                   x_AMBITO == "FSE" ~ ue_asse_prioritario,
                                   x_AMBITO == "YEI" ~ ue_asse_prioritario,
                                   x_AMBITO == "CTE" ~ ue_asse_prioritario,
                                   x_AMBITO == "ENI" ~ ue_asse_prioritario,
                                   x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_area_tematica,
                                   x_AMBITO == "FSC" & psc_sezione != "" ~ psc_area_tematica,
                                   x_AMBITO == "FSC" ~ fsc_settore_strategico,
                                   x_AMBITO == "POC" ~ pac_asse_tematico,
                                   x_AMBITO == "SNAI" ~ pac_asse_tematico,
                                   x_AMBITO == "FEASR" ~ cod_misura_feasr),
           # x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "FSE" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "YEI" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "CTE" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "ENI" ~ ue_descr_asse_prioritario,
           #                             x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_area_tematica,
           #                             x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_area_tematica,
           #                             x_AMBITO == "FSC" ~ fsc_descr_settore_strategico,
           #                             x_AMBITO == "POC" ~ pac_descr_asse_tematico,
           #                             x_AMBITO == "SNAI" ~ pac_descr_asse_tematico,
           #                             x_AMBITO == "FEASR" ~ descr_misura_feasr),
           x_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_ob_specifico,
                                   x_AMBITO == "FSE" ~ ue_ob_specifico,
                                   x_AMBITO == "YEI" ~ ue_ob_specifico,
                                   x_AMBITO == "CTE" ~ ue_ob_specifico,
                                   x_AMBITO == "ENI" ~ ue_ob_specifico,
                                   x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sett_interv,
                                   x_AMBITO == "FSC" & psc_sezione != "" ~ psc_sett_interv,
                                   x_AMBITO == "FSC" ~ fsc_asse_tematico,
                                   x_AMBITO == "POC" ~ pac_lineazione,
                                   x_AMBITO == "SNAI" ~ pac_lineazione,
                                   x_AMBITO == "FEASR" ~ cod_submisura_feasr),
           # x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "FSE" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "YEI" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "CTE" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "ENI" ~ ue_descr_ob_specifico,
           #                             x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sett_interv,
           #                             x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_sett_interv,
           #                             x_AMBITO == "FSC" ~ fsc_descr_asse_tematico,
           #                             x_AMBITO == "POC" ~ pac_descr_lineazione,
           #                             x_AMBITO == "SNAI" ~ pac_descr_lineazione,
           #                             x_AMBITO == "FEASR" ~ descr_submisura_feasr)
    ) %>% 
    # variabili coesione
    mutate(COE = oc_costo_coesione,
           COE_IMP = oc_impegni_coesione,
           COE_PAG = oc_tot_pagamenti_coesione) %>%
    # risolve trasferimenti
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, CUP_COD_NATURA),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE_SUD = costo_ammesso_MZ,
           COE_CN = costo_ammesso_CN,
           COE_IMP_SUD = case_when(CUP_COD_NATURA == "08" ~ imp_trasf_ammesso_MZ,
                                   TRUE ~ imp_ammesso_MZ),
           COE_IMP_CN = case_when(CUP_COD_NATURA == "08" ~ imp_trasf_ammesso_CN,
                                  TRUE ~ imp_ammesso_CN),
           COE_PAG_SUD = case_when(CUP_COD_NATURA == "08" ~ pag_trasf_ammesso_MZ,
                                   TRUE ~ pag_ammesso_MZ),
           COE_PAG_CN = case_when(CUP_COD_NATURA == "08" ~ pag_trasf_ammesso_CN,
                                  TRUE ~ pag_ammesso_CN)) %>%
    select(-CUP_COD_NATURA) %>%
    # integra ciclo
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, x_CICLO=X_CICLO),
    #           by = "COD_LOCALE_PROGETTO") %>%
    # clean
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           # x_CICLO,
           OC_COD_CICLO, #MEMO: serve per psc in workflow_macroaree_sub_ciclo()
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE,
           COE_SUD,
           COE_CN,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN)
  
  
  # integra ciclo
  operazioni_1420 <- workflow_macroaree_sub_ciclo(operazioni_1420_0, po)
  # MEMO: anticipato perché serve in workflow_macroaree_sub_programmazione() per x_CATREG
  
  
  # ----------------------------------------------------------------------------------- #
  # macroaree 1420----
  
  # integra territori
  operazioni_1420_1 <- workflow_macroaree_sub_programmazione(operazioni_1420, operazioni_1420_raw, progetti)
  
  # chk <- operazioni_1420_1 %>% 
  #   count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
  # write.xlsx(chk, file.path(TEMP, "chk_macroarea_caterg_regione_1420.xlsx"))
  
  # studio mapping
  # chk <- workflow_macroaree_sub_studio(operazioni_1420_1, debug=FALSE)
  
  # mapping  
  operazioni_1420_2 <- workflow_macroaree_sub_mapping(operazioni_1420_1)
  
  # chk
  operazioni_1420_2 %>%
    group_by(x_AMBITO) %>%
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE),
              COE_ND = sum(COE_ND, na.rm = TRUE)) %>%
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2)
  
  # operazioni_1420_2 %>%
  #   group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_SUD = sum(COE_SUD, na.rm = TRUE),
  #             COE_CN = sum(COE_CN, na.rm = TRUE),
  #             COE_ND = sum(COE_ND, na.rm = TRUE)) %>%
  #   mutate(tot2 = COE_SUD + COE_CN + COE_ND,
  #          chk2 = COE - tot2) %>% 
  #   filter(chk2 > 0)
  
  # chk <- operazioni_1420_2 %>%
  #   mutate(tot2 = COE_SUD + COE_CN + COE_ND,
  #          chk2 = COE - tot2) %>% 
  #   filter(round(chk2, 2) > 0) %>% 
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE,
  #          x_LIVELLO_0, x_LIVELLO_1, x_LIVELLO_2, x_REGNAZ,
  #          COE, COE_SUD, COE_CN, COE_ND, chk_coe, tot2, chk2)
  
  # pivot
  operazioni_1420_3 <- workflow_macroaree_sub_pivot(operazioni_1420_2) 
  
  # fix
  operazioni_1420_4 <- workflow_macroaree_sub_fixing(operazioni_1420_3)
  
  # chk
  sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_4$COE, na.rm = TRUE)
  sum(operazioni_1420$COE_IMP, na.rm = TRUE) - sum(operazioni_1420_4$COE_IMP, na.rm = TRUE)
  sum(operazioni_1420$COE_PAG, na.rm = TRUE) - sum(operazioni_1420_4$COE_PAG, na.rm = TRUE)
  # 0
  
  # chk <- operazioni_1420_4 %>% count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE, x_REGNAZ)
  
  
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni 713----
  
  
  message("Preparazione dati 713...")
  
  # rename
  operazioni_713_raw_2 <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto, 
           OC_CODICE_PROGRAMMA = oc_cod_programma)
  
  # blocco speciale per duplicazione pac
  # appo <- workflow_macroaree_sub_duplicate_pac(operazioni_713_raw)
  # DEV: blocco abbandonato post migrazione PSC
  appo <- operazioni_713_raw_2
  
  # clean
  operazioni_713_0 <- appo %>%
    # fix per caratteri spuri
    fix_macroaree_713(.) %>%
    # creo ambito e ciclo
    mutate(x_AMBITO = case_when(#TODO: OC_CODICE_PROGRAMMA == "2007IT005FAMG1" ~ "PAC", #MEMO: caso che prima gestivo per duplicazione righe su due ambiti
      OC_COD_FONTE == "FS0713" ~ QSN_FONDO_COMUNITARIO,
      OC_COD_FONTE == "FSC1420" ~ "FSC",
      OC_COD_FONTE == "FSC0713" ~ "FSC",
      OC_COD_FONTE == "PAC" ~ "PAC")) %>%
    # fix per ERDF e ESF su 713
    mutate(x_AMBITO = case_when(x_AMBITO == "ERDF" ~ "FESR",
                                x_AMBITO == "ESF" ~ "FSE",
                                TRUE ~ x_AMBITO)) %>% 
    # articolazioni
    mutate(x_LIVELLO_0 = NA_character_,
           x_DES_LIVELLO_0 = NA_character_) %>% 
    mutate(COD_LINEA = as.character(COD_LINEA),
           COD_AZIONE = as.character(COD_AZIONE)) %>% 
    mutate(x_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ PO_CODICE_ASSE,
                                   x_AMBITO == "FSE" ~ PO_CODICE_ASSE,
                                   x_AMBITO == "FSC" ~ COD_LINEA,
                                   x_AMBITO == "PAC" ~ COD_LINEA),
           # x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ PO_DENOMINAZIONE_ASSE,
           #                             x_AMBITO == "FSE" ~ PO_DENOMINAZIONE_ASSE,
           #                             x_AMBITO == "FSC" ~ descr_linea,
           #                             x_AMBITO == "PAC" ~ descr_linea),
           x_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ PO_COD_OBIETTIVO_OPERATIVO,
                                   x_AMBITO == "FSE" ~ PO_COD_OBIETTIVO_OPERATIVO,
                                   x_AMBITO == "FSC" ~ COD_AZIONE,
                                   x_AMBITO == "PAC" ~ COD_AZIONE) #,
           # x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ PO_OBIETTIVO_OPERATIVO,
           #                             x_AMBITO == "FSE" ~ PO_OBIETTIVO_OPERATIVO,
           #                             x_AMBITO == "FSC" ~ descr_azione,
           #                             x_AMBITO == "PAC" ~ descr_azione)
    ) %>%
    # variabili coesione
    mutate(COE = oc_costo_coesione,
           COE_IMP = oc_impegni_coesione,
           COE_PAG = oc_tot_pagamenti_coesione)  %>% 
    # TODO: chk se operazioni è uguale
    mutate(COE_SUD = 0, 
           COE_CN = 0,
           COE_IMP_SUD = 0, 
           COE_IMP_CN = 0, 
           COE_PAG_SUD = 0, 
           COE_PAG_CN = 0)  %>%
    # integra ciclo 
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, x_CICLO=X_CICLO),
    #           by = "COD_LOCALE_PROGETTO") %>%
    # clean
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           # x_CICLO,
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE,
           COE_SUD,
           COE_CN,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN)
  
  # integra ciclo
  operazioni_713 <- workflow_macroaree_sub_ciclo(operazioni_713_0, po)
  # MEMO: anticipato perché serve in workflow_macroaree_sub_programmazione() per x_CATREG
  
  
  # ----------------------------------------------------------------------------------- #
  # macroaree 713----
  
  # fix per territori
  operazioni_713_raw <- operazioni_713_raw %>% 
    mutate(ue_categ_regione = NA_character_)
  
  # territori
  operazioni_713_1 <- workflow_macroaree_sub_programmazione(operazioni_713, operazioni_713_raw, progetti)
  
  # chk <- operazioni_713_1 %>% 
  #   count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
  # write.xlsx(chk, file.path(TEMP, "chk_macroarea_caterg_regione_713.xlsx"))
  
  # mapping
  operazioni_713_2 <- workflow_macroaree_sub_mapping(operazioni_713_1)
  
  # operazioni_713_2 %>%
  #   group_by(x_AMBITO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_SUD = sum(COE_SUD, na.rm = TRUE),
  #             COE_CN = sum(COE_CN, na.rm = TRUE),
  #             COE_ND = sum(COE_ND, na.rm = TRUE)) %>%
  #   mutate(tot2 = COE_SUD + COE_CN + COE_ND,
  #          chk2 = COE - tot2)
  
  # pivot
  operazioni_713_3 <- workflow_macroaree_sub_pivot(operazioni_713_2)
  
  # DEBUG:
  # sum(operazioni_713$COE, na.rm = TRUE) - sum(operazioni_713_3$COE, na.rm = TRUE) #CHK
  # sum(operazioni_713$COE_IMP, na.rm = TRUE) - sum(operazioni_713_3$COE_IMP, na.rm = TRUE) #CHK
  # 
  # dim(operazioni_713)[1] - dim(operazioni_713_3)[1]
  # operazioni_713 %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  # operazioni_713_3 %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  # 
  # operazioni_713 %>% 
  #   filter(COE_IMP>0, COE==0)
  # operazioni_713 %>% 
  #   filter(COE_IMP>0, is.na(COE))
  # 
  # chk <- operazioni_713_3 %>% 
  #   group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE_IMP = sum(COE_IMP, na.rm = TRUE)) %>% 
  #   full_join(operazioni_713 %>% 
  #               group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE_IMP = sum(COE_IMP, na.rm = TRUE)),
  #             by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
  #   mutate(CHK = COE_IMP.x - COE_IMP.y) %>% 
  #   filter(abs(CHK)>1)
  # sum(chk$CHK)
  
  # fix
  operazioni_713_4 <- workflow_macroaree_sub_fixing(operazioni_713_3)
  
  # chk
  sum(operazioni_713$COE, na.rm = TRUE) - sum(operazioni_713_4$COE, na.rm = TRUE)
  sum(operazioni_713$COE_IMP, na.rm = TRUE) - sum(operazioni_713_4$COE_IMP, na.rm = TRUE)
  sum(operazioni_713$COE_PAG, na.rm = TRUE) - sum(operazioni_713_4$COE_PAG, na.rm = TRUE)
  # CHK: vedo delta su impegni e pagamenti

  # ----------------------------------------------------------------------------------- #
  # bind----
  
  message("Unione dati...")
  
  operazioni_1 <- operazioni_1420_4 %>%
    bind_rows(operazioni_713_4) %>%
    bind_rows(operazioni_extra_4) %>%
    # intega x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA")
  
  # memory mgm
  rm(operazioni_extra_raw, operazioni_1420_raw, operazioni_713_raw,
     operazioni_extra_1, operazioni_1420_1, operazioni_713_1,
     operazioni_extra_2, operazioni_1420_2, operazioni_713_2,
     operazioni_extra_3, operazioni_1420_3, operazioni_713_3)
  gc()
  
  # ----------------------------------------------------------------------------------- #
  # chk vari----
  
  # DEBUG:
  # operazioni_1 <- appo
  
  temp <- operazioni_1 %>% filter(is.na(x_CICLO))
  msg <- paste0("Operazioni senza ciclo: ", dim(temp)[1])
  message(msg)
  
  temp <- operazioni_1 %>% filter(is.na(x_AMBITO))
  msg <- paste0("Operazioni senza ambito: ", dim(temp)[1])
  message(msg)
  
  temp <- operazioni_1 %>% filter(is.na(x_MACROAREA))
  msg <- paste0("Operazioni senza macroarea: ", dim(temp)[1])
  message(msg)
  
  temp <- sum(operazioni_extra_4$COE, na.rm=TRUE) - sum(operazioni_extra$COE, na.rm=TRUE)
  msg <- paste0("Delta coe per extra ante trasformazione: ", temp)
  message(msg)
  
  temp <- sum(operazioni_1420_4$COE, na.rm=TRUE) - sum(operazioni_1420$COE, na.rm=TRUE)
  msg <- paste0("Delta coe per 1420 ante trasformazione: ", temp)
  message(msg)
  
  temp <- sum(operazioni_713_4$COE, na.rm=TRUE) - sum(operazioni_713$COE, na.rm=TRUE)
  msg <- paste0("Delta coe per 713 ante trasformazione: ", temp)
  message(msg)
  
  temp <- sum(operazioni_713_4$COE_IMP, na.rm=TRUE) - sum(operazioni_713$COE_IMP, na.rm=TRUE)
  msg <- paste0("Delta coe_imp per 713 ante trasformazionea: ", temp)
  message(msg)
  
  temp <- sum(operazioni_713_4$COE_PAG, na.rm=TRUE) - sum(operazioni_713$COE_PAG, na.rm=TRUE)
  msg <- paste0("Delta coe_pag per 713 ante trasformazione: ", temp)
  message(msg)
  
  
  # memory mgm
  rm(operazioni_extra_4, operazioni_1420_4, operazioni_713_4,
     operazioni_extra, operazioni_1420, operazioni_713)
  gc()
  
  # ----------------------------------------------------------------------------------- #
  # debug----
  
  if (debug == TRUE) {
    
    # chk territori
    chk <- operazioni_1 %>% 
      count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
    write.xlsx(chk, file.path(TEMP, "chk_macroarea_caterg_regione_all.xlsx"))
    
  }
  
  # ----------------------------------------------------------------------------------- #
  # variabili progetti----
  
  message("Integra variabili progetti")
  
  out <- operazioni_1 %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_GRUPPO, x_AMBITO, x_CICLO, 
           x_MACROAREA, x_CATREG, x_REGIONE, x_REGNAZ, # COD_REGIONE_REV=COD_REGIONE,
           x_LIVELLO_0, x_LIVELLO_1, x_LIVELLO_2,
           COE, COE_IMP, COE_PAG) %>% 
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO,
                       CUP,
                       OC_TITOLO_PROGETTO,
                       # CP = OC_FINANZ_TOT_PUB_NETTO, 
                       # IMP = IMPEGNI, 
                       # PAG = TOT_PAGAMENTI,
                       # OC_SINTESI_PROGETTO,
                       # OC_LINK,
                       # OC_COD_CICLO,
                       # OC_DESCR_CICLO,
                       OC_COD_TEMA_SINTETICO,
                       # OC_TEMA_SINTETICO,
                       # COD_GRANDE_PROGETTO,
                       # DESCRIZIONE_GRANDE_PROGETTO,
                       # OC_COD_FONTE,
                       # OC_DESCR_FONTE,
                       # FONDO_COMUNITARIO,
                       # OC_CODICE_PROGRAMMA,
                       # OC_DESCRIZIONE_PROGRAMMA,
                       # COD_OB_TEMATICO,
                       # DESCR_OB_TEMATICO,
                       # COD_PRIORITA_INVEST,
                       # DESCR_PRIORITA_INVEST,
                       # COD_RISULTATO_ATTESO,
                       # DESCR_RISULTATO_ATTESO,
                       # OC_COD_CATEGORIA_SPESA,
                       # OC_DESCR_CATEGORIA_SPESA,
                       # OC_ARTICOLAZIONE_PROGRAMMA,
                       # OC_SUBARTICOLAZIONE_PROGRAMMA,
                       # OC_COD_ARTICOLAZ_PROGRAMMA,
                       OC_DESCR_ARTICOLAZ_PROGRAMMA,
                       # OC_COD_SUBARTICOLAZ_PROGRAMMA,
                       OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
                       # COD_STRUMENTO,
                       # DESCR_STRUMENTO,
                       # DESCR_TIPO_STRUMENTO,
                       # COD_PROGETTO_COMPLESSO,
                       # DESCRIZIONE_PROGETTO_COMPLESSO,
                       # COD_TIPO_COMPLESSITA,
                       # DESCR_TIPO_COMPLESSITA,
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
                       COD_REGIONE,
                       DEN_REGIONE,
                       COD_PROVINCIA,
                       DEN_PROVINCIA,
                       COD_COMUNE,
                       DEN_COMUNE,
                       # OC_STATO_PROGETTO,
                       # OC_STATO_PROCEDURALE,
                       # OC_COD_FASE_CORRENTE,
                       # OC_DESCR_FASE_CORRENTE,
                       COD_PROCED_ATTIVAZIONE,
                       DESCR_PROCED_ATTIVAZIONE,
                       OC_CODFISC_BENEFICIARIO,
                       OC_DENOM_BENEFICIARIO,
                       OC_FLAG_VISUALIZZAZIONE,
                       OC_FLAG_AGGREGATO,
                       # x_STATO #MEMO: vedi nota sotto
                       ),
              by = "COD_LOCALE_PROGETTO")
  
  # NOTA:
  # x_STATO viene ricalcolato fuori da workflow in setup_operazioni perché:
  # 1) qui non ho variabili date nel select, sono troppe
  # 2) in progetti light ho calcolato x_STATO, ma qui uso preesteso
  
  
  # ----------------------------------------------------------------------------------- #
  # export----
  
  return(out)
  
  # memory mgm
  rm(operazioni_1, progetti)
  gc()
}






#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_extra_2127 <- function(df) {
  
  # TODO:
  # do sometging
  
  
  return(df)
}

#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_macroaree_1420 <- function(df) {
  
  
  # patch per caratteri spuri
  # MEMO: ripristino quelli in progetti
  df <- df %>%
    mutate(COD_LOCALE_PROGETTO = case_when(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607\t" ~ "5IGRUEC291_12208_660607",
                                           TRUE ~ COD_LOCALE_PROGETTO))
  
  
  df <- df %>%
    mutate(ue_descr_fondo = case_when(ue_descr_fondo == "EAFRD" ~ "FEASR",
                                      ue_descr_fondo == "ESF" ~ "FSE",
                                      ue_descr_fondo == "IOG:::FSE" ~ "IOG",
                                      TRUE ~ ue_descr_fondo))
  
  return(df)
}


#' Fix temporaneo per i dataset operazioni (versione per 713)
#'
#' Integra il dataset risolvendo il problema di caratteri spuri. Attenzione perché va fatto anche in "progetti".
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_macroaree_713 <- function(df) {
  
  
  # df <- df %>%
  #   mutate(COD_LOCALE_PROGETTO = case_when(grepl("^1MISE174", COD_LOCALE_PROGETTO) ~ "1MISE174",
  #                                          grepl("^1MISE397", COD_LOCALE_PROGETTO) ~ "1MISE397",
  #                                          grepl("^1MISE496", COD_LOCALE_PROGETTO) ~ "1MISE496",
  #                                          grepl("^1MISE608", COD_LOCALE_PROGETTO) ~ "1MISE608",
  #                                          TRUE ~ COD_LOCALE_PROGETTO))
  # MEMO: questa soluzione non risolve il problema perché restano i duplicati in progetti, anche se non visualizzati, ma uno dei due è senza match
  
  
  # patch per caratteri spuri
  # MEMO: ripristino quelli in progetti
  df <- df %>%
    mutate(COD_LOCALE_PROGETTO = case_when(COD_LOCALE_PROGETTO == "1MISE174 Ass.ne GorÃ¨e onlus - Sociale" ~ "1MISE174 Ass.ne GorÃše onlus - Sociale",
                                           COD_LOCALE_PROGETTO == "1MISE397 ColorÃ¨ Soc. Coop.-Sociale" ~ "1MISE397 ColorÃš Soc. Coop.-Sociale",
                                           COD_LOCALE_PROGETTO == "1MISE496TeknÃ¨-Sociale" ~ "1MISE496TeknÃš-Sociale",
                                           COD_LOCALE_PROGETTO == "1MISE608 KoinÃ¨ soc coop- Sociale" ~ "1MISE608 KoinÃš soc coop- Sociale",
                                           TRUE ~ COD_LOCALE_PROGETTO))
  
  return(df)
}


#' Integra la macroarea da localizzazioni con correzioni per programmazione
#'
#' Integra in x_MACROAREA la macroarea dalle localizzazioni di PREESTESO partendo da OC_MACROAREA. 
#' Corregge per i programmi regionali forzando l'attribuzione alla regione di rifeirmento del programma.
#'
#' @param df Dataset con un perimetro in formato "progetti".
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @return Il dataset con la variabile x_MACROAREA, come factor con levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero").
workflow_macroaree_sub_programmazione_old <- function(df, progetti) {
  
  # DEV:
  # 1) crea macroarea
  # 2) crea regione
  # 3) crea categoria di regione
  # 4) fix su tutte per casi forzati
  
  # DEBUG:
  # df <- operazioni_extra
  # df <- operazioni_1420
  # df <- operazioni_713
  
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  
  reg_rms_713 <- c("015", "016", "018", "019")
  names(reg_rms_713) <- c("CAMPANIA", "PUGLIA", "CALABRIA", "SICILIA")
  
  reg_rms_1420 <- c("015", "016", "017", "018", "019")
  names(reg_rms_1420) <- c("CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA")
  
  reg_rms_2127 <- c("014", "015", "016", "017", "018", "019", "020")
  names(reg_rms_2127) <- c("MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  reg_rt_1420 <- c("013", "014", "020")
  names(reg_rt_1420) <- c("ABRUZZO", "MOLISE", "SARDEGNA")
  
  
  # integra cod_regione da oc
  if (!any(names(df) == "COD_REGIONE")) {
    df <- df %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, COD_REGIONE, DEN_REGIONE, COD_PROVINCIA),
                by = "COD_LOCALE_PROGETTO")
  }
  
  # sostituisce cod_regione con programmazione
  appo <- octk::po_riclass %>%
    filter(!is.na(OC_CODICE_PROGRAMMA)) %>%
    # select(x_CICLO, OC_CODICE_PROGRAMMA, x_REGNAZ) %>% # MEMO: senza x_CICLO genera dupli per 2016XXAMPSAP00 e 2017TOPIOMBIFSC
    select(OC_CODICE_PROGRAMMA, x_REGNAZ) %>% # MEMO: senza x_CICLO genera dupli per 2016XXAMPSAP00 e 2017TOPIOMBIFSC
    mutate(COD_REGIONE_NEW =
             case_when(x_REGNAZ == "PIEMONTE" ~ "001",
                       x_REGNAZ == "VALLE D'AOSTA" ~ "002", # "VALLE D'AOSTA"
                       x_REGNAZ == "LOMBARDIA" ~ "003",
                       x_REGNAZ == "PA TRENTO" ~ "004", # "TRENTINO-ALTO ADIGE"
                       x_REGNAZ == "PA BOLZANO" ~ "004", # "TRENTINO-ALTO ADIGE"
                       x_REGNAZ == "VENETO" ~ "005",
                       x_REGNAZ == "FRIULI-VENEZIA GIULIA" ~ "006", # "FRIULI-VENEZIA GIULIA"
                       x_REGNAZ == "LIGURIA" ~ "007",
                       x_REGNAZ == "EMILIA-ROMAGNA" ~ "008", # "EMILIA-ROMAGNA"
                       x_REGNAZ == "TOSCANA" ~ "009",
                       x_REGNAZ == "UMBRIA" ~ "010",
                       x_REGNAZ == "MARCHE" ~ "011",
                       x_REGNAZ == "LAZIO" ~ "012",
                       x_REGNAZ == "ABRUZZO" ~ "013",
                       x_REGNAZ == "MOLISE" ~ "014",
                       x_REGNAZ == "SARDEGNA" ~ "020",
                       x_REGNAZ == "CAMPANIA" ~ "015",
                       x_REGNAZ == "PUGLIA" ~ "016",
                       x_REGNAZ == "BASILICATA" ~ "017",
                       x_REGNAZ == "CALABRIA" ~ "018",
                       x_REGNAZ == "SICILIA" ~ "019",
                       x_REGNAZ == "NAZ" ~ "",
                       TRUE ~ "CHK"))
  
  df1 <- df %>%
    # left_join(appo, by = c("x_CICLO", "OC_CODICE_PROGRAMMA")) %>%
    left_join(appo, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(COD_REGIONE = case_when(COD_REGIONE_NEW == "" ~  COD_REGIONE,
                                   TRUE ~ COD_REGIONE_NEW)) %>%
    select(-COD_REGIONE_NEW)
  
  # chk <- df1 %>% filter(is.na(COD_REGIONE)) %>% count(OC_CODICE_PROGRAMMA, DEN_REGIONE, x_REGNAZ)
  
  
  strip_000_tokens <- function(x, sep_out = ":::") {
    vapply(x, function(s) {
      
      # MEMO:
      # data_vector <- c("000", "001", "000:::001", "000:::000", "000:::001:::000", "000:::000:::000", "001:::000:::002", "001:::003", "001:::020")
      # strip_000_tokens(data_vector)
      
      if (is.na(s)) return(NA_character_)
      
      # split su una o più ":" (gestisce ":::","::", ecc.)
      parts <- unlist(stringr::str_split(s, ":+"))
      parts <- parts[parts != "" & parts != "000"]
      
      if (length(parts) == 0L) "000" else paste(parts, collapse = sep_out)
    }, character(1))
  }
  
  # fix per macroarea
  chk_regione <- function(data_vector, test_vector) {
    # DEBUG:
    # temp <- c("001:::002", "001:::003", "001:::020")
    # chk_regione(temp, reg_cn)
    
    # OLD:
    sapply(data_vector, function(x) {all(unlist(str_split(x, pattern = ":::")) %in% test_vector)})
    
    # NEW:
    # sapply(data_vector, function(x) {
    #   parts <- unlist(stringr::str_split(x, pattern = ":::"))
    #   parts <- parts[parts != "000" & parts != ""]
    #   length(parts) > 0 && all(parts %in% test_vector)
    # })
  }
  
  df2 <- df1 %>%
    mutate(COD_REGIONE = strip_000_tokens(COD_REGIONE)) %>% 
    mutate(x_MACROAREA = case_when(COD_REGIONE %in% reg_cn ~ "Centro-Nord",
                                   COD_REGIONE %in% reg_sud ~ "Mezzogiorno",
                                   COD_REGIONE == "000" ~ "Ambito nazionale", # AMBITO NAZIONALE
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_cn) == TRUE ~ "Centro-Nord",
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_sud) == TRUE ~ "Mezzogiorno",
                                   grepl(":::", COD_REGIONE) ~ "Trasversale", # MEMO: multi-regionale su più macroaree
                                   COD_REGIONE == "997" ~ "Estero",
                                   COD_REGIONE == "998" ~ "Estero",
                                   TRUE ~ "chk")) 
  
  # DEBUG:
  # df2 %>% filter(x_MACROAREA == "chk") %>% count(COD_REGIONE, DEN_REGIONE)
  
  # integra x_REGIONE
  df3 <- df2 %>%
    mutate(x_REGIONE = case_when(
      COD_REGIONE == "001" ~ "PIEMONTE",
      COD_REGIONE == "002" ~ "VALLE D'AOSTA",
      COD_REGIONE == "003" ~ "LOMBARDIA",
      COD_REGIONE == "005" ~ "VENETO",
      COD_REGIONE == "006" ~ "FRIULI-VENEZIA GIULIA",
      COD_REGIONE == "007" ~ "LIGURIA",
      COD_REGIONE == "008" ~ "EMILIA-ROMAGNA",
      COD_REGIONE == "009" ~ "TOSCANA",
      COD_REGIONE == "010" ~ "UMBRIA",
      COD_REGIONE == "011" ~ "MARCHE",
      COD_REGIONE == "012" ~ "LAZIO",
      COD_REGIONE == "013" ~ "ABRUZZO",
      COD_REGIONE == "014" ~ "MOLISE",
      COD_REGIONE == "020" ~ "SARDEGNA",
      COD_REGIONE == "015" ~ "CAMPANIA",
      COD_REGIONE == "016" ~ "PUGLIA",
      COD_REGIONE == "017" ~ "BASILICATA",
      COD_REGIONE == "018" ~ "CALABRIA",
      COD_REGIONE == "019" ~ "SICILIA",
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" ~ "PA TRENTO",
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" ~ "PA BOLZANO",
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" ~ "PA BOLZANO",
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" ~ "PA TRENTO",
      TRUE ~ "ALTRO TERRITORIO")) 
  
  # integra x_CATERG
  df4 <- df3 %>%
    mutate(x_CATREG = case_when(
      COD_REGIONE == "001" & x_CICLO == "2000-2006" ~ "OB2", #"PIEMONTE"
      COD_REGIONE == "001" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "001" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "001" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "002" & x_CICLO == "2000-2006" ~ "OB2", #"VALLE D'AOSTA"
      COD_REGIONE == "002" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "002" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "002" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "003" & x_CICLO == "2000-2006" ~ "OB2", #"LOMBARDIA"
      COD_REGIONE == "003" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "003" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "003" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2000-2006" ~ "OB2", #"PA TRENTO"
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2000-2006" ~ "OB2", #"PA BOLZANO"
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2000-2006" ~ "OB2", #"PA TRENTO" #MEMO: COD_REGIONE == "004" serve per nn sovrascrivere regione da programmazione
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2000-2006" ~ "OB2", #"PA BOLZANO"
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2021-2027" ~ "RS", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004022" & x_CICLO == "2000-2006" ~ "OB2", #"PA TRENTO" #MEMO: eccezione da gestire a monte
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004022" & x_CICLO == "2007-2013" ~ "CRO", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004022" & x_CICLO == "2014-2020" ~ "RS", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004022" & x_CICLO == "2021-2027" ~ "RS", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004021" & x_CICLO == "2000-2006" ~ "OB2", #"PA BOLZANO"
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004021" & x_CICLO == "2007-2013" ~ "CRO", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004021" & x_CICLO == "2014-2020" ~ "RS", 
      # COD_REGIONE == "000:::004" & COD_PROVINCIA == "004021" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "005" & x_CICLO == "2000-2006" ~ "OB2", #"VENETO"
      COD_REGIONE == "005" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "005" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "005" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "006" & x_CICLO == "2000-2006" ~ "OB2", #"FRIULI-VENEZIA GIULIA"
      COD_REGIONE == "006" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "006" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "006" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "007" & x_CICLO == "2000-2006" ~ "OB2", #"LIGURIA"
      COD_REGIONE == "007" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "007" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "007" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "008" & x_CICLO == "2000-2006" ~ "OB2", #"EMILIA-ROMAGNA"
      COD_REGIONE == "008" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "008" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "008" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "009" & x_CICLO == "2000-2006" ~ "OB2", #"TOSCANA"
      COD_REGIONE == "009" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "009" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "009" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "010" & x_CICLO == "2000-2006" ~ "OB2", #"UMBRIA"
      COD_REGIONE == "010" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "010" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "010" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "011" & x_CICLO == "2000-2006" ~ "OB2", #"MARCHE"
      COD_REGIONE == "011" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "011" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "011" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "012" & x_CICLO == "2000-2006" ~ "OB2", #"LAZIO"
      COD_REGIONE == "012" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "012" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "012" & x_CICLO == "2021-2027" ~ "RS",
      COD_REGIONE == "013" & x_CICLO == "2000-2006" ~ "OB2", #"ABRUZZO"
      COD_REGIONE == "013" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "013" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "013" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "014" & x_CICLO == "2000-2006" ~ "OB1-PHO", #"MOLISE"
      COD_REGIONE == "014" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "014" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "014" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "020" & x_CICLO == "2000-2006" ~ "OB2", #"SARDEGNA" #CHK
      COD_REGIONE == "020" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "020" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "020" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "015" & x_CICLO == "2000-2006" ~ "OB1", #"CAMPANIA"
      COD_REGIONE == "015" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "015" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "015" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "016" & x_CICLO == "2000-2006" ~ "OB1", #"PUGLIA"
      COD_REGIONE == "016" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "016" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "016" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "017" & x_CICLO == "2000-2006" ~ "OB1", #"BASILICATA" #CHK
      COD_REGIONE == "017" & x_CICLO == "2007-2013" ~ "CONV-PHO",
      COD_REGIONE == "017" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "017" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "018" & x_CICLO == "2000-2006" ~ "OB1", #"CALABRIA"
      COD_REGIONE == "018" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "018" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "018" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "019" & x_CICLO == "2000-2006" ~ "OB1", #"SICILIA"
      COD_REGIONE == "019" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "019" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "019" & x_CICLO == "2021-2027" ~ "RMS",
      TRUE ~ "ALTRO TERRITORIO")) 
  
  
  # forza Mezzogiorno per alcuni programmi nazionali di ambiti 713 (senza livelli gerarchici)
  df5 <- df4 %>%
    mutate(x_MACROAREA =  case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "Mezzogiorno", # pon cultura
                                    OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "Mezzogiorno", # pon infrastrutture
                                    # OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "Mezzogiorno", # pon pmi
                                    
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "Mezzogiorno", # pn cultura
                                    OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "Mezzogiorno", # pn salute
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "Mezzogiorno", # pn ric
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "Mezzogiorno", # pn legalità
                                    OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "Mezzogiorno", # jtf
                                    TRUE ~ x_MACROAREA),
           x_CATREG =  case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "CONV",
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "CONV",
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "CONV",
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "RMS", # pon cultura
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "RMS", # pon infrastrutture
                                 
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rms_1420) ~ "RMS", # pon pmi
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rt_1420) ~ "RT",
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "ALTRO TERRITORIO",
                                 
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "RMS", # pn cultura
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "RMS", # pn salute
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "RMS", # pn ric
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "RMS", # pn legalità
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "RMS", # jtf
                                 TRUE ~ x_CATREG),
           x_REGIONE = case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" & COD_REGIONE %in% reg_rms_1420 ~ x_REGIONE, # pon cultura
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" & COD_REGIONE %in% reg_rms_1420 ~ x_REGIONE, # pon infrastrutture
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rms_1420, reg_rt_1420) ~ x_REGIONE, # pon pmi
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "ALTRO TERRITORIO",
                                 
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn cultura
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn salute
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn ric
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn legalità
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # jtf
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "ALTRO TERRITORIO",
                                 TRUE ~ x_REGIONE)) 
  # TODO: reg_sud da sostituire con lista convergenza, rms, ecc. a seconda
  
  # chk <- df5 %>% filter(x_MACROAREA == "chk")
  # chk <- df5 %>% filter(x_MACROAREA == "Trasversale", x_REGIONE == "PA TRENTO") 
  
  return(df5)
}

#' Integra la macroarea da localizzazioni con correzioni per programmazione
#'
#' Integra in x_MACROAREA la macroarea dalle localizzazioni di PREESTESO partendo da OC_MACROAREA. 
#' Corregge per i programmi regionali forzando l'attribuzione alla regione di rifeirmento del programma.
#'
#' @param df Dataset con un perimetro in formato "progetti".
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @return Il dataset con la variabile x_MACROAREA, come factor con levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero").
workflow_macroaree_sub_programmazione <- function(df, df_raw, progetti) {
  
  # DEV:
  # creo x_regione come ora
  # creo x_catereg in base a oc_catereg se presente o x_regione se assente
  # fix x_regione to altro territorio se mismatch con oc_catereg
  # creo x_macroarea in base a x_regione
  # fix
  
  # DEBUG:
  # df <- operazioni_extra
  # df <- operazioni_1420
  # df <- operazioni_713
  # df_raw <- operazioni_extra_raw
  # df_raw <- operazioni_1420_raw
  
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  
  reg_rms_713 <- c("015", "016", "018", "019")
  names(reg_rms_713) <- c("CAMPANIA", "PUGLIA", "CALABRIA", "SICILIA")
  
  reg_rms_1420 <- c("015", "016", "017", "018", "019")
  names(reg_rms_1420) <- c("CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA")
  
  reg_rms_2127 <- c("014", "015", "016", "017", "018", "019", "020")
  names(reg_rms_2127) <- c("MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  reg_rt_1420 <- c("013", "014", "020")
  names(reg_rt_1420) <- c("ABRUZZO", "MOLISE", "SARDEGNA")
  
  reg_rt_2127 <- c("010", "011", "013")
  names(reg_rt_1420) <- c("UMBRIA", "MARCHE", "ABRUZZO")
  
  reg_rs_2127 <- c("001", "002", "003", "004", "005", "006",
                   "007", "008", "009", "012")
  names(reg_rs_2127) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                          "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "LAZIO")
  
  reg_rs_1420 <- c("001", "002", "003", "004", "005", "006",
                   "007", "008", "009", "010", "011", "012")
  names(reg_rs_1420) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                          "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  strip_000_tokens <- function(x, sep_out = ":::") {
    vapply(x, function(s) {
      
      # MEMO:
      # data_vector <- c("000", "001", "000:::001", "000:::000", "000:::001:::000", "000:::000:::000", "001:::000:::002", "001:::003", "001:::020")
      # strip_000_tokens(data_vector)
      
      if (is.na(s)) return(NA_character_)
      
      # split su una o più ":" (gestisce ":::","::", ecc.)
      parts <- unlist(stringr::str_split(s, ":+"))
      parts <- parts[parts != "" & parts != "000"]
      
      if (length(parts) == 0L) "000" else paste(parts, collapse = sep_out)
    }, character(1))
  }
  
  # fix per macroarea
  chk_regione <- function(data_vector, test_vector) {
    # DEBUG:
    # temp <- c("001:::002", "001:::003", "001:::020")
    # chk_regione(temp, reg_cn)
    
    sapply(data_vector, function(x) {all(unlist(str_split(x, pattern = ":::")) %in% test_vector)})
  }
  
  # sostituisce cod_regione con programmazione
  appo <- octk::po_riclass %>%
    filter(!is.na(OC_CODICE_PROGRAMMA)) %>%
    select(OC_CODICE_PROGRAMMA, x_REGNAZ) %>% # MEMO: senza x_CICLO genera dupli per 2016XXAMPSAP00 e 2017TOPIOMBIFSC
    mutate(COD_REGIONE_NEW =
             case_when(x_REGNAZ == "PIEMONTE" ~ "001",
                       x_REGNAZ == "VALLE D'AOSTA" ~ "002", # "VALLE D'AOSTA"
                       x_REGNAZ == "LOMBARDIA" ~ "003",
                       x_REGNAZ == "PA TRENTO" ~ "004", # "TRENTINO-ALTO ADIGE"
                       x_REGNAZ == "PA BOLZANO" ~ "004", # "TRENTINO-ALTO ADIGE"
                       x_REGNAZ == "VENETO" ~ "005",
                       x_REGNAZ == "FRIULI-VENEZIA GIULIA" ~ "006", # "FRIULI-VENEZIA GIULIA"
                       x_REGNAZ == "LIGURIA" ~ "007",
                       x_REGNAZ == "EMILIA-ROMAGNA" ~ "008", # "EMILIA-ROMAGNA"
                       x_REGNAZ == "TOSCANA" ~ "009",
                       x_REGNAZ == "UMBRIA" ~ "010",
                       x_REGNAZ == "MARCHE" ~ "011",
                       x_REGNAZ == "LAZIO" ~ "012",
                       x_REGNAZ == "ABRUZZO" ~ "013",
                       x_REGNAZ == "MOLISE" ~ "014",
                       x_REGNAZ == "SARDEGNA" ~ "020",
                       x_REGNAZ == "CAMPANIA" ~ "015",
                       x_REGNAZ == "PUGLIA" ~ "016",
                       x_REGNAZ == "BASILICATA" ~ "017",
                       x_REGNAZ == "CALABRIA" ~ "018",
                       x_REGNAZ == "SICILIA" ~ "019",
                       x_REGNAZ == "NAZ" ~ "",
                       TRUE ~ "CHK"))
  
  # integra cod_regione da oc
  if (!any(names(df) == "COD_REGIONE")) {
    df <- df %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, COD_REGIONE, DEN_REGIONE, COD_PROVINCIA),
                by = "COD_LOCALE_PROGETTO")
  }
  
  if (!any(names(df) == "ue_categ_regione")) {
    df <- df %>%
      left_join(df_raw %>%
                  select(COD_LOCALE_PROGETTO = cod_locale_progetto, OC_CODICE_PROGRAMMA=oc_cod_programma, ue_categ_regione),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  }
  
  # sum(df1$COE, na.rm = TRUE) - sum(df$COE, na.rm = TRUE) #CHK
  # dim(df1)[1] - dim(df)[1]
  # 
  # df %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  # df1 %>% count(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  
  df1 <- df %>%
    left_join(appo, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(COD_REGIONE = case_when(COD_REGIONE_NEW == "" ~  COD_REGIONE,
                                   TRUE ~ COD_REGIONE_NEW)) %>%
    select(-COD_REGIONE_NEW)
  
  # DEBUG:
  df %>% filter(is.na(COD_REGIONE))
  df1 %>% filter(is.na(COD_REGIONE))
  appo %>% filter(is.na(COD_REGIONE_NEW))
  chk <- df1 %>% filter(is.na(COD_REGIONE))
  appo %>% semi_join(chk, by = "OC_CODICE_PROGRAMMA")
  
  df2 <- df1 %>%
    mutate(COD_REGIONE = strip_000_tokens(COD_REGIONE)) %>% 
    mutate(x_REGIONE = case_when(
      COD_REGIONE == "001" ~ "PIEMONTE",
      COD_REGIONE == "002" ~ "VALLE D'AOSTA",
      COD_REGIONE == "003" ~ "LOMBARDIA",
      COD_REGIONE == "005" ~ "VENETO",
      COD_REGIONE == "006" ~ "FRIULI-VENEZIA GIULIA",
      COD_REGIONE == "007" ~ "LIGURIA",
      COD_REGIONE == "008" ~ "EMILIA-ROMAGNA",
      COD_REGIONE == "009" ~ "TOSCANA",
      COD_REGIONE == "010" ~ "UMBRIA",
      COD_REGIONE == "011" ~ "MARCHE",
      COD_REGIONE == "012" ~ "LAZIO",
      COD_REGIONE == "013" ~ "ABRUZZO",
      COD_REGIONE == "014" ~ "MOLISE",
      COD_REGIONE == "020" ~ "SARDEGNA",
      COD_REGIONE == "015" ~ "CAMPANIA",
      COD_REGIONE == "016" ~ "PUGLIA",
      COD_REGIONE == "017" ~ "BASILICATA",
      COD_REGIONE == "018" ~ "CALABRIA",
      COD_REGIONE == "019" ~ "SICILIA",
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" ~ "PA TRENTO",
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" ~ "PA BOLZANO",
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" ~ "PA BOLZANO",
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" ~ "PA TRENTO",
      TRUE ~ "ALTRO TERRITORIO")) 
  
  # integra x_CATERG
  df3 <- df2 %>%
    mutate(x_CATREG = case_when(
      COD_REGIONE == "001" & x_CICLO == "2000-2006" ~ "OB2", #"PIEMONTE"
      COD_REGIONE == "001" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "001" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "001" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "002" & x_CICLO == "2000-2006" ~ "OB2", #"VALLE D'AOSTA"
      COD_REGIONE == "002" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "002" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "002" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "003" & x_CICLO == "2000-2006" ~ "OB2", #"LOMBARDIA"
      COD_REGIONE == "003" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "003" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "003" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2000-2006" ~ "OB2", #"PA TRENTO"
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA TRENTO" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2000-2006" ~ "OB2", #"PA BOLZANO"
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & x_REGNAZ == "PA BOLZANO" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2000-2006" ~ "OB2", #"PA TRENTO" #MEMO: COD_REGIONE == "004" serve per nn sovrascrivere regione da programmazione
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004022" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2000-2006" ~ "OB2", #"PA BOLZANO"
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "004" & COD_PROVINCIA == "004021" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "005" & x_CICLO == "2000-2006" ~ "OB2", #"VENETO"
      COD_REGIONE == "005" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "005" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "005" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "006" & x_CICLO == "2000-2006" ~ "OB2", #"FRIULI-VENEZIA GIULIA"
      COD_REGIONE == "006" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "006" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "006" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "007" & x_CICLO == "2000-2006" ~ "OB2", #"LIGURIA"
      COD_REGIONE == "007" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "007" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "007" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "008" & x_CICLO == "2000-2006" ~ "OB2", #"EMILIA-ROMAGNA"
      COD_REGIONE == "008" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "008" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "008" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "009" & x_CICLO == "2000-2006" ~ "OB2", #"TOSCANA"
      COD_REGIONE == "009" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "009" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "009" & x_CICLO == "2021-2027" ~ "RS", 
      COD_REGIONE == "010" & x_CICLO == "2000-2006" ~ "OB2", #"UMBRIA"
      COD_REGIONE == "010" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "010" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "010" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "011" & x_CICLO == "2000-2006" ~ "OB2", #"MARCHE"
      COD_REGIONE == "011" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "011" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "011" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "012" & x_CICLO == "2000-2006" ~ "OB2", #"LAZIO"
      COD_REGIONE == "012" & x_CICLO == "2007-2013" ~ "CRO", 
      COD_REGIONE == "012" & x_CICLO == "2014-2020" ~ "RS", 
      COD_REGIONE == "012" & x_CICLO == "2021-2027" ~ "RS",
      COD_REGIONE == "013" & x_CICLO == "2000-2006" ~ "OB2", #"ABRUZZO"
      COD_REGIONE == "013" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "013" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "013" & x_CICLO == "2021-2027" ~ "RT",
      COD_REGIONE == "014" & x_CICLO == "2000-2006" ~ "OB1-PHO", #"MOLISE"
      COD_REGIONE == "014" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "014" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "014" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "020" & x_CICLO == "2000-2006" ~ "OB2", #"SARDEGNA" #CHK
      COD_REGIONE == "020" & x_CICLO == "2007-2013" ~ "CRO-PHIN",
      COD_REGIONE == "020" & x_CICLO == "2014-2020" ~ "RT",
      COD_REGIONE == "020" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "015" & x_CICLO == "2000-2006" ~ "OB1", #"CAMPANIA"
      COD_REGIONE == "015" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "015" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "015" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "016" & x_CICLO == "2000-2006" ~ "OB1", #"PUGLIA"
      COD_REGIONE == "016" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "016" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "016" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "017" & x_CICLO == "2000-2006" ~ "OB1", #"BASILICATA" #CHK
      COD_REGIONE == "017" & x_CICLO == "2007-2013" ~ "CONV-PHO",
      COD_REGIONE == "017" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "017" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "018" & x_CICLO == "2000-2006" ~ "OB1", #"CALABRIA"
      COD_REGIONE == "018" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "018" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "018" & x_CICLO == "2021-2027" ~ "RMS",
      COD_REGIONE == "019" & x_CICLO == "2000-2006" ~ "OB1", #"SICILIA"
      COD_REGIONE == "019" & x_CICLO == "2007-2013" ~ "CONV",
      COD_REGIONE == "019" & x_CICLO == "2014-2020" ~ "RMS",
      COD_REGIONE == "019" & x_CICLO == "2021-2027" ~ "RMS",
      TRUE ~ "ALTRO TERRITORIO")) 
  
  
  df4 <- df3 %>% 
    mutate(ue_categ_regione = gsub(":::", "", ue_categ_regione)) %>% 
    mutate(x_CATREG = case_when(ue_categ_regione == "L" ~ "RMS",
                                ue_categ_regione == "T" ~ "RT",
                                ue_categ_regione == "M" ~ "RS",
                                ue_categ_regione == "LM" ~ "ALTRO TERRITORIO", #casi da ":::"
                                ue_categ_regione == "LMT" ~ "ALTRO TERRITORIO",
                                ue_categ_regione == "LR" ~ "ALTRO TERRITORIO", #R per react
                                ue_categ_regione == "LT" ~ "ALTRO TERRITORIO",
                                ue_categ_regione == "LMTLMT" ~ "ALTRO TERRITORIO",
                                is.na(ue_categ_regione) ~ x_CATREG,
                                TRUE ~ x_CATREG))
  
  # chk <- df4 %>% count(x_CICLO, x_CATREG, x_REGIONE)
  
  df5 <- df4 %>%
    mutate(x_MACROAREA = case_when(COD_REGIONE %in% reg_cn ~ "Centro-Nord",
                                   COD_REGIONE %in% reg_sud ~ "Mezzogiorno",
                                   COD_REGIONE == "000" ~ "Ambito nazionale", # AMBITO NAZIONALE
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_cn) == TRUE ~ "Centro-Nord",
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_sud) == TRUE ~ "Mezzogiorno",
                                   grepl(":::", COD_REGIONE) ~ "Trasversale", # MEMO: multi-regionale su più macroaree
                                   COD_REGIONE == "997" ~ "Estero",
                                   COD_REGIONE == "998" ~ "Estero",
                                   TRUE ~ "chk")) 
  
  # DBEUG:
  # df5 %>% filter(x_MACROAREA == "chk") %>% select(COD_REGIONE)
  df %>% filter(is.na(COD_REGIONE))
  df1 %>% filter(is.na(COD_REGIONE))
  
  df6 <- df5 %>% 
    mutate(x_REGIONE = case_when(x_CICLO == "2021-2027" & x_CATREG == "RMS" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE,
                                 x_CICLO == "2021-2027" & x_CATREG == "RT" & COD_REGIONE %in% reg_rt_2127 ~ x_REGIONE,
                                 x_CICLO == "2021-2027" & x_CATREG == "RS" & COD_REGIONE %in% reg_rs_2127 ~ x_REGIONE,
                                 x_CICLO == "2021-2027" ~ "ALTRO TERRITORIO",
                                 x_CICLO == "2014-2020" & x_CATREG == "RMS" & COD_REGIONE %in% reg_rms_1420 ~ x_REGIONE,
                                 x_CICLO == "2014-2020" & x_CATREG == "RT" & COD_REGIONE %in% reg_rt_1420 ~ x_REGIONE,
                                 x_CICLO == "2014-2020" & x_CATREG == "RS" & COD_REGIONE %in% reg_rs_1420 ~ x_REGIONE,
                                 x_CICLO == "2014-2020" ~ "ALTRO TERRITORIO",
                                 TRUE ~ x_REGIONE)) %>% 
    mutate(x_MACROAREA = case_when(x_CICLO == "2021-2027" & x_CATREG == "RMS" ~ "Mezzogiorno",
                                   x_CICLO == "2021-2027" & x_CATREG == "RT" & COD_REGIONE == "013" ~ "Mezzogiorno",
                                   x_CICLO == "2021-2027" & x_CATREG == "RT" & COD_REGIONE %in% c("010", "011") ~ "Centro-Nord",
                                   x_CICLO == "2021-2027" & x_CATREG == "RS" ~ "Centro-Nord",
                                   x_CICLO == "2021-2027" & x_CATREG == "ALTRO TERRITORIO" ~ x_MACROAREA,
                                   x_CICLO == "2021-2027" ~ "Trasversale",
                                   x_CICLO == "2014-2020" & x_CATREG == "RMS" ~ "Mezzogiorno",
                                   x_CICLO == "2014-2020" & x_CATREG == "RT" ~ "Mezzogiorno",
                                   x_CICLO == "2014-2020" & x_CATREG == "RS" ~ "Centro-Nord",
                                   x_CICLO == "2014-2020" & x_CATREG == "ALTRO TERRITORIO" ~ x_MACROAREA,
                                   x_CICLO == "2014-2020" ~ "Trasversale",
                                   TRUE ~ x_MACROAREA))
  
  chk <- df6 %>% count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
  
  
  # forza Mezzogiorno per alcuni programmi nazionali di ambiti 713 (senza livelli gerarchici)
  df7 <- df6 %>%
    mutate(x_MACROAREA =  case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "Mezzogiorno",
                                    OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "Mezzogiorno", # pon cultura
                                    OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "Mezzogiorno", # pon infrastrutture
                                    # OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "Mezzogiorno", # pon pmi
                                    
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "Mezzogiorno", # pn cultura
                                    OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "Mezzogiorno", # pn salute
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "Mezzogiorno", # pn ric
                                    OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "Mezzogiorno", # pn legalità
                                    OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "Mezzogiorno", # jtf
                                    TRUE ~ x_MACROAREA),
           x_CATREG =  case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "CONV",
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "CONV",
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "CONV",
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "RMS", # pon cultura
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "RMS", # pon infrastrutture
                                 
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rms_1420) ~ "RMS", # pon pmi
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rt_1420) ~ "RT",
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "ALTRO TERRITORIO",
                                 
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "RMS", # pn cultura
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "RMS", # pn salute
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "RMS", # pn ric
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "RMS", # pn legalità
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "RMS", # jtf
                                 TRUE ~ x_CATREG),
           x_REGIONE = case_when(x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "FESR" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "FSE" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" & COD_REGIONE %in% reg_rms_713 ~ x_REGIONE,
                                 x_CICLO == "2007-2013" & x_AMBITO == "PAC" & x_REGNAZ == "NAZ" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" & COD_REGIONE %in% reg_rms_1420 ~ x_REGIONE, # pon cultura
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP001" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" & COD_REGIONE %in% reg_rms_1420 ~ x_REGIONE, # pon infrastrutture
                                 OC_CODICE_PROGRAMMA == "2014IT16RFOP002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" & COD_REGIONE %in% c(reg_rms_1420, reg_rt_1420) ~ x_REGIONE, # pon pmi
                                 OC_CODICE_PROGRAMMA == "2015IT16RFSM001" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn cultura
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR003" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn salute
                                 OC_CODICE_PROGRAMMA == "2021IT05FFPR002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn ric
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR001" ~ "ALTRO TERRITORIO", 
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # pn legalità
                                 OC_CODICE_PROGRAMMA == "2021IT16RFPR002" ~ "ALTRO TERRITORIO",
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" & COD_REGIONE %in% reg_rms_2127 ~ x_REGIONE, # jtf
                                 OC_CODICE_PROGRAMMA == "2021IT16JTPR001" ~ "ALTRO TERRITORIO",
                                 TRUE ~ x_REGIONE)) 
  
  
  chk <- df7 %>% count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE)
  
  return(df7)
}

workflow_macroaree_sub_studio <- function(df, debug=FALSE) {
  
  # DEBUG:
  # df <- operazioni_1420_1
  
  po_react <- c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05SFOP001", "2014IT05SFOP001",  "2014IT05SFOP002", 
                "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003")
  po_yei <- c("2014IT05M9OP001")
  po_psc <- octk::po_riclass %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  po_ant <- octk::po_riclass %>% filter(x_CICLO == "2021-2027", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  
  
  # studio casistiche
  chk <- df %>%
    # select(-costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -imp_trasf_ammesso_MZ, -imp_trasf_ammesso_CN,
    #        -pag_ammesso_MZ, -pag_ammesso_CN, -pag_trasf_ammesso_MZ, -pag_trasf_ammesso_CN) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # mutate_if(is.numeric, round, digits=2) %>% 
    # mutate(COE = round(COE, 2),
    #        COE_SUD = round(COE_SUD, 2),
    #        COE_CN = round(COE_CN, 2)) %>% 
    mutate(chk_coe = COE - COE_SUD - COE_CN,
           chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>%
    # mutate(chk_coe = round(chk_coe, 2)) %>%
    mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
                              OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
                              OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
                              OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
                              TRUE ~ "")) %>% 
    # mutate(CLASSE = case_when(x_AMBITO == "JTF" ~ "JTF",
    #                           x_AMBITO == "CTE" ~ "CTE",
    #                           TRUE ~ "")) %>% 
    mutate(CHK_COE = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ "tutto sud localizzazioni e livelli",
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ "tutto sud ma manca una parte da livelli",
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ "tutto sud solo localizzazioni",
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ "divergenza livelli vs localizzazioni (no delta)",
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ "divergenza livelli vs localizzazioni (con delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ "divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ "divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ "tutto cn localizzazioni e livelli",
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ "tutto cn ma manca una parte da livelli",
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ "tutto cn solo localizzazioni",
                               
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ "divergenza livelli vs localizzazioni (no delta)",
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ "divergenza livelli vs localizzazioni (con delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ "divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ "divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ "divergenza parziale livelli vs localizzazioni (ambito nazionale)",
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ "divergenza parziale livelli vs localizzazioni (ambito nazionale)",
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ "divergenza parziale livelli vs localizzazioni (ambito nazionale)",
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ "ambito nazionale puro",
                               
                               COE == 0 ~ "fin nullo",
                               COE < 1 ~ "fin quasi nullo",
                               chk_coe < 0 ~ "negativi",
                               TRUE ~ "chk")) %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  # debug
  if (debug == TRUE) {
    
    chk %>% 
      group_by(CHK_COE) %>% 
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))  
    
    
    # chk2 <- chk %>%
    #   group_by(x_AMBITO, CHK_COE) %>%
    #   summarise(N =n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_SUD = sum(COE_SUD, na.rm = TRUE),
    #             COE_CN = sum(COE_CN, na.rm = TRUE),
    #             tot = sum(tot, na.rm = TRUE),
    #             chk = sum(chk, na.rm = TRUE))
    
    temp <- chk %>% 
      filter(x_AMBITO == "FDR") %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (no delta)") 
    temp %>% 
      group_by(OC_CODICE_PROGRAMMA, x_MACROAREA) %>% 
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_fdr.xlsx"))
    
    temp <- chk %>% 
      filter(x_AMBITO == "POC") %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (no delta)") 
    temp %>% 
      group_by(OC_CODICE_PROGRAMMA, x_MACROAREA) %>% 
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_poc.xlsx"))
    
    temp <- chk %>% 
      filter(x_AMBITO == "FSC") %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (no delta)") 
    temp %>% 
      group_by(OC_CODICE_PROGRAMMA, x_MACROAREA) %>% 
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_fsc.xlsx"))
    
    temp <- chk %>% 
      filter(x_AMBITO == "FESR") %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (no delta)") %>% 
      left_join(po %>% select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA), by = "OC_CODICE_PROGRAMMA") 
    temp %>% 
      # group_by(x_GRUPPO, x_MACROAREA) %>%
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_MACROAREA) %>%
      group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_fesr.xlsx"))
    
    
    temp <- chk %>% 
      filter(x_AMBITO == "FSE") %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (no delta)") %>% 
      left_join(po %>% select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA), by = "OC_CODICE_PROGRAMMA") 
    temp %>% 
      # group_by(x_GRUPPO, x_MACROAREA) %>%
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_MACROAREA) %>%
      group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_fse.xlsx"))
    
    temp <- chk %>% 
      filter(CHK_COE == "tutto sud ma manca una parte da livelli") %>% 
      left_join(po %>% select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA), by = "OC_CODICE_PROGRAMMA") 
    temp %>% 
      group_by(x_GRUPPO, x_MACROAREA) %>%
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_MACROAREA) %>% 
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>% 
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    # x_GRUPPO x_MACROAREA          N        COE    COE_SUD     COE_CN        tot   chk
    # <chr>    <fct>            <int>      <dbl>      <dbl>      <dbl>      <dbl> <dbl>
    # 1 ACCORDI  Ambito nazionale     4   2302178.   2302178.         0    2302178.     0 -> x_MACROAREE
    # 2 ACCORDI  Estero               5    883477.    883477.         0     883477.     0 -> x_MACROAREE
    # 3 PN       Ambito nazionale  4120 301042403. 193443822. 107598581. 301042403.     0 -> quanti progetti andranno a finire su due macroaree?
    # 4 PN       Trasversale         21   6356800.   6356800.         0    6356800.     0 -> quanti progetti andranno a finire su due macroaree?
    # 5 PR       Ambito nazionale    25  12804972.   5013418    7791554.  12804972.     0 -> x_MACROAREE
    # 6 PR       Trasversale          4   1766275.   1493135.    273140    1766275.     0 -> x_MACROAREE
    # 7 PR       Estero              16   1560487.   1476823.     83665.   1560487.     0 -> x_MACROAREE
    # duplicati molise
    write.xlsx(temp, file.path(TEMP, "chk_macroarea_molise.xlsx"))
    
    
    
    temp <- chk %>% 
      filter(CHK_COE == "divergenza livelli vs localizzazioni (con delta)") %>% 
      left_join(po %>% select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA), by = "OC_CODICE_PROGRAMMA") 
    temp %>% 
      # group_by(x_GRUPPO, x_MACROAREA) %>%
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_MACROAREA) %>% 
      group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    
    temp <- chk %>% 
      filter(CHK_COE == "divergenza parziale livelli vs localizzazioni (ambito nazionale)") %>% 
      left_join(po %>% select(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA), by = "OC_CODICE_PROGRAMMA") 
    temp %>% 
      # group_by(x_GRUPPO, x_MACROAREA) %>%
      group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_MACROAREA) %>%
      # group_by(OC_CODICE_PROGRAMMA, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
      summarise(N =n(),
                COE = sum(COE, na.rm = TRUE),
                COE_SUD = sum(COE_SUD, na.rm = TRUE),
                COE_CN = sum(COE_CN, na.rm = TRUE),
                tot = sum(tot, na.rm = TRUE),
                chk = sum(chk, na.rm = TRUE))
    # write.xlsx(temp, file.path(TEMP, "chk_macroarea_salute.xlsx"))
    
  }
  
  # epxort
  return(chk)
  
}

workflow_macroaree_sub_mapping <- function(df) {
  
  # DEBUG:
  # df <- operazioni_1420_1
  
  df2 <- df %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # fix per anomalie floating
    # mutate_if(is.numeric, round, digits=2) %>% 
    # mutate(COE = round(COE, 2),
    #        COE_SUD = round(COE_SUD, 2),
    #        COE_CN = round(COE_CN, 2),
    #        
    #        COE_IMP = round(COE_IMP, 2),
    #        COE_IMP_SUD = round(COE_IMP_SUD, 2),
    #        COE_IMP_CN = round(COE_IMP_CN, 2),
    #        
    #        COE_PAG = round(COE_PAG, 2),
  #        COE_PAG_SUD = round(COE_PAG_SUD, 2),
  #        COE_PAG_CN = round(COE_PAG_CN, 2)) %>% 
  mutate(chk_coe = COE - COE_SUD - COE_CN,
         chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
         chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>%
    # fix casi anomali con delta negativo
    mutate(chk_coe = if_else(chk_coe < 0, 0, chk_coe),
           chk_coe_imp = if_else(chk_coe_imp < 0, 0, chk_coe_imp),
           chk_coe_pag = if_else(chk_coe_pag < 0, 0, chk_coe_pag)) %>% 
    # mutate(chk_coe = round(chk_coe, 2),
    #        chk_coe_imp = round(chk_coe_imp, 2),
    #        chk_coe_pag = round(chk_coe_pag, 2)) %>% 
    # mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
    #                           OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
    #                           OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
    #                           OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
    #                           TRUE ~ "")) %>% 
    mutate(COE_SUD = case_when(
      COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"tutto sud localizzazioni e livelli"
      COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD + chk_coe, #"tutto sud ma manca una parte da livelli"
      COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ chk_coe, #"tutto sud solo localizzazioni"
      
      COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
      COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
      COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
      COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
      
      COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ 0, #"tutto cn localizzazioni e livelli"
      COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
      COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ 0, #"tutto cn solo localizzazioni"
      
      COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (no delta)"
      COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (con delta)"
      COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
      COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
      
      COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
      COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
      COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
      COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
      
      COE == 0 ~ 0, #"fin nullo"
      COE < 1 ~ 0, #"fin quasi nullo"
      TRUE ~ 0),
      
      COE_CN = case_when(
        COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ 0, #"tutto sud localizzazioni e livelli"
        COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
        COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ 0, #"tutto sud solo localizzazioni"
        
        COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (no delta)"
        COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (con delta)"
        COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
        COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
        
        COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"tutto cn localizzazioni e livelli"
        COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN + chk_coe, #"tutto cn ma manca una parte da livelli"
        COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ chk_coe, #"tutto cn solo localizzazioni"
        
        COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
        COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
        COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
        COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
        
        COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
        COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
        COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
        COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
        
        COE == 0 ~ 0, #"fin nullo"
        COE < 1 ~ 0, #"fin quasi nullo"
        TRUE ~ 0),
      
      # TODO: finire di semplificare con 0
      COE_ND = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ 0, #"tutto sud localizzazioni e livelli"
                         COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
                         COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno"& chk_coe > 0 ~ 0, #"tutto sud solo localizzazioni"
                         
                         COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                         COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ chk_coe, #"divergenza livelli vs localizzazioni (con delta)"
                         COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)",
                         COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ chk_coe, #"divergenza parziale livelli vs localizzazioni (con delta)",
                         
                         COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ 0, #"tutto cn localizzazioni e livelli"
                         COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
                         COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ 0, #"tutto cn solo localizzazioni"
                         
                         COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                         COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ chk_coe, #"divergenza livelli vs localizzazioni (con delta)"
                         COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)"
                         COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ chk_coe, #"divergenza parziale livelli vs localizzazioni (con delta)"
                         
                         COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                         COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                         COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                         COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe, #"ambito nazionale puro"
                         
                         COE == 0 ~ 0, #"fin nullo"
                         COE < 1 ~ 0, #"fin quasi nullo"
                         TRUE ~ 0)) %>% 
    
    #impegni  
    mutate(COE_IMP_SUD = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"tutto sud localizzazioni e livelli"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud ma manca una parte da livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ chk_coe_imp, #"tutto sud solo localizzazioni"
                                   
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                   
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ 0, #"tutto cn localizzazioni e livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ 0, #"tutto cn solo localizzazioni"
                                   
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                   
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
                                   
                                   COE_IMP == 0 ~ 0, #"fin nullo"
                                   COE_IMP < 1 ~ 0, #"fin quasi nullo"
                                   TRUE ~ 0),
           
           COE_IMP_CN = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ 0, #"tutto sud localizzazioni e livelli"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ 0, #"tutto sud solo localizzazioni"
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"tutto cn localizzazioni e livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ chk_coe_imp, #"tutto cn solo localizzazioni"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
                                  
                                  COE_IMP == 0 ~ 0, #"fin nullo"
                                  COE_IMP < 1 ~ 0, #"fin quasi nullo"
                                  TRUE ~ 0),
           
           COE_IMP_ND = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ 0, #"tutto sud localizzazioni e livelli"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno"& chk_coe_imp > 0 ~ 0, #"tutto sud solo localizzazioni"
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ chk_coe_imp, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ chk_coe_imp, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ 0, #"tutto cn localizzazioni e livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ 0, #"tutto cn solo localizzazioni"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ chk_coe_imp, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ chk_coe_imp, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_imp, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_imp, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_imp, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_imp, #"ambito nazionale puro"
                                  
                                  COE_IMP == 0 ~ 0, #"fin nullo"
                                  COE_IMP < 1 ~ 0, #"fin quasi nullo"
                                  TRUE ~ 0)) %>% 
    
    # pagamenti 
    mutate(COE_PAG_SUD = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"tutto sud localizzazioni e livelli"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud ma manca una parte da livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ chk_coe_pag, #"tutto sud solo localizzazioni"
                                   
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                   
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ 0, #"tutto cn localizzazioni e livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ 0, #"tutto cn solo localizzazioni"
                                   
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                   
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
                                   
                                   COE_PAG == 0 ~ 0, #"fin nullo"
                                   COE_PAG < 1 ~ 0, #"fin quasi nullo"
                                   TRUE ~ 0),
           
           COE_PAG_CN = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ 0, #"tutto sud localizzazioni e livelli"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ 0, #"tutto sud solo localizzazioni"
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"tutto cn localizzazioni e livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ chk_coe_pag, #"tutto cn solo localizzazioni"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ 0, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ 0, #"ambito nazionale puro"
                                  
                                  COE_PAG == 0 ~ 0, #"fin nullo"
                                  COE_PAG < 1 ~ 0, #"fin quasi nullo"
                                  TRUE ~ 0),
           
           COE_PAG_ND = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ 0, #"tutto sud localizzazioni e livelli"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ 0, #"tutto sud ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno"& chk_coe_pag > 0 ~ 0, #"tutto sud solo localizzazioni"
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ chk_coe_pag, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ chk_coe_pag, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ 0, #"tutto cn localizzazioni e livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ 0, #"tutto cn ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ 0, #"tutto cn solo localizzazioni"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ 0, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ chk_coe_pag, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ 0, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ chk_coe_pag, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_pag, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_pag, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_pag, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ chk_coe_pag, #"ambito nazionale puro"
                                  
                                  COE_PAG == 0 ~ 0, #"fin nullo"
                                  COE_PAG < 1 ~ 0, #"fin quasi nullo"
                                  TRUE ~ 0)) %>% 
    
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2) %>% 
    # fix per anomalie floating
    mutate_if(is.numeric, round, digits=2) 
  
  return(df2)
}


#' Workflow per creazione file di base per macroaree
#'
#' Workflow per creazione file di base per macroaree.
#' Funziona con input specifici per sie non react e per psc migrati.
#'
#' @param df Dataset di classe operazioni
#' @return Dataset pivot con macroaree per setup_macroaree_sie e setup_macroaree_psc
workflow_macroaree_sub_pivot <- function(df) {
  
  # DEBUG:
  # df <- operazioni_extra_2
  
  # pivot costo
  appo_costo <- df  %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           x_CICLO,
           x_MACROAREA,
           x_CATREG,
           x_REGIONE,
           x_REGNAZ,
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE,
           COE_SUD,
           COE_CN,
           COE_ND)
  
  # chk
  appo_costo %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_SUD + COE_CN + COE_ND,
           chk = COE - tot) %>%
    group_by(x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_costo <- appo_costo %>% 
    select(-COE) %>%
    pivot_longer(cols = c(COE_SUD, COE_CN, COE_ND), names_to = "TEMP", values_to = "COE") %>% 
    filter(COE != 0) %>% # elimina righe vuote
    mutate(TEMP = case_when(TEMP == "COE_SUD" ~ "Mezzogiorno",
                            TEMP == "COE_CN" ~ "Centro-Nord",
                            TEMP == "COE_ND" ~ "Ambito nazionale",
                            TRUE ~ "CHK")) %>% 
    mutate(x_MACROAREA_OLD = x_MACROAREA,
           x_MACROAREA = TEMP) %>% 
    select(-x_MACROAREA_OLD, -TEMP)
  
  sum(appo_costo$COE, na.rm = TRUE) - sum(df$COE, na.rm = TRUE)
  sum(pivo_costo$COE, na.rm = TRUE) - sum(df$COE, na.rm = TRUE)
  
  # pivo_costo %>% filter(x_MACROAREA == "Centro-Nord", x_CATREG == "RMS") %>% count(OC_CODICE_PROGRAMMA, x_MACROAREA, x_MACROAREA_OLD)
  # pivo_costo %>% filter(x_MACROAREA == "Mezzogiorno", x_CATREG == "RS") %>% count(OC_CODICE_PROGRAMMA, x_MACROAREA, x_MACROAREA_OLD)
  
  # pivot impegni
  appo_imp <- df %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           x_CICLO,
           x_MACROAREA,
           x_CATREG,
           x_REGIONE,
           x_REGNAZ,
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN,
           COE_IMP_ND)
  
  appo_imp  %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_IMP_SUD + COE_IMP_CN + COE_IMP_ND,
           chk = COE_IMP - tot) %>%
    group_by(x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  pivo_imp <- appo_imp %>% 
    select(-COE_IMP) %>% 
    pivot_longer(cols = c(COE_IMP_SUD, COE_IMP_CN, COE_IMP_ND), names_to = "TEMP", values_to = "COE_IMP") %>% 
    filter(COE_IMP != 0) %>% # elimina righe vuote
    mutate(TEMP = case_when(TEMP == "COE_IMP_SUD" ~ "Mezzogiorno",
                            TEMP == "COE_IMP_CN" ~ "Centro-Nord",
                            TEMP == "COE_IMP_ND" ~ "Ambito nazionale",
                            TRUE ~ "CHK")) %>% 
    mutate(x_MACROAREA_OLD = x_MACROAREA,
           x_MACROAREA = TEMP) %>% 
    select(-x_MACROAREA_OLD, -TEMP)
  
  
  # pivot pagamenti
  appo_pag <- df %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           x_CICLO,
           x_MACROAREA,
           x_CATREG,
           x_REGIONE,
           x_REGNAZ,
           x_LIVELLO_0, 
           x_LIVELLO_1, 
           x_LIVELLO_2,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN,
           COE_PAG_ND)
  
  appo_pag  %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_PAG_SUD + COE_PAG_CN + COE_PAG_ND,
           chk = COE_PAG - tot) %>%
    group_by(x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_pag <- appo_pag %>% 
    select(-COE_PAG) %>% 
    pivot_longer(cols = c(COE_PAG_SUD, COE_PAG_CN, COE_PAG_ND), names_to = "TEMP", values_to = "COE_PAG") %>% 
    filter(COE_PAG != 0) %>% # elimina righe vuote
    mutate(TEMP = case_when(TEMP == "COE_PAG_SUD" ~ "Mezzogiorno",
                            TEMP == "COE_PAG_CN" ~ "Centro-Nord",
                            TEMP == "COE_PAG_ND" ~ "Ambito nazionale",
                            TRUE ~ "CHK")) %>% 
    mutate(x_MACROAREA_OLD = x_MACROAREA,
           x_MACROAREA = TEMP) %>% 
    select(-x_MACROAREA_OLD, -TEMP)
  
  
  # join
  pivo <- pivo_costo %>% 
    full_join(pivo_imp, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO", "x_MACROAREA",
                               "x_CATREG", "x_REGIONE", "x_REGNAZ", "x_LIVELLO_0", "x_LIVELLO_1", "x_LIVELLO_2")) %>% 
    full_join(pivo_pag, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO", "x_MACROAREA",
                               "x_CATREG", "x_REGIONE", "x_REGNAZ", "x_LIVELLO_0", "x_LIVELLO_1", "x_LIVELLO_2")) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) 
  # %>% 
  #   # clean vuoti
  #   filter(COE != 0)
  
  chk <- pivo %>% 
    filter(COE == 0, COE_IMP > 0)
  
  # chk
  sum(pivo$COE, na.rm = TRUE) - (sum(appo_costo$COE_SUD, na.rm = TRUE) + sum(appo_costo$COE_CN, na.rm = TRUE) + sum(appo_costo$COE_ND, na.rm = TRUE))
  sum(pivo$COE_IMP, na.rm = TRUE) - (sum(appo_imp$COE_IMP_SUD, na.rm = TRUE) + sum(appo_imp$COE_IMP_CN, na.rm = TRUE) + sum(appo_imp$COE_IMP_ND, na.rm = TRUE))
  sum(pivo$COE_PAG, na.rm = TRUE) - (sum(appo_pag$COE_PAG_SUD, na.rm = TRUE) + sum(appo_pag$COE_PAG_CN, na.rm = TRUE) + sum(appo_pag$COE_PAG_ND, na.rm = TRUE))
  
  # export
  return(pivo)
  
}

workflow_macroaree_sub_ciclo <- function(df, po) {
  
  # DEBUG:
  # df <- operazioni_1420_0
  
  # lista psc
  psc <- po %>% filter(x_GRUPPO == "PSC", x_CICLO == "2014-2020", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  
  if (!("OC_COD_CICLO" %in% names(df))) {
    # print("aggiungo OC_COD_CICLO standard")
    df <- df %>% 
      mutate(OC_COD_CICLO = 1) #forzo 713 -> PERCHE?
  }
  
  df1 <- df %>% 
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO_0=x_CICLO),
              by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(CHK_PSC = if_else(OC_CODICE_PROGRAMMA %in% psc, 1, 0)) %>% 
    mutate(x_CICLO = case_when(CHK_PSC == 1 & OC_COD_CICLO == 1 ~ "2007-2013",
                               CHK_PSC == 1 & OC_COD_CICLO == 2 ~ "2014-2020",
                               CHK_PSC == 1 & OC_COD_CICLO == 9 ~ "2000-2006",
                               TRUE ~ x_CICLO_0))
  
  # chk
  # df1 %>% count(x_CICLO, OC_COD_CICLO)
  # chk <- df1 %>% filter(x_CICLO == "2014-2020", OC_COD_CICLO == 1)
  
  return(df1)
  
}

workflow_macroaree_sub_fixing <- function(df) {
  
  # DEV:
  # fix x_regione per macroarea
  # fix x_catreg per macroarea
  
  # DEBUG:
  # df <- operazioni_extra_3
  
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  
  # reg_rms_713 <- c("015", "016", "018", "019")
  # names(reg_rms_713) <- c("CAMPANIA", "PUGLIA", "CALABRIA", "SICILIA")
  # 
  # reg_rms_1420 <- c("015", "016", "017", "018", "019")
  # names(reg_rms_1420) <- c("CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA")
  # 
  # reg_rms_2127 <- c("014", "015", "016", "017", "018", "019", "020")
  # names(reg_rms_2127) <- c("MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  # 
  # reg_rt_1420 <- c("013", "014", "020")
  # names(reg_rt_1420) <- c("ABRUZZO", "MOLISE", "SARDEGNA")
  # 
  # reg_rt_2127 <- c("010", "011", "013")
  # names(reg_rt_1420) <- c("UMBRIA", "MARCHE", "ABRUZZO")
  # 
  # reg_rs_2127 <- c("001", "002", "003", "004", "005", "006",
  #                  "007", "008", "009", "012")
  # names(reg_rs_2127) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
  #                         "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "LAZIO")
  # 
  # reg_rs_1420 <- c("001", "002", "003", "004", "005", "006",
  #                  "007", "008", "009", "010", "011", "012")
  # names(reg_rs_1420) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
  #                         "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  df1 <- df %>% 
    mutate(x_REGIONE = case_when(x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_REGIONE %in% names(reg_sud) ~ x_REGIONE,
                                 x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" ~ "ALTRO TERRITORIO",
                                 x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_REGIONE %in% names(reg_cn) ~ x_REGIONE,
                                 x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" ~ "ALTRO TERRITORIO",
                                 TRUE ~ x_REGIONE),
           x_CATREG = case_when(x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2014-2020" & x_CATREG == "RMS" ~ x_CATREG, # COD_REGIONE %in% reg_rms_1420
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2014-2020" & x_CATREG == "RT" ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2014-2020" ~ "ALTRO TERRITORIO",
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_CICLO == "2014-2020" & x_CATREG == "RS"  ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_CICLO == "2014-2020" ~ "ALTRO TERRITORIO",
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2021-2027" & x_CATREG == "RMS" ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2021-2027" & x_CATREG == "RT" & x_REGIONE == "ABRUZZO" ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Mezzogiorno" & x_CICLO == "2021-2027" ~ "ALTRO TERRITORIO",
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_CICLO == "2021-2027" & x_CATREG == "RS" ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_CICLO == "2021-2027" & x_CATREG == "RT" & x_REGIONE %in% c("MARCHE", "UMBRIA") ~ x_CATREG,
                                x_REGNAZ == "NAZ" & x_MACROAREA == "Centro-Nord" & x_CICLO == "2021-2027" ~ "ALTRO TERRITORIO",
                                TRUE ~ x_CATREG))
  
  # DEBUG:
  # chk <- df1 %>% count(x_CICLO, x_MACROAREA, x_CATREG, x_REGIONE, x_REGNAZ)
  
  return(df1)
  
}

