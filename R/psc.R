# Funzioni per la gestione dei PSC (versione fino a 20240831 con migrazione in corso)

# DEV:
# questa versione resta viva per elaborazioni retrospettive

#' Inizializza PSC
#'
#' Inizializza global environment per flusso di lavoro PSC
#' 
#' @param PSC Folder di supporto per dati PSC
#' @param light Vuoi usare versione leggera (senza liste progetti art44, cds, ecc.)?
#' @return Carica i file necessario al flusso nel global environment
#' @note ...
init_psc <- function(PSC=NULL, light=FALSE) {
  
  if (is.null(PSC)) {
    PSC <- file.path(DRIVE, "DATI", "PSC")
  } 
  
  PSC <<- PSC
    
  matrix_po_psc <<- read_csv2(file.path(PSC, "info", "matrix_po_psc.csv")) %>%
    mutate(ID_PSC = paste0("PSC_", gsub(" ", "_", PSC))) %>% 
    # fix per codice programma con "0X"
    mutate(OC_CODICE_PROGRAMMA = case_when(X_CICLO == "2000-2006" & nchar(OC_CODICE_PROGRAMMA) == 1 ~ str_pad(OC_CODICE_PROGRAMMA, 2, pad = "0"),
                                           TRUE ~ OC_CODICE_PROGRAMMA)) %>% 
    fix_id_psc_15_digit(.)
  # MEMO: non contiene direttrici ferroviarie e salerno reggio calabria da gestire a parte con po_naz
  
  po_naz <<- read_csv2(file.path(PSC, "info", "ricodifica_po_naz.csv")) 
  
  matrix_06 <<- read_xlsx(file.path(PSC, "info", "mapping.xlsx"), sheet = "matrix_06")
  matrix_713 <<- read_xlsx(file.path(PSC, "info", "mapping.xlsx"), sheet = "matrix_713")
  matrix_1420 <<- read_xlsx(file.path(PSC, "info", "mapping.xlsx"), sheet = "matrix_1420", col_types = rep("text", 8))
  
  matrix_temi_settori <<- read_csv2(file.path(PSC, "info", "matrix_temi_settori.csv"))
  
  lista_psc <<- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
    filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
    rename(ID_PSC = OC_CODICE_PROGRAMMA) %>% 
    distinct(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE) %>% 
    # fix per nuove codifiche
    left_join(matrix_po_psc %>% 
                filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
                select(OC_CODICE_PROGRAMMA, TEMP = ID_PSC) %>%
                rename(ID_PSC = OC_CODICE_PROGRAMMA),
              by = "ID_PSC") %>% 
      mutate(ID_PSC = if_else(is.na(TEMP), ID_PSC, TEMP)) %>% 
      select(-TEMP)

  if (light == FALSE) {
    art44 <<- read_csv2(file.path(PSC, "info", "art44_progetti_20201209.csv"), guess_max = 70000) %>% 
      # ricodifica opos
      separate(COD_OPOS_NEW, into = c("A", "B"), sep = "\\.", remove = TRUE) %>% 
      mutate(COD_OPOS_NEW = paste(A, str_pad(B, 2, "left", "0"), sep = ".")) %>% 
      select(-A, -B) %>% 
      mutate(COD_OPOS_NEW = case_when(COD_OPOS_NEW == "NA" ~ NA_character_,
                                      COD_OPOS_NEW == "NA.NA" ~ NA_character_,
                                      TRUE ~ COD_OPOS_NEW)) %>% 
      # fix per codice programma con "0X"
      mutate(OC_CODICE_PROGRAMMA = case_when(CICLO == "0006" & nchar(OC_CODICE_PROGRAMMA) == 1 ~ str_pad(OC_CODICE_PROGRAMMA, 2, pad = "0"),
                                             TRUE ~ OC_CODICE_PROGRAMMA)) %>% 
      # ricodifica PSC
      mutate(PSC = case_when(PSC == "MIPAF" ~ "MIPAAF",
                             PSC == "MIUR-RICERCA" ~ "MUR",
                             PSC == "MIUR-SCUOLA" ~ "MI",
                             OC_CODICE_PROGRAMMA == "OPCM_CAM" ~ "CAMPANIA",
                             OC_CODICE_PROGRAMMA == "DEBITI_CAM" ~ "CAMPANIA",
                             OC_CODICE_PROGRAMMA == "2016MISEBULFSC1" ~ "MISE",
                             OC_CODICE_PROGRAMMA == "FONDOGARANFSC" ~ "MISE",
                             COD_LOCALE_PROGETTO == "1MISESARCANAS121SV" ~ "CAMPANIA",
                             COD_LOCALE_PROGETTO == "1MISESARCANAS154" ~ "CAMPANIA",
                             COD_LOCALE_PROGETTO == "1MISESARCANAS2" ~ "BASILICATA",
                             COD_LOCALE_PROGETTO == "1MISESARCANAS3.3" ~ "CALABRIA",
                             COD_LOCALE_PROGETTO == "5MTRA1B1R003" ~ "MIT",
                             COD_LOCALE_PROGETTO == "5MTRA1B2R008" ~ "MIT",
                             TRUE ~ PSC)) %>% 
      mutate(ID_PSC = case_when(is.na(PSC) ~ "",
                                PSC == "DEAD" ~ "",
                                PSC == "???" ~ "",
                                PSC == "ACT" ~ "",
                                PSC == "COMM_BONIFICHE" ~ "",
                                PSC == "MIDIFESA" ~ "",
                                TRUE ~ paste0("PSC_", gsub(" ", "_", PSC)))) %>% 
      fix_id_psc_15_digit(.) %>% 
      # scarta progetti non psc
      filter(ID_PSC != "") %>% 
      # clean
      mutate(x_CICLO = X_CICLO) %>% 
      # sovrascrive descrizione tema
      mutate(COD_SETTORE_INTERVENTO = COD_OPOS_NEW) %>% 
      left_join(matrix_temi_settori, 
                by = "COD_SETTORE_INTERVENTO") %>% 
      mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
             SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO)) %>% 
      # variabili coesione
      mutate(COE = case_when(PSC == "MOLISE" ~ OC_FINANZ_COESIONE_NETTO,
                             PSC == "CAMPANIA" ~ OC_FINANZ_COESIONE_NETTO,
                             PSC == "SICILIA" ~ OC_FINANZ_COESIONE_NETTO,
                             PSC == "ABRUZZO" ~ OC_FINANZ_COESIONE_NETTO,
                             TRUE ~ OC_FINANZ_COE_NETTOMIX),
             COE_IMP = OC_IMPEGNI_COESIONE, # CHK: verificare associazione
             COE_PAG = OC_PAG_COESIONE, # CHK: verificare associazione
             # COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO,
             COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_COESIONE_NETTO) %>% 
      # ricodifica po_naz
      left_join(po_naz %>% 
                  select(COD_LOCALE_PROGETTO, 
                         OC_CODICE_PROGRAMMA_NEW = OC_CODICE_PROGRAMMA, 
                         OC_CODICE_PROGRAMMA = OC_CODICE_PROGRAMMA_EVO),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
      mutate(OC_CODICE_PROGRAMMA = case_when(is.na(OC_CODICE_PROGRAMMA_NEW) ~ OC_CODICE_PROGRAMMA,
                                             TRUE ~ OC_CODICE_PROGRAMMA_NEW)) %>% 
      select(COD_LOCALE_PROGETTO, 
             CUP, 
             OC_TITOLO_PROGETTO = TITOLO_PROGETTO,
             OC_CODICE_PROGRAMMA, 
             x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA, 
             OC_STATO_PROCEDURALE, 
             PSC,
             ID_PSC, 
             x_CICLO, 
             COD_TEMA_NEW,
             DESCR_TEMA_NEW, 
             COD_OPOS_NEW, 
             DESCR_OPOS_NEW,
             AREA_TEMATICA,
             COD_AREA_TEMATICA,
             DESCR_AREA_TEMATICA,
             SETTORE_INTERVENTO, 
             COD_SETTORE_INTERVENTO,
             DESCR_SETTORE_INTERVENTO, 
             COE,
             COE_IMP,
             COE_PAG,
             COE_ECO,
             OC_FLAG_VISUALIZZAZIONE)
    
    art44_liste <<- read_csv2(file.path(DRIVE, "ELAB", "20201031", "DB_FSC", "crea_db", "V.01", "temp", 
                                        "lista_progetti_all.csv"))%>% 
      rename(COD_LOCALE_PROGETTO = CODICE_LOCALE_PROGETTO) %>% 
      mutate(AREA_TEMATICA = gsub(" - ", "-", AREA_TEMATICA)) %>%
      # ricodifica po_naz
      left_join(po_naz %>% 
                  select(COD_LOCALE_PROGETTO, 
                         OC_CODICE_PROGRAMMA_NEW = OC_CODICE_PROGRAMMA, 
                         OC_CODICE_PROGRAMMA = OC_CODICE_PROGRAMMA_EVO),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
      mutate(OC_CODICE_PROGRAMMA = case_when(is.na(OC_CODICE_PROGRAMMA_NEW) ~ OC_CODICE_PROGRAMMA,
                                             TRUE ~ OC_CODICE_PROGRAMMA_NEW)) %>% 
      rename(x_MACROAREA = MACROAREA,
             SETTORE_INTERVENTO = SETTORE_INTERVENTO_PSC,
             OC_TITOLO_PROGETTO = TITOLO_PROGETTO,
             x_CICLO = CICLO_PROGRAMMAZIONE,
             x_PROGRAMMA = PROGRAMMA,
             COE = RISORSE) %>% 
      mutate(ID_PSC = paste0("PSC_", gsub(" ", "_", PSC))) %>% 
      fix_id_psc_15_digit(.)
    
    # art44 <<- art44_psc %>% 
    #   semi_join(lista_psc %>% 
    #               filter(TIPOLOGIA_AMMINISTRAZIONE == "REGIONALE"),
    #             by = "ID_PSC") %>% 
    #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_TEMA_NEW, DESCR_TEMA_NEW, 
    #          COD_OPOS_NEW, DESCR_OPOS_NEW, COE) %>% 
    #   bind_rows(art44_liste %>% 
    #               semi_join(lista_psc %>% 
    #                           filter(TIPOLOGIA_AMMINISTRAZIONE != "REGIONALE"),
    #                         by = "ID_PSC") %>% 
    #               select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_TEMA_NEW, DESCR_TEMA_NEW, 
    #                      COD_OPOS_NEW, DESCR_OPOS_NEW, COE))
    # DEV: mancano variabili in art44_liste
    
    interventi_cds <<- read_csv2(file.path(PSC, "info", "lista_interventi_cds.csv")) 
    
    salvaguardia <<- read_xlsx(file.path(PSC, "info", "salvaguardia_dl50.xlsx"))
    
    ogv_conseguita <<- read_xlsx(file.path(PSC, "info", "ogv_conseguita_dl50.xlsx"))
    
  }
}


#' Dati FSC per elaborazioni PSC
#'
#' Crea il file con operazioni FSC compatibile con PSC
#' 
#' @param bimestre Bimestre di riferimento, nella versione specifica per i dati PSC (es. 20211231.01 oppure 20211231), che viene ricondotto automaticamente al bimestre OC
#' @param versione Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param matrix_po_psc Matrice di riconciliazione PO - PSC
#' @param art44 Dati di base per istruttoria art44 al 20200630
#' @param matrix_1420 Matrice di riconciliazione tra temi prioritari FSC 2014-2020 e settori di intervento PSC
#' @param matrix_713 Matrice di riconciliazione tra obiettivi specifici QSN 2007-2013 e settori di intervento PSC
#' @param matrix_temi_settori Dominio per aree tematiche e settori di intervento PSC
#' @param progetti Dataset progetti da load_progetti(visualizzati = FALSE, light = FALSE)
#' @param progetti_pub Dataset progetti esteso pubblicato
#' @param versione_sgp Versione di riferimento dei dati provenienti da SGP
#' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' @param forza_flag Vuoi forzare il flag viasulizzazione a 7 per i duplicati tecnici da migrazione?
#' @return File "dati_psc_BIMESTRE.csv" in TEMP 
#' @note ...
prep_dati_psc_bimestre <- function(bimestre, versione, matrix_po_psc, po_naz, art44, 
                                   matrix_1420, matrix_713, matrix_temi_settori, progetti=NULL, progetti_pub=NULL, versione_sgp, chk_today, forza_flag=FALSE) {
  
  # DEBUG:
  # progetti_pub <- progetti
  
  bimestre_oc <- str_sub(bimestre, 1, 8) # 
  DATA <- file.path(dirname(DATA), bimestre_oc)
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati = FALSE, light = FALSE) 
    # %>% 
    #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_COD_ARTICOLAZ_PROGRAMMA, 
    #          COD_STRUMENTO, COD_PROCED_ATTIVAZIONE, OC_DENOM_PROGRAMMATORE, OC_TITOLO_PROGETTO)
    # MEMO: serve light = FALSE perché light non contiene finanziamento privato (che ora non serve più)
  }
  
  if (is.null(progetti_pub)) {
    progetti_pub <- read_csv2(file.path(DATA, paste0("progetti_esteso_", bimestre_oc, ".csv")), guess_max = 1200000) 
    # %>% 
    #   select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, 
    #          FINANZ_STATO_FSC, OC_FINANZ_STATO_FSC_NETTO,
    #          FINANZ_TOTALE_PUBBLICO, FINANZ_PRIVATO, FINANZ_DA_REPERIRE, ECONOMIE_TOTALI,
    #          OC_FINANZ_TOT_PUB_NETTO)
  }

  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  # rettifica DATA per bimestri diversi
  # OLD:
  # DATA <- paste0(str_sub(DATA, 1, nchar(DATA)-8), bimestre)
  # NEW:
  # DATA <- paste0(str_sub(DATA, 1, nchar(DATA)-8), bimestre_oc)
  # print(DATA)
  
  # ------------------ load dati ------------------ #
  
  # operazioni <- load_operazioni(bimestre) 
  # TODO: qui c'è chiamata interna a DATA che non viene ridefinita, forse perché in cache?
  operazioni <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre_oc, ".csv")), guess_max = 1e+06) %>% 
    # filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    get_simply_non_loc() %>% 
    refactor_ambito() %>% 
    refactor_ciclo()
  
  temp_operazioni <- read_sas(file.path(DATA, "oper_pucok_preesteso.sas7bdat")) %>%
    filter(STATO == 1, oc_cod_fonte == "FSC1420") 
  
  if ("psc_area_tematica" %in% names(temp_operazioni)) {
    operazioni_1420 <- temp_operazioni %>%
      rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
             OC_CODICE_PROGRAMMA = oc_cod_programma,
             COD_SETTORE_STRATEGICO_FSC = fsc_settore_strategico,
             # DESCR_SETTORE_STRATEGICO_FSC = fsc_descr_settore_strategico,
             COD_ASSE_TEMATICO_FSC = fsc_asse_tematico,
             # DESCR_ASSE_TEMATICO_FSC = fsc_descr_asse_tematico,
             COSTO_AMM_FSC = costo_ammesso_FSC) %>% # MEMO: porto dietro per analisi variabili COE
      distinct(COD_LOCALE_PROGETTO,
               OC_CODICE_PROGRAMMA,
               COD_SETTORE_STRATEGICO_FSC, 
               # DESCR_SETTORE_STRATEGICO_FSC,
               COD_ASSE_TEMATICO_FSC, 
               # DESCR_ASSE_TEMATICO_FSC,
               COSTO_AMM_FSC,
               psc_macroarea,  
               psc_descr_macroarea,
               psc_sezione,
               psc_descr_sezione,
               psc_area_tematica,
               psc_descr_area_tematica,
               psc_sett_interv,
               psc_descr_sett_interv) %>% 
      # fix manuali (due aree per un settore)
      mutate(psc_area_tematica = case_when(COD_LOCALE_PROGETTO == "1MISECMFZ18ALB-17" & psc_area_tematica == "08:::05" & psc_sett_interv == "01" ~ "08",
                                           COD_LOCALE_PROGETTO == "1MISECMSM2-ACAM-01bis" & psc_area_tematica == "07:::02" & psc_sett_interv == "01" ~ "07",
                                           COD_LOCALE_PROGETTO == "1MISEABRSE13-29" & psc_area_tematica == "11:::06" & psc_sett_interv == "01" ~ "11",
                                           COD_LOCALE_PROGETTO == "1MISEABRSE011-52" & psc_area_tematica == "11:::06" & psc_sett_interv == "01" ~ "11",
                                           COD_LOCALE_PROGETTO == "1MISEABRSE011-50" & psc_area_tematica == "11:::06" & psc_sett_interv == "01" ~ "11",
                                           COD_LOCALE_PROGETTO == "1MISEABRSE011-49" & psc_area_tematica == "11:::06" & psc_sett_interv == "01" ~ "11",
                                           TRUE ~ psc_area_tematica)) %>% 
      # NEW: fix per temi doppi
      separate_rows(c("psc_area_tematica", "psc_sett_interv"), sep = ":::") %>% 
      # fix per psc monitorati
      mutate(COD_AREA_TEMATICA = psc_area_tematica, 
             COD_SETTORE_INTERVENTO = case_when(is.na(psc_sett_interv) ~ NA_character_,
                                                psc_sett_interv == ""~ NA_character_,
                                                TRUE ~ paste0(psc_area_tematica, ".", psc_sett_interv))) %>% 
      left_join(matrix_temi_settori %>% 
                  select(COD_SETTORE_INTERVENTO, DESCR_AREA_TEMATICA, DESCR_SETTORE_INTERVENTO), 
                by = "COD_SETTORE_INTERVENTO") %>% 
      mutate(COD_SETTORE_STRATEGICO_FSC = case_when(COD_SETTORE_STRATEGICO_FSC == "4.a" ~ "4", # matera
                                                    COD_SETTORE_STRATEGICO_FSC == "4.b" ~ "4", # matera
                                                    TRUE ~ COD_SETTORE_STRATEGICO_FSC),
             COD_ASSE_TEMATICO_FSC = case_when(COD_ASSE_TEMATICO_FSC == "01" ~ "1", # pozzuoli
                                               COD_ASSE_TEMATICO_FSC == "2:::3:::5" ~ "2", # mattm
                                               COD_ASSE_TEMATICO_FSC == "1:::2:::3:" ~ "2", # mattm
                                               COD_ASSE_TEMATICO_FSC == "1:::2" ~ "2", # mise
                                               TRUE ~ COD_ASSE_TEMATICO_FSC)) %>% 
      # NEW: riprende da fix sopra
      group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_SETTORE_STRATEGICO_FSC, COD_ASSE_TEMATICO_FSC,
      psc_macroarea, psc_descr_macroarea, psc_sezione, psc_descr_sezione) %>% 
      summarise(psc_area_tematica = paste0(psc_area_tematica, collapse = ":::"),
                psc_descr_area_tematica = paste0(psc_descr_area_tematica, collapse = ":::"),
                psc_sett_interv = paste0(psc_sett_interv, collapse = ":::"),
                psc_descr_sett_interv = paste0(psc_descr_sett_interv, collapse = ":::"),
                COD_AREA_TEMATICA = paste0(COD_AREA_TEMATICA, collapse = ":::"),
                COD_SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, collapse = ":::"),
                DESCR_AREA_TEMATICA = paste0(DESCR_AREA_TEMATICA, collapse = ":::"),
                DESCR_SETTORE_INTERVENTO = paste0(DESCR_SETTORE_INTERVENTO, collapse = ":::"),
                COSTO_AMM_FSC = unique(COSTO_AMM_FSC, na.rm = TRUE)) %>% 
      ungroup() %>% 
      # fix per casi da programmi diversi da PSC
      mutate(COD_SETTORE_INTERVENTO = case_when(is.na(COD_SETTORE_INTERVENTO) ~ NA_character_,
                                                COD_SETTORE_INTERVENTO == "" ~ NA_character_,
                                                COD_SETTORE_INTERVENTO == "NA" ~ NA_character_,
                                                TRUE ~ COD_SETTORE_INTERVENTO)) %>% 
      mutate(DESCR_SETTORE_INTERVENTO = case_when(is.na(DESCR_SETTORE_INTERVENTO) ~ NA_character_,
                                                  DESCR_SETTORE_INTERVENTO == "" ~ NA_character_,
                                                  DESCR_SETTORE_INTERVENTO == "NA" ~ NA_character_,
                                                  TRUE ~ DESCR_SETTORE_INTERVENTO)) %>% 
      mutate(COD_AREA_TEMATICA = case_when(is.na(COD_AREA_TEMATICA) ~ NA_character_,
                                           COD_AREA_TEMATICA == "" ~ NA_character_,
                                           COD_AREA_TEMATICA == "NA" ~ NA_character_,
                                           TRUE ~ COD_AREA_TEMATICA)) %>% 
      mutate(DESCR_AREA_TEMATICA = case_when(is.na(DESCR_AREA_TEMATICA) ~ NA_character_,
                                             DESCR_AREA_TEMATICA == "" ~ NA_character_,
                                             DESCR_AREA_TEMATICA == "NA" ~ NA_character_,
                                             TRUE ~ DESCR_AREA_TEMATICA))
      
  } else {
    operazioni_1420 <- temp_operazioni %>%
      rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
             OC_CODICE_PROGRAMMA = oc_cod_programma,
             COD_SETTORE_STRATEGICO_FSC = fsc_settore_strategico,
             # DESCR_SETTORE_STRATEGICO_FSC = fsc_descr_settore_strategico,
             COD_ASSE_TEMATICO_FSC = fsc_asse_tematico,
             # DESCR_ASSE_TEMATICO_FSC = fsc_descr_asse_tematico,
             COSTO_AMM_FSC = costo_ammesso_FSC) %>% # MEMO: porto dietro per analisi variabili COE
      distinct(COD_LOCALE_PROGETTO,
               OC_CODICE_PROGRAMMA,
               COD_SETTORE_STRATEGICO_FSC, 
               # DESCR_SETTORE_STRATEGICO_FSC,
               COD_ASSE_TEMATICO_FSC, 
               # DESCR_ASSE_TEMATICO_FSC,
               COSTO_AMM_FSC) %>%
      # fix 
      mutate(COD_SETTORE_STRATEGICO_FSC = case_when(COD_SETTORE_STRATEGICO_FSC == "4.a" ~ "4", # matera
                                                    COD_SETTORE_STRATEGICO_FSC == "4.b" ~ "4", # matera
                                                    TRUE ~ COD_SETTORE_STRATEGICO_FSC),
             COD_ASSE_TEMATICO_FSC = case_when(COD_ASSE_TEMATICO_FSC == "01" ~ "1", # pozzuoli
                                               COD_ASSE_TEMATICO_FSC == "2:::3:::5" ~ "2", # mattm
                                               COD_ASSE_TEMATICO_FSC == "1:::2:::3:" ~ "2", # mattm
                                               COD_ASSE_TEMATICO_FSC == "1:::2" ~ "2", # mise
                                               TRUE ~ COD_ASSE_TEMATICO_FSC))
  }

  # chk <- operazioni_1420 %>% 
  #   filter(OC_CODICE_PROGRAMMA == "PSCFRIULI")
  
  # operazioni_1420 %>% 
  #   filter(grepl(":::", psc_sett_interv)) %>% 
  #   count(OC_CODICE_PROGRAMMA, psc_sezione, psc_area_tematica, psc_sett_interv, COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO)
  
  temp <- read_sas(file.path(DATA, "oper_fltok_preesteso.sas7bdat")) 
  
  # risolve conflitti di naming (che ancora cambiano ogni volta...)
  if ("OC_COD_PROGRAMMA" %in% names(temp)){
    operazioni_713 <- temp %>% 
      select(COD_LOCALE_PROGETTO = cod_locale_progetto,
             QSN_CODICE_OBIETTIVO_SPECIFICO = qsn_codice_obiettivo_specifico, 
             OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA)
  } else if ("OC_CODICE_PROGRAMMA" %in% names(temp)) {
    operazioni_713 <- temp %>% 
      select(COD_LOCALE_PROGETTO = cod_locale_progetto,
             QSN_CODICE_OBIETTIVO_SPECIFICO = qsn_codice_obiettivo_specifico, 
             OC_CODICE_PROGRAMMA)
  } else if ("oc_cod_programma" %in% names(temp)) {
    operazioni_713 <- temp %>% 
      select(COD_LOCALE_PROGETTO = cod_locale_progetto,
             QSN_CODICE_OBIETTIVO_SPECIFICO = qsn_codice_obiettivo_specifico, 
             OC_CODICE_PROGRAMMA = oc_cod_programma)
  }
  
  # ----------------------- chk ---------------------- #
  
  # operazioni_713 %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(QSN_CODICE_OBIETTIVO_SPECIFICO) # 6.1.3 mappato male in matrix
  # appo %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(COD_SETTORE_INTERVENTO) # NA (fino a 279)
  # appo %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(COD_SETTORE_INTERVENTO) # 05.05 (tutto)
  # art44 %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(COD_SETTORE_INTERVENTO) # 07.02
  # art44 %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(COD_OPOS_NEW) # 07.02
  # appo %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(OC_CODICE_PROGRAMMA) # 2007IT001FA005
  # art44 %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(OC_CODICE_PROGRAMMA) # CIS_NABA_CAM
  # art44_liste %>% filter(COD_LOCALE_PROGETTO == "1MISE770") %>% select(OC_CODICE_PROGRAMMA) # CIS_NABA_CAM
  
  
  # ------------------ elaborazione ------------------ #
  
  # chk <- appo %>% filter(COD_LOCALE_PROGETTO == "9CA20015CP000000095")
  
  
  appo <- operazioni %>% 
    # filter(COD_LOCALE_PROGETTO == "9CA20015CP000000095") %>% #DEBUG
    # filter(x_AMBITO == "FSC") %>% 
    # inner_join(matrix_po_psc, by = "OC_CODICE_PROGRAMMA") %>% 
    # # ricodifica piani nazionali (direttrici ferroviarie-2007IT001FA005 e salerno reggio calabria-2007AN0021FA01)
    # left_join(po_naz,
    #           by = "COD_LOCALE_PROGETTO") %>% 
    # mutate(ID_PSC = if_else(is.na(ID_PSC.y), ID_PSC.x, ID_PSC.y),
    #        PSC = if_else(is.na(PSC.y), PSC.x, PSC.y)) %>% 
    # select(-ID_PSC.y, -ID_PSC.x, -PSC.x, -PSC.y) %>% 
    filter(x_AMBITO == "FSC") %>% 
    # ricodifica direttrici ferroviarie e salerno-reggio calabria
    left_join(po_naz %>% 
              select(-OC_CODICE_PROGRAMMA_EVO),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    # codice psc
    left_join(matrix_po_psc %>% 
                distinct(OC_CODICE_PROGRAMMA, ID_PSC), 
              by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(ID_PSC = if_else(is.na(ID_PSC.y), ID_PSC.x, ID_PSC.y)) %>% # MEMO: x viene da po_naz, y da matrix genrale
    select(-ID_PSC.y, -ID_PSC.x) %>% 
    # label psc
    left_join(matrix_po_psc %>% 
                distinct(ID_PSC, PSC), 
              by = "ID_PSC") %>% 
    filter(!is.na(ID_PSC)) %>% 
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FINANZ_TOT_PUB_NETTO),
    #           by = "COD_LOCALE_PROGETTO") %>% 
    left_join(art44 %>% 
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_TEMA_NEW, DESCR_TEMA_NEW, 
                       COD_SETTORE_INTERVENTO = COD_OPOS_NEW, DESCR_OPOS_NEW),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    # mutate(#COD_AREA_TEMATICA = COD_TEMA_NEW,
    #   #DESCR_AREA_TEMATICA = DESCR_TEMA_NEW,
    #   COD_SETTORE_INTERVENTO = COD_OPOS_NEW,
    #   # DESCR_SETTORE_INTERVENTO = DESCR_OPOS_NEW
    # ) %>% 
    # rettifica temi da primo cds
    left_join(interventi_cds,
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.y), COD_SETTORE_INTERVENTO.x, COD_SETTORE_INTERVENTO.y)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    # OLD
    # # integra temi mancanti per 1420
    # left_join(operazioni_1420 %>% 
    #             select(-COD_SETTORE_INTERVENTO), # MEMO: questo va tolto qui perché il fix viene fatto dopo
    #           by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    # NEW: sovrascrive dati da BDU
    left_join(operazioni_1420, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>%
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.y), COD_SETTORE_INTERVENTO.x, COD_SETTORE_INTERVENTO.y)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    left_join(matrix_1420 %>% 
                select(COD_SETTORE_STRATEGICO_FSC, COD_ASSE_TEMATICO_FSC, COD_SETTORE_INTERVENTO),
              by = c("COD_SETTORE_STRATEGICO_FSC", "COD_ASSE_TEMATICO_FSC")) %>% 
    # integra per casi residui
    # TODO: qui va fix per ferrovie vs strade
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.x), COD_SETTORE_INTERVENTO.y, COD_SETTORE_INTERVENTO.x)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    # integra temi mancanti per 713
    left_join(operazioni_713,
              by = c("COD_LOCALE_PROGETTO" , "OC_CODICE_PROGRAMMA")) %>% 
    left_join(matrix_713 %>% 
                select(QSN_CODICE_OBIETTIVO_SPECIFICO, COD_SETTORE_INTERVENTO),
              by = "QSN_CODICE_OBIETTIVO_SPECIFICO") %>% 
    # TODO: qui va fix per ferrovie vs strade
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.x), COD_SETTORE_INTERVENTO.y, COD_SETTORE_INTERVENTO.x)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y)
  
  
  # sovrascrive temi per psc già monitorati e scarta sezioni speciali
  if ("psc_area_tematica" %in% names(temp_operazioni)) {
    # OLD:
    # appo1 <- appo %>% 
    #   left_join(operazioni_1420 %>% 
    #               select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO),
    #             by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    #   mutate(COD_SETTORE_INTERVENTO = case_when(COD_AREA_TEMATICA.y == "" ~ COD_SETTORE_INTERVENTO.x, 
    #                                             is.na(COD_AREA_TEMATICA.y) ~ COD_SETTORE_INTERVENTO.x, 
    #                                             TRUE ~ COD_SETTORE_INTERVENTO.y)) %>% 
    #   # CHK: chk <- appo1 %>% select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_SETTORE_INTERVENTO, COD_SETTORE_INTERVENTO.x, COD_SETTORE_INTERVENTO.y, COD_AREA_TEMATICA.x, COD_AREA_TEMATICA.y)
    #   # CHK: chk2 <- chk %>% count( COD_SETTORE_INTERVENTO, COD_SETTORE_INTERVENTO.x, COD_SETTORE_INTERVENTO.y, COD_AREA_TEMATICA.x, COD_AREA_TEMATICA.y)
    #   select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y)  %>% 
    #   # sovrascrive tema per tutti
    #   left_join(matrix_temi_settori, 
    #             by = "COD_SETTORE_INTERVENTO") %>% 
    #   mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
    #          SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO)) %>% 
    #   # scarta sezioni speciali
    #   # filter(psc_sezione != "SS_1" & psc_sezione != "SS_2" | is.na(psc_sezione))
    #   mutate(SEZIONE = psc_sezione)
    # NEW:
    appo1 <- appo %>% 
      # fix
      mutate(DESCR_AREA_TEMATICA = case_when(COD_AREA_TEMATICA == "07:::07" ~ "TRASPORTI E MOBILITÀ",
                                             COD_AREA_TEMATICA == "06:::06" ~ "CULTURA",
                                             COD_AREA_TEMATICA == "05:::10" ~ "AMBIENTE E RISORSE NATURALI",
                                             COD_AREA_TEMATICA == "06:::12" ~ "CULTURA",
                                             COD_AREA_TEMATICA == "12:::06" ~ "CULTURA",
                                             TRUE ~ DESCR_AREA_TEMATICA)) %>% 
      mutate(COD_AREA_TEMATICA = case_when(COD_AREA_TEMATICA == "07:::07" ~ "07",
                                           COD_AREA_TEMATICA == "06:::06" ~ "06",
                                           COD_AREA_TEMATICA == "05:::10" ~ "05",
                                           COD_AREA_TEMATICA == "06:::12" ~ "06",
                                           COD_AREA_TEMATICA == "12:::06" ~ "06",
                                           TRUE ~ COD_AREA_TEMATICA)) %>% 
      
      # fix
      mutate(DESCR_SETTORE_INTERVENTO = case_when(COD_SETTORE_INTERVENTO == "07.01:::07.02" ~ "TRASPORTO FERROVIARIO",
                                                  COD_SETTORE_INTERVENTO == "05.02:::10.03" ~ "RISORSE IDRICHE",
                                                  COD_SETTORE_INTERVENTO == "06.01:::06.02" ~ "PATRIMONIO E PAESAGGIO",
                                                  COD_SETTORE_INTERVENTO == "06.01:::12.02" ~ "PATRIMONIO E PAESAGGIO",
                                                  COD_SETTORE_INTERVENTO == "12.02:::06.01" ~ "PATRIMONIO E PAESAGGIO",
                                                  TRUE ~ DESCR_SETTORE_INTERVENTO)) %>% 
      mutate(COD_SETTORE_INTERVENTO = case_when(COD_SETTORE_INTERVENTO == "07.01:::07.02" ~ "07.02",
                                                COD_SETTORE_INTERVENTO == "05.02:::10.03" ~ "05.02",
                                                COD_SETTORE_INTERVENTO == "06.01:::06.02" ~ "06.01",
                                                COD_SETTORE_INTERVENTO == "06.01:::12.02" ~ "06.01",
                                                COD_SETTORE_INTERVENTO == "12.02:::06.01" ~ "06.01",
                                                TRUE ~ COD_SETTORE_INTERVENTO)) %>% 
      

    
      
      # sovrascrive tema per tutti
      left_join(matrix_temi_settori %>% 
                  select(COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO, DESCR_AREA_TEMATICA, DESCR_SETTORE_INTERVENTO), 
                by = "COD_SETTORE_INTERVENTO") %>% 
      mutate(COD_AREA_TEMATICA = if_else(is.na(COD_AREA_TEMATICA.x), COD_AREA_TEMATICA.y, COD_AREA_TEMATICA.x)) %>% 
      select(-COD_AREA_TEMATICA.x, -COD_AREA_TEMATICA.y) %>%
      mutate(DESCR_AREA_TEMATICA = if_else(is.na(DESCR_AREA_TEMATICA.x), DESCR_AREA_TEMATICA.y, DESCR_AREA_TEMATICA.x)) %>% 
      select(-DESCR_AREA_TEMATICA.x, -DESCR_AREA_TEMATICA.y) %>% 
      mutate(DESCR_SETTORE_INTERVENTO = if_else(is.na(DESCR_SETTORE_INTERVENTO.x), DESCR_SETTORE_INTERVENTO.y, DESCR_SETTORE_INTERVENTO.x)) %>% 
      select(-DESCR_SETTORE_INTERVENTO.x, -DESCR_SETTORE_INTERVENTO.y)%>% 
      mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
             SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO)) %>% 
      # scarta sezioni speciali
      # filter(psc_sezione != "SS_1" & psc_sezione != "SS_2" | is.na(psc_sezione))
      mutate(SEZIONE = psc_sezione)

  } else {
    # MEMO: questo vale per bimestri prima di giugno 2022
    appo1 <- appo %>% 
      # sovrascrive tema per tutti
      left_join(matrix_temi_settori, 
                by = "COD_SETTORE_INTERVENTO") %>% 
      mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
             SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO))  
  }
  
  # integra temi mancanti per 1420
  
  # chk totale coe
  appo1 %>% 
    filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    summarise(COE = sum(COE, na.rm = TRUE))
  
  appo %>% 
    filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    filter(psc_sezione != "SS_1" & psc_sezione != "SS_2" | is.na(psc_sezione)) %>% 
    summarise(COE = sum(COE, na.rm = TRUE))
  
  
  # chk classi
  # chk <- appo1 %>% count(AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(grepl(":::", SETTORE_INTERVENTO))
  # chk2 <- appo1 %>% filter(SETTORE_INTERVENTO == "07.01:::07.02-TRASPORTO STRADALE:::TRASPORTO FERROVIARIO")
  # chk3 <- appo1 %>% count(COD_SETTORE_INTERVENTO, DESCR_SETTORE_INTERVENTO)
  
  # fix classi
  appo2 <- appo1 %>% 
    mutate(AREA_TEMATICA = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & AREA_TEMATICA == "NA-NA" & SETTORE_INTERVENTO == "09.02-NA" ~ "08-RIQUALIFICAZIONE URBANA",
                                     OC_CODICE_PROGRAMMA == "2017FSCRICERCA" & AREA_TEMATICA == "NA-NA" ~ "01-RICERCA E INNOVAZIONE",
                                     OC_CODICE_PROGRAMMA == "PSCTURISMO" & AREA_TEMATICA == "NA-NA" ~ "03-COMPETITIVITÀ IMPRESE",
                                     OC_CODICE_PROGRAMMA == "2016PATTISICI" & SETTORE_INTERVENTO == "NA-NA" &
                                       grepl("05.1", COD_RISULTATO_ATTESO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                     TRUE ~ AREA_TEMATICA)) %>% 
    mutate(SETTORE_INTERVENTO = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & SETTORE_INTERVENTO == "09.02-NA" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                          OC_CODICE_PROGRAMMA == "2017FSCRICERCA" & SETTORE_INTERVENTO == "NA-NA" ~ "01.01-RICERCA E SVILUPPO",
                                          OC_CODICE_PROGRAMMA == "PSCTURISMO" & SETTORE_INTERVENTO == "03:::12.02-NA" ~ "03.02-TURISMO E OSPITALITÀ",
                                          OC_CODICE_PROGRAMMA == "2016PATTISICI" & SETTORE_INTERVENTO == "NA-NA" &
                                            grepl("05.1", COD_RISULTATO_ATTESO) ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                          TRUE ~ SETTORE_INTERVENTO))

  # debug classi
  print(appo2 %>% count(AREA_TEMATICA))
  message("Se ci sono casi con ::: vanno corretti nel codice... Sorry!")
  chk <- appo2 %>% filter(AREA_TEMATICA == "NA-NA")
  write_csv2(chk, file.path(TEMP, paste0("chk_classi_missing_", bimestre_oc, ".csv")))
  
  message("Questi sono gli strumenti attuativi non censiti in matrix:")
  chk2 <- chk %>% 
    mutate(CHK = case_when(COE > 0 ~ "to_fix",
                           COE <= 0 ~ "zero",
                           is.na(COE) ~ "na")) %>% 
    count(x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, SETTORE_INTERVENTO, CHK)
  print(chk2)
  write_csv2(chk2, file.path(TEMP, paste0("fix_mapping_", bimestre_oc, ".csv")))
  message("Se ci sono NA-NA con COE NA in 'zero' va bene, restano così.")
  message("Per tutti gli altri casi con 'to_fix' va rivisto lo script, che incorpora direttamente le correzioni.")
  message("Controlla in TEMP il file 'chk_classi_missing.csv'")

  chk3 <- chk %>% filter(!is.na(COE))
  # # fix classi # DEV: spostato sopra
  # appo <- appo %>% 
  #   mutate(AREA_TEMATICA = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & AREA_TEMATICA == "NA-NA" & SETTORE_INTERVENTO == "09.02-NA" ~ "08-RIQUALIFICAZIONE URBANA",
  #                                    OC_CODICE_PROGRAMMA == "2017FSCRICERCA" & AREA_TEMATICA == "NA-NA" ~ "01-RICERCA E INNOVAZIONE",
  #                                    TRUE ~ AREA_TEMATICA)) %>% 
  #   mutate(SETTORE_INTERVENTO = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & SETTORE_INTERVENTO == "09.02-NA" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
  #                                         OC_CODICE_PROGRAMMA == "2017FSCRICERCA" & SETTORE_INTERVENTO == "NA-NA" ~ "01.01-RICERCA E SVILUPPO",
  #                                    TRUE ~ SETTORE_INTERVENTO))
  # message("Il caso Puglia con 09.02-NA è già gestito. Il caso di Ricerca è temporaneo.")
  
  # calcola costo realizzato e economie
  appo3 <- appo2 %>% 
    # filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    # OLD:
    # left_join(progetti %>% 
    #             select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, FINANZ_STATO_FSC, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_PRIVATO_NETTO),
    #           by = "COD_LOCALE_PROGETTO") %>% 
    # mutate_if(is.numeric, replace_na, replace = 0) %>% 
    # mutate(FINANZ_TOT = OC_FINANZ_TOT_PUB_NETTO + OC_FINANZ_PRIVATO_NETTO,
    #        # x = OC_FINANZ_STATO_FSC_NETTO/FINANZ_TOT,
    #        x = COE/FINANZ_TOT, # deve rimanere COE al numeratore perché ci sono progetti fatti da due oeprazione FSC
    #        COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x,
    #        COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO) %>%
    # NEW:
    left_join(progetti_pub %>% 
                select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, 
                       FINANZ_STATO_FSC, OC_FINANZ_STATO_FSC_NETTO,
                       FINANZ_TOTALE_PUBBLICO, FINANZ_PRIVATO, FINANZ_DA_REPERIRE, ECONOMIE_TOTALI,
                       OC_FINANZ_TOT_PUB_NETTO, DB) %>% 
                mutate(across(where(is.numeric), ~replace_na(., replace = 0))),
              by = "COD_LOCALE_PROGETTO") %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(FINANZ_TOT = FINANZ_TOTALE_PUBBLICO + FINANZ_PRIVATO + FINANZ_DA_REPERIRE - ECONOMIE_TOTALI,
           # x = OC_FINANZ_STATO_FSC_NETTO/FINANZ_TOT,
           x = COE/FINANZ_TOT, # deve rimanere COE al numeratore perché ci sono progetti fatti da due oeprazione FSC
           COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x,
           COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO,
           CP = OC_FINANZ_TOT_PUB_NETTO,
           # CPP = FINANZ_TOTALE_PUBBLICO, FINANZ_PRIVATO)
           CPP = FINANZ_TOTALE_PUBBLICO + FINANZ_PRIVATO)
  
  
  # chk
  # dim(appo)[1] == dim(appo3)[1] # MEMO: questo è falso da quando ho tolto sezioni speciali
  dim(appo1)[1] == dim(appo3)[1]
  
  
  # chk finanziamenti vs costi
  chk <- appo3 %>% 
    group_by(COD_LOCALE_PROGETTO, x_CICLO, x_PROGRAMMA) %>% 
    summarise(COE = sum(COE, na.rm = TRUE),
              FINANZ_STATO_FSC = sum(FINANZ_STATO_FSC, na.rm = TRUE),
              OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
              COSTO_AMM_FSC = sum(COSTO_AMM_FSC, na.rm = TRUE)) %>% 
    mutate(CHK = case_when(# x_CICLO == "2014-2020" & abs(COE - COSTO_AMM_FSC) < 0.1 ~ "cost", # possibile solo su 1420
                           abs(COE - COSTO_AMM_FSC) < 0.1 ~ "cost", # MEMO: post migrazione 
                           abs(COE - OC_FINANZ_STATO_FSC_NETTO) < 0.1 ~ "fin_net",
                           abs(COE- FINANZ_STATO_FSC) < 0.1 ~ "fin",
                           TRUE ~ "chk")) 
  chk %>% 
    # filter(x_CICLO == "2014-2020") %>%
    # filter(x_CICLO == "2007-2013") %>%
    group_by(CHK, x_CICLO) %>% 
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              FINANZ_STATO_FSC = sum(FINANZ_STATO_FSC, na.rm = TRUE),
              OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
              COSTO_AMM_FSC = sum(COSTO_AMM_FSC, na.rm = TRUE))
  
  chk %>% 
    ungroup() %>% 
    filter(x_CICLO == "2014-2020") %>%
    # filter(x_CICLO == "2007-2013") %>%
    filter(CHK == "fin_net") %>% 
    group_by(x_CICLO, x_PROGRAMMA) %>% 
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              FINANZ_STATO_FSC = sum(FINANZ_STATO_FSC, na.rm = TRUE),
              OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
              COSTO_AMM_FSC = sum(COSTO_AMM_FSC, na.rm = TRUE))
  
  # chk economie
  appo3 %>% 
    mutate(FINANZ_STATO_FSC = if_else(is.na(FINANZ_STATO_FSC), 0, FINANZ_STATO_FSC),
           OC_FINANZ_STATO_FSC_NETTO = if_else(is.na(OC_FINANZ_STATO_FSC_NETTO), 0, OC_FINANZ_STATO_FSC_NETTO)) %>% 
    mutate(ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO) %>% 
    filter(ECO != 0) %>%
    mutate(OC_STATO_PROGETTO = case_when(OC_STATO_PROGETTO == "Concluso" ~ "Concluso",
                                         OC_STATO_PROGETTO == "Liquidato" ~ "Concluso",
                                         TRUE ~ "In corso")) %>% 
    group_by(OC_STATO_PROGETTO) %>% 
    summarise(N = n(),
              FSC = sum(FINANZ_STATO_FSC, na.rm = TRUE),
              FSC_NET = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE)) %>% 
    mutate(ECO = FSC - FSC_NET)
  # OC_STATO_PROGETTO     N         FSC     FSC_NET        ECO
  # <chr>             <int>       <dbl>       <dbl>      <dbl>
  # 1 Concluso           5269 1739358008. 1562285368. 177072641.
  # 2 In corso           2437 2595703054. 2212930337. 382772717.
  
  appo3 %>% 
    mutate(ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO,
           DELTA = OC_FINANZ_STATO_FSC_NETTO - COSTO_AMM_FSC) %>% 
    filter(DELTA != 0) %>%
    summarise(N = n(),
              DELTA = sum(DELTA, na.rm = TRUE),
              ECO = sum(ECO, na.rm = TRUE))
  
  # appo1 %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO, na.rm = TRUE),
  #             OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
  #             OC_FINANZ_TOT_PUB_NETTO = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
  #             COSTO_REALIZZATO_2 = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
  # A tibble: 1 x 6
  # COE      COE_PAG COSTO_REALIZZATO OC_FINANZ_STATO_FSC_NETTO OC_FINANZ_TOT_PUB_NETTO COSTO_REALIZZATO_2
  # <dbl>        <dbl>            <dbl>                     <dbl>                   <dbl>              <dbl>
  # 49290164401. 10040154286.     21986749485.              50629969321.            68164313459.       15828065459.
  
  # report per programma
  # chk <- appo1 %>%
  #   group_by(ID_PSC, PSC, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
  
  # stato progetto per economie
  appo4 <- appo3 %>%
    rename(COE_CR = COSTO_REALIZZATO_2) %>% 
    mutate(STATO = case_when(COE_PAG / COE > 0.95 ~ "Conclusi",
                             COE_CR / COE > 0.95 ~ "Conclusi",
                             OC_STATO_PROCEDURALE == "Eseguito" ~ "Conclusi",
                             TRUE ~ "In corso"))
  
  # NEW: sovrascrive COE per eliminare max(costo, fin), tiene costo
  # appo5 <- appo4 %>% 
  #   mutate(COE = case_when(OC_CODICE_PROGRAMMA == "2016PATTISICI" & COE>COSTO_AMM_FSC ~ COSTO_AMM_FSC,
  #                          TRUE ~ COE))
  appo5 <- appo4 # MEMO: risolta nei dati da fabio
  
  chk <- appo3 %>% 
    filter(OC_CODICE_PROGRAMMA == "2016PATTISICI" & COE>COSTO_AMM_FSC) %>% 
    select(COD_LOCALE_PROGETTO, COE, costo_ammesso_MZ, COE_SUD)
  
  chk2 <- appo5 %>% 
    semi_join(chk, by = "COD_LOCALE_PROGETTO") %>% 
    select(COD_LOCALE_PROGETTO, COE, costo_ammesso_MZ, COE_SUD)
  
  # DEV: allineare numerazione
  appo2 <- appo5
  
  # fix CIS Taranto in PRA Puglia
  appo3 <- fix_ciclo_cis_taranto_pra_puglia(progetti_psc = appo2)
  
  # fix visualizzati PRA Campania
  appo4 <- fix_visualizzati_pra_campania(progetti_psc = appo3)
  
  # fix assegnazioni di legge  PRA Campania
  appo4bis <- fix_assegnazioni_legge_pra_campania(progetti_psc = appo4, progetti = progetti)
  
  # fix visualizzati APQ MARI Marche
  appo5 <- fix_visualizzati_apq_mari_marche(progetti_psc = appo4bis)

  # sposta progetti su PSC Turismo
  if ("psc_area_tematica" %in% names(temp_operazioni)) {
    # MEMO: ora i dati di attuatione e programmazione sono scorporati
    appo6 <- appo5
  } else {
    appo6 <- fix_sposta_psc_turismo(progetti_psc = appo5, progetti = progetti)
  }

  
  # fix temi amministrazioni centrali
  appo7 <- fix_aree_settori_amm_centrali(progetti_psc = appo6, progetti = progetti, interventi_cds = interventi_cds)
  # MEMO: solo mims tra le amm. centrali ha risposto alla rilevazione sulle classificazioni post cds 

  # fix macroarea
  appo8 <- fix_macroarea_progetti_psc(progetti_psc = appo7)
  
  # fix stato procedurale
  appo_stato <- progetti %>% # OLD: progetti_pub %>% 
    # semi_join(appo, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% # DEV: qui confrontavo progetti e operazioni
    semi_join(appo8, by = "COD_LOCALE_PROGETTO") %>%
    get_stato_attuazione(., chk_today = chk_today) %>% 
    select(COD_LOCALE_PROGETTO, OC_STATO_PROCEDURALE_NEW = STATO_PROCED)
  message("Se ci sono 12 waring su clean_data va bene perché è il numero delle variabili e il warning indica date NA in input")

  # DEBUG: 
  # appo_stato <- out %>% 
  #   select(COD_LOCALE_PROGETTO, OC_STATO_PROCEDURALE = STATO_PROCED) 

  appo9 <- appo8 %>% 
    rename(OC_STATO_PROCEDURALE_OLD = OC_STATO_PROCEDURALE) %>% 
    left_join(appo_stato, by = "COD_LOCALE_PROGETTO") %>% 
    # NEW da fabio
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, OC_STATO_PROCEDURALE_OGV),
              by = "COD_LOCALE_PROGETTO")

  # chk <- appo9 %>% count(OC_STATO_PROCEDURALE, OC_STATO_PROCEDURALE_OLD)
  
  # switch per stato procedurale
  # appo10 <- appo9 %>% 
  #   mutate(OC_STATO_PROCEDURALE_NEW = OC_STATO_PROCEDURALE,
  #          OC_STATO_PROCEDURALE = OC_STATO_PROCEDURALE_OGV)
  # MEMO: ora è gestito nei repor, con default su new
  appo10 <- appo9
  
  # accoda 06
  progetti_sgp <- read_csv2(file.path(PSC, "sgp", paste0("dati_sgp_", bimestre, "_", versione_sgp, ".csv")), col_types = "ccccccccccdddddddcccd") %>% 
    mutate(OC_STATO_PROCEDURALE_OGV = OC_STATO_PROCEDURALE,
           OC_STATO_PROCEDURALE_OLD = OC_STATO_PROCEDURALE,
           OC_STATO_PROCEDURALE_NEW = OC_STATO_PROCEDURALE) # MEMO: crea colonne per allineamento (il valore è sempre equivaente a NEW)

  # integra aggiudicazioni
  appo11 <- appo10 %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, IMPORTO_AGGIUDICATO, 
                IMPORTO_AGGIUDICATO_NODATA, IMPORTO_AGGIUDICATO_BANDITO))
  
  # fix sezione
  appo12 <- appo11 %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SO" ~ "SO",
                               SEZIONE == "SO:::" ~ "SO",
                               SEZIONE == "SO:::SOCIS_RC" ~ "SO",
                               SEZIONE == "SOCIS" ~ "SO",
                               SEZIONE == "SOCIS_CO" ~ "SO",
                               SEZIONE == "SOCIS_NA" ~ "SO",
                               SEZIONE == "SOCIS_PAL" ~ "SO",
                               SEZIONE == "SOCIS_RC" ~ "SO",
                               SEZIONE == "SOCIS_SA" ~ "SO",
                               SEZIONE == "SOCIS_VENT" ~ "SO",
                               SEZIONE == "SOCISTA" ~ "SO",
                               SEZIONE == "SS_1" ~ "SS_1",
                               SEZIONE == "SS_2" ~ "SS_2",
                               SEZIONE == "SS_2:" ~ "SS_2",
                               is.na(SEZIONE) ~ "SO",
                               SEZIONE == "" ~ "SO",
                               TRUE ~ "CHK"))

  appo12 %>% count(SEZIONE)

  if ("psc_area_tematica" %in% names(temp_operazioni)) {
    out <- appo12 %>%
      mutate(OC_STATO_PROCEDURALE = OC_STATO_PROCEDURALE_NEW) %>% # MEMO: valore di default, serve per report
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
             OC_STATO_PROCEDURALE_OGV, # nuova versione fabio
             OC_STATO_PROCEDURALE_OLD, # vecchia versione fabio
             OC_STATO_PROCEDURALE_NEW, # mia versione
             COE, 
             COE_IMP,
             COE_CR,
             COE_PAG,
             COE_ECO,
             CP,
             CPP,
             PSC,
             ID_PSC,
             STATO,
             OC_FLAG_VISUALIZZAZIONE,
             IMPORTO_AGGIUDICATO, 
             IMPORTO_AGGIUDICATO_NODATA, 
             IMPORTO_AGGIUDICATO_BANDITO,
             DB) %>% 
      bind_rows(progetti_sgp)
  } else {
    out <- appo12 %>%
      mutate(OC_STATO_PROCEDURALE = OC_STATO_PROCEDURALE_NEW) %>% # MEMO: valore di default, serve per report
      select(COD_LOCALE_PROGETTO, 
             CUP, 
             OC_TITOLO_PROGETTO,
             x_CICLO,
             x_PROGRAMMA,
             OC_CODICE_PROGRAMMA,
             x_MACROAREA,
             AREA_TEMATICA, 
             SETTORE_INTERVENTO,
             OC_STATO_PROCEDURALE,
             OC_STATO_PROCEDURALE_OGV, # nuova versione fabio
             OC_STATO_PROCEDURALE_OLD, # vecchia versione fabio
             OC_STATO_PROCEDURALE_NEW, # mia versione
             COE, 
             COE_IMP,
             COE_CR,
             COE_PAG,
             COE_ECO,
             CP,
             PSC,
             ID_PSC,
             STATO,
             OC_FLAG_VISUALIZZAZIONE,
             DB) %>% 
      bind_rows(progetti_sgp)
  }
  
  # fix psc
  out <- fix_id_psc_15_digit(out, var1="ID_PSC")
  out <- fix_id_psc_ministeri(out, var1="PSC")
  
  # TODO:
  # fix sezione
  # out <- out %>% 
  #   mutate(SEZIONE = case_when(
  #                    SEZIONE == "SO" ~ "SO",
  #                    SEZIONE == "SOCIS" ~ "SO",
  #                    SEZIONE == "SS_1" ~ "SS_1",
  #                    SEZIONE == "SS_2" ~ "SS_2",
  #                    is.na(SEZIONE) & x_CICLO == "2000-2006" ~ "SO",
  #                    is.na(SEZIONE) & x_CICLO == "2007-2013" ~ "SO",
  #                    is.na(SEZIONE) & x_CICLO == "2014-2020" ~ "chk", # CHK: forzo sezione ordinaria?
  #                    TRUE ~ "chk"))
  
  # DEBUG:
  out %>% filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% filter(x_CICLO != "2000-2006") %>% summarise(COE = sum(COE, na.rm = T))
  appo3 %>% filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% filter(x_CICLO != "2000-2006") %>% summarise(COE = sum(COE, na.rm = T))
  appo4 %>% filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% filter(x_CICLO != "2000-2006") %>% summarise(COE = sum(COE, na.rm = T))
  # MEMO: con fix_visualizzati_pra_campania ripristino ogv in psc (perdo allineamento ai conteggi sopra!)
  # appo3 %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2007CA001FA009", OC_FLAG_VISUALIZZAZIONE == 4, COD_LOCALE_PROGETTO != "1MISETPL.EAV01") %>% 
  #   summarise(COE = sum(COE))
  out %>% filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% summarise(COE = sum(COE, na.rm = T))
  
  # forza flag 7 su duplicati tecnici da migrazione
  if (forza_flag == TRUE) {
    out <- out %>%  
      mutate(OC_FLAG_VISUALIZZAZIONE = case_when(OC_CODICE_PROGRAMMA == "2016PATTIABR" ~ 7, # MEMO: patti quasi interamente migrati
                                                 OC_CODICE_PROGRAMMA == "2016PATTIBASIL" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2018POFSCEMROM" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTILAZ" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2018POFSCBO" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTIFIR" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTIMES" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTINAP" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTIRC" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2016PATTIVEN" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "POSPORTFSC" ~ 7,
                                                 # programmi 713
                                                 OC_CODICE_PROGRAMMA == "2007LO002FA006" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007FR002FA003" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007MA002FA007" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007LI002FA005" ~ 7, #solo COE = 0
                                                 OC_CODICE_PROGRAMMA == "2007BO002FA009" ~ 7, #solo COE = 0
                                                 # altri
                                                 OC_CODICE_PROGRAMMA == "2007LO002FA006" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007FR002FA003" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007MA002FA007" ~ 7,
                                                 OC_CODICE_PROGRAMMA == "2007LI002FA005" ~ 7, #solo COE = 0
                                                 # NEW da confronto con liste OGV
                                                 ID_PSC == "PSC_VALLE_D_AOS" & OC_CODICE_PROGRAMMA == "2007VA002FA014" & OC_FLAG_VISUALIZZAZIONE == 1 ~ 7,
                                                 ID_PSC == "PSC_UMBRIA" & OC_CODICE_PROGRAMMA == "2007UM002FA013" & OC_FLAG_VISUALIZZAZIONE == 1 ~ 7,
                                                 # ID_PSC == "PSC_PIEMONTE" & OC_CODICE_PROGRAMMA == "2018REGPIEMFSC" ~ 7, # ospedale verbano -> da disattivare, non è duplo
                                                 ID_PSC == "PSC_PIEMONTE" & OC_CODICE_PROGRAMMA == "2007UM002FA013" ~ 7,
                                                 ID_PSC == "PSC_PIEMONTE" & OC_CODICE_PROGRAMMA == "PSCPIEMONTE" & OC_FLAG_VISUALIZZAZIONE == 1 ~ 7,
                                                 ID_PSC == "PSC_PA_BOLZANO" & OC_CODICE_PROGRAMMA == "2007BO002FA009" ~ 7,
                                                 TRUE ~ OC_FLAG_VISUALIZZAZIONE))
  }

  write.csv2(out, file.path(PSC, "psc", paste0("dati_psc_", bimestre, "_", versione, ".csv")), row.names = FALSE)
  
}

#' Dati SGP per elaborazioni PSC
#'
#' Crea dati SGP compatibili per elaborazioni PSC partendo da repository in DATI > SGP
#' 
#' @param bimestre Bimestre di riferimento, nella versione specifica per i dati PSC (es. 20211231.01 oppure 20211231)
#' @param versione Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param filename Nome file xlsx in DATI > SGP
#' @param matrix06 Matrice di riconciliazione tra denominazione APQ 2000-20006 e settori di intervento PSC
#' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' @return File "dati_sgp_BIMESTRE.csv" in TEMP 
#' @note ...
prep_dati_sgp_bimestre <- function(bimestre, versione, filename, matrix_06, chk_today) {
  # filename <- "Estrazioni dati e calcolo indicatori _28_02_21_v01.xlsx"
  # filename <- "Estrazione dati e calcolo indicatori_30062021.xlsx"
  # chk_today <- as.POSIXct("2021-02-28")
  # chk_today <- as.POSIXct("2021-04-30")
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  # print(bimestre)
  # DATA <- paste0(str_sub(DATA, 1, nchar(DATA)-8), bimestre)
  
  SGP <- file.path(DRIVE, "DATI", "SGP", bimestre)
  
  
  # # fix per sovrascrivere dati
  # appo <- read.table(file.path(SGP, filename),
  #                    header = TRUE, sep = ";", dec = ".", quote = "\"", fill = TRUE,
  #                    stringsAsFactors = FALSE)
  # appo1 <- appo %>% 
  #   mutate(COSTO_REALIZZATO = as.numeric(str_replace(COSTO_REALIZZATO, "^\\.", "0.")),
  #                  COSTO_DA_REALIZZARE = as.numeric(str_replace(COSTO_DA_REALIZZARE, "^\\.", "0.")),
  #                  COSTO_TOTALE = as.numeric(str_replace(COSTO_TOTALE, "^\\.", "0.")),
  #                  TOTALE_IMPEGNI = as.numeric(str_replace(TOTALE_IMPEGNI, "^\\.", "0.")),
  #                  TOTALE_PAGAMENTI = as.numeric(str_replace(TOTALE_PAGAMENTI, "^\\.", "0.")),
  #                  TOTALE_FINANZIAMENTI_FSC = as.numeric(str_replace(TOTALE_FINANZIAMENTI_FSC, "^\\.", "0.")),
  #                  TOTALE_FINANZIAMENTI_PVT = as.numeric(str_replace(TOTALE_FINANZIAMENTI_PVT, "^\\.", "0.")),
  #                  TOTALE_FINANZIAMENTI_RPR = as.numeric(str_replace(TOTALE_FINANZIAMENTI_RPR, "^\\.", "0.")),
  #                  TOTALE_FINANZIAMENTI_ALTRO = as.numeric(str_replace(TOTALE_FINANZIAMENTI_ALTRO, "^\\.", "0.")),
  #                  TOTALE_FINANZIAMENTI = as.numeric(str_replace(TOTALE_FINANZIAMENTI, "^\\.", "0.")),
  #                  TOTALE_ECONOMIE_FSC = as.numeric(str_replace(TOTALE_ECONOMIE_FSC, "^\\.", "0.")),
  #                  TOTALE_ECONOMIE_PVT = as.numeric(str_replace(TOTALE_ECONOMIE_PVT, "^\\.", "0.")),
  #                  TOTALE_ECONOMIE_ALTRO = as.numeric(str_replace(TOTALE_ECONOMIE_ALTRO, "^\\.", "0.")),
  #                  TOTALE_PAGAMENTI_TIPO_P = as.numeric(str_replace(TOTALE_PAGAMENTI_TIPO_P, "^\\.", "0.")),
  #                  TOTALE_PAGAMENTI_TIPO_R = as.numeric(str_replace(TOTALE_PAGAMENTI_TIPO_R, "^\\.", "0.")),
  #                  TOTALE_PAGAMENTI_FAS_TIPO_P = as.numeric(str_replace(TOTALE_PAGAMENTI_FAS_TIPO_P, "^\\.", "0.")),
  #                  TOTALE_PAGAMENTI_FAS_TIPO_R = as.numeric(str_replace(TOTALE_PAGAMENTI_FAS_TIPO_R, "^\\.", "0.")),
  #                  TOTALE_IMPEGNI_TIPO_I = as.numeric(str_replace(TOTALE_IMPEGNI_TIPO_I, "^\\.", "0.")),
  #                  TOTALE_IMPEGNI_TIPO_D = as.numeric(str_replace(TOTALE_IMPEGNI_TIPO_D, "^\\.", "0.")),
  #                  TOTALE_ECONOMIE = as.numeric(str_replace(TOTALE_ECONOMIE, "^\\.", "0.")),
  #                  TOTALE_ECONOMIE_RPR = as.numeric(str_replace(TOTALE_ECONOMIE_RPR, "^\\.", "0.")),
  #                  FINANZIAMENTO_TOTALE_PUBBLICO = as.numeric(str_replace(FINANZIAMENTO_TOTALE_PUBBLICO, "^\\.", "0.")),
  #                  FINANZIAMENTO_TOTALE_PUBBLICO_NETTO = as.numeric(str_replace(FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, "^\\.", "0.")),
  #                  FINANZIAMENTO_FSC = as.numeric(str_replace(FINANZIAMENTO_FSC, "^\\.", "0.")),
  #                  FINANZIAMENTO_FSC_NETTO = as.numeric(str_replace(FINANZIAMENTO_FSC_NETTO, "^\\.", "0.")),
  #                  IMPEGNI = as.numeric(str_replace(IMPEGNI, "^\\.", "0.")),
  #                  PAGAMENTI_TOTALI = as.numeric(str_replace(PAGAMENTI_TOTALI, "^\\.", "0.")),
  #                  PAGAMENTI_FSC = as.numeric(str_replace(PAGAMENTI_FSC, "^\\.", "0.")),
  #                  IMPORTO_NAZIONALE = as.numeric(str_replace(IMPORTO_NAZIONALE, "^\\.", "0.")),
  #                  IMPORTO_REGIONALE = as.numeric(str_replace(IMPORTO_REGIONALE, "^\\.", "0.")),
  #                  IMPORTO_NON_DEFINITO = as.numeric(str_replace(IMPORTO_NON_DEFINITO, "^\\.", "0.")))
  # chk <- appo1 %>% 
  #   select(COD_LOCALE_PROGETTO, IMPEGNI) %>% 
  #   filter(is.na(IMPEGNI)) %>% 
  #   left_join(appo %>% 
  #               select(COD_LOCALE_PROGETTO, IMPEGNI),
  #             by = "COD_LOCALE_PROGETTO")
  
  temp <- str_sub(filename, start = -4, end = -1)
  if (temp == "xlsx") {
    appo <- read_xlsx(file.path(SGP, filename), guess_max = 25000)
  } else {
    # NEW:
    appo <- read.csv(file.path(SGP, filename), sep = "~", dec = ".", skip = 1, stringsAsFactors = FALSE)
    # OLD: varie opzioni, sogei cambia sempre
    # appo <- read.csv(file.path(SGP, filename), sep = ";", dec = ".", stringsAsFactors = FALSE)
    # appo <- read.csv(file.path(SGP, filename), sep = "~", dec = ".", skip = 1, stringsAsFactors = FALSE)
    # appo <- read.table(file.path(SGP, filename),
    #                    skip = 1,
    #                    header = TRUE, sep = "~", dec = ".", 
    #                    quote = "\"",
    #                    comment.char = "", # MEMO: il deafult è "#" ma siccome i testi non sono quotati v
    #                    fill = TRUE,
    #                    stringsAsFactors = FALSE)
    # appo <- read_csv2(file.path(SGP, filename), guess_max = 25000, 
    #                   locale = locale(decimal_mark = "."))
  }
  
  for (x in names(appo)) {
    temp <- paste(names(appo[x]), class(appo[[x]]), sep = ": ")
    print(temp)
  }
  
  # chk <- appo %>% 
  #   filter(IMPEGNI != "") %>% 
  #   select(COD_LOCALE_PROGETTO, IMPEGNI) %>% 
  #   # filter(COD_LOCALE_PROGETTO == "TOSCWCW028PT") # 106376,99
  #   filter(COD_LOCALE_PROGETTO == "CMFZ18MSC-03") # ".1"


  # appo <- read_csv2(file.path(SGP, filename), guess_max = 25000) # 24928 progetti
  # sum(appo$FINANZIAMENTO_FSC_NETTO) # 19027452171
  # sum(appo$FINANZIAMENTO_TOTALE_PUBBLICO_NETTO) # 71662352262
  
  # addattamento per versioni diverse
  if ("TITOLO_PROGETTO" %in% names(appo)) {
    appo <- appo %>% 
      rename(TITOLO = TITOLO_PROGETTO)
  }
  
  if ("descrizione strumento" %in% names(appo) & "DESCRIZIONE_STRUMENTO" %!in% names(appo)) {
    appo <- appo %>% 
      rename(DESCRIZIONE_STRUMENTO = `descrizione strumento`)
  }
  
  if ("CODICE_STRUMENTO...2" %in% names(appo)) {
    appo <- appo %>% 
      rename(CODICE_STRUMENTO = CODICE_STRUMENTO...2)
  }
  
  if ("COSTO_REALIZZATO...103" %in% names(appo)) {
    appo <- appo %>% 
      rename(COSTO_REALIZZATO = COSTO_REALIZZATO...103)
  }
  
  if ("COSTO_REALIZZATO...104" %in% names(appo)) {
    appo <- appo %>% 
      rename(COSTO_REALIZZATO = COSTO_REALIZZATO...104)
  }
  

  if ("DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA...183" %in% names(appo)) {
    appo <- appo %>% 
      rename(DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA = DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA...183)
  }
  
  if (!("DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA" %in% names(appo))) {
    appo <- appo %>% 
      mutate(DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA = NA)
  }
  
  
  if (!("IMPORTO_REGIONALE" %in% names(appo))) {
    temp <- read_csv2(file.path(INPUT, "quota_regionale_sgp_20191231.csv"))
    
    appo <- appo %>% 
      left_join(temp, by = "COD_LOCALE_PROGETTO")
    
    # mutate(IMPORTO_NAZIONALE = NA, 
    #        IMPORTO_REGIONALE = NA, 
    #        IMPORTO_NON_DEFINITO = NA)
    message("ATTENZIONE: manca quota regionale, la importo da 20191231")
  } 
  
  appo1 <- appo
  # appo1 <- appo %>% 
  #   select(COD_LOCALE_PROGETTO, 
  #          CUP,
  #          TITOLO, 
  #          DENOMINAZIONE_INTESA, CODICE_STRUMENTO,
  #          FINANZIAMENTO_FSC_NETTO, FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, 
  #          TOTALE_FINANZIAMENTI_PVT, TOTALE_ECONOMIE_PVT, 
  #          TOTALE_FINANZIAMENTI, TOTALE_ECONOMIE,
  #          IMPORTO_NAZIONALE, IMPORTO_REGIONALE, IMPORTO_NON_DEFINITO,
  #          COSTO_REALIZZATO,
  #          IMPEGNI,
  #          PAGAMENTI_TOTALI, PAGAMENTI_FSC)
  
  # chk costo realizzato doppia
  # appo %>% 
  #   summarise(CR_1 = sum(`COSTO_REALIZZATO...104`, na.rm=TRUE),
  #             CR_2 = sum(`COSTO_REALIZZATO...170`, na.rm=TRUE))
  # 
  # appo %>% 
  #   mutate(CHK = `COSTO_REALIZZATO...104` - `COSTO_REALIZZATO...170`) %>% 
  #   summarise(CHK = sum(CHK, na.rm=TRUE))
  
  # chk NA su importi
  chk <- appo %>% 
    filter(is.na(IMPORTO_NAZIONALE) | is.na(IMPORTO_REGIONALE) | is.na(IMPORTO_NON_DEFINITO)) 
  chk %>% count()
  chk %>% count(COD_LOCALE_PROGETTO)
  chk <- appo %>% filter(COD_LOCALE_PROGETTO == "PIERSR/01/144")
  # PIERSR/01/144 1
  # MEMO: questo è il progetto che veniva con importo NA perché conteneva un "#" in una stringa
  
  # chk finanziamenti
  appo %>% 
    mutate(TOT_IMPORTI = IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO) %>% 
    summarise(TOT_IMPORTI = sum(TOT_IMPORTI, na.rm = TRUE),
              FINANZIAMENTO_FSC = sum(FINANZIAMENTO_FSC, na.rm = TRUE),
              FINANZIAMENTO_FSC_NETTO = sum(FINANZIAMENTO_FSC_NETTO, na.rm = TRUE)) %>% 
    mutate(CHK = TOT_IMPORTI - FINANZIAMENTO_FSC,
           CHK_NET = TOT_IMPORTI - FINANZIAMENTO_FSC_NETTO)
  
  chk <- appo %>% 
    mutate(TOT_IMPORTI = IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO,
           CHK = TOT_IMPORTI - FINANZIAMENTO_FSC,
           CHK_NET = TOT_IMPORTI - FINANZIAMENTO_FSC_NETTO) %>% 
    # filter(CHK != 0 | CHK_NET != 0) %>% 
    filter(CHK != 0) %>% 
    select(COD_LOCALE_PROGETTO, DENOMINAZIONE_INTESA, CODICE_STRUMENTO, 
           TOT_IMPORTI,
           IMPORTO_NAZIONALE, IMPORTO_REGIONALE, IMPORTO_NON_DEFINITO,
           FINANZIAMENTO_FSC, FINANZIAMENTO_FSC_NETTO,
           CHK, CHK_NET)
  chk %>% 
    summarise(N = n(),
              TOT_IMPORTI = sum(TOT_IMPORTI, na.rm = TRUE),
              FINANZIAMENTO_FSC = sum(FINANZIAMENTO_FSC, na.rm = TRUE))
  write.csv2(chk, file.path(TEMP, paste0("progetti_delta_importi_", bimestre, ".csv")))
  
  # chk pagamenti fSC
  # appo %>% summarise(PAG = sum(PAGAMENTI_FSC, na.rm = TRUE))
  # appo %>% summarise(CP = sum(FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, na.rm = TRUE))
  
  # chk su economie
  appo %>% 
    # mutate(FINANZIAMENTO_FSC_NETTO = as.numeric(FINANZIAMENTO_FSC_NETTO),
    #        TOTALE_ECONOMIE_FSC = as.numeric(TOTALE_ECONOMIE_FSC)) %>% 
    mutate(FINANZIAMENTO_FSC = if_else(is.na(FINANZIAMENTO_FSC), 0, FINANZIAMENTO_FSC),
           FINANZIAMENTO_FSC_NETTO = if_else(is.na(FINANZIAMENTO_FSC_NETTO), 0, FINANZIAMENTO_FSC_NETTO),
           TOTALE_ECONOMIE_FSC = if_else(is.na(TOTALE_ECONOMIE_FSC), 0, TOTALE_ECONOMIE_FSC)) %>% 
    mutate(DELTA = FINANZIAMENTO_FSC - FINANZIAMENTO_FSC_NETTO,
           CHK = TOTALE_ECONOMIE_FSC - DELTA) %>% 
    filter(CHK != 0) %>%
    summarise(N = n(),
              DELTA = sum(DELTA, na.rm = TRUE),
              ECO = sum(TOTALE_ECONOMIE_FSC, na.rm = TRUE),
              CHK = sum(CHK, na.rm = TRUE))
  
  # chk <- appo %>% select(COD_LOCALE_PROGETTO, FINANZIAMENTO_FSC_NETTO, TOTALE_ECONOMIE_FSC)
  # write.csv2(chk, file.path(TEMP, "progetti_finanziamento_carattere.csv"))
  
  # fix
  # appo1 <- appo1 %>% 
  #   mutate(FINANZIAMENTO_FSC_NETTO = as.numeric(FINANZIAMENTO_FSC_NETTO),
  #        TOTALE_ECONOMIE_FSC = as.numeric(TOTALE_ECONOMIE_FSC),
  #        TOTALE_FINANZIAMENTI_PVT = as.numeric(TOTALE_FINANZIAMENTI_PVT),
  #        TOTALE_ECONOMIE = as.numeric(TOTALE_ECONOMIE),
  #        IMPEGNI = as.numeric(IMPEGNI))
  
  # elab
  appo2 <- appo1 %>% 
    mutate(CICLO = "2000-2006") %>% 
    mutate(DENOMINAZIONE_INTESA = toupper(DENOMINAZIONE_INTESA)) %>% 
    mutate(OC_CODICE_PROGRAMMA = case_when(DENOMINAZIONE_INTESA == "ABRUZZO" ~ "33",
                                           DENOMINAZIONE_INTESA == "BASILICATA" ~ "37",
                                           DENOMINAZIONE_INTESA == "CALABRIA" ~ "38",
                                           DENOMINAZIONE_INTESA == "CAMPANIA" ~ "35",
                                           DENOMINAZIONE_INTESA == "EMILIA-ROMAGNA" ~ "28",
                                           DENOMINAZIONE_INTESA == "EMILIA ROMAGNA" ~ "28",
                                           DENOMINAZIONE_INTESA == "FRIULI-VENEZIA GIULIA" ~ "26",
                                           DENOMINAZIONE_INTESA == "LAZIO" ~ "32",
                                           DENOMINAZIONE_INTESA == "LIGURIA" ~ "27",
                                           DENOMINAZIONE_INTESA == "LOMBARDIA" ~ "01",
                                           DENOMINAZIONE_INTESA == "MARCHE" ~ "04",
                                           DENOMINAZIONE_INTESA == "MOLISE" ~ "34",
                                           DENOMINAZIONE_INTESA == "P.A. BOLZANO" ~ "41",
                                           DENOMINAZIONE_INTESA == "P.A. TRENTO" ~ "40",
                                           DENOMINAZIONE_INTESA == "PIEMONTE" ~ "21",
                                           DENOMINAZIONE_INTESA == "PUGLIA" ~ "36",
                                           DENOMINAZIONE_INTESA == "SARDEGNA" ~ "05",
                                           DENOMINAZIONE_INTESA == "SICILIA" ~ "39",
                                           DENOMINAZIONE_INTESA == "TOSCANA" ~ "02",
                                           DENOMINAZIONE_INTESA == "UMBRIA" ~ "03",
                                           DENOMINAZIONE_INTESA == "VALLE D'AOSTA" ~ "22",
                                           DENOMINAZIONE_INTESA == "VENETO" ~ "25")) %>% 
    mutate(MACROAREA = case_when(DENOMINAZIONE_INTESA == "ABRUZZO" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "BASILICATA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "CALABRIA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "CAMPANIA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "EMILIA-ROMAGNA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "EMILIA ROMAGNA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "FRIULI-VENEZIA GIULIA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "LAZIO" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "LIGURIA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "LOMBARDIA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "MARCHE" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "MOLISE" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "P.A. BOLZANO" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "P.A. TRENTO" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "PIEMONTE" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "PUGLIA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "SARDEGNA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "SICILIA" ~ "Mezzogiorno",
                                 DENOMINAZIONE_INTESA == "TOSCANA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "UMBRIA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "VALLE D'AOSTA" ~ "Centro-Nord",
                                 DENOMINAZIONE_INTESA == "VENETO" ~ "Centro-Nord")) %>% 
    # psc
    left_join(matrix_po_psc %>% 
                distinct(OC_CODICE_PROGRAMMA, ID_PSC, PSC), 
              by = "OC_CODICE_PROGRAMMA") %>% 
    # tema
    left_join(art44 %>% 
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, COD_TEMA_NEW, DESCR_TEMA_NEW, 
                       COD_OPOS_NEW, DESCR_OPOS_NEW),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(COD_SETTORE_INTERVENTO = COD_OPOS_NEW) %>% 
    # rettifica temi da primo cds
    left_join(interventi_cds,
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.y), COD_SETTORE_INTERVENTO.x, COD_SETTORE_INTERVENTO.y)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    # integra temi mancanti
    left_join(matrix_06 %>% 
                select(CODICE_STRUMENTO, COD_SETTORE_INTERVENTO),
              by = "CODICE_STRUMENTO") %>% 
    # TODO: qui va fix per ferrovie vs strade
    mutate(COD_SETTORE_INTERVENTO = if_else(is.na(COD_SETTORE_INTERVENTO.x), COD_SETTORE_INTERVENTO.y, COD_SETTORE_INTERVENTO.x)) %>% 
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    # sovrascrive tema per tutti
    left_join(matrix_temi_settori, 
              by = "COD_SETTORE_INTERVENTO") %>% 
    mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
           SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO)) %>% 
    # stato procedurale
    get_stato_attuazione(., chk_today = chk_today) %>% 
    # risorse coesione
    mutate(across(where(is.numeric), ~replace_na(., replace = 0))) %>% 
    # OLD:
    # mutate(x = FINANZIAMENTO_FSC_NETTO / FINANZIAMENTO_TOTALE_PUBBLICO_NETTO,
    #        # x = FINANZIAMENTO_FSC / FINANZIAMENTO_TOTALE_PUBBLICO,
    #        # k = if_else(is.na(IMPORTO_REGIONALE), 0, # MEMO: da 2010630 non si verifica più il problema; prima c'era quota random di 0.75
    #        #             IMPORTO_REGIONALE/(IMPORTO_REGIONALE+IMPORTO_NAZIONALE+IMPORTO_NON_DEFINITO)),
    #        k = IMPORTO_REGIONALE / (IMPORTO_REGIONALE + IMPORTO_NAZIONALE + IMPORTO_NON_DEFINITO),
    #        COE = FINANZIAMENTO_FSC_NETTO * k,
    #        COE_IMP = IMPEGNI * x * k,
    #        COE_CR = COSTO_REALIZZATO * x * k,
    #        COE_PAG = PAGAMENTI_TOTALI * x * k, # MEMO: PAGAMENTI_FSC sono vuoti
    #        COE_ECO = TOTALE_ECONOMIE_FSC * k)  %>% 
    # NEW:
    mutate(x = FINANZIAMENTO_FSC_NETTO / (FINANZIAMENTO_TOTALE_PUBBLICO + TOTALE_FINANZIAMENTI_PVT + TOTALE_FINANZIAMENTI_RPR - TOTALE_ECONOMIE),
           k = IMPORTO_REGIONALE / (IMPORTO_REGIONALE + IMPORTO_NAZIONALE + IMPORTO_NON_DEFINITO),
           COE = FINANZIAMENTO_FSC_NETTO * k,
           COE_IMP = IMPEGNI * x * k,
           COE_CR = COSTO_REALIZZATO * x * k,
           COE_PAG = PAGAMENTI_TOTALI * x * k, # MEMO: PAGAMENTI_FSC sono vuoti
           COE_ECO = TOTALE_ECONOMIE_FSC * k,
           CP = FINANZIAMENTO_TOTALE_PUBBLICO_NETTO,
           CPP = FINANZIAMENTO_TOTALE_PUBBLICO + TOTALE_FINANZIAMENTI_PVT) 
  
  # chk <- appo2 %>% 
  #   filter(STATO_PROCED == "Non avviato")
  
  # calcola costo realizzato
  # appo3 <- appo2 %>% 
  #   mutate_if(is.numeric, replace_na, replace = 0) %>% 
  #   mutate(FIN_TOT = TOTALE_FINANZIAMENTI - TOTALE_ECONOMIE,
  #          x = FINANZIAMENTO_FSC_NETTO / FIN_TOT,
  #          k = IMPORTO_REGIONALE / (IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO),
  #          COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x * k,
  #          COE = FINANZIAMENTO_FSC_NETTO * k,
  #          COE_IMP = IMPEGNI * x * k,
  #          COE_PAG = PAGAMENTI_TOTALI * x * k)
  
  # economie
  appo3 <- appo2 
  
  # appo3 %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             PAGAMENTI_TOTALI = sum(PAGAMENTI_TOTALI, na.rm = TRUE),
  #             PAGAMENTI_FSC = sum(PAGAMENTI_FSC, na.rm = TRUE),
  #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO, na.rm = TRUE),
  #             FINANZIAMENTO_FSC_NETTO = sum(FINANZIAMENTO_FSC_NETTO, na.rm = TRUE),
  #             FINANZIAMENTO_TOTALE_PUBBLICO_NETTO = sum(FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, na.rm = TRUE),
  #             IMPORTO_REGIONALE = sum(IMPORTO_REGIONALE, na.rm = TRUE),
  #             IMPORTO_NAZIONALE = sum(IMPORTO_NAZIONALE, na.rm = TRUE),
  #             IMPORTO_NON_DEFINITO = sum(IMPORTO_NON_DEFINITO, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE))
  appo3 %>%
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              COE_CR = sum(COE_CR, na.rm = TRUE), 
              COE_ECO = sum(COE_ECO, na.rm = TRUE))
  # 
  # appo3 %>%
  #   group_by(ID_PSC, PSC, OC_CODICE_PROGRAMMA, CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE))
  
  
  # debug
  print(appo3 %>% count(AREA_TEMATICA))
  chk <- appo3 %>% filter(AREA_TEMATICA == "NA-NA")
  write_csv2(chk, file.path(TEMP, paste0("chk_classi_missing_0006_", bimestre, ".csv")))
  message("Questi sono gli strumenti attuativi non censiti in matrix:")
  chk2 <- chk %>% count(CODICE_STRUMENTO, DESCRIZIONE_STRUMENTO)
  print(chk2)
  write_csv2(chk2, file.path(TEMP, paste0("add_to_mapping_0006_", bimestre, ".csv")))
  message("Se ci sono NA-NA, correggi mapping.xlsx e rilancia")
  
  # fix
  appo4 <- fix_progetti_sgp(progetti_sgp = appo3, bimestre)
  
  
  # stato progetto per economie
  appo5 <- appo4 %>%
    rename(OC_STATO_PROCEDURALE = STATO_PROCED) %>% 
    mutate(STATO = case_when(COE_PAG / COE > 0.95 ~ "Conclusi",
                             COE_CR / COE > 0.95 ~ "Conclusi",
                             OC_STATO_PROCEDURALE == "Eseguito" ~ "Conclusi",
                             TRUE ~ "In corso"))
  
  
  # export
  out <- appo5 %>%
    mutate(OC_FLAG_VISUALIZZAZIONE = 0) %>% 
    select(COD_LOCALE_PROGETTO, 
           CUP, 
           OC_TITOLO_PROGETTO = TITOLO,
           x_CICLO = CICLO,
           x_PROGRAMMA = DENOMINAZIONE_INTESA,
           OC_CODICE_PROGRAMMA,
           x_MACROAREA = MACROAREA,
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
           PSC,
           ID_PSC, 
           STATO,
           OC_FLAG_VISUALIZZAZIONE)
  
  # da 20191231 (manuale)
  # out <- appo2 %>% 
  #   select(COD_LOCALE_PROGETTO,
  #          IMPORTO_REGIONALE, IMPORTO_NAZIONALE, IMPORTO_NON_DEFINITO)
  # write_csv2(out, file.path(INPUT, "quota_regionale_sgp_20191231.csv"))
  
  print(out %>% 
          summarise(COE = sum(COE, na.rm = TRUE),
                    COE_IMP = sum(COE_IMP, na.rm = TRUE),
                    COE_CR = sum(COE_CR, na.rm = TRUE),
                    COE_PAG = sum(COE_PAG, na.rm = TRUE),
                    CP = sum(CP, na.rm = TRUE)))
  
  write_csv2(out, file.path(PSC, "sgp", paste0("dati_sgp_", bimestre, "_", versione, ".csv")))
  
  
}


clean_data_dmy <- function(colonna_data) {
  
  # DEBUG:
  # colonna_data <- appo0$DATA_FINE_EFF_ESECUZIONE
  # colonna_data <- appo0$DATA_FINE_EFF_STIP_ATTRIB

  require("lubridate")
  out <- dmy(colonna_data)
  # out <- tryCatch(dmy(colonna_data),
  #                 error = ymd(colonna_data))
  return(out)

}

clean_data_ymd <- function(colonna_data) {
  
  # DEBUG:
  # colonna_data <- appo0$DATA_FINE_EFF_ESECUZIONE
  # colonna_data <- appo0$DATA_FINE_EFF_STIP_ATTRIB
  
  require("lubridate")
  out <- ymd(colonna_data)
  return(out)
  
}


get_stato_attuazione <- function(df, chk_today) {
  # MEMO: 
  # formato per data è diverso da standad oc
  # può essere necessaria qualche pulizia nelle date in excel

  # chk_today <- as.POSIXct("2019-12-31")
  chk_today <- as.POSIXct(chk_today)
  
  require(lubridate)
  # DEBUG:
  # df <- appo2
  # df <- appo_stato
  # chk_today = "2022-04-30"
  
  # switch per ciclo
  if ("A00_DATA_INIZIO_EFFETTIVA" %in% names(df)) {
    
    test <- is.POSIXct(df$A00_DATA_INIZIO_EFFETTIVA)
    
    # fix per xls fino al 31/12/2021 (vengono lette come date e non funziona case_when dopo)
    if (test == TRUE) {
      df <- df %>% 
        mutate_if(is.POSIXct, list(~str_sub(as.character(.), 1, 10)))
    }
    
    #2000-2006 da sgp
    appo0 <- df %>%
      mutate(DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA = case_when(is.na(DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA) & !is.na(A00_DATA_INIZIO_EFFETTIVA) ~ A00_DATA_INIZIO_EFFETTIVA,
                                                                  # DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA == "" & A00_DATA_INIZIO_EFFETTIVA != "" ~ A00_DATA_INIZIO_EFFETTIVA,
                                                                  DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA == "" & !is.na(A00_DATA_INIZIO_EFFETTIVA) & A00_DATA_INIZIO_EFFETTIVA != "" ~ A00_DATA_INIZIO_EFFETTIVA,
                                                                  TRUE ~ DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA),
             DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA = case_when(is.na(DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA) & !is.na(A00_DATA_FINE_EFFETTIVA) ~ A00_DATA_FINE_EFFETTIVA,
                                                                # DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA == "" & A00_DATA_FINE_EFFETTIVA != "" ~ A00_DATA_FINE_EFFETTIVA,
                                                                DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA == "" & !is.na(A00_DATA_FINE_EFFETTIVA) & A00_DATA_FINE_EFFETTIVA != "" ~ A00_DATA_FINE_EFFETTIVA,
                                                                TRUE ~ DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA),
             DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE = case_when(is.na(DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE) & !is.na(A01_DATA_INIZIO_EFFETTIVA) ~ A01_DATA_INIZIO_EFFETTIVA,
                                                                   # DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE == "" & A01_DATA_INIZIO_EFFETTIVA != "" ~ A01_DATA_INIZIO_EFFETTIVA,
                                                                   DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE == "" & !is.na(A01_DATA_INIZIO_EFFETTIVA) & A01_DATA_INIZIO_EFFETTIVA != "" ~ A01_DATA_INIZIO_EFFETTIVA,
                                                                   TRUE ~ DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE),
             DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE = case_when(is.na(DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE) & !is.na(A01_DATA_FINE_EFFETTIVA) ~ A01_DATA_FINE_EFFETTIVA,
                                                                 # DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE == "" & A01_DATA_FINE_EFFETTIVA != "" ~ A01_DATA_FINE_EFFETTIVA,
                                                                 DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE == "" & !is.na(A01_DATA_FINE_EFFETTIVA) & A01_DATA_FINE_EFFETTIVA != "" ~ A01_DATA_FINE_EFFETTIVA,
                                                                 TRUE ~ DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE),
             DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA = case_when(is.na(DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA) & !is.na(A02_DATA_INIZIO_EFFETTIVA) ~ A02_DATA_INIZIO_EFFETTIVA,
                                                                  # DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA == "" & A02_DATA_INIZIO_EFFETTIVA != "" ~ A02_DATA_INIZIO_EFFETTIVA,
                                                                  DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA == "" & !is.na(A02_DATA_INIZIO_EFFETTIVA) & A02_DATA_INIZIO_EFFETTIVA != "" ~ A02_DATA_INIZIO_EFFETTIVA,
                                                                  TRUE ~ DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA),
             DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA = case_when(is.na(DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA) & !is.na(A02_DATA_FINE_EFFETTIVA) ~ A02_DATA_FINE_EFFETTIVA,
                                                                # DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA =="" & A02_DATA_FINE_EFFETTIVA != "" ~ A02_DATA_FINE_EFFETTIVA,
                                                                DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA =="" & !is.na(A02_DATA_FINE_EFFETTIVA) & A02_DATA_FINE_EFFETTIVA != "" ~ A02_DATA_FINE_EFFETTIVA,
                                                                TRUE ~ DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA),
             DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA = case_when(is.na(DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA) & !is.na(A03_DATA_INIZIO_EFFETTIVA) ~ A03_DATA_INIZIO_EFFETTIVA,
                                                                 # DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA == "" & A03_DATA_INIZIO_EFFETTIVA != "" ~ A03_DATA_INIZIO_EFFETTIVA,
                                                                 DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA == "" & !is.na(A03_DATA_INIZIO_EFFETTIVA) & A03_DATA_INIZIO_EFFETTIVA != "" ~ A03_DATA_INIZIO_EFFETTIVA,
                                                                 TRUE ~ DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA),
             DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA = case_when(is.na(DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA) & !is.na(A03_DATA_FINE_EFFETTIVA) ~ A03_DATA_FINE_EFFETTIVA,
                                                               # DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA == "" & A03_DATA_FINE_EFFETTIVA != "" ~ A03_DATA_FINE_EFFETTIVA,
                                                               DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA == "" & !is.na(A03_DATA_FINE_EFFETTIVA) & A03_DATA_FINE_EFFETTIVA != "" ~ A03_DATA_FINE_EFFETTIVA,
                                                               TRUE ~ DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA),
             DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO = case_when(is.na(DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO) & !is.na(B01_DATA_INIZIO_EFFETTIVA) ~ B01_DATA_INIZIO_EFFETTIVA,
                                                                 # DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO == "" & B01_DATA_INIZIO_EFFETTIVA != "" ~ B01_DATA_INIZIO_EFFETTIVA,
                                                                 DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO == "" & !is.na(B01_DATA_INIZIO_EFFETTIVA) & B01_DATA_INIZIO_EFFETTIVA != "" ~ B01_DATA_INIZIO_EFFETTIVA,
                                                                 is.na(DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO) & !is.na(C01_DATA_INIZIO_EFFETTIVA) ~ C01_DATA_INIZIO_EFFETTIVA,
                                                                 # DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO == "" & C01_DATA_INIZIO_EFFETTIVA != "" ~ C01_DATA_INIZIO_EFFETTIVA,
                                                                 DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO == "" & !is.na(C01_DATA_INIZIO_EFFETTIVA) & C01_DATA_INIZIO_EFFETTIVA != "" ~ C01_DATA_INIZIO_EFFETTIVA,
                                                                 # step non presente per lavori
                                                                 TRUE ~ DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO),
             DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO = case_when(is.na(DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO) & !is.na(B01_DATA_FINE_EFFETTIVA) ~ B01_DATA_FINE_EFFETTIVA,
                                                               # DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO == "" & B01_DATA_FINE_EFFETTIVA != "" ~ B01_DATA_FINE_EFFETTIVA,
                                                               DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO == "" & !is.na(B01_DATA_FINE_EFFETTIVA) & B01_DATA_FINE_EFFETTIVA != "" ~ B01_DATA_FINE_EFFETTIVA,
                                                               is.na(DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO) & !is.na(C01_DATA_FINE_EFFETTIVA) ~ C01_DATA_FINE_EFFETTIVA,
                                                               # DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO == "" & C01_DATA_FINE_EFFETTIVA != "" ~ C01_DATA_FINE_EFFETTIVA,
                                                               DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO == "" & !is.na(C01_DATA_FINE_EFFETTIVA) & C01_DATA_FINE_EFFETTIVA != "" ~ C01_DATA_FINE_EFFETTIVA,
                                                               # step non presente per lavori
                                                               TRUE ~ DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO),
             DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE = case_when(is.na(DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(A04_DATA_INIZIO_EFFETTIVA) ~ A04_DATA_INIZIO_EFFETTIVA,
                                                                           # DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & A04_DATA_INIZIO_EFFETTIVA != "" ~ A04_DATA_INIZIO_EFFETTIVA,
                                                                           DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(A04_DATA_INIZIO_EFFETTIVA) & A04_DATA_INIZIO_EFFETTIVA != "" ~ A04_DATA_INIZIO_EFFETTIVA,
                                                                           is.na(DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(B02_DATA_INIZIO_EFFETTIVA) ~ B02_DATA_INIZIO_EFFETTIVA,
                                                                           # DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & B02_DATA_INIZIO_EFFETTIVA != "" ~ B02_DATA_INIZIO_EFFETTIVA,
                                                                           DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(B02_DATA_INIZIO_EFFETTIVA) & B02_DATA_INIZIO_EFFETTIVA != "" ~ B02_DATA_INIZIO_EFFETTIVA,
                                                                           is.na(DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(C02_DATA_INIZIO_EFFETTIVA) ~ C02_DATA_INIZIO_EFFETTIVA,
                                                                           # DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & C02_DATA_INIZIO_EFFETTIVA != "" ~ C02_DATA_INIZIO_EFFETTIVA,
                                                                           DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(C02_DATA_INIZIO_EFFETTIVA) & C02_DATA_INIZIO_EFFETTIVA != "" ~ C02_DATA_INIZIO_EFFETTIVA,
                                                                           TRUE ~ DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE),
             DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE = case_when(is.na(DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(A04_DATA_FINE_EFFETTIVA) ~ A04_DATA_FINE_EFFETTIVA,
                                                                         # DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & A04_DATA_FINE_EFFETTIVA != "" ~ A04_DATA_FINE_EFFETTIVA,
                                                                         DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(A04_DATA_FINE_EFFETTIVA) & A04_DATA_FINE_EFFETTIVA != "" ~ A04_DATA_FINE_EFFETTIVA,
                                                                         is.na(DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(B02_DATA_FINE_EFFETTIVA) ~ B02_DATA_FINE_EFFETTIVA,
                                                                         # DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & B02_DATA_FINE_EFFETTIVA != "" ~ B02_DATA_FINE_EFFETTIVA,
                                                                         DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(B02_DATA_FINE_EFFETTIVA) & B02_DATA_FINE_EFFETTIVA != "" ~ B02_DATA_FINE_EFFETTIVA,
                                                                         is.na(DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE) & !is.na(C02_DATA_FINE_EFFETTIVA) ~ C02_DATA_FINE_EFFETTIVA,
                                                                         # DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & C02_DATA_FINE_EFFETTIVA != "" ~ C02_DATA_FINE_EFFETTIVA,
                                                                         DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE == "" & !is.na(C02_DATA_FINE_EFFETTIVA) & C02_DATA_FINE_EFFETTIVA != "" ~ C02_DATA_FINE_EFFETTIVA,
                                                                         TRUE ~ DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE)) %>%
      select(COD_LOCALE_PROGETTO,
             # DATA_FINE_EFF_COLLAUDO,
             # DATA_INIZIO_EFF_COLLAUDO,
             DATA_FINE_EFF_ESECUZIONE = DATA_FINE_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE,
             DATA_INIZIO_EFF_ESECUZIONE = DATA_INIZIO_EFFETTIVA_ESECUZIONE_LAVORI_FORNITURE,
             DATA_FINE_EFF_STIP_ATTRIB = DATA_FINE_EFFETTIVA_STIPULA_CONTRATTO,
             DATA_INIZIO_EFF_STIP_ATTRIB = DATA_INIZIO_EFFETTIVA_STIPULA_CONTRATTO,
             # DATA_FINE_EFF_AGG_BANDO,
             # DATA_INIZIO_EFF_AGG_BANDO,
             DATA_FINE_EFF_PROG_ESEC = DATA_FINE_EFFETTIVA_PROGETT_ESECUTIVA,
             DATA_INIZIO_EFF_PROG_ESEC = DATA_INIZIO_EFFETTIVA_PROGETT_ESECUTIVA,
             DATA_FINE_EFF_PROG_DEF = DATA_FINE_EFFETTIVA_PROGETT_DEFINITIVA,
             DATA_INIZIO_EFF_PROG_DEF = DATA_INIZIO_EFFETTIVA_PROGETT_DEFINITIVA,
             DATA_FINE_EFF_PROG_PREL = DATA_FINE_EFFETTIVA_PROGETT_PRELIMINARE,
             DATA_INIZIO_EFF_PROG_PREL = DATA_INIZIO_EFFETTIVA_PROGETT_PRELIMINARE, 
             DATA_FINE_EFF_STUDIO_FATT = DATA_FINE_EFFETTIVA_STUDIO_FATTIBILITA,
             DATA_INIZIO_EFF_STUDIO_FATT = DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA)
    
    if (test == TRUE) {
      appo <- appo0 %>% 
        mutate(DATA_FINE_EFF_ESECUZIONE = clean_data_ymd(DATA_FINE_EFF_ESECUZIONE),
               DATA_INIZIO_EFF_ESECUZIONE = clean_data_ymd(DATA_INIZIO_EFF_ESECUZIONE),
               DATA_FINE_EFF_STIP_ATTRIB = clean_data_ymd(DATA_FINE_EFF_STIP_ATTRIB),
               DATA_INIZIO_EFF_STIP_ATTRIB = clean_data_ymd(DATA_INIZIO_EFF_STIP_ATTRIB),
               # DATA_FINE_EFF_AGG_BANDO,
               # DATA_INIZIO_EFF_AGG_BANDO,
               DATA_FINE_EFF_PROG_ESEC = clean_data_ymd(DATA_FINE_EFF_PROG_ESEC),
               DATA_INIZIO_EFF_PROG_ESEC = clean_data_ymd(DATA_INIZIO_EFF_PROG_ESEC),
               DATA_FINE_EFF_PROG_DEF = clean_data_ymd(DATA_FINE_EFF_PROG_DEF),
               DATA_INIZIO_EFF_PROG_DEF = clean_data_ymd(DATA_INIZIO_EFF_PROG_DEF),
               DATA_FINE_EFF_PROG_PREL = clean_data_ymd(DATA_FINE_EFF_PROG_PREL),
               DATA_INIZIO_EFF_PROG_PREL = clean_data_ymd(DATA_INIZIO_EFF_PROG_PREL), 
               DATA_FINE_EFF_STUDIO_FATT = clean_data_ymd(DATA_FINE_EFF_STUDIO_FATT),
               DATA_INIZIO_EFF_STUDIO_FATT = clean_data_ymd(DATA_INIZIO_EFF_STUDIO_FATT))
      
    } else {
      appo <- appo0 %>% 
        mutate(DATA_FINE_EFF_ESECUZIONE = clean_data_dmy(DATA_FINE_EFF_ESECUZIONE),
               DATA_INIZIO_EFF_ESECUZIONE = clean_data_dmy(DATA_INIZIO_EFF_ESECUZIONE),
               DATA_FINE_EFF_STIP_ATTRIB = clean_data_dmy(DATA_FINE_EFF_STIP_ATTRIB),
               DATA_INIZIO_EFF_STIP_ATTRIB = clean_data_dmy(DATA_INIZIO_EFF_STIP_ATTRIB),
               # DATA_FINE_EFF_AGG_BANDO,
               # DATA_INIZIO_EFF_AGG_BANDO,
               DATA_FINE_EFF_PROG_ESEC = clean_data_dmy(DATA_FINE_EFF_PROG_ESEC),
               DATA_INIZIO_EFF_PROG_ESEC = clean_data_dmy(DATA_INIZIO_EFF_PROG_ESEC),
               DATA_FINE_EFF_PROG_DEF = clean_data_dmy(DATA_FINE_EFF_PROG_DEF),
               DATA_INIZIO_EFF_PROG_DEF = clean_data_dmy(DATA_INIZIO_EFF_PROG_DEF),
               DATA_FINE_EFF_PROG_PREL = clean_data_dmy(DATA_FINE_EFF_PROG_PREL),
               DATA_INIZIO_EFF_PROG_PREL = clean_data_dmy(DATA_INIZIO_EFF_PROG_PREL), 
               DATA_FINE_EFF_STUDIO_FATT = clean_data_dmy(DATA_FINE_EFF_STUDIO_FATT),
               DATA_INIZIO_EFF_STUDIO_FATT = clean_data_dmy(DATA_INIZIO_EFF_STUDIO_FATT))

    }
    
    
  } else {
    appo0 <- df %>%
      mutate(DATA_INIZIO_EFF_STUDIO_FATT = paste0(str_sub(DATA_INIZIO_EFF_STUDIO_FATT, 7, 8), "/", str_sub(DATA_INIZIO_EFF_STUDIO_FATT, 5, 6), "/", str_sub(DATA_INIZIO_EFF_STUDIO_FATT, 1, 4)),
             DATA_FINE_EFF_STUDIO_FATT = paste0(str_sub(DATA_FINE_EFF_STUDIO_FATT, 7, 8), "/", str_sub(DATA_FINE_EFF_STUDIO_FATT, 5, 6), "/", str_sub(DATA_FINE_EFF_STUDIO_FATT, 1, 4)),
             DATA_INIZIO_EFF_PROG_PREL = paste0(str_sub(DATA_INIZIO_EFF_PROG_PREL, 7, 8), "/", str_sub(DATA_INIZIO_EFF_PROG_PREL, 5, 6), "/", str_sub(DATA_INIZIO_EFF_PROG_PREL, 1, 4)),
             DATA_FINE_EFF_PROG_PREL = paste0(str_sub(DATA_FINE_EFF_PROG_PREL, 7, 8), "/", str_sub(DATA_FINE_EFF_PROG_PREL, 5, 6), "/", str_sub(DATA_FINE_EFF_PROG_PREL, 1, 4)),
             DATA_INIZIO_EFF_PROG_DEF = paste0(str_sub(DATA_INIZIO_EFF_PROG_DEF, 7, 8), "/", str_sub(DATA_INIZIO_EFF_PROG_DEF, 5, 6), "/", str_sub(DATA_INIZIO_EFF_PROG_DEF, 1, 4)),
             DATA_FINE_EFF_PROG_DEF = paste0(str_sub(DATA_FINE_EFF_PROG_DEF, 7, 8), "/", str_sub(DATA_FINE_EFF_PROG_DEF, 5, 6), "/", str_sub(DATA_FINE_EFF_PROG_DEF, 1, 4)),
             DATA_INIZIO_EFF_PROG_ESEC = paste0(str_sub(DATA_INIZIO_EFF_PROG_ESEC, 7, 8), "/", str_sub(DATA_INIZIO_EFF_PROG_ESEC, 5, 6), "/", str_sub(DATA_INIZIO_EFF_PROG_ESEC, 1, 4)),
             DATA_FINE_EFF_PROG_ESEC = paste0(str_sub(DATA_FINE_EFF_PROG_ESEC, 7, 8), "/", str_sub(DATA_FINE_EFF_PROG_ESEC, 5, 6), "/", str_sub(DATA_FINE_EFF_PROG_ESEC, 1, 4)),
             DATA_INIZIO_EFF_STIP_ATTRIB = paste0(str_sub(DATA_INIZIO_EFF_STIP_ATTRIB, 7, 8), "/", str_sub(DATA_INIZIO_EFF_STIP_ATTRIB, 5, 6), "/", str_sub(DATA_INIZIO_EFF_STIP_ATTRIB, 1, 4)),
             DATA_FINE_EFF_STIP_ATTRIB = paste0(str_sub(DATA_FINE_EFF_STIP_ATTRIB, 7, 8), "/", str_sub(DATA_FINE_EFF_STIP_ATTRIB, 5, 6), "/", str_sub(DATA_FINE_EFF_STIP_ATTRIB, 1, 4)),
             DATA_INIZIO_EFF_ESECUZIONE = paste0(str_sub(DATA_INIZIO_EFF_ESECUZIONE, 7, 8), "/", str_sub(DATA_INIZIO_EFF_ESECUZIONE, 5, 6), "/", str_sub(DATA_INIZIO_EFF_ESECUZIONE, 1, 4)),
             DATA_FINE_EFF_ESECUZIONE = paste0(str_sub(DATA_FINE_EFF_ESECUZIONE, 7, 8), "/", str_sub(DATA_FINE_EFF_ESECUZIONE, 5, 6), "/", str_sub(DATA_FINE_EFF_ESECUZIONE, 1, 4))) %>%
      select(COD_LOCALE_PROGETTO,
             # DATA_FINE_EFF_COLLAUDO,
             # DATA_INIZIO_EFF_COLLAUDO,
             DATA_FINE_EFF_ESECUZIONE,
             DATA_INIZIO_EFF_ESECUZIONE,
             DATA_FINE_EFF_STIP_ATTRIB,
             DATA_INIZIO_EFF_STIP_ATTRIB,
             # DATA_FINE_EFF_AGG_BANDO,
             # DATA_INIZIO_EFF_AGG_BANDO,
             DATA_FINE_EFF_PROG_ESEC,
             DATA_INIZIO_EFF_PROG_ESEC,
             DATA_FINE_EFF_PROG_DEF,
             DATA_INIZIO_EFF_PROG_DEF,
             DATA_FINE_EFF_PROG_PREL,
             DATA_INIZIO_EFF_PROG_PREL, 
             DATA_FINE_EFF_STUDIO_FATT,
             DATA_INIZIO_EFF_STUDIO_FATT)
    
    appo <- appo0 %>% 
      mutate(DATA_FINE_EFF_ESECUZIONE = clean_data_dmy(DATA_FINE_EFF_ESECUZIONE),
             DATA_INIZIO_EFF_ESECUZIONE = clean_data_dmy(DATA_INIZIO_EFF_ESECUZIONE),
             DATA_FINE_EFF_STIP_ATTRIB = clean_data_dmy(DATA_FINE_EFF_STIP_ATTRIB),
             DATA_INIZIO_EFF_STIP_ATTRIB = clean_data_dmy(DATA_INIZIO_EFF_STIP_ATTRIB),
             # DATA_FINE_EFF_AGG_BANDO,
             # DATA_INIZIO_EFF_AGG_BANDO,
             DATA_FINE_EFF_PROG_ESEC = clean_data_dmy(DATA_FINE_EFF_PROG_ESEC),
             DATA_INIZIO_EFF_PROG_ESEC = clean_data_dmy(DATA_INIZIO_EFF_PROG_ESEC),
             DATA_FINE_EFF_PROG_DEF = clean_data_dmy(DATA_FINE_EFF_PROG_DEF),
             DATA_INIZIO_EFF_PROG_DEF = clean_data_dmy(DATA_INIZIO_EFF_PROG_DEF),
             DATA_FINE_EFF_PROG_PREL = clean_data_dmy(DATA_FINE_EFF_PROG_PREL),
             DATA_INIZIO_EFF_PROG_PREL = clean_data_dmy(DATA_INIZIO_EFF_PROG_PREL), 
             DATA_FINE_EFF_STUDIO_FATT = clean_data_dmy(DATA_FINE_EFF_STUDIO_FATT),
             DATA_INIZIO_EFF_STUDIO_FATT = clean_data_dmy(DATA_INIZIO_EFF_STUDIO_FATT))
    
  }
  
  message("Se ci sono 12 waring su clean_data va bene perché è il numero delle variabili e il warning indica date NA in input")

  # appo <- appo0 %>% 
  #   mutate(DATA_FINE_EFF_ESECUZIONE = clean_data(DATA_FINE_EFF_ESECUZIONE),
  #          DATA_INIZIO_EFF_ESECUZIONE = clean_data(DATA_INIZIO_EFF_ESECUZIONE),
  #          DATA_FINE_EFF_STIP_ATTRIB = clean_data(DATA_FINE_EFF_STIP_ATTRIB),
  #          DATA_INIZIO_EFF_STIP_ATTRIB = clean_data(DATA_INIZIO_EFF_STIP_ATTRIB),
  #          # DATA_FINE_EFF_AGG_BANDO,
  #          # DATA_INIZIO_EFF_AGG_BANDO,
  #          DATA_FINE_EFF_PROG_ESEC = clean_data(DATA_FINE_EFF_PROG_ESEC),
  #          DATA_INIZIO_EFF_PROG_ESEC = clean_data(DATA_INIZIO_EFF_PROG_ESEC),
  #          DATA_FINE_EFF_PROG_DEF = clean_data(DATA_FINE_EFF_PROG_DEF),
  #          DATA_INIZIO_EFF_PROG_DEF = clean_data(DATA_INIZIO_EFF_PROG_DEF),
  #          DATA_FINE_EFF_PROG_PREL = clean_data(DATA_FINE_EFF_PROG_PREL),
  #          DATA_INIZIO_EFF_PROG_PREL = clean_data(DATA_INIZIO_EFF_PROG_PREL), 
  #          DATA_FINE_EFF_STUDIO_FATT = clean_data(DATA_FINE_EFF_STUDIO_FATT),
  #          DATA_INIZIO_EFF_STUDIO_FATT = clean_data(DATA_INIZIO_EFF_STUDIO_FATT))
  # MEMO: recupera solo le variabili che non sono gia presenti in df
  
  out <- appo %>%
    mutate(CHK_END = case_when(# DATA_FINE_EFF_COLLAUDO <= chk_today ~ 1,
                               # DATA_INIZIO_EFF_COLLAUDO <= chk_today ~ 1,
                               DATA_FINE_EFF_ESECUZIONE <= chk_today ~ 1,
                               TRUE ~ 0),
      CHK_ESEC = case_when(DATA_INIZIO_EFF_ESECUZIONE <= chk_today ~ 1,
                           DATA_FINE_EFF_STIP_ATTRIB <= chk_today ~ 1, # MEMO: da portare sotto...? altrimenti resta classe GARA quasi vuota
                           # DATA_FINE_EFF_AGG_BANDO <= chk_today ~ 1,
                           # is.na(DATA_INIZIO_EFF_ESECUZIONE) ~ 0,
                           TRUE ~ 0),
      CHK_GARA = case_when(DATA_INIZIO_EFF_STIP_ATTRIB <= chk_today ~ 1,
                           DATA_FINE_EFF_PROG_ESEC <= chk_today ~ 1, # MEMO: allineamento a regola OC
                           # DATA_FINE_EFF_AGG_BANDO <= chk_today ~ 1,
                           # DATA_INIZIO_EFF_AGG_BANDO <= chk_today ~ 1,
                           # DATA_FINE_EFF_PROG_ESEC <= chk_today ~ 1,
                           TRUE ~ 0),
      # MEMO: blocco su progettazione presente solo per le opere
      CHK_PROG = case_when( # as.POSIXct(DATA_FINE_EFF_PROG_ESEC) <= chk_today ~ 1, # MEMO: allineamento a regola OC
                           as.POSIXct(DATA_INIZIO_EFF_PROG_ESEC) <= chk_today ~ 1, 
                           as.POSIXct(DATA_FINE_EFF_PROG_DEF) <= chk_today ~ 1,
                           as.POSIXct(DATA_INIZIO_EFF_PROG_DEF) <= chk_today ~ 1,
                           as.POSIXct(DATA_FINE_EFF_PROG_PREL) <= chk_today ~ 1,
                           as.POSIXct(DATA_INIZIO_EFF_PROG_PREL) <= chk_today ~ 1,
                           as.POSIXct(DATA_FINE_EFF_STUDIO_FATT) <= chk_today ~ 1, # MEMO: allineamento a regola OC
                           # DATA_FINE_EFF_STUDIO_FATT <= chk_today ~ 1,
                           # DATA_INIZIO_EFF_STUDIO_FATT <= chk_today ~ 1,
                           TRUE ~ 0),
      CHK_AVVP = case_when(# DATA_FINE_EFF_STUDIO_FATT <= chk_today ~ 1, # MEMO: allineamento a regola OC
                           DATA_INIZIO_EFF_STUDIO_FATT <= chk_today ~ 1,
                           TRUE ~ 0)) %>%
    mutate(STATO_PROCED = case_when(CHK_END == 1 ~ "Eseguito",
                                    CHK_ESEC == 1 ~ "In esecuzione",
                                    CHK_GARA == 1 ~ "In affidamento",
                                    CHK_PROG == 1 ~ "In corso di progettazione",
                                    CHK_AVVP == 1 ~ "In avvio di progettazione",
                                    # IMPEGNI > 0 ~ "esecuzione", # MEMO: assegnazione forzata per risolvere anomalie
                                    TRUE ~ "Non avviato")) %>%
    mutate(STATO_PROCED = factor(STATO_PROCED, levels = c("Non avviato", "In avvio di progettazione", "In corso di progettazione", 
                                                          "In affidamento", "In esecuzione", "Eseguito")))
  
  out <- df %>%
    left_join(out %>%
                select(COD_LOCALE_PROGETTO, STATO_PROCED),
              by = "COD_LOCALE_PROGETTO")
  
  print(out %>% count(STATO_PROCED))
  
  return(out)
}



#' Carica dati di base per PSC
#'
#' Carica dati di base per PSC
#'
#' @param bimestre Bimestre di riferimento
#' @param versione Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param fix_no_temi_no_coe Logico. Vuoi scartare i progetti con tema missing e finanziamenti pari a 0?
#' @return Dataframe
load_progetti_psc <- function(bimestre, versione, fix_no_temi_no_coe=FALSE) {
  
  # progetti_psc <- read_csv2(file.path(PSC, "psc", paste0("dati_psc_", bimestre, "_", versione, ".csv")))
  progetti_psc <- read_csv2(file.path(DRIVE, "DATI", "PSC", "psc", paste0("dati_psc_", bimestre, "_", versione, ".csv"))) # MEMO:così è più generica, non serve psc_init()
  
  if (fix_no_temi_no_coe == TRUE) {
    progetti_psc <- progetti_psc %>% 
      filter(!(AREA_TEMATICA == "NA-NA" & COE == 0))
  } 
  
  return(progetti_psc)
}



#' Report PSC e PO d'origine
#'
#' Crea report di confronto programmazione attuazione per PSC e PO d'origine
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO d'origine. Le risorse delle sezioni speciali, nuove o da riprogrammazione, sono in righe separate.
#' @note Contiene patch per incorporare patti città metro anche se non sono PSC in DBCOE
make_report_po_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, export=FALSE, export_xls=FALSE) {
  
  # DEV: uso matrice_po_psc senza dichiararla!!!
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>% 
    select(-SEZIONE)
  
  if (is.null(programmazione)) {
    # OLD:
    # programmazione <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx")) %>% 
    #   select(ID_PSC, PSC, CICLO_PROGRAMMAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, FINANZ_TOTALE)
    
    # NEW:
    programmazione <- init_programmazione_dati(use_713=TRUE, use_sog = TRUE, use_articolaz=TRUE, use_po_psc=TRUE) %>%
      filter(AMBITO == "FSC") %>% 
      filter(COD_LIVELLO_1 == "SEZ_ORD" | COD_LIVELLO_1 == "DA_PROGRAMMARE") %>% # MEMO: esclude "da programmare"
      filter(!is.na(ID_PSC)) %>% 
      select(ID_PSC, PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, FINANZ_TOTALE)
    message("programmi caricato")
  
  }
  # OLD:
  # # fix per CIS Taranto (nel PSC i progetti PRA per 320 sono spostati da 713 a 1420)
  # # 2007PU001FA010
  # temp <- programmazione %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2007PU001FA010")
  # 
  # appo <- programmazione %>% 
  #   filter(OC_CODICE_PROGRAMMA != "2007PU001FA010")
  # 
  # programmazione_2 <- appo %>% 
  #   # pra puglia normale
  #   bind_rows(temp %>% 
  #               mutate(# CICLO_PROGRAMMAZIONE = "2007-2013",
  #                      FINANZ_TOTALE = 2318573538)) %>% 
  #   # pra puglia in CIS
  #   bind_rows(temp %>% 
  #               mutate(CICLO_PROGRAMMAZIONE = "2014-2020",
  #                      FINANZ_TOTALE = 320667143))
  # # MEMO: la somma delle due parti del PSC presa dalla Tavola 2 del PSC sballa rispetto al valore nel DB, ma è trascurabile
  # # 2639264638 - 2639240681 = 23957
  # 
  # # sum(programmazione$FINANZ_TOTALE) - sum(programmazione_2$FINANZ_TOTALE)
  # # 23957.45
  # 
  # programmazione <- programmazione_2
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
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
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE) %>% 
    group_by(ID_PSC, PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    # OLD:
    # left_join(appo2 %>%
    # NEW: gestisce match per cicli diversi da 06 e progetti migrati in SGP ma non ancora in BDU
    full_join(appo2 %>%
                select(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA, COE, COE_IMP, COE_CR, COE_PAG, N),
              by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0)))
  
  report %>% count(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, OC_CODICE_PROGRAMMA) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "OC_CODICE_PROGRAMMA"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  # in chk2 solo la parte fittizia della sicilia di direttrici ferroviarie
  
  report <- fix_id_psc_ministeri(report, var1="PSC")
  
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
      write_csv2(report, file.path(TEMP, "report_po_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_po_psc_nodupli.xlsx"))
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_po_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_po_psc_dupli.xlsx"))
    }
  }

  
  
  return(report)
}




#' Report PSC per temi
#'
#' Crea report di confronto programmazione attuazione per PSC e tema
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_temi_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>% 
    select(-SEZIONE)
  
  # OLD:
  # if (is.null(programmazione)) {
  #   programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
  #     filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
  #     rename(ID_PSC = OC_CODICE_PROGRAMMA)
  # }
  # DEV: qui ho già perso articolazione che contiene sezione
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE, use_articolaz = TRUE) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA, SEZIONE = DESCR_LIVELLO_1) %>% 
      filter(SEZIONE != "SEZ_SPEC_1_COVID", SEZIONE != "SEZ_SPEC_2_FS") %>% 
      # sposta CIS su sezione ordinaria
      mutate(SEZIONE = case_when(SEZIONE == "SEZ_CIS_ABR" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_BZP" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_CS" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_MECTPA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NABA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_PA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SARC" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SSOT" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_TA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_VENTO" ~ "SEZ_ORD",
                                 TRUE ~ SEZIONE)) %>% 
      # clean
      select(-COD_RISULTATO_ATTESO, -DESCR_RISULTATO_ATTESO, -COD_LIVELLO_1) 
  }
  
  # fix per ciclo
  progetti_psc <- progetti_psc %>% 
    mutate(x_CICLO = "2014-2020")
  
  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimetre, visualizzati = FALSE, light = TRUE)
  # }
  # 
  # # add totali
  # progetti_psc <- progetti_psc %>% 
  #   left_join(progetti %>% 
  #               select(COD_LOCALE_PROGETTO, CP = OC_FINANZ_TOT_PUB_NETTO), 
  #             by = "COD_LOCALE_PROGETTO")
  
  # fix 
  # progetti_psc <- progetti_psc %>%
  #   mutate(CP = 0)
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }
  
  # temp <- tibble(ID_PSC = c("PSC_BARI", "PSC_BOLOGNA", "PSC_CAGLIARI", "PSC_CATANIA",
  #                           "PSC_FIRENZE", "PSC_GENOVA", "PSC_MESSINA", "PSC_MILANO", 
  #                           "PSC_NAPOLI", "PSC_PALERMO", "PSC_REGGIO_CALABRIA", "PSC_VENEZIA"))
  
  # OLD:
  # appo2 <- appo1 %>% 
  #   # anti_join(temp) %>% 
  #   group_by(ID_PSC, AREA_TEMATICA, x_CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             N = n())
  # DEV: qui perdo settore di intervento
  
  # NEW:
  # temp <- programmazione %>% as_tibble() %>% filter(TIPOLOGIA_AMMINISTRAZIONE == "NAZIONALE") %>% distinct(ID_PSC) %>% mutate(TEMP = 1)
  
  
  if (show_cp == TRUE) {
    appo2 <- appo1 %>% 
      # left_join(temp, by = "ID_PSC") %>%
      # # forza NA su settore di intervento per psc regionali e metropolitani
      # mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
      #                                       TRUE ~ "")) %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE)) %>% 
      # aggiunge valore di default (per ora non ci sono casi riferibili alle sezioni speciali)
      mutate(SEZIONE = "SEZ_ORD")%>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    appo2 <- appo1 %>% 
      # left_join(temp, by = "ID_PSC") %>%
      # # forza NA su settore di intervento per psc regionali e metropolitani
      # mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
      #                                       TRUE ~ "")) %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n()) %>% 
      # aggiunge valore di default (per ora non ci sono casi riferibili alle sezioni speciali)
      mutate(SEZIONE = "SEZ_ORD") %>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # OLD:
  # # confronto programmazione e attuazione
  # report <- programmazione %>% 
  #   # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
  #   #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
  #          AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC) %>% 
  #   group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA) %>% 
  #   summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
  #   # MEMO: que serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
  #   full_join(appo2 %>% 
  #               select(ID_PSC, x_CICLO, AREA_TEMATICA, COE, COE_IMP, COE_CR, COE_PAG, N),
  #             by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA")) %>% 
  #   mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  #   # recupera variabili perse da full_join
  #   left_join(programmazione %>% 
  #               distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
  #             by = "ID_PSC") %>% 
  #   mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
  #          TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
  #   select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA, RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  # fix per nuove codifiche (riporto tutto a codifica fittizia)
  programmazione <- programmazione %>% 
    left_join(matrix_po_psc %>% 
              filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
              select(OC_CODICE_PROGRAMMA, TEMP = ID_PSC) %>%
              rename(ID_PSC = OC_CODICE_PROGRAMMA),
            by = "ID_PSC") %>% 
  mutate(ID_PSC = if_else(is.na(TEMP), ID_PSC, TEMP)) %>% 
  select(-TEMP)
  
  # NEW:
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           SEZIONE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO = DESCR_SETTORE_INTERVENTO_PSC) %>% 
      # toglie NA da settore di intervento
    mutate(SETTORE_INTERVENTO = case_when(is.na(SETTORE_INTERVENTO) ~ "",
                                          TRUE ~ SETTORE_INTERVENTO)) %>% 
    group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2,
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    # recupera variabili perse da full_join
    # QUI GENERA DUPLI
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  if (show_cp == TRUE) {
    report <- report %>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    report <- report%>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # OLD:
  # report %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # appo2 %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  # chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  
  report %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  
  # OLD:
  # crea sezione
  # report <- report %>% 
  #   mutate(SEZIONE = case_when(AREA_TEMATICA == "Risorse da nuove assegnazioni FSC 2014-2020" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da riprogrammazione ex art. 44" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da compensazioni CSR" ~ "Sez. speciale",
  #                              TRUE ~ "Sez. ordinaria"))
  
  # NEW:
  report <- report %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SEZ_ORD" ~ "Sez. ordinaria",
                               SEZIONE == "SEZ_SPEC_1_COVID" ~ "Sez. speciale 1-Covid",
                               SEZIONE == "SEZ_SPEC_2_FS" ~ "Sez. speciale 2-FS",
                               SEZIONE == "CSR" ~ "Da programmare",
                               TRUE ~ "CHK"))
  # fix
  report <- report %>% 
    # fix SIN BRINDISI lato DBCOE
    filter(!(ID_PSC == "PSC_MATTM" & is.na(AREA_TEMATICA) & RISORSE == 0))

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



#' Report PSC per temi con macroarea
#'
#' Crea report di confronto programmazione attuazione per temi con apertura per macroarea
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param operazioni Dataset da load_operazioni()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_temi_macroaree_psc <- function(progetti_psc, operazioni=NULL, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>% 
    select(-SEZIONE) %>%
    # forzo a 0 perché non ha senso rispetto a COE_IMP e COE_PAG, andrebbe riproporzionato per macroarea
    mutate(COE_CR = 0)
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, visualizzati = FALSE)
  }
  
  
  progetti_psc_migrati <- setup_macroaree_psc(progetti_psc, operazioni)
  
  appo <- progetti_psc %>% 
    anti_join(progetti_psc_migrati, 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_CICLO")) %>% 
    bind_rows(progetti_psc_migrati)

  progetti_psc <- appo
  
  # OLD:
  # if (is.null(programmazione)) {
  #   programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
  #     filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
  #     rename(ID_PSC = OC_CODICE_PROGRAMMA)
  # }
  # DEV: qui ho già perso articolazione che contiene sezione
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati_old(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE, 
                                               use_articolaz = TRUE, use_location = TRUE) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA, SEZIONE = DESCR_LIVELLO_1)  %>% 
      filter(SEZIONE != "SEZ_SPEC_1_COVID", SEZIONE != "SEZ_SPEC_2_FS") %>% 
      # sposta CIS su sezione ordinaria
      mutate(SEZIONE = case_when(SEZIONE == "SEZ_CIS_ABR" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_BZP" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_CS" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_MECTPA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NABA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_PA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SARC" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SSOT" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_TA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_VENTO" ~ "SEZ_ORD",
                                 TRUE ~ SEZIONE)) %>% 
      # clean
      select(-COD_RISULTATO_ATTESO, -DESCR_RISULTATO_ATTESO, -COD_LIVELLO_1, x_MACROAREA) 
  }

  # fix per ciclo (lato programmazione è tutto 1420)
  progetti_psc <- progetti_psc %>% 
    mutate(x_CICLO = "2014-2020")
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }

  
  # temp <- tibble(ID_PSC = c("PSC_BARI", "PSC_BOLOGNA", "PSC_CAGLIARI", "PSC_CATANIA",
  #                           "PSC_FIRENZE", "PSC_GENOVA", "PSC_MESSINA", "PSC_MILANO", 
  #                           "PSC_NAPOLI", "PSC_PALERMO", "PSC_REGGIO_CALABRIA", "PSC_VENEZIA"))
  
  # OLD:
  # appo2 <- appo1 %>% 
  #   # anti_join(temp) %>% 
  #   group_by(ID_PSC, AREA_TEMATICA, x_CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             N = n())
  # DEV: qui perdo settore di intervento
  
  if (show_cp == TRUE) {
    # NEW:
    temp <- programmazione %>% as_tibble() %>% filter(TIPOLOGIA_AMMINISTRAZIONE == "NAZIONALE") %>% distinct(ID_PSC) %>% mutate(TEMP = 1)
    appo2 <- appo1 %>% 
      left_join(temp, by = "ID_PSC") %>%
      # forza NA su settore di intervento per psc regionali e metropolitani
      mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
                                            TRUE ~ "")) %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO, x_MACROAREA) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE)) %>% 
      # aggiunge valore di default (per ora non ci sono casi riferibili alle sezioni speciali)
      mutate(SEZIONE = "SEZ_ORD") %>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA, COE, COE_IMP, COE_CR, COE_PAG, N, CP)
    
  } else {
    # NEW:
    temp <- programmazione %>% as_tibble() %>% filter(TIPOLOGIA_AMMINISTRAZIONE == "NAZIONALE") %>% distinct(ID_PSC) %>% mutate(TEMP = 1)
    appo2 <- appo1 %>% 
      left_join(temp, by = "ID_PSC") %>%
      # forza NA su settore di intervento per psc regionali e metropolitani
      mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
                                            TRUE ~ "")) %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO, x_MACROAREA) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n()) %>% 
      # aggiunge valore di default (per ora non ci sono casi riferibili alle sezioni speciali)
      mutate(SEZIONE = "SEZ_ORD") %>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA, COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # OLD:
  # # confronto programmazione e attuazione
  # report <- programmazione %>% 
  #   # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
  #   #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
  #          AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC) %>% 
  #   group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA) %>% 
  #   summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
  #   # MEMO: que serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
  #   full_join(appo2 %>% 
  #               select(ID_PSC, x_CICLO, AREA_TEMATICA, COE, COE_IMP, COE_CR, COE_PAG, N),
  #             by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA")) %>% 
  #   mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  #   # recupera variabili perse da full_join
  #   left_join(programmazione %>% 
  #               distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
  #             by = "ID_PSC") %>% 
  #   mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
  #          TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
  #   select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA, RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  # fix per nuove codifiche (riporto tutto a codifica fittizia)
  programmazione <- programmazione %>% 
    left_join(matrix_po_psc %>% 
                filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
                select(OC_CODICE_PROGRAMMA, TEMP = ID_PSC) %>%
                rename(ID_PSC = OC_CODICE_PROGRAMMA),
              by = "ID_PSC") %>% 
    mutate(ID_PSC = if_else(is.na(TEMP), ID_PSC, TEMP)) %>% 
    select(-TEMP)
  
  # NEW:
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           SEZIONE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO = DESCR_SETTORE_INTERVENTO_PSC, 
           x_MACROAREA) %>% 
    # toglie NA da settore di intervento
    mutate(SETTORE_INTERVENTO = case_when(is.na(SETTORE_INTERVENTO) ~ "",
                                          TRUE ~ SETTORE_INTERVENTO)) %>% 
    group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2,
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO", "x_MACROAREA")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    # recupera variabili perse da full_join
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  if (show_cp == TRUE)  {
    report <- report %>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    report <- report %>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, x_MACROAREA,
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  # OLD:
  # report %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # appo2 %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  # chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  
  report %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  
  # OLD:
  # crea sezione
  # report <- report %>% 
  #   mutate(SEZIONE = case_when(AREA_TEMATICA == "Risorse da nuove assegnazioni FSC 2014-2020" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da riprogrammazione ex art. 44" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da compensazioni CSR" ~ "Sez. speciale",
  #                              TRUE ~ "Sez. ordinaria"))
  
  # NEW:
  report <- report %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SEZ_ORD" ~ "Sez. ordinaria",
                               SEZIONE == "SEZ_SPEC_1_COVID" ~ "Sez. speciale 1-Covid",
                               SEZIONE == "SEZ_SPEC_2_FS" ~ "Sez. speciale 2-FS",
                               SEZIONE == "CSR" ~ "Da programmare",
                               TRUE ~ "CHK"))
  # fix
  report <- report %>% 
    # fix SIN BRINDISI lato DBCOE
    filter(!(ID_PSC == "PSC_MATTM" & is.na(AREA_TEMATICA) & RISORSE == 0))
  
  if (usa_meuro == TRUE) {
    if (show_cp == TRUE)  {
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
      write_csv2(report, file.path(TEMP, "report_temi_macroaree_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_temi_macroaree_psc_nodupli.xlsx"))
      
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_macroaree_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_temi_macroaree_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}



#' Report PSC per temi con stato procedurale
#'
#' Crea report di confronto programmazione attuazione per temi con apertura per stato procedurale
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param  usa_meuro Logico. Vuoi dati in Meuro?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_temi_stato_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE != "SS_1" & SEZIONE != "SS_2" | is.na(SEZIONE)) %>% 
    select(-SEZIONE)
  
  # OLD:
  # if (is.null(programmazione)) {
  #   programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
  #     filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
  #     rename(ID_PSC = OC_CODICE_PROGRAMMA)
  # }
  # DEV: qui ho già perso articolazione che contiene sezione
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE, use_articolaz = TRUE) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA, SEZIONE = DESCR_LIVELLO_1) %>% 
      filter(SEZIONE != "SEZ_SPEC_1_COVID", SEZIONE != "SEZ_SPEC_2_FS") %>% 
      # sposta CIS su sezione ordinaria
      mutate(SEZIONE = case_when(SEZIONE == "SEZ_CIS_ABR" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_BZP" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_CS" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_MECTPA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_NABA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_PA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SARC" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_SSOT" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_TA" ~ "SEZ_ORD",
                                 SEZIONE == "SEZ_CIS_VENTO" ~ "SEZ_ORD",
                                 TRUE ~ SEZIONE)) %>% 
      # clean
      select(-COD_RISULTATO_ATTESO, -DESCR_RISULTATO_ATTESO, -COD_LIVELLO_1) 
  }
  
  
  # fix per ciclo
  progetti_psc <- progetti_psc %>% 
    mutate(x_CICLO = "2014-2020")
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }
  
  # temp <- tibble(ID_PSC = c("PSC_BARI", "PSC_BOLOGNA", "PSC_CAGLIARI", "PSC_CATANIA",
  #                           "PSC_FIRENZE", "PSC_GENOVA", "PSC_MESSINA", "PSC_MILANO", 
  #                           "PSC_NAPOLI", "PSC_PALERMO", "PSC_REGGIO_CALABRIA", "PSC_VENEZIA"))
  
  # OLD:
  # appo2 <- appo1 %>% 
  #   # anti_join(temp) %>% 
  #   group_by(ID_PSC, AREA_TEMATICA, x_CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             N = n())
  # DEV: qui perdo settore di intervento
  
  # NEW:
  # psc_naz <- programmazione %>% as_tibble() %>% filter(TIPOLOGIA_AMMINISTRAZIONE == "NAZIONALE") %>% distinct(ID_PSC) %>% mutate(TEMP = 1)
  appo2 <- appo1 %>% 
    # left_join(psc_naz, by = "ID_PSC") %>%
    # # forza NA su settore di intervento per psc regionali e metropolitani
    # mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
    #                                       TRUE ~ "")) %>% 
    mutate(OC_STATO_PROCEDURALE = case_when(OC_STATO_PROCEDURALE == "Non avviato" ~ "Non_avviato",
                                            OC_STATO_PROCEDURALE == "In avvio di progettazione" ~ "Progettazione",
                                            OC_STATO_PROCEDURALE == "In corso di progettazione" ~ "Progettazione",
                                            OC_STATO_PROCEDURALE == "In affidamento" ~ "Affidamento",
                                            OC_STATO_PROCEDURALE == "In esecuzione" ~ "Esecuzione",
                                            OC_STATO_PROCEDURALE == "Eseguito" ~ "Eseguito")) %>% 
    group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, x_CICLO, OC_STATO_PROCEDURALE) %>%
    summarise(COE = sum(COE, na.rm = TRUE)) %>% 
    mutate(SEZIONE = "SEZ_ORD") # aggiunge valore di default (per ora non ci sono casi riferibili alle sezioni speciali)
  
  appo3 <- appo2 %>% 
    pivot_wider(names_from = OC_STATO_PROCEDURALE, values_from = COE, values_fill = 0)
  
  # OLD:
  # # confronto programmazione e attuazione
  # report <- programmazione %>% 
  #   # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
  #   #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
  #          AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC) %>% 
  #   group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA) %>% 
  #   summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
  #   # MEMO: que serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
  #   full_join(appo2 %>% 
  #               select(ID_PSC, x_CICLO, AREA_TEMATICA, COE, COE_IMP, COE_CR, COE_PAG, N),
  #             by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA")) %>% 
  #   mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  #   # recupera variabili perse da full_join
  #   left_join(programmazione %>% 
  #               distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
  #             by = "ID_PSC") %>% 
  #   mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
  #          TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
  #   select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA, RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  
  # fix per nuove codifiche (riporto tutto a codifica fittizia)
  programmazione <- programmazione %>% 
    left_join(matrix_po_psc %>% 
                filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
                select(OC_CODICE_PROGRAMMA, TEMP = ID_PSC) %>%
                rename(ID_PSC = OC_CODICE_PROGRAMMA),
              by = "ID_PSC") %>% 
    mutate(ID_PSC = if_else(is.na(TEMP), ID_PSC, TEMP)) %>% 
    select(-TEMP)
  
  # NEW:
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           SEZIONE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO = DESCR_SETTORE_INTERVENTO_PSC) %>% 
    # toglie NA da settore di intervento
    mutate(SETTORE_INTERVENTO = case_when(is.na(SETTORE_INTERVENTO) ~ "",
                                          TRUE ~ SETTORE_INTERVENTO)) %>% 
    group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # OLD:
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    # full_join(appo2,
    #           by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    # mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    # mutate(temp = RISORSE - Eseguito - Affidamento - Esecuzione - Progettazione - Non_avviato, # non monitorato
    #        Non_avviato = Non_avviato + temp) %>% 
    # fix temporaneo
    # mutate(Non_avviato =  if_else(Non_avviato < 0, 0, Non_avviato)) %>% 
    full_join(appo2 %>% 
                group_by(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    full_join(appo3,
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    mutate(RISK_NO_MONIT = RISORSE - COE,
           CANTIERABILI = Affidamento + Esecuzione + Eseguito,
           RISK_RITARDO = Progettazione + Non_avviato,
           # RISK_NET = if_else(RISK_NO_MONIT + RISK_RITARDO < 0, 0, RISK_NO_MONIT + RISK_RITARDO)) %>% 
           RISK_NET = RISK_NO_MONIT + RISK_RITARDO) %>% 
    # recupera variabili perse da full_join
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
           RISORSE, COE, RISK_NO_MONIT, Non_avviato, Progettazione, Affidamento, Esecuzione, Eseguito, 
           CANTIERABILI, RISK_RITARDO, RISK_NET)
  
  # NEW: integra valore duplicati
  report <- report %>% 
    left_join(progetti_psc %>% 
                filter(OC_FLAG_VISUALIZZAZIONE == 1) %>% 
                # left_join(psc_naz, by = "ID_PSC") %>%
                # # forza NA su settore di intervento per psc regionali e metropolitani
                # mutate(SETTORE_INTERVENTO = case_when(TEMP == 1 ~ SETTORE_INTERVENTO,
                #                                       TRUE ~ "")) %>% 
                mutate(SEZIONE = "SEZ_ORD") %>% 
                group_by(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
                summarise(COE_DUPLI = sum(COE, na.rm = TRUE)),
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) 
  
  # OLD:
  # report %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # appo2 %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  # chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  
  report %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  
  # OLD:
  # crea sezione
  # report <- report %>% 
  #   mutate(SEZIONE = case_when(AREA_TEMATICA == "Risorse da nuove assegnazioni FSC 2014-2020" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da riprogrammazione ex art. 44" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da compensazioni CSR" ~ "Sez. speciale",
  #                              TRUE ~ "Sez. ordinaria"))
  
  # NEW:
  report <- report %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SEZ_ORD" ~ "Sez. ordinaria",
                               SEZIONE == "SEZ_SPEC_1_COVID" ~ "Sez. speciale 1-Covid",
                               SEZIONE == "SEZ_SPEC_2_FS" ~ "Sez. speciale 2-FS",
                               SEZIONE == "CSR" ~ "Da programmare",
                               TRUE ~ "CHK"))
  # fix
  report <- report %>% 
    # fix SIN BRINDISI lato DBCOE
    filter(!(ID_PSC == "PSC_MATTM" & is.na(AREA_TEMATICA) & RISORSE == 0))
  
  if (usa_meuro == TRUE) {
    report <- report %>% 
      mutate(RISORSE = RISORSE/1000000, 
             COE = COE/1000000,
             RISK_NO_MONIT = RISK_NO_MONIT/1000000,
             Non_avviato = Non_avviato/1000000, 
             Progettazione = Progettazione/1000000, 
             Affidamento = Affidamento/1000000, 
             Esecuzione = Esecuzione/1000000, 
             Eseguito = Eseguito/1000000,
             CANTIERABILI = CANTIERABILI/1000000,
             RISK_RITARDO = RISK_RITARDO/1000000,
             RISK_NET = RISK_NET/1000000,
             COE_DUPLI = COE_DUPLI/1000000)
    
  }
  
  if (visualizzati == TRUE){
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_stato_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_temi_stato_psc_nodupli.xlsx"))
      
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_stato_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      # message("Da implementare")
      write.xlsx(report, file.path(OUTPUT, "report_temi_stato_psc_dupli.xlsx"))
    }
  }
  
  return(report)
}





#' #' Calcola costo realizzato
#' #'
#' #' Calcola costo realizzato con riproporzionamento su risorse coesione
#' #' 
#' #' @param bimestre Bimestre di riferimento
#' #' @param filename Nome file xlsx in DATI > SGP
#' #' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' #' @param matrix_po_psc Matrice di riconciliazione PO - PSC
#' #' @return File "dati_sgp_BIMESTRE.csv" in TEMP 
#' #' @note ...
#' get_costo_realizzato_713_1420 <- function(progetti_psc, progetti, progetti_sgp, matrix_po_psc) {
#'   
#'   # clean sgp
#'   progetti_sgp <- progetti_sgp %>% 
#'     select(COD_LOCALE_PROGETTO, TITOLO, 
#'            DENOMINAZIONE_INTESA, CODICE_STRUMENTO,
#'            FINANZIAMENTO_FSC_NETTO, FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, 
#'            TOTALE_FINANZIAMENTI_PVT, TOTALE_ECONOMIE_PVT, 
#'            TOTALE_FINANZIAMENTI, TOTALE_ECONOMIE,
#'            IMPORTO_NAZIONALE, IMPORTO_REGIONALE, IMPORTO_NON_DEFINITO,
#'            COSTO_REALIZZATO, COSTO_REALIZZATO_1,
#'            IMPEGNI,
#'            PAGAMENTI_TOTALI, PAGAMENTI_FSC) %>% 
#'     mutate(OC_CODICE_PROGRAMMA = case_when(DENOMINAZIONE_INTESA == "ABRUZZO" ~ "33",
#'                                            DENOMINAZIONE_INTESA == "BASILICATA" ~ "37",
#'                                            DENOMINAZIONE_INTESA == "CALABRIA" ~ "38",
#'                                            DENOMINAZIONE_INTESA == "CAMPANIA" ~ "35",
#'                                            DENOMINAZIONE_INTESA == "EMILIA-ROMAGNA" ~ "28",
#'                                            DENOMINAZIONE_INTESA == "FRIULI-VENEZIA GIULIA" ~ "26",
#'                                            DENOMINAZIONE_INTESA == "LAZIO" ~ "32",
#'                                            DENOMINAZIONE_INTESA == "LIGURIA" ~ "27",
#'                                            DENOMINAZIONE_INTESA == "LOMBARDIA" ~ "01",
#'                                            DENOMINAZIONE_INTESA == "MARCHE" ~ "04",
#'                                            DENOMINAZIONE_INTESA == "MOLISE" ~ "34",
#'                                            DENOMINAZIONE_INTESA == "P.A. BOLZANO" ~ "41",
#'                                            DENOMINAZIONE_INTESA == "P.A. TRENTO" ~ "40",
#'                                            DENOMINAZIONE_INTESA == "PIEMONTE" ~ "21",
#'                                            DENOMINAZIONE_INTESA == "PUGLIA" ~ "36",
#'                                            DENOMINAZIONE_INTESA == "SARDEGNA" ~ "05",
#'                                            DENOMINAZIONE_INTESA == "SICILIA" ~ "39",
#'                                            DENOMINAZIONE_INTESA == "TOSCANA" ~ "02",
#'                                            DENOMINAZIONE_INTESA == "UMBRIA" ~ "03",
#'                                            DENOMINAZIONE_INTESA == "VALLE D'AOSTA" ~ "22",
#'                                            DENOMINAZIONE_INTESA == "VENETO" ~ "25"))
#'   
#'   
#'   # calcola quota 713-1420
#'   message("calcola cr per 713 e 1420")
#'   appo <- progetti_psc %>% 
#'     # filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
#'     left_join(progetti %>% 
#'                 select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_PRIVATO_NETTO),
#'               by = "COD_LOCALE_PROGETTO") %>% 
#'     mutate_if(is.numeric, replace_na, replace = 0) %>% 
#'     mutate(FINANZ_TOT = OC_FINANZ_TOT_PUB_NETTO + OC_FINANZ_PRIVATO_NETTO,
#'            # x = OC_FINANZ_STATO_FSC_NETTO/FINANZ_TOT,
#'            x = COE/FINANZ_TOT,
#'            COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x)
#'   
#'   dim(appo)[1] == dim(progetti_psc)[1]
#'   
#'   
#'   appo %>% 
#'     summarise(COE = sum(COE, na.rm = TRUE),
#'               COE_PAG = sum(COE_PAG, na.rm = TRUE),
#'               COSTO_REALIZZATO = sum(COSTO_REALIZZATO, na.rm = TRUE),
#'               OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
#'               OC_FINANZ_TOT_PUB_NETTO = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
#'               COSTO_REALIZZATO_2 = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
#'   # A tibble: 1 x 6
#'   # COE      COE_PAG COSTO_REALIZZATO OC_FINANZ_STATO_FSC_NETTO OC_FINANZ_TOT_PUB_NETTO COSTO_REALIZZATO_2
#'   # <dbl>        <dbl>            <dbl>                     <dbl>                   <dbl>              <dbl>
#'   # 49290164401. 10040154286.     21986749485.              50629969321.            68164313459.       15828065459.
#'   
#'   # report per programma
#'   # appo1 <- appo %>% 
#'   #   group_by(ID_PSC, PSC, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO) %>% 
#'   #   summarise(COE = sum(COE, na.rm = TRUE),
#'   #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
#'   #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
#'   #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
#'   
#'   # integra 06
#'   message("calcola cr per 06")
#'   temp <- progetti_sgp %>% 
#'     mutate(x_CICLO = "2000-2006",
#'            x_PROGRAMMA = paste0("INTESA ", DENOMINAZIONE_INTESA)) %>% 
#'     left_join(matrix_po_psc %>% 
#'                 select(OC_CODICE_PROGRAMMA, ID_PSC, PSC),
#'               by = "OC_CODICE_PROGRAMMA") %>% 
#'     mutate_if(is.numeric, replace_na, replace = 0) %>% 
#'     mutate(FIN_TOT = TOTALE_FINANZIAMENTI - TOTALE_ECONOMIE,
#'            x = FINANZIAMENTO_FSC_NETTO / FIN_TOT,
#'            k = IMPORTO_REGIONALE / (IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO),
#'            COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x * k,
#'            COE = FINANZIAMENTO_FSC_NETTO * k,
#'            COE_IMP = IMPEGNI * x * k,
#'            COE_PAG = PAGAMENTI_TOTALI * x * k)
#'   
#'   dim(temp)[1] == dim(progetti_sgp)[1]
#'   
#'   # temp %>% 
#'   #   summarise(COE = sum(COE, na.rm = TRUE),
#'   #             PAGAMENTI_TOTALI = sum(PAGAMENTI_TOTALI, na.rm = TRUE),
#'   #             PAGAMENTI_FSC = sum(PAGAMENTI_FSC, na.rm = TRUE),
#'   #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO, na.rm = TRUE),
#'   #             FINANZIAMENTO_FSC_NETTO = sum(FINANZIAMENTO_FSC_NETTO, na.rm = TRUE),
#'   #             FINANZIAMENTO_TOTALE_PUBBLICO_NETTO = sum(FINANZIAMENTO_TOTALE_PUBBLICO_NETTO, na.rm = TRUE),
#'   #             IMPORTO_REGIONALE = sum(IMPORTO_REGIONALE, na.rm = TRUE),
#'   #             IMPORTO_NAZIONALE = sum(IMPORTO_NAZIONALE, na.rm = TRUE),
#'   #             IMPORTO_NON_DEFINITO = sum(IMPORTO_NON_DEFINITO, na.rm = TRUE),
#'   #             COSTO_REALIZZATO_2 = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
#'   # temp %>% 
#'   #   summarise(COE = sum(COE, na.rm = TRUE),
#'   #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
#'   #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
#'   #             COSTO_REALIZZATO_2 = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
#'   # 
#'   # temp1 <- temp %>% 
#'   #   group_by(ID_PSC, PSC, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO) %>% 
#'   #   summarise(COE = sum(COE, na.rm = TRUE),
#'   #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
#'   #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
#'   #             COSTO_REALIZZATO = sum(COSTO_REALIZZATO_2, na.rm = TRUE))
#'   
#'   # bind
#'   # appo2 <- appo1 %>%
#'   #   bind_rows(temp1)
#'   message("bind di 713-1420 e 06")
#'   
#'   appo2 <- appo %>%
#'     filter(x_CICLO != "2000-2006") %>% 
#'     bind_rows(temp) %>% 
#'     select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO_2) %>% 
#'     rename(COSTO_REALIZZATO = COSTO_REALIZZATO_2)
#'   
#'   chk <- appo %>% 
#'     bind_rows(temp) %>% 
#'     mutate(CHK = COSTO_REALIZZATO_2 - COE) %>% 
#'     filter(CHK > 1)
#'   
#'   chk %>% count(x_CICLO)
#'   # CHK: queste sono le uniche anomalie?
#'   
#'   # export
#'   message("export")
#'   dim(progetti_psc)[1] == dim(appo2)[1]
#'   
#'   progetti_psc <- progetti_psc %>% 
#'     left_join(appo2, by = "COD_LOCALE_PROGETTO")
#'   
#'   return(progetti_psc)
#'   
#' }
#' 


#' Correzioni dati SGP
#'
#' Correzioni dati SGP per bimestre
#'
#' @param progetti_sgp Dataset progetti PSC da dentro prep_dati_sgp_bimestre()
#' @param bimestre Bimestre di riferimento
#' @return dataframe
#' @note Da usare dentro prep_dati_sgp_bimestre()
fix_progetti_sgp <- function(progetti_sgp, bimestre) {
  
  if (bimestre == "20210630"){
    progetti_sgp <- progetti_sgp %>% 
      mutate(ID_PSC = case_when(COD_LOCALE_PROGETTO == "EMIRIMO25" ~ "PSC_EMILIA-ROMA",
                                COD_LOCALE_PROGETTO == "EMIVUFE04/V" ~ "PSC_EMILIA-ROMA",
                                COD_LOCALE_PROGETTO == "UMBDTFR_09" ~ "PSC_UMBRIA",
                                COD_LOCALE_PROGETTO == "UMBDTFR_10" ~ "PSC_UMBRIA",
                                COD_LOCALE_PROGETTO == "UMBDTFR_15" ~ "PSC_UMBRIA",
                                TRUE ~ ID_PSC))
  }
  
  return(progetti_sgp)
  
}


#' Correzione progetti CIS Taranto in PRA Puglia
#'
#' Correzione progetti CIS Taranto in PRA Puglia, che vengono spostati su ciclo 2014-2020 ma restano nel PRA Puglia
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre()
fix_ciclo_cis_taranto_pra_puglia <- function(progetti_psc) {
  
  progetti_psc <- progetti_psc %>% 
    mutate(x_CICLO = as.character(x_CICLO)) %>% 
    mutate(x_CICLO = case_when(COD_LOCALE_PROGETTO == "1PUFC3.100006" ~ "2014-2020",
                               COD_LOCALE_PROGETTO == "1PUFC5.100029" ~ "2014-2020",
                               COD_LOCALE_PROGETTO == "1PUFC2.600004" ~ "2014-2020",
                               COD_LOCALE_PROGETTO == "1PUFC2.600003" ~ "2014-2020", # CHK: perché manca in interventi CIS?
                               COD_LOCALE_PROGETTO == "1PUFC2.600002" ~ "2014-2020",
                               COD_LOCALE_PROGETTO == "1PUFC2.600001" ~ "2014-2020",
                               TRUE ~ x_CICLO)) %>% 
    refactor_ciclo(.)
  
  return(progetti_psc)
  
}



#' Correzione progetti con OC_FLAG_VISUALIZZAZIONE = 4 in PRA Campania
#'
#' Correzione progetti con OC_FLAG_VISUALIZZAZIONE = 4 in PRA Campania, che sono tutti ripresi perché presenti nel PSC se relativi a compensazioni ambientali e rifiuti (ex OPCM), tranne quello relativo ai debiti di EAV (1MISETPL.EAV01)
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre()
 fix_visualizzati_pra_campania <- function(progetti_psc) {
  
  progetti_psc <- progetti_psc %>% 
    mutate(OC_FLAG_VISUALIZZAZIONE = case_when(OC_CODICE_PROGRAMMA == "2007CA001FA009" &
                                                 OC_FLAG_VISUALIZZAZIONE == 4 & COD_LOCALE_PROGETTO != "1MISETPL.EAV01" ~ 0,
                                               TRUE ~ OC_FLAG_VISUALIZZAZIONE))
  
  return(progetti_psc)
  
}


 #' Correzione progetti con OC_FLAG_VISUALIZZAZIONE = 5 in APQ MARI Marche
 #'
 #' Correzione progetti con OC_FLAG_VISUALIZZAZIONE = 5 in APQ MARI Marche
 #' 
 #' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
 #' @return dataframe
 #' @note Da usare dentro prep_dati_psc_bimestre()
 fix_visualizzati_apq_mari_marche <- function(progetti_psc) {
   
   progetti_psc <- progetti_psc %>% 
     mutate(OC_FLAG_VISUALIZZAZIONE = case_when(OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI04" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI10" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI11NORD" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI11SUD" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI12" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI13" ~ 0,
                                                OC_CODICE_PROGRAMMA == "2007PI004MA007" &
                                                  OC_FLAG_VISUALIZZAZIONE == 5 & COD_LOCALE_PROGETTO == "1MISEMARI14" ~ 0,
                                                TRUE ~ OC_FLAG_VISUALIZZAZIONE))
   
   return(progetti_psc)
   
 }


#' Cambia programma per progetti in assegnazioni di legge PRA Campania
#'
#' Cambia programma per progetti in assegnazioni di legge PRA Campania, che sono ricompresi nelle articolazioni 21 (rifiuti) e 22 (compensazioni ambientali).
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @param progetti Dataset progetti da load_progetti(bimestre, visualizzati = FALSE, light = FALSE), come usato in dentro prep_dati_psc_bimestre()
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre(). Deve seguire fix_visualizzati_pra_campania() altrimenti il fix visualizzati non funziona perché qui cambia il codice programma
fix_assegnazioni_legge_pra_campania <- function(progetti_psc, progetti) {
  
  fix_list <- progetti %>% 
    filter(OC_CODICE_PROGRAMMA == "2007CA001FA009") %>% 
    filter(OC_COD_ARTICOLAZ_PROGRAMMA == "21" | OC_COD_ARTICOLAZ_PROGRAMMA == "22") %>% 
    mutate(OC_CODICE_PROGRAMMA_NEW = case_when(OC_COD_ARTICOLAZ_PROGRAMMA == "21" ~ "OPCM_CAM_A", # rifiuti
                                               OC_COD_ARTICOLAZ_PROGRAMMA == "22" ~ "OPCM_CAM_B", # compensazioni ambientali
                                               TRUE ~ "CHK"),
           x_PROGRAMMA_NEW = case_when(OC_COD_ARTICOLAZ_PROGRAMMA == "21" ~ "CICLO INTEGRATO DEI RIFIUTI", # rifiuti
                                       OC_COD_ARTICOLAZ_PROGRAMMA == "22"  ~ "COMPENSAZIONI AMBIENTALI", # compensazioni ambientali
                                       TRUE ~ "CHK")) %>% 
    select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA_NEW, x_PROGRAMMA_NEW)
  
  # OPCM_CAM_A	CICLO INTEGRATO DEI RIFIUTI
  # OPCM_CAM_B	COMPENSAZIONI AMBIENTALI

  progetti_psc <- progetti_psc %>% 
    left_join(fix_list, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(OC_CODICE_PROGRAMMA  = if_else(is.na(OC_CODICE_PROGRAMMA_NEW), OC_CODICE_PROGRAMMA, OC_CODICE_PROGRAMMA_NEW),
           x_PROGRAMMA  = if_else(is.na(x_PROGRAMMA_NEW), x_PROGRAMMA, x_PROGRAMMA_NEW)) %>% 
    select(-OC_CODICE_PROGRAMMA_NEW, -x_PROGRAMMA_NEW)
  
  return(progetti_psc)
  
}


#' Associa progetti a PSC Turismo
#'
#' Associa progetti a PSC Turismo
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @param progetti Dataset progetti da load_progetti(bimestre, visualizzati = FALSE, light = FALSE), come usato in dentro prep_dati_psc_bimestre()
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre()
fix_sposta_psc_turismo <- function(progetti_psc, progetti) {
  
  fix_list <- progetti %>% 
    filter(OC_CODICE_PROGRAMMA == "2016MBCSGFSC006") %>% 
    mutate(ID_PSC = case_when(COD_STRUMENTO == "FSC_WIFI_I" ~ "PSC_MTUR", # Wi-Fi Italia
                              COD_STRUMENTO == "FSC_IDMS" ~ "PSC_MTUR", # Italia Destination Management System
                              COD_STRUMENTO == "FSC_GDTS" ~ "PSC_MTUR", # Grandi destinazioni per un turismo sostenibile
                              COD_STRUMENTO == "FSC_DT" ~ "PSC_MTUR", # Dashboard turismo
                              # ... "Montagna Italia"
                              # ... AT
                              # TRUE ~ "PSC_MIBACT"),
                              TRUE ~ "PSC_MIC"),
           PSC = case_when(COD_STRUMENTO == "FSC_WIFI_I" ~ "PSC MINISTERO DEL TURISMO", # Wi-Fi Italia
                           COD_STRUMENTO == "FSC_IDMS" ~ "PSC MINISTERO DEL TURISMO", # Italia Destination Management System
                           COD_STRUMENTO == "FSC_GDTS" ~ "PSC MINISTERO DEL TURISMO", # Grandi destinazioni per un turismo sostenibile
                           COD_STRUMENTO == "FSC_DT" ~ "PSC MINISTERO DEL TURISMO", # Dashboard turismo
                           # ... "Montagna Italia"
                           # ... AT
                           TRUE ~ "PSC MINISTERO CULTURA"),
           OC_CODICE_PROGRAMMA = case_when(COD_STRUMENTO == "FSC_WIFI_I" ~ "SCORPORO_TURISMO", # Wi-Fi Italia
                           COD_STRUMENTO == "FSC_IDMS" ~ "SCORPORO_TURISMO", # Italia Destination Management System
                           COD_STRUMENTO == "FSC_GDTS" ~ "SCORPORO_TURISMO", # Grandi destinazioni per un turismo sostenibile
                           COD_STRUMENTO == "FSC_DT" ~ "SCORPORO_TURISMO", # Dashboard turismo
                           # ... "Montagna Italia"
                           # ... AT
                           TRUE ~ OC_CODICE_PROGRAMMA),
           x_PROGRAMMA = case_when(COD_STRUMENTO == "FSC_WIFI_I" ~ "SCORPORO PSC TURISMO DA MIBACT", # Wi-Fi Italia
                                           COD_STRUMENTO == "FSC_IDMS" ~ "SCORPORO PSC TURISMO DA MIBACT", # Italia Destination Management System
                                           COD_STRUMENTO == "FSC_GDTS" ~ "SCORPORO PSC TURISMO DA MIBACT", # Grandi destinazioni per un turismo sostenibile
                                           COD_STRUMENTO == "FSC_DT" ~ "SCORPORO PSC TURISMO DA MIBACT", # Dashboard turismo
                                           # ... "Montagna Italia"
                                           # ... AT
                                           TRUE ~ "PIANO OPERATIVO CULTURA E TURISMO")) %>% 
    select(COD_LOCALE_PROGETTO, ID_PSC_NEW = ID_PSC, PSC_NEW = PSC, OC_CODICE_PROGRAMMA_NEW = OC_CODICE_PROGRAMMA, x_PROGRAMMA_NEW = x_PROGRAMMA)

  
  progetti_psc <- progetti_psc %>% 
    left_join(fix_list, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(ID_PSC  = if_else(is.na(ID_PSC_NEW), ID_PSC, ID_PSC_NEW),
           PSC  = if_else(is.na(PSC_NEW), PSC, PSC_NEW),
           OC_CODICE_PROGRAMMA  = if_else(is.na(OC_CODICE_PROGRAMMA_NEW), OC_CODICE_PROGRAMMA, OC_CODICE_PROGRAMMA_NEW),
           x_PROGRAMMA  = if_else(is.na(x_PROGRAMMA_NEW), x_PROGRAMMA, x_PROGRAMMA_NEW)) %>% 
    select(-ID_PSC_NEW, -PSC_NEW, -OC_CODICE_PROGRAMMA_NEW, -x_PROGRAMMA_NEW)
  
  return(progetti_psc)
  
}




#' Cambia classificazioni tematiche per amministrazioni centrali
#'
#' Cambia classificazioni tematiche per amministrazioni centrali
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @param progetti Dataset progetti da load_progetti(bimestre, visualizzati = FALSE, light = FALSE), come usato in dentro prep_dati_psc_bimestre()
#' @param interventi_cds Settori di intervento definiti in CdS
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre(). 
fix_aree_settori_amm_centrali <- function(progetti_psc, progetti, interventi_cds) {
  
  message("Avvio fix temi per Amminsitrazioni centrali...")
  # progetti_psc <- appo6
  
  # PSC_PCM-SPORT
  # 11.01-STRUTTURE EDUCATIVE E FORMATIVE -> 08.01-EDILIZIA PUBBLICA
  fix_list_sport <- progetti_psc %>% 
    filter(OC_CODICE_PROGRAMMA == "POSPORTFSC") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(SETTORE_INTERVENTO == "08.01-EDILIZIA E SPAZI PUBBLICI" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                              SETTORE_INTERVENTO == "11.01-STRUTTURE EDUCATIVE E FORMATIVE" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(SETTORE_INTERVENTO == "08.01-EDILIZIA E SPAZI PUBBLICI" ~ "08-RIQUALIFICAZIONE URBANA",
                                         SETTORE_INTERVENTO == "11.01-STRUTTURE EDUCATIVE E FORMATIVE" ~ "08-RIQUALIFICAZIONE URBANA",
                                         TRUE ~ "CHK")) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  
  # PSC_MUR
  # 01.02-STRUTTURE DI RICERCA resta vuoto ma forse è PRATT8293	Avviso PIR -Potenziamento Infrastrutture Ricerca
  fix_list_mur <- progetti_psc %>% 
    filter(OC_CODICE_PROGRAMMA == "2017FSCRICERCA") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_LOCALE_PROGETTO == "1MISEATARS" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_LOCALE_PROGETTO == "1MISEFSCSISTEMA2" ~ "12.02-ASSISTENZA TECNICA",
                                              # COD_PROCED_ATTIVAZIONE == "PRATT8293" ~ "01.02-STRUTTURE DI RICERCA",
                                              # COD_PROCED_ATTIVAZIONE == "PRATT8293:::PRATT8293" ~ "01.02-STRUTTURE DI RICERCA",
                                              COD_PROCED_ATTIVAZIONE == "PRATT23582" ~ "01.02-STRUTTURE DI RICERCA",
                                              TRUE ~ SETTORE_INTERVENTO),
           AREA_TEMATICA_NEW = case_when(COD_LOCALE_PROGETTO == "1MISEATARS" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_LOCALE_PROGETTO == "1MISEFSCSISTEMA2" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         # COD_PROCED_ATTIVAZIONE == "PRATT8293" ~ "01-RICERCA E INNOVAZIONE",
                                         # COD_PROCED_ATTIVAZIONE == "PRATT8293:::PRATT8293" ~ "01-RICERCA E INNOVAZIONE",
                                         COD_PROCED_ATTIVAZIONE == "PRATT23582" ~ "01-RICERCA E INNOVAZIONE",
                                         TRUE ~ AREA_TEMATICA)) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  
  
  
  # PSC_MTUR
  fix_list_mtur <- progetti_psc %>% 
    filter(ID_PSC == "PSC_MTUR") %>% 
    # semi_join(progetti_psc %>% 
    #             filter(ID_PSC == "PSC_MTUR"), 
    #           by = "COD_LOCALE_PROGETTO") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(SETTORE_INTERVENTO == "03.02-TURISMO E OSPITALITÀ" ~ "03.02-TURISMO E OSPITALITÀ",
                                              SETTORE_INTERVENTO == "06.01-PATRIMONIO E PAESAGGIO" ~ "03.02-TURISMO E OSPITALITÀ",
                                              SETTORE_INTERVENTO == "06.02-ATTIVITÀ CULTURALI" ~ "03.02-TURISMO E OSPITALITÀ",
                                              SETTORE_INTERVENTO == "05.05-NATURA E BIODIVERSITÀ" ~ "03.02-TURISMO E OSPITALITÀ",
                                              TRUE ~ SETTORE_INTERVENTO), # OLD: "CHK"
           AREA_TEMATICA_NEW = case_when(SETTORE_INTERVENTO == "03.02-TURISMO E OSPITALITÀ" ~ "03-COMPETITIVITÀ IMPRESE",
                                         SETTORE_INTERVENTO == "06.01-PATRIMONIO E PAESAGGIO" ~ "03-COMPETITIVITÀ IMPRESE",
                                         SETTORE_INTERVENTO == "06.02-ATTIVITÀ CULTURALI" ~ "03-COMPETITIVITÀ IMPRESE",
                                         SETTORE_INTERVENTO == "05.05-NATURA E BIODIVERSITÀ" ~ "03-COMPETITIVITÀ IMPRESE",
                                         TRUE ~ AREA_TEMATICA)) %>% # OLD: "CHK"
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)

  # PSC_MIT
  # MEMO: va in funzione a parte perché deve lavorare solo su delta che non è in interventi_cds
  # usare soggetto programmatore, poi resta vuoto "dissesto" (tutto in "strade")
  fix_list_mit <- progetti_psc %>%
    filter(OC_CODICE_PROGRAMMA == "2017POINFRASFSC") %>%
    left_join(progetti %>%
                distinct(COD_LOCALE_PROGETTO, OC_DENOM_PROGRAMMATORE),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_LOCALE_PROGETTO == "5MTRA1E7003" ~ "12.02-ASSISTENZA TECNICA",
                                              OC_DENOM_PROGRAMMATORE == "MIT" ~ "12.02-ASSISTENZA TECNICA",
                                              OC_DENOM_PROGRAMMATORE == "MIT - DG DIGHE" ~ "05.02-RISORSE IDRICHE",
                                              OC_DENOM_PROGRAMMATORE == "MIT SVILTER DIV V" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                              OC_DENOM_PROGRAMMATORE == "MIT SVILTER STRUTT" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG AEREO" ~ "07.04-TRASPORTO AEREO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG PORTI" ~ "07.03-TRASPORTO MARITTIMO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" & grepl("ciclovi", OC_TITOLO_PROGETTO) ~ "05.05-NATURA E BIODIVERSITÀ",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" & grepl("ciclab", OC_TITOLO_PROGETTO) ~ "05.05-NATURA E BIODIVERSITÀ",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" ~ "07.05-MOBILITÀ URBANA",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG TPL4" ~ "07.05-MOBILITÀ URBANA",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DG TPL5" ~ "07.05-MOBILITÀ URBANA",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGDIGHE" ~ "05.02-RISORSE IDRICHE",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGFERRO" ~ "07.02-TRASPORTO FERROVIARIO",
                                              # OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" ~ SETTORE_INTERVENTO,
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("frana", OC_TITOLO_PROGETTO) ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("frane", OC_TITOLO_PROGETTO) ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("dissest", OC_TITOLO_PROGETTO) ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("versant", OC_TITOLO_PROGETTO) ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" ~ "07.01-TRASPORTO STRADALE",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DGTPL4" ~ "07.05-MOBILITÀ URBANA",
                                              OC_DENOM_PROGRAMMATORE == "MIT-DIGES" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(COD_LOCALE_PROGETTO == "5MTRA1E7003" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         OC_DENOM_PROGRAMMATORE == "MIT" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         OC_DENOM_PROGRAMMATORE == "MIT - DG DIGHE" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT SVILTER DIV V" ~ "08-RIQUALIFICAZIONE URBANA",
                                         OC_DENOM_PROGRAMMATORE == "MIT SVILTER STRUTT" ~ "08-RIQUALIFICAZIONE URBANA",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG AEREO" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG PORTI" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" & grepl("ciclovi", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" & grepl("ciclab", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG TPL3" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG TPL4" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DG TPL5" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGDIGHE" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGFERRO" ~ "07-TRASPORTI E MOBILITÀ",
                                         # OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" ~ AREA_TEMATICA,
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("frana", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("frane", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("dissest", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" & grepl("versant", OC_TITOLO_PROGETTO) ~ "05-AMBIENTE E RISORSE NATURALI",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGSTRADE" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DGTPL4" ~ "07-TRASPORTI E MOBILITÀ",
                                         OC_DENOM_PROGRAMMATORE == "MIT-DIGES" ~ "08-RIQUALIFICAZIONE URBANA",
                                         TRUE ~ "CHK")) %>% 
    anti_join(interventi_cds, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>%
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW) 

  # PSC_MISE
  fix_list_mise <- progetti_psc %>% 
    filter(OC_CODICE_PROGRAMMA == "2017POIMPCOMFSC" | OC_CODICE_PROGRAMMA == "2018FSCVOUCHER" | 
             OC_CODICE_PROGRAMMA == "FONDOGARANFSC" | OC_CODICE_PROGRAMMA == "2015MSIAIFSC011" | 
             OC_CODICE_PROGRAMMA == "2016MISEBULFSC1") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(# 2017POIMPCOMFSC (piano imprese)
                                              COD_PROCED_ATTIVAZIONE == "PRATT12954" ~ "01.01-RICERCA E SVILUPPO", # Piano Space Economy
                                              COD_PROCED_ATTIVAZIONE == "PRATT19931" ~ "02.02-CONNETTIVITÀ DIGITALE", # BUL Bolzano
                                              COD_PROCED_ATTIVAZIONE == "PRATT20376" ~ "12.02-ASSISTENZA TECNICA", # Progetto Blockchain
                                              COD_PROCED_ATTIVAZIONE == "PRATT20379" ~ "12.02-ASSISTENZA TECNICA", # 	AT - Capacità strumentale
                                              COD_PROCED_ATTIVAZIONE == "PRATT20378" ~ "12.02-ASSISTENZA TECNICA", # 	AT CDS FSC
                                              COD_PROCED_ATTIVAZIONE == "PRATT7716" ~ "12.02-ASSISTENZA TECNICA", # Convenzione MISE-Invitalia FSC
                                              COD_PROCED_ATTIVAZIONE == "PRATT20377" ~ "12.02-ASSISTENZA TECNICA", #	AT - Space Economy
                                              OC_CODICE_PROGRAMMA == "2017POIMPCOMFSC" ~ "03.01-INDUSTRIA E SERVIZI",
                                              # 2016MISEBULFSC1 (piano BUL)
                                              COD_PROCED_ATTIVAZIONE == "PRATT28156" ~ "02.01-TECNOLOGIE E SERVIZI DIGITALI", # Voucher fase 1
                                              COD_PROCED_ATTIVAZIONE == "PRATT20393" ~ "02.01-TECNOLOGIE E SERVIZI DIGITALI", # 	Radio Monitoring
                                              OC_CODICE_PROGRAMMA == "2016MISEBULFSC1" ~ "02.02-CONNETTIVITÀ DIGITALE",
                                              # altri programmi
                                              OC_CODICE_PROGRAMMA == "2018FSCVOUCHER" ~ "02.01-TECNOLOGIE E SERVIZI DIGITALI",
                                              OC_CODICE_PROGRAMMA == "FONDOGARANFSC" ~ "03.01-INDUSTRIA E SERVIZI",
                                              OC_CODICE_PROGRAMMA == "2015MSIAIFSC011" ~ "03.01-INDUSTRIA E SERVIZI",
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(# 2017POIMPCOMFSC (piano imprese)
                                         COD_PROCED_ATTIVAZIONE == "PRATT12954" ~ "01-RICERCA E INNOVAZIONE",
                                         COD_PROCED_ATTIVAZIONE == "PRATT19931" ~ "02-DIGITALIZZAZIONE",
                                         COD_PROCED_ATTIVAZIONE == "PRATT20376" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_PROCED_ATTIVAZIONE == "PRATT20379" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_PROCED_ATTIVAZIONE == "PRATT20378" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_PROCED_ATTIVAZIONE == "PRATT7716" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_PROCED_ATTIVAZIONE == "PRATT20377" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         OC_CODICE_PROGRAMMA == "2017POIMPCOMFSC" ~ "03-COMPETITIVITÀ IMPRESE",
                                         # 2016MISEBULFSC1 (piano BUL)
                                         OC_CODICE_PROGRAMMA == "2016MISEBULFSC1" ~ "02-DIGITALIZZAZIONE",
                                         # altri programmi
                                         OC_CODICE_PROGRAMMA == "2018FSCVOUCHER" ~ "02-DIGITALIZZAZIONE",
                                         OC_CODICE_PROGRAMMA == "FONDOGARANFSC" ~ "03-COMPETITIVITÀ IMPRESE",
                                         OC_CODICE_PROGRAMMA == "2015MSIAIFSC011" ~ "03-COMPETITIVITÀ IMPRESE",
                                         TRUE ~ "CHK")) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  
  
  
  # PSC_MISALUTE
  fix_list_salute <- progetti_psc %>% 
    filter(OC_CODICE_PROGRAMMA == "2018POSALUTEFSC") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_LOCALE_PROGETTO == "2MSALDG02_12818_AT_FSC_POSALUTE" ~ "12.02-ASSISTENZA TECNICA",
                                              TRUE ~ "01.01-RICERCA E SVILUPPO"),
           AREA_TEMATICA_NEW = case_when(COD_LOCALE_PROGETTO == "2MSALDG02_12818_AT_FSC_POSALUTE" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         TRUE ~ "01-RICERCA E INNOVAZIONE")) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  # PSC_MIPAAF
  # 05.02-RISORSE IDRICHE -> 03.03-AGRICOLTURA
  # 05.04-BONIFICHE -> 03.03-AGRICOLTURA
  # OLD:
  # fix_list_mipaaf <- progetti_psc %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2017POAGRICOFSC") %>% 
  #   mutate(SETTORE_INTERVENTO_NEW = case_when(SETTORE_INTERVENTO == "03.03-AGRICOLTURA" ~ "03.03-AGRICOLTURA",
  #                                             SETTORE_INTERVENTO == "05.02-RISORSE IDRICHE" ~ "03.03-AGRICOLTURA",
  #                                             SETTORE_INTERVENTO == "05.04-BONIFICHE" ~ "03.03-AGRICOLTURA",
  #                                             SETTORE_INTERVENTO == "03.01-INDUSTRIA E SERVIZI" ~ "03.01-INDUSTRIA E SERVIZI",
  #                                             TRUE ~ "CHK"),
  #          AREA_TEMATICA_NEW = case_when(SETTORE_INTERVENTO == "03.03-AGRICOLTURA" ~ "03-COMPETITIVITA' IMPRESE",
  #                                        SETTORE_INTERVENTO == "05.02-RISORSE IDRICHE" ~ "03-COMPETITIVITA' IMPRESE",
  #                                        SETTORE_INTERVENTO == "05.04-BONIFICHE" ~ "03-COMPETITIVITA' IMPRESE",
  #                                        SETTORE_INTERVENTO == "03.01-INDUSTRIA E SERVIZI" ~ "03-COMPETITIVITA' IMPRESE",
  #                                        TRUE ~ "CHK")) %>% 
  #   select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  # NEW:
  fix_list_mipaaf <- progetti_psc %>% 
    filter(OC_CODICE_PROGRAMMA == "2017POAGRICOFSC") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_PROCED_ATTIVAZIONE == "PRATT18580" ~ "03.03-AGRICOLTURA", # Interventi in infrastrutture irrigue
                                              COD_PROCED_ATTIVAZIONE == "PRATT20984" ~ "03.03-AGRICOLTURA", # Interventi in infrastrutture irrigue
                                              COD_PROCED_ATTIVAZIONE == "PRATT6940" ~ "03.03-AGRICOLTURA", # Agricoltura 2.0
                                              COD_PROCED_ATTIVAZIONE == "PRATT17968" ~ "03.01-INDUSTRIA E SERVIZI", # IV Bando nazionale - DM 1192 contratti di filiera e distretto
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(OC_CODICE_PROGRAMMA == "2017POAGRICOFSC" ~ "03-COMPETITIVITÀ IMPRESE",
                                         TRUE ~ "CHK")) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  # PSC_MIBACT
  # 05.05-NATURA E BIODIVERSITA' -> 06.01-PATRIMONIO E PAESAGGIO (ma controllare progetti)
  # 06.02-ATTIVITA’ CULTURALI -> 06.01-PATRIMONIO E PAESAGGIO (ma controllare progetti)
  # 03.02-TURISMO E OSPITALITA’ in overbooking anche senza PSC_MTUR
  fix_list_mibact <- progetti_psc %>% 
    semi_join(progetti_psc %>% 
                # filter(ID_PSC == "PSC_MIBACT"), 
                filter(ID_PSC == "PSC_MIC"), 
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(SETTORE_INTERVENTO_NEW = case_when(SETTORE_INTERVENTO == "03.02-TURISMO E OSPITALITÀ" ~ "03.02-TURISMO E OSPITALITÀ",
                                              SETTORE_INTERVENTO == "06.01-PATRIMONIO E PAESAGGIO" ~ "06.01-PATRIMONIO E PAESAGGIO",
                                              SETTORE_INTERVENTO == "06.02-ATTIVITÀ CULTURALI" ~ "06.01-PATRIMONIO E PAESAGGIO",
                                              SETTORE_INTERVENTO == "05.05-NATURA E BIODIVERSITÀ" ~ "03.02-TURISMO E OSPITALITÀ",
                                              SETTORE_INTERVENTO == "12.02-ASSISTENZA TECNICA" ~ "12.02-ASSISTENZA TECNICA",
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(SETTORE_INTERVENTO == "03.02-TURISMO E OSPITALITÀ" ~ "03-COMPETITIVITÀ IMPRESE",
                                         SETTORE_INTERVENTO == "06.01-PATRIMONIO E PAESAGGIO" ~ "06-CULTURA",
                                         SETTORE_INTERVENTO == "06.02-ATTIVITÀ CULTURALI" ~ "06-CULTURA",
                                         SETTORE_INTERVENTO == "05.05-NATURA E BIODIVERSITÀ" ~ "03-COMPETITIVITÀ IMPRESE",
                                         SETTORE_INTERVENTO == "12.02-ASSISTENZA TECNICA" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         TRUE ~ "CHK")) %>% 
    select(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  
  # PSC_MI
  # ok
  
  # PSC_MATTM
  # mapping su strumento attuativo
  fix_list_mattm <- progetti %>% 
    filter(OC_CODICE_PROGRAMMA == "2017POAMBIENFSC") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_LOCALE_PROGETTO == "1MATTMDGCLE_24_0067" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_LOCALE_PROGETTO == "1MATTMDGRIN_21_0027" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_0269" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_0270" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_23_25_0001" ~ "12.02-ASSISTENZA TECNICA",
                                              COD_STRUMENTO == "POFSCMAC25" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO", 
                                              COD_STRUMENTO == "POFSCPNM25" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO", 
                                              COD_STRUMENTO == "POFSCSTA25" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO", 
                                              COD_STRUMENTO == "POFSCRIN21" ~ "05.03-RIFIUTI", 
                                              COD_STRUMENTO == "POFSCSTA23" ~ "05.04-BONIFICHE", 
                                              COD_STRUMENTO == "POFSCCLE24" ~ "04.01-EFFICIENZA ENERGETICA", 
                                              COD_STRUMENTO == "POFSCSTA22" ~ "05.02-RISORSE IDRICHE", 
                                              TRUE ~ "CHK"),
           AREA_TEMATICA_NEW = case_when(COD_LOCALE_PROGETTO == "1MATTMDGCLE_24_0067" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_LOCALE_PROGETTO == "1MATTMDGRIN_21_0027" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_0269" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_0270" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_LOCALE_PROGETTO == "1MATTMDGSTA_22_23_25_0001" ~ "12-CAPACITÀ AMMINISTRATIVA",
                                         COD_STRUMENTO == "POFSCMAC25" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         COD_STRUMENTO == "POFSCPNM25" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         COD_STRUMENTO == "POFSCSTA25" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         COD_STRUMENTO == "POFSCRIN21" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         COD_STRUMENTO == "POFSCSTA23" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         COD_STRUMENTO == "POFSCCLE24" ~ "04-ENERGIA", 
                                         COD_STRUMENTO == "POFSCSTA22" ~ "05-AMBIENTE E RISORSE NATURALI", 
                                         TRUE ~ "CHK")) %>% 
    distinct(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)

  # fix per piano stralcio dissesto con progetti su bonifiche
  fix_list_mattm_713 <- progetti %>% 
    filter(OC_CODICE_PROGRAMMA == "2016XXAMPSAP00") %>% 
    mutate(SETTORE_INTERVENTO_NEW = case_when(COD_LOCALE_PROGETTO == "1MISE09IR010/G4_2" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              COD_LOCALE_PROGETTO == "1MISEABRPSRA-01" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              COD_LOCALE_PROGETTO == "1MISEPSRA03IR001/G4" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              COD_LOCALE_PROGETTO == "1MISEPSRA03IR002/G4" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              COD_LOCALE_PROGETTO == "1MISEPSRA03IR004/G4" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              COD_LOCALE_PROGETTO == "1MISEPSRA03IR006/G4" ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO",
                                              TRUE ~ "05.01-RISCHI E ADATTAMENTO CLIMATICO"),
           AREA_TEMATICA_NEW = case_when(COD_LOCALE_PROGETTO == "1MISE09IR010/G4_2" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         COD_LOCALE_PROGETTO == "1MISEABRPSRA-01" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         COD_LOCALE_PROGETTO == "1MISEPSRA03IR001/G4" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         COD_LOCALE_PROGETTO == "1MISEPSRA03IR002/G4" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         COD_LOCALE_PROGETTO == "1MISEPSRA03IR004/G4" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         COD_LOCALE_PROGETTO == "1MISEPSRA03IR006/G4" ~ "05-AMBIENTE E RISORSE NATURALI",
                                         TRUE ~ "05-AMBIENTE E RISORSE NATURALI")) %>% 
    distinct(COD_LOCALE_PROGETTO, SETTORE_INTERVENTO_NEW, AREA_TEMATICA_NEW)
  
  # unione fix
  fix_list <- fix_list_mattm %>% 
    bind_rows(fix_list_mattm_713) %>% 
    bind_rows(fix_list_mtur) %>% 
    bind_rows(fix_list_mibact) %>% 
    bind_rows(fix_list_sport) %>% 
    bind_rows(fix_list_mipaaf) %>% 
    bind_rows(fix_list_mur) %>% 
    bind_rows(fix_list_mit) %>%
    bind_rows(fix_list_salute) %>% 
    bind_rows(fix_list_mise)
  
  # chk duplicati
  temp <- fix_list %>% count(COD_LOCALE_PROGETTO) %>% filter(n > 1)
  message("Controlla se ci sono duplicati in fix_list:")
  print(temp)
  
  # chk NA
  temp <- fix_list %>% filter(AREA_TEMATICA_NEW == "CHK") %>% 
    left_join(progetti_psc %>% 
                distinct(COD_LOCALE_PROGETTO, ID_PSC)) %>% 
    count(ID_PSC)
  message("Controlla se ci sono errori segnati con CHK in fix_list:")
  print(temp)
  
  progetti_psc <- progetti_psc %>% 
    left_join(fix_list, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(SETTORE_INTERVENTO  = if_else(is.na(SETTORE_INTERVENTO_NEW), SETTORE_INTERVENTO, SETTORE_INTERVENTO_NEW),
           AREA_TEMATICA  = if_else(is.na(AREA_TEMATICA_NEW), AREA_TEMATICA, AREA_TEMATICA_NEW)) %>% 
    select(-SETTORE_INTERVENTO_NEW, -AREA_TEMATICA_NEW)
  
  return(progetti_psc)
  
}


#' Correzione macroarea per progetti anomali
#'
#' Correzione macroarea per progetti anomali
#'
#' @param progetti_psc Dataset progetti PSC da dentro prep_dati_psc_bimestre()
#' @return dataframe
#' @note Da usare dentro prep_dati_psc_bimestre()
fix_macroarea_progetti_psc <- function(progetti_psc) {
  
  progetti_psc <- progetti_psc %>% 
    mutate(x_MACROAREA = as.character(x_MACROAREA)) %>% 
    mutate(x_MACROAREA = case_when(COD_LOCALE_PROGETTO == "4MISEF/130052/01/X38" ~ "Mezzogiorno",
                                   COD_LOCALE_PROGETTO == "1MISE761" ~ "Mezzogiorno",
                                   # progetti patto campania gestiti da MISE (su ambito nazionale)
                                   OC_CODICE_PROGRAMMA == "2016PATTICAMP" & COD_LOCALE_PROGETTO == "4MISECDS000605_RS1_1_2_3" ~ "Mezzogiorno",
                                   OC_CODICE_PROGRAMMA == "2016PATTICAMP" & COD_LOCALE_PROGETTO == "4MISECDS000609_0_RS1_1_R1_2" ~ "Mezzogiorno",
                                   OC_CODICE_PROGRAMMA == "2016PATTICAMP" & COD_LOCALE_PROGETTO == "4MISEFONDO_CDS_FSC" ~ "Mezzogiorno",
                                   TRUE ~ x_MACROAREA)) %>% 
    refactor_macroarea(.)
  
  return(progetti_psc)
  

}


#' Controlla report PSC
#'
#' Controlla allineamento tra report PSC
#' 
#' @param progetti_psc Lista progetti PSC da load_progetti_psc()
#' @return Lista con esito controlli
#' @note Il label nella lsita indica la variabile controllata (RISORSE o COE) e i report esaminati nell'ordine della sottrazione.
chk_report_psc <- function(progetti_psc) {
  
  # report po-psc
  report1 <- make_report_po_psc(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=TRUE, 
                                       export=FALSE, export_xls=FALSE) 
  
  # report temi-psc
  report2 <- make_report_temi_psc(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=TRUE, 
                                         show_cp = TRUE, export=FALSE, export_xls=FALSE) 
  
  # report temi-psc-macorarea
  report3 <- make_report_temi_macroaree_psc(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=TRUE, export=FALSE, export_xls=FALSE) 
  
  # report temi-psc-stato
  report4 <- make_report_temi_stato_psc(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=TRUE, export=FALSE, export_xls=FALSE) 
  
  
  # --------------------------------------------------------------------------------------------------- #
  # controlli
  
  memo <- list()
  
  chk <- report1 %>% 
    group_by(ID_PSC) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    full_join(report2 %>% 
                group_by(ID_PSC) %>% 
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)), 
              by = "ID_PSC") %>% 
    mutate(CHK = RISORSE.x - RISORSE.y)
  memo[["risorse_popsc_vs_temi"]] <- sum(chk$CHK, na.rm = TRUE)
  # -0.03167309 fisiologico per arrotondamenti lato temi
  
  chk <- report1 %>% 
    group_by(ID_PSC) %>% 
    summarise(COE = sum(COE, na.rm = TRUE)) %>% 
    full_join(report2 %>% 
                group_by(ID_PSC) %>% 
                summarise(COE = sum(COE, na.rm = TRUE)), 
              by = "ID_PSC") %>% 
    mutate(CHK = COE.x - COE.y)
  memo[["coe_popsc_vs_temi"]] <- sum(chk$CHK, na.rm = TRUE)
  # delta trascurabile
  
  chk <- report3 %>% 
    group_by(ID_PSC) %>% 
    summarise(COE = sum(COE, na.rm = TRUE)) %>% 
    full_join(report2 %>% 
                group_by(ID_PSC) %>% 
                summarise(COE = sum(COE, na.rm = TRUE)), 
              by = "ID_PSC") %>% 
    mutate(CHK = COE.x - COE.y)
  memo[["coe_macroaree_vs_temi"]]  <- sum(chk$CHK, na.rm = TRUE)
  # delta trascurabile
  
  chk <- report4 %>% 
    group_by(ID_PSC) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    full_join(report2 %>% 
                group_by(ID_PSC) %>% 
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)), 
              by = "ID_PSC") %>% 
    mutate(CHK = RISORSE.x - RISORSE.y)
  memo[["risorse_stato_vs_temi"]] <- sum(chk$CHK, na.rm = TRUE)
  # 0
  
  return(memo)
  
}


#' Report PSC per temi con sezioni speciali 
#'
#' Crea report di confronto programmazione attuazione per PSC e tema su sezione speciale 1
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param usa_meuro Logico. Vuoi dati in Meuro?
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_sezioni_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, show_cp=FALSE, export=FALSE, export_xls=FALSE) {
  
  # isola sezione ordinaria
  progetti_psc <- progetti_psc %>% 
    filter(SEZIONE == "SS_1" | SEZIONE == "SS_2") %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SS_1" ~ "SEZ_SPEC_1_COVID",
                               SEZIONE == "SS_2" ~ "SEZ_SPEC_2_FS"))
  
  # OLD:
  # if (is.null(programmazione)) {
  #   programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
  #     filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
  #     rename(ID_PSC = OC_CODICE_PROGRAMMA)
  # }
  # DEV: qui ho già perso articolazione che contiene sezione
  
  # NEW:
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE, use_articolaz = TRUE) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA, SEZIONE = DESCR_LIVELLO_1) %>% 
      filter(SEZIONE == "SEZ_SPEC_1_COVID" | SEZIONE == "SEZ_SPEC_2_FS") %>% 
      # clean
      select(-COD_RISULTATO_ATTESO, -DESCR_RISULTATO_ATTESO, -COD_LIVELLO_1) 
  }
  
  # fix per ciclo
  progetti_psc <- progetti_psc %>% 
    mutate(x_CICLO = "2014-2020")
  
  # if (is.null(progetti)) {
  #   progetti <- load_progetti(bimetre, visualizzati = FALSE, light = TRUE)
  # }
  # 
  # # add totali
  # progetti_psc <- progetti_psc %>% 
  #   left_join(progetti %>% 
  #               select(COD_LOCALE_PROGETTO, CP = OC_FINANZ_TOT_PUB_NETTO), 
  #             by = "COD_LOCALE_PROGETTO")
  
  # fix 
  # progetti_psc <- progetti_psc %>%
  #   mutate(CP = 0)
  
  if (visualizzati == TRUE){
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE == 0)
  } else {
    # appo1 <- progetti_psc
    appo1 <- progetti_psc %>% filter(OC_FLAG_VISUALIZZAZIONE != 4 &
                                       OC_FLAG_VISUALIZZAZIONE != 5)
    # MEMO: così scarto solo casi anomali PSC, rilevante per debiti e opcm campania
    
  }
  
  # temp <- tibble(ID_PSC = c("PSC_BARI", "PSC_BOLOGNA", "PSC_CAGLIARI", "PSC_CATANIA",
  #                           "PSC_FIRENZE", "PSC_GENOVA", "PSC_MESSINA", "PSC_MILANO", 
  #                           "PSC_NAPOLI", "PSC_PALERMO", "PSC_REGGIO_CALABRIA", "PSC_VENEZIA"))
  
  # OLD:
  # appo2 <- appo1 %>% 
  #   # anti_join(temp) %>% 
  #   group_by(ID_PSC, AREA_TEMATICA, x_CICLO) %>%
  #   summarise(COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_CR = sum(COE_CR, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE),
  #             N = n())
  # DEV: qui perdo settore di intervento
  
  # NEW:
  # temp <- programmazione %>% as_tibble() %>% filter(TIPOLOGIA_AMMINISTRAZIONE == "NAZIONALE") %>% distinct(ID_PSC) %>% mutate(TEMP = 1)
  
  
  if (show_cp == TRUE) {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n(),
                CP = sum(CP, na.rm = TRUE)) %>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    appo2 <- appo1 %>% 
      group_by(ID_PSC, AREA_TEMATICA, SETTORE_INTERVENTO, SEZIONE, x_CICLO) %>%
      summarise(COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_CR = sum(COE_CR, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N = n()) %>% 
      select(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # OLD:
  # # confronto programmazione e attuazione
  # report <- programmazione %>% 
  #   # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
  #   #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
  #          AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC) %>% 
  #   group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA) %>% 
  #   summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
  #   # MEMO: que serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
  #   full_join(appo2 %>% 
  #               select(ID_PSC, x_CICLO, AREA_TEMATICA, COE, COE_IMP, COE_CR, COE_PAG, N),
  #             by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA")) %>% 
  #   mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  #   # recupera variabili perse da full_join
  #   left_join(programmazione %>% 
  #               distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
  #             by = "ID_PSC") %>% 
  #   mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
  #          TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
  #   select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
  #   select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA, RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  # fix per nuove codifiche (riporto tutto a codifica fittizia)
  programmazione <- programmazione %>% 
    left_join(matrix_po_psc %>% 
                filter(str_starts(OC_CODICE_PROGRAMMA, "PSC")) %>% 
                select(OC_CODICE_PROGRAMMA, TEMP = ID_PSC) %>%
                rename(ID_PSC = OC_CODICE_PROGRAMMA),
              by = "ID_PSC") %>% 
    mutate(ID_PSC = if_else(is.na(TEMP), ID_PSC, TEMP)) %>% 
    select(-TEMP)
  
  # NEW:
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           SEZIONE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO = DESCR_SETTORE_INTERVENTO_PSC) %>% 
    # toglie NA da settore di intervento
    mutate(SETTORE_INTERVENTO = case_when(is.na(SETTORE_INTERVENTO) ~ "",
                                          TRUE ~ SETTORE_INTERVENTO)) %>% 
    group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, 
             SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: qui serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2,
              by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    # recupera variabili perse da full_join
    # QUI GENERA DUPLI
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) 
  
  if (show_cp == TRUE) {
    report <- report %>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N, CP)
  } else {
    report <- report%>% 
      select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO, 
             RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  }
  
  
  # OLD:
  # report %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # appo2 %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  # chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  # chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  
  report %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, SEZIONE, AREA_TEMATICA, SETTORE_INTERVENTO) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "SEZIONE", "AREA_TEMATICA", "SETTORE_INTERVENTO"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  
  # OLD:
  # crea sezione
  # report <- report %>% 
  #   mutate(SEZIONE = case_when(AREA_TEMATICA == "Risorse da nuove assegnazioni FSC 2014-2020" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da riprogrammazione ex art. 44" ~ "Sez. speciale",
  #                              AREA_TEMATICA == "Risorse da compensazioni CSR" ~ "Sez. speciale",
  #                              TRUE ~ "Sez. ordinaria"))
  
  # NEW:
  report <- report %>% 
    mutate(SEZIONE = case_when(SEZIONE == "SEZ_ORD" ~ "Sez. ordinaria",
                               SEZIONE == "SEZ_SPEC_1_COVID" ~ "Sez. speciale 1-Covid",
                               SEZIONE == "SEZ_SPEC_2_FS" ~ "Sez. speciale 2-FS",
                               SEZIONE == "CSR" ~ "Da programmare",
                               TRUE ~ "CHK"))
  # fix
  report <- report %>% 
    # fix SIN BRINDISI lato DBCOE
    filter(!(ID_PSC == "PSC_MATTM" & is.na(AREA_TEMATICA) & RISORSE == 0))
  
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


#' Crea file di base per macroaree
#'
#' Crea file di base per macroaree, da operazioni, per psc migrati.
#'
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param operazioni Dataset da load_operazioni(), con visualizzati=FALSE
#' @param export vuoi salvare il file?
#' @return Dataset macroaree per il report make_report_temi_macroaree_psc
setup_macroaree_psc <- function(progetti_psc, operazioni, export=FALSE) {
  
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

#' Correzione per codici PSC superiori a 15 digit
#'
#' Correzione per codici PSC superiori a 15 digit
#'  
#' @param df Dataframe
#' @param var1 Nome variabile da modificare (OC_CODICE_PROGRAMMA oppure ID_PSC)
#' @return Dataframe con valori modificati
fix_id_psc_15_digit <- function(df, var1="ID_PSC") {
  
  # fix_id_psc_15_digit(df, var1="ID_PSC")
  # fix_id_psc_15_digit(df, var1="OC_CODICE_PROGRAMMA")
  
  if (var1 == "OC_CODICE_PROGRAMMA") {
    df <- df %>% 
      mutate(OC_CODICE_PROGRAMMA = 
               case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
                         OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
                         OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
                         OC_CODICE_PROGRAMMA == "PSC_REGGIO_CALABRIA" ~ "PSC_REGGIO_CALA",
                         TRUE ~ OC_CODICE_PROGRAMMA))
  } else {
    df <- df %>% 
      mutate(ID_PSC = 
               case_when(ID_PSC == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
                         ID_PSC == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
                         ID_PSC == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
                         ID_PSC == "PSC_REGGIO_CALABRIA" ~ "PSC_REGGIO_CALA",
                         TRUE ~ ID_PSC))
  }
  return(df)
}



#' Correzione per denominazioni PSC ministeri
#'
#' Correzione per denominazioni PSC ministeri
#'  
#' @param df Dataframe con variabili 
#' @param var1 Nome variabile da modificare (PSC oppure DESCRIZIONE_PROGRAMMA)
#' @return Dataframe con valori modificati per la variabile PSC oppure DESCRIZIONE_PROGRAMMA in funzione dei valori diu ID_PSC oppure OC_CODICE_PROGRAMMA
fix_id_psc_ministeri <- function(df, var1="PSC") {
  
  # fix_id_psc_ministeri(df, "DESCRIZIONE_PROGRAMMA")
  
  if (var1 == "DESCRIZIONE_PROGRAMMA") {
    df <- df %>% 
      mutate(DESCRIZIONE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
                                               OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
                                               OC_CODICE_PROGRAMMA == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
                                               # OC_CODICE_PROGRAMMA == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
                                               OC_CODICE_PROGRAMMA == "PSC_MIC" ~ "PSC MINISTERO CULTURA",
                                               OC_CODICE_PROGRAMMA == "PSC_MITUR" ~ "PSC MINISTERO TURISMO",
                                               OC_CODICE_PROGRAMMA == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
                                               OC_CODICE_PROGRAMMA == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
                                               OC_CODICE_PROGRAMMA == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
                                               OC_CODICE_PROGRAMMA == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
                                               OC_CODICE_PROGRAMMA == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
                                               TRUE ~ DESCRIZIONE_PROGRAMMA))
  } else {
    df <- df %>% 
      mutate(PSC = case_when(ID_PSC == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
                             ID_PSC == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
                             ID_PSC == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
                             # ID_PSC == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
                             ID_PSC == "PSC_MIC" ~ "PSC MINISTERO CULTURA",
                             ID_PSC == "PSC_MITUR" ~ "PSC MINISTERO TURISMO",
                             ID_PSC == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
                             ID_PSC == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
                             ID_PSC == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
                             ID_PSC == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
                             ID_PSC == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
                             TRUE ~ PSC))
  }
  
  return(df)
}

