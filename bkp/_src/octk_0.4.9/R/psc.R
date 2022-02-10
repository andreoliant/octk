# Script per la gestione dei PSC

#' Inizializza PSC
#'
#' Inizializza global environment per flusso di lavoro PSC
#' 
#' @param PSC Folder di supporto per dati PSC
#' @return Carica i file necessario al flusso nel global environment
#' @note ...
init_psc <- function(PSC=NULL) {
  
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
    distinct(ID_PSC, TIPOLOGIA_AMMINISTRAZIONE)

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
  
}


#' Dati FSC per elaborazioni PSC
#'
#' Crea il file con operazioni FSC compatibile con PSC
#' 
#' @param bimestre Bimestre di riferimento
#' @param matrix_po_psc Matrice di riconciliazione PO - PSC
#' @param po_naz Ricodifica programmi nazionali su base regionale (direttrici ferroviarie e autostrada SA-RC)
#' @param art44 Dati di base per istruttoria art44 al 20200630
#' @param matrix_1420 Matrice di riconciliazione tra temi prioritari FSC 2014-2020 e settori di intervento PSC
#' @param matrix_713 Matrice di riconciliazione tra obiettivi specifici QSN 2007-2013 e settori di intervento PSC
#' @param matrix_temi_settori Dominio per aree tematiche e settori di intervento PSC
#' @param progetti Dataset progetti da load_progetti(visualizzati = FALSE, light = FALSE). Attennzione: serve light = FALSE perché non contiene finanziamento privato
#' @return File "dati_psc_BIMESTRE.csv" in TEMP 
#' @note ...
prep_dati_psc_bimestre <- function(bimestre, matrix_po_psc, po_naz, art44, 
                                   matrix_1420, matrix_713, matrix_temi_settori, progetti=NULL) {
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati = FALSE, light = FALSE)
    # MEMO: serve light = FALSE perché non contiene finanziamento privato
  }
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  print(bimestre)
  # rettifica DATA per bimestri diversi
  bimestre <- bimestre
  DATA <- paste0(str_sub(DATA, 1, nchar(DATA)-8), bimestre)
  # print(DATA)
  
  # ------------------ load dati ------------------ #
  
  # operazioni <- load_operazioni(bimestre, usa_meuro = FALSE) 
  # TODO: qui c'è chiamata interna a DATA che non viene ridefinita, forse perché in cache?
  operazioni <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1e+06) %>% 
    # filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    get_simply_non_loc() %>% 
    refactor_ambito() %>% 
    refactor_ciclo()
  
  operazioni_1420 <- read_sas(file.path(DATA, "oper_pucok_preesteso.sas7bdat")) %>%
    filter(STATO == 1, oc_cod_fonte == "FSC1420") %>%
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
             COSTO_AMM_FSC
    ) %>%
    # fix 
    mutate(COD_SETTORE_STRATEGICO_FSC = case_when(COD_SETTORE_STRATEGICO_FSC == "4.a" ~ "4", # matera
                                                  COD_SETTORE_STRATEGICO_FSC == "4.b" ~ "4", # matera
                                                  TRUE ~ COD_SETTORE_STRATEGICO_FSC),
           COD_ASSE_TEMATICO_FSC = case_when(COD_ASSE_TEMATICO_FSC == "01" ~ "1", # pozzuoli
                                             COD_ASSE_TEMATICO_FSC == "2:::3:::5" ~ "2", # mattm
                                             COD_ASSE_TEMATICO_FSC == "1:::2:::3:" ~ "2", # mattm
                                             COD_ASSE_TEMATICO_FSC == "1:::2" ~ "2", # mise
                                             TRUE ~ COD_ASSE_TEMATICO_FSC))
  
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
  
  
  
  # ------------------ elaborazione ------------------ #
  
  appo <- operazioni %>% 
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
    left_join(po_naz, # %>% 
              # select(-OC_CODICE_PROGRAMMA),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    # codice psc
    left_join(matrix_po_psc %>% 
                distinct(OC_CODICE_PROGRAMMA, ID_PSC), 
              by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(ID_PSC = if_else(is.na(ID_PSC.y), ID_PSC.x, ID_PSC.y)) %>% 
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
                       COD_OPOS_NEW, DESCR_OPOS_NEW),
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(#COD_AREA_TEMATICA = COD_TEMA_NEW,
      #DESCR_AREA_TEMATICA = DESCR_TEMA_NEW,
      COD_SETTORE_INTERVENTO = COD_OPOS_NEW,
      # DESCR_SETTORE_INTERVENTO = DESCR_OPOS_NEW
    ) %>% 
    # integra temi mancanti per 1420
    left_join(operazioni_1420,
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    left_join(matrix_1420 %>% 
                select(COD_SETTORE_STRATEGICO_FSC, COD_ASSE_TEMATICO_FSC, COD_SETTORE_INTERVENTO),
              by = c("COD_SETTORE_STRATEGICO_FSC", "COD_ASSE_TEMATICO_FSC")) %>% 
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
    select(-COD_SETTORE_INTERVENTO.x, -COD_SETTORE_INTERVENTO.y) %>% 
    # sovrascrive tema per tutti
    left_join(matrix_temi_settori, 
              by = "COD_SETTORE_INTERVENTO") %>% 
    mutate(AREA_TEMATICA = paste0(COD_AREA_TEMATICA, "-", DESCR_AREA_TEMATICA),
           SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, "-", DESCR_SETTORE_INTERVENTO)) 
  
  # chk classi
  # chk <- appo %>% count(AREA_TEMATICA, SETTORE_INTERVENTO)
  # appo %>% count(AREA_TEMATICA) #134 progetti con NA in patto sicilia e pugli
  # 
  # chk <- appo %>% filter(AREA_TEMATICA == "NA-NA")
  # chk2 <- appo %>%
  #   semi_join(chk, by = "COD_LOCALE_PROGETTO") %>% 
  #   select(COD_LOCALE_PROGETTO, x_PROGRAMMA, x_CICLO, COD_SETTORE_STRATEGICO_FSC, COD_ASSE_TEMATICO_FSC, QSN_CODICE_OBIETTIVO_SPECIFICO)
  # 
  # write_csv2(chk2, file.path(TEMP, "chk_classi_missing.csv"))
  
  # debug classi
  print(appo %>% count(AREA_TEMATICA))
  chk <- appo %>% filter(AREA_TEMATICA == "NA-NA")
  write_csv2(chk, file.path(TEMP, paste0("chk_classi_missing_", bimestre, ".csv")))
  
  message("Questi sono gli strumenti attuativi non censiti in matrix:")
  chk2 <- chk %>% 
    mutate(CHK = case_when(COE > 0 ~ "to_fix",
                           COE <= 0 ~ "zero",
                           is.na(COE) ~ "na")) %>% 
    count(x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, SETTORE_INTERVENTO, CHK)
  print(chk2)
  write_csv2(chk2, file.path(TEMP, paste0("fix_mapping_", bimestre, ".csv")))
  message("Se ci sono NA-NA con COE NA va bene, restano così. Per tutti gli altri casi va rivisto lo script, che incorpora direttamente le correzioni")
  
  # fix classi
  appo <- appo %>% 
    mutate(AREA_TEMATICA = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & AREA_TEMATICA == "NA-NA" & SETTORE_INTERVENTO == "09.02-NA" ~ "08-RIQUALIFICAZIONE URBANA",
                                     TRUE ~ AREA_TEMATICA)) %>% 
    mutate(SETTORE_INTERVENTO = case_when(OC_CODICE_PROGRAMMA == "2016PATTIPUG" & SETTORE_INTERVENTO == "09.02-NA" ~ "08.01-EDILIZIA E SPAZI PUBBLICI",
                                     TRUE ~ SETTORE_INTERVENTO))
  message("Il caso Puglia con 09.02-NA è già gestito")
  
  # calcola costo realizzato e economie
  appo1 <- appo %>% 
    # filter(OC_FLAG_VISUALIZZAZIONE == 0) %>% 
    left_join(progetti %>% 
              select(COD_LOCALE_PROGETTO, COSTO_REALIZZATO, FINANZ_STATO_FSC, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_PRIVATO_NETTO),
              by = "COD_LOCALE_PROGETTO") %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(FINANZ_TOT = OC_FINANZ_TOT_PUB_NETTO + OC_FINANZ_PRIVATO_NETTO,
           # x = OC_FINANZ_STATO_FSC_NETTO/FINANZ_TOT,
           x = COE/FINANZ_TOT,
           COSTO_REALIZZATO_2 = COSTO_REALIZZATO * x,
           COE_ECO = FINANZ_STATO_FSC - OC_FINANZ_STATO_FSC_NETTO)
  
  dim(appo)[1] == dim(appo1)[1]
  
  # chk finanziamenti vs costi
  chk <- appo1 %>% 
    group_by(COD_LOCALE_PROGETTO, x_CICLO) %>% 
    summarise(COE = sum(COE, na.rm = TRUE),
              FINANZ_STATO_FSC = sum(FINANZ_STATO_FSC, na.rm = TRUE),
              OC_FINANZ_STATO_FSC_NETTO = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
              COSTO_AMM_FSC = sum(COSTO_AMM_FSC, na.rm = TRUE)) %>% 
    mutate(CHK = case_when(x_CICLO == "2014-2020" & abs(COE - COSTO_AMM_FSC) < 0.1 ~ "cost", # possibile solo su 1420
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
  
  # chk economie
  appo1 %>% 
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
  
  appo1 %>% 
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
  appo2 <- appo1 %>%
    rename(COE_CR = COSTO_REALIZZATO_2) %>% 
    mutate(STATO = case_when(COE_PAG / COE > 0.95 ~ "Conclusi",
                             COE_CR / COE > 0.95 ~ "Conclusi",
                             OC_STATO_PROCEDURALE == "Eseguito" ~ "Conclusi",
                             TRUE ~ "In corso"))
  
  # fix CIS Taranto in PRA Puglia
  appo3 <- fix_ciclo_cis_taranto_pra_puglia(progetti_psc = appo2)
  
  # fix visualizzati PRA Campania
  appo4 <- fix_visualizzati_pra_campania(progetti_psc = appo3)
  
  # accoda 06
  temp <- read_csv2(file.path(PSC, "sgp", paste0("dati_sgp_", bimestre, ".csv")), col_types = "ccccccccccdddddcccd")
  
  out <- appo4 %>%
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
           COE, 
           COE_IMP,
           COE_CR,
           COE_PAG,
           COE_ECO,
           PSC,
           ID_PSC,
           STATO,
           OC_FLAG_VISUALIZZAZIONE) %>% 
    bind_rows(temp)
  
  
  # fix psc
  out <- fix_id_psc_15_digit(out, var1="ID_PSC")
  out <- fix_id_psc_ministeri(out, var1="PSC")
  
  # fix temi
  # TODO: ora è in main script, forse da aggiungere qui
  
  write.csv2(out, file.path(PSC, "psc", paste0("dati_psc_", bimestre, ".csv")), row.names = FALSE)
  
}

#' Dati SGP per elaborazioni PSC
#'
#' Crea dati SGP compatibili per elaborazioni PSC partendo da repository in DATI > SGP
#' 
#' @param bimestre Bimestre di riferimento
#' @param filename Nome file xlsx in DATI > SGP
#' @param matrix06 Matrice di riconciliazione tra denominazione APQ 2000-20006 e settori di intervento PSC
#' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' @return File "dati_sgp_BIMESTRE.csv" in TEMP 
#' @note ...
prep_dati_sgp_bimestre <- function(bimestre, filename, matrix_06, chk_today) {
  # filename <- "Estrazioni dati e calcolo indicatori _28_02_21_v01.xlsx"
  # filename <- "Estrazione dati e calcolo indicatori_30062021.xlsx"
  # chk_today <- as.POSIXct("2021-02-28")
  # chk_today <- as.POSIXct("2021-04-30")
  
  PSC <- file.path(DRIVE, "DATI", "PSC")
  
  print(bimestre)
  DATA <- paste0(str_sub(DATA, 1, nchar(DATA)-8), bimestre)
  
  SGP <- file.path(DRIVE, "DATI", "SGP", bimestre)
  appo <- read_xlsx(file.path(SGP, filename), guess_max = 25000)
  # appo <- read_csv2(file.path(SGP, filename), guess_max = 25000) # 24928 progetti
  # sum(appo$FINANZIAMENTO_FSC_NETTO) # 19027452171
  # sum(appo$FINANZIAMENTO_TOTALE_PUBBLICO_NETTO) # 71662352262
  
  # addattamento per versioni diverse
  if ("TITOLO_PROGETTO" %in% names(appo)) {
    appo <- appo %>% 
      rename(TITOLO = TITOLO_PROGETTO)
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
  appo %>% 
    filter(is.na(IMPORTO_NAZIONALE) | is.na(IMPORTO_REGIONALE) | is.na(IMPORTO_NON_DEFINITO)) %>% 
    count()
  
  # chk finanziamenti
  appo %>% 
    mutate(TOT_IMPORTI = IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO) %>% 
    summarise(TOT_IMPORTI = sum(TOT_IMPORTI, na.rm = TRUE),
              FINANZIAMENTO_FSC = sum(FINANZIAMENTO_FSC, na.rm = TRUE))
  
  appo %>% 
    mutate(TOT_IMPORTI = IMPORTO_NAZIONALE + IMPORTO_REGIONALE + IMPORTO_NON_DEFINITO,
           CHK = TOT_IMPORTI - FINANZIAMENTO_FSC) %>% 
    filter(CHK != 0) %>% 
    summarise(N = n(),
              TOT_IMPORTI = sum(TOT_IMPORTI, na.rm = TRUE),
              FINANZIAMENTO_FSC = sum(FINANZIAMENTO_FSC, na.rm = TRUE))
  
  # chk pagamenti fSC
  appo %>% summarise(PAG = sum(PAGAMENTI_FSC, na.rm = TRUE))
  
  
  # chk su economie
  appo %>% 
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
  
  # elab
  appo2 <- appo1 %>% 
    mutate(CICLO = "2000-2006") %>% 
    mutate(OC_CODICE_PROGRAMMA = case_when(DENOMINAZIONE_INTESA == "ABRUZZO" ~ "33",
                                           DENOMINAZIONE_INTESA == "BASILICATA" ~ "37",
                                           DENOMINAZIONE_INTESA == "CALABRIA" ~ "38",
                                           DENOMINAZIONE_INTESA == "CAMPANIA" ~ "35",
                                           DENOMINAZIONE_INTESA == "EMILIA-ROMAGNA" ~ "28",
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
    mutate(x = FINANZIAMENTO_FSC_NETTO / FINANZIAMENTO_TOTALE_PUBBLICO_NETTO,
           # x = FINANZIAMENTO_FSC / FINANZIAMENTO_TOTALE_PUBBLICO,
           # k = if_else(is.na(IMPORTO_REGIONALE), 0, # MEMO: da 2010630 non si verifica più il problema; prima c'era quota random di 0.75
           #             IMPORTO_REGIONALE/(IMPORTO_REGIONALE+IMPORTO_NAZIONALE+IMPORTO_NON_DEFINITO)),
           k = IMPORTO_REGIONALE / (IMPORTO_REGIONALE + IMPORTO_NAZIONALE + IMPORTO_NON_DEFINITO),
           COE = FINANZIAMENTO_FSC_NETTO * k,
           COE_IMP = IMPEGNI * x * k,
           COE_CR = COSTO_REALIZZATO * x * k,
           COE_PAG = PAGAMENTI_TOTALI * x * k, # MEMO: PAGAMENTI_FSC sono vuoti
           COE_ECO = TOTALE_ECONOMIE_FSC * k) 

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
                    COE_PAG = sum(COE_PAG, na.rm = TRUE)))
  
  write_csv2(out, file.path(PSC, "sgp", paste0("dati_sgp_", bimestre, ".csv")))
  
  
}


clean_data <- function(colonna_data) {
  temp <- sapply(colonna_data, function(x) {tryCatch(as.POSIXct(x), error = function(e) NA)}, USE.NAMES = FALSE)
  out <- as.POSIXct(temp, origin = "1970-01-01")
  return(out)
}

get_stato_attuazione <- function(df, chk_today) {
  # MEMO: 
  # formato per data è diverso da standad oc
  # può essere necessaria qualche pulizia nelle date in excel
  
  
  # chk_today <- as.POSIXct("2019-12-31")
  chk_today <- as.POSIXct(chk_today)
  
  # DEBUG:
  # df <- appo1
  
  appo <- df %>%
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
           DATA_INIZIO_EFF_STUDIO_FATT = DATA_INIZIO_EFFETTIVA_STUDIO_FATTIBILITA) %>% 
    mutate(DATA_FINE_EFF_ESECUZIONE = clean_data(DATA_FINE_EFF_ESECUZIONE),
           DATA_INIZIO_EFF_ESECUZIONE = clean_data(DATA_INIZIO_EFF_ESECUZIONE),
           DATA_FINE_EFF_STIP_ATTRIB = clean_data(DATA_FINE_EFF_STIP_ATTRIB),
           DATA_INIZIO_EFF_STIP_ATTRIB = clean_data(DATA_INIZIO_EFF_STIP_ATTRIB),
           # DATA_FINE_EFF_AGG_BANDO,
           # DATA_INIZIO_EFF_AGG_BANDO,
           DATA_FINE_EFF_PROG_ESEC = clean_data(DATA_FINE_EFF_PROG_ESEC),
           DATA_INIZIO_EFF_PROG_ESEC = clean_data(DATA_INIZIO_EFF_PROG_ESEC),
           DATA_FINE_EFF_PROG_DEF = clean_data(DATA_FINE_EFF_PROG_DEF),
           DATA_INIZIO_EFF_PROG_DEF = clean_data(DATA_INIZIO_EFF_PROG_DEF),
           DATA_FINE_EFF_PROG_PREL = clean_data(DATA_FINE_EFF_PROG_PREL),
           DATA_INIZIO_EFF_PROG_PREL = clean_data(DATA_INIZIO_EFF_PROG_PREL), 
           DATA_FINE_EFF_STUDIO_FATT = clean_data(DATA_FINE_EFF_STUDIO_FATT),
           DATA_INIZIO_EFF_STUDIO_FATT = clean_data(DATA_INIZIO_EFF_STUDIO_FATT))
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
                           # DATA_FINE_EFF_AGG_BANDO <= chk_today ~ 1,
                           # DATA_INIZIO_EFF_AGG_BANDO <= chk_today ~ 1,
                           # DATA_FINE_EFF_PROG_ESEC <= chk_today ~ 1,
                           TRUE ~ 0),
      # MEMO: blocco su progettazione presente solo per le opere
      CHK_PROG = case_when(as.POSIXct(DATA_FINE_EFF_PROG_ESEC) <= chk_today ~ 1,
                           as.POSIXct(DATA_INIZIO_EFF_PROG_ESEC) <= chk_today ~ 1,
                           as.POSIXct(DATA_FINE_EFF_PROG_DEF) <= chk_today ~ 1,
                           as.POSIXct(DATA_INIZIO_EFF_PROG_DEF) <= chk_today ~ 1,
                           as.POSIXct(DATA_FINE_EFF_PROG_PREL) <= chk_today ~ 1,
                           as.POSIXct(DATA_INIZIO_EFF_PROG_PREL) <= chk_today ~ 1,
                           # DATA_FINE_EFF_STUDIO_FATT <= chk_today ~ 1,
                           # DATA_INIZIO_EFF_STUDIO_FATT <= chk_today ~ 1,
                           TRUE ~ 0),
      CHK_AVVP = case_when(DATA_FINE_EFF_STUDIO_FATT <= chk_today ~ 1,
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
#' @param fix_no_temi_no_coe Logico. Vuoi scartare i progetti con tema missing e finanziamenti pari a 0?
#' @return Dataframe
load_progetti_psc <- function(bimestre, fix_no_temi_no_coe=FALSE) {
  progetti_psc <- read_csv2(file.path(PSC, "psc", paste0("dati_psc_", bimestre, ".csv")))
  
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
#' @param  usa_meuro Logico. Vuoi dati in Meuro?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param fix_cis_taranto Vuoi correggere il ciclo del CIS Taranto? Nel PSC i progetti PRA del CIS per 320 sono spostati da 713 a 1420, il fix crea un programma fittizio lato risorse. Lato attuazione va sviluppato.
#' @return Report di confronto programmazione attuazione per PSC e PO d'origine. Le risorse delle sezioni speciali, nuove o da riprogrammazione, sono in righe separate.
#' @note Contiene patch per incorporare patti città metro anche se non sono PSC in DBCOE
make_report_report_po_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, export=FALSE, export_xls=FALSE, fix_cis_taranto=TRUE) {
  
  # DEV: uso matrice_po_psc senza dichiararla!!!
  
  if (is.null(programmazione)) {
    programmazione <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx")) %>% 
      select(ID_PSC, PSC, CICLO_PROGRAMMAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, FINANZ_TOTALE)
    
    # # patch per patti metro
    # patti <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
    #   filter(TIPOLOGIA_PROGRAMMA == "PATTI") %>% 
    #   left_join(matrix_po_psc %>% 
    #               select(OC_CODICE_PROGRAMMA, ID_PSC, PSC),
    #             by = "OC_CODICE_PROGRAMMA") %>% 
    #   mutate(DESCRIZIONE_PROGRAMMA = PSC) %>% 
    #   select(ID_PSC, PSC, CICLO_PROGRAMMAZIONE = x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, FINANZ_TOTALE)
    # 
    # programmazione <- programmazione %>% 
    #   bind_rows(patti)
  }
  
  # fix per CIS Taranto (nel PSC i progetti PRA per 320 sono spostati da 713 a 1420)
  # 2007PU001FA010
  temp <- programmazione %>% 
    filter(OC_CODICE_PROGRAMMA == "2007PU001FA010")
  
  appo <- programmazione %>% 
    filter(OC_CODICE_PROGRAMMA != "2007PU001FA010")
  
  programmazione_2 <- appo %>% 
    # pra puglia normale
    bind_rows(temp %>% 
                mutate(# CICLO_PROGRAMMAZIONE = "2007-2013",
                       FINANZ_TOTALE = 2318573538)) %>% 
    # pra puglia in CIS
    bind_rows(temp %>% 
                mutate(CICLO_PROGRAMMAZIONE = "2014-2020",
                       FINANZ_TOTALE = 320667143))
  # MEMO: la somma delle due parti del PSC presa dalla Tavola 2 del PSC sballa rispetto al valore nel DB, ma è trascurabile
  # 2639264638 - 2639240681 = 23957
  
  # sum(programmazione$FINANZ_TOTALE) - sum(programmazione_2$FINANZ_TOTALE)
  # 23957.45
  
  programmazione <- programmazione_2
  
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
    select(ID_PSC, PSC, x_CICLO = CICLO_PROGRAMMAZIONE, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE) %>% 
    group_by(ID_PSC, PSC, x_CICLO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    left_join(appo2 %>% 
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
      message("Da implementare")
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_po_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      message("Da implementare")
    }
  }

  
  
  return(report)
}




#' Report PSC per temi
#'
#' Crea report di confronto programmazione attuazione per PSC e PO in essi confluiti
#' 
#' @param progetti_psc Dataset da load_progetti_psc()
#' @param programmazione Dati di programmazione DBCOE di tipo "fsc_matrice_po_psc.xlsx"
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param  usa_meuro Logico. Vuoi dati in Meuro?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return Report di confronto programmazione attuazione per PSC e PO in essi confluiti. I nuovi 
#' @note ...
make_report_report_temi_psc <- function(progetti_psc, programmazione=NULL, visualizzati=TRUE, usa_meuro=FALSE, export=FALSE, export_xls=FALSE) {
  
  if (is.null(programmazione)) {
    programmazione <- init_programmazione_dati(use_temi = TRUE, use_713 = TRUE, use_flt = TRUE, use_sog = TRUE) %>% 
      filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA)
  }
  
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
  
  appo2 <- appo1 %>% 
    # anti_join(temp) %>% 
    group_by(ID_PSC, AREA_TEMATICA, x_CICLO) %>%
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_CR = sum(COE_CR, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              N = n())
  
  # confronto programmazione e attuazione
  report <- programmazione %>% 
    # filter(TIPOLOGIA_PROGRAMMA != "COVID", # considero solo sezione ordinaria
    #        TIPOLOGIA_PROGRAMMA != "CSR") %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, RISORSE = FINANZ_TOTALE,
           AREA_TEMATICA = DESCR_AREA_TEMATICA_PSC) %>% 
    group_by(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    # MEMO: que serve full join perché ci possono essere progetti monitorati associati a temi assenti in programmazione
    full_join(appo2 %>% 
                select(ID_PSC, x_CICLO, AREA_TEMATICA, COE, COE_IMP, COE_CR, COE_PAG, N),
              by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA")) %>% 
    mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
    # recupera variabili perse da full_join
    left_join(programmazione %>% 
                distinct(ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE),
              by = "ID_PSC") %>% 
    mutate(DESCRIZIONE_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA.x),  DESCRIZIONE_PROGRAMMA.y, DESCRIZIONE_PROGRAMMA.x),
           TIPOLOGIA_AMMINISTRAZIONE= if_else(is.na(TIPOLOGIA_AMMINISTRAZIONE.x),  TIPOLOGIA_AMMINISTRAZIONE.y, TIPOLOGIA_AMMINISTRAZIONE.x)) %>% 
    select(-DESCRIZIONE_PROGRAMMA.y, -DESCRIZIONE_PROGRAMMA.x, -TIPOLOGIA_AMMINISTRAZIONE.y, -TIPOLOGIA_AMMINISTRAZIONE.x) %>% 
    select(ID_PSC, x_CICLO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE, AREA_TEMATICA, RISORSE, COE, COE_IMP, COE_CR, COE_PAG, N)
  
  report %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  appo2 %>% count(ID_PSC, x_CICLO, AREA_TEMATICA) %>% filter(n>1)
  chk1 <- report %>% anti_join(appo2, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  chk2 <- appo2 %>% anti_join(report, by = c("ID_PSC", "x_CICLO", "AREA_TEMATICA"))
  # in chk1 tutte le assegnazioni nei psc non monitorate
  
  # crea sezione
  report <- report %>% 
    mutate(SEZIONE = case_when(AREA_TEMATICA == "Risorse da nuove assegnazioni FSC 2014-2020" ~ "Sez. speciale",
                               AREA_TEMATICA == "Risorse da riprogrammazione ex art. 44" ~ "Sez. speciale",
                               AREA_TEMATICA == "Risorse da compensazioni CSR" ~ "Sez. speciale",
                               TRUE ~ "Sez. ordinaria"))
  
  # fix
  report <- report %>% 
    # fix SIN BRINDISI lato DBCOE
    filter(!(ID_PSC == "PSC_MATTM" & is.na(AREA_TEMATICA) & RISORSE == 0))

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
      write_csv2(report, file.path(TEMP, "report_temi_psc_nodupli.csv"))
    }
    
    if (export_xls == TRUE) {
      message("Da implementare")
    }
  } else {
    if (export == TRUE) {
      write_csv2(report, file.path(TEMP, "report_temi_psc_dupli.csv"))
    }
    
    if (export_xls == TRUE) {
      message("Da implementare")
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



