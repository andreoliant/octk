# funzioni del blocco "coesione"


#' Crea il dataset operazioni
#'
#' Crea il dataset operazioni a partire da sottoprodotti del workflow SAS.
#'
#' @param bimestre Bimestre di riferimento
#' @param progetti Dataset con un perimetro in formato "progetti".
#' @param use_ecomix Vuoi calcolare il costo coesione al netto delle economie solo per i progetti conclusi?
#' @param use_sito Vuoi usare il calcolo dellle risorse coesione fatto per il sito OC?
#' @return Il dataset operazioni con le variabili coesione calcolate: COE, COE_IMP e COE_PAG.
#' @note La modalità **debug** esporta diversi csv in TEMP La modalità **export** esporta operazioni_light.csv in DATA.
setup_operazioni <- function(bimestre, progetti=NULL, export=FALSE, use_fix=FALSE, use_ecomix=FALSE, use_sito=FALSE, debug=FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {
    
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, light = FALSE)
      message("Dataset progetti caricato")
    }
    # MEMO: da qui veniva l'esclusione dei non visualizzati da operazioni? no perché sotto c'è left_join
    
    if (use_fix == TRUE) {
      progetti <- fix_progetti(progetti)
      message("Fix su progetti effettuati")
    }
    
    # main
    # DEBUG: debug <- TRUE
    if (use_ecomix == TRUE) {
      operazioni <- workflow_operazioni_ecomix(bimestre, progetti, debug=debug)
      
    } else if (use_sito == TRUE) {
      operazioni <- workflow_operazioni_sito(bimestre, progetti, debug=debug)
    } else {
      operazioni <- workflow_operazioni(bimestre, progetti, debug=debug)
    }
    
    # export
    if (export == TRUE) {
      
      # ----------------------------------------------------------------------------------- #
      #  export light
      
      # macroarea
      operazioni_light <- get_macroarea(operazioni, progetti, real_reg=TRUE)
      operazioni_light <- get_regione_simply(operazioni_light, progetti)
      
      # variabili
      operazioni_light <- operazioni_light %>%
        left_join(progetti %>%
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
                           # OC_COD_FONTE,
                           # OC_DESCR_FONTE,
                           FONDO_COMUNITARIO,
                           # OC_CODICE_PROGRAMMA,
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
                           # COD_REGIONE,
                           # DEN_REGIONE,
                           # COD_PROVINCIA,
                           # DEN_PROVINCIA,
                           # COD_COMUNE,
                           # DEN_COMUNE,
                           OC_STATO_PROGETTO,
                           OC_STATO_PROCEDURALE,
                           OC_COD_FASE_CORRENTE,
                           OC_DESCR_FASE_CORRENTE,
                           COD_PROCED_ATTIVAZIONE,
                           DESCR_PROCED_ATTIVAZIONE,
                           OC_CODFISC_BENEFICIARIO,
                           OC_DENOM_BENEFICIARIO,
                           OC_FLAG_VISUALIZZAZIONE),
                  by = "COD_LOCALE_PROGETTO")
      
      
      # export
      if (use_ecomix == TRUE) {
        write.csv2(operazioni_light, file.path(DATA, paste0("operazioni_ecomix_", bimestre, ".csv")), row.names = FALSE)
        
      } else {
        write.csv2(operazioni_light, file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)
        # write.csv2(operazioni_light, file.path(TEMP, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)
      }
      
    } else {
      return(operazioni_light)
    }
    
  } else {
    message("Non hai definito il folder DATA. Carica 'oc' ed inizializza 'oc_init()'.")
  }
}


#' Workflow per creare il dataset operazioni
workflow_operazioni <- function(bimestre, progetti, debug=FALSE) {
  
  # MEMO:
  # i programmi con ciclo sbagliato non sono ancora spostati nei file operazioni.sas
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass
  
  # if (is.null(progetti)) {
  #   # progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, light = FALSE)
  # }
  
  # MEMO: ora è dentro in pre-esteso
  # recupera finanziamenti lordi (se assente in df)
  # if (!(any(names(progetti) == "FINANZ_TOTALE_PUBBLICO"))) {
  #
  #   temp <- paste0("progetti_esteso_", bimestre, ".csv")
  #   progetti_ext <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
  #
  #   progetti <- progetti %>%
  #     left_join(progetti_ext %>%
  #                 select(COD_LOCALE_PROGETTO, FINANZ_TOTALE_PUBBLICO, FINANZ_STATO_FSC, FINANZ_STATO_PAC),
  #               by = "COD_LOCALE_PROGETTO")
  #
  #   rm(progetti_ext)
  # }
  
  operazioni_1420_raw <- read_sas(file.path(DATA, "oper_pucok_preesteso.sas7bdat"))
  message("Operazioni raw caricate per 1420")
  
  operazioni_713_raw <- read_sas(file.path(DATA, "oper_fltok_preesteso.sas7bdat"))
  message("Operazioni raw caricate per 713")
  
  
  # ----------------------------------------------------------------------------------- #
  # prep
  
  # progetti in programmi multi con ":::"
  progetti_multi <- progetti %>%
    filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # progetti in programmi multi con ":::" in 2 programmi fsc
  progetti_multi_fsc_1420 <- progetti %>%
    # filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    filter(OC_COD_FONTE == "FSC1420:::FSC1420") %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # chk
  # progetti %>%
  #   filter(OC_COD_FONTE %in% c("FS0713:::FSC0713", "FS0713:::PAC")) %>%
  #   group_by(OC_COD_FONTE, FONDO_COMUNITARIO) %>%
  #   summarise(N = n(),
  #             FSC = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
  #             PAC = sum(OC_FINANZ_STATO_PAC_NETTO, na.rm = TRUE),
  #             UE = sum(COSTO_RENDICONTABILE_UE, na.rm = TRUE),
  #             CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2014-2020
  
  # chk
  # operazioni_1420_raw %>% count(oc_cod_fonte, ue_descr_fondo, CODICE_TIPOLOGIA_PROGRAMMA, STATO)
  # oc_cod_fonte ue_descr_fondo      n
  # <chr>        <chr>           <int>
  # 1 FS1420       ""                  1 <- ? 2014IT16RFOP010 POR LAZIO FESR (da eliminare)
  # 2 FS1420       FEAMP            6856
  # 3 FS1420       FESR            58220
  # 4 FS1420       FSE            304988
  # 5 FS1420       IOG            195158
  # 6 FS1420       PAC                25 <- ? SNAI
  # 7 FSC1420      FSC              8435
  # 8 PAC1420      PAC               858
  
  # chk su ""
  # chk <- operazioni_1420_raw %>% filter(ue_descr_fondo == "", STATO == 1)
  
  # chk su STATO
  # chk <- operazioni_1420_raw %>% filter(STATO == 2)
  # 96 operazioni a giugno
  # sum(chk$costo_ammesso_FSC, na.rm = TRUE)
  # 6800000 > sono interamente riassorbiti da incremento via finanziamenti
  # sum(chk$costo_rendicontabile_UE, na.rm = TRUE)
  # 325273535
  
  # chk su multi
  # operazioni_1420_raw %>%
  #   rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
  #   left_join(progetti_multi %>%
  #               mutate(MULTI = TRUE),
  #             by = "COD_LOCALE_PROGETTO") %>%
  #     count(oc_cod_fonte, ue_descr_fondo, MULTI)
  
  
  message("Preparazione dati 1420...")
  
  # clean
  operazioni_1420 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni(.) %>%
    # mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
    #                                     PO_FONDO == "FESR" ~ "FESR",
    #                                   oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
    #                                   TRUE ~ ue_descr_fondo)) %>%
    # creo ambito
    mutate(x_AMBITO = case_when(oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "IPA" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "SNAI",
                                oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
                                oc_cod_fonte == "FSC1420" ~ "FSC",
                                oc_cod_fonte == "PAC1420" ~ "POC")) %>%
    # DEBUG: operazioni_1420 %>% count(x_AMBITO, oc_cod_fonte, ue_descr_fondo, CODICE_TIPOLOGIA_PROGRAMMA)
    # map per ambito
    # mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ costo_ammesso_FSC,
    #                            x_AMBITO == "POC" ~ costo_ammesso_NAZ,
    #                            x_AMBITO == "SNAI" ~ costo_ammesso_NAZ,
    #                            TRUE ~ costo_rendicontabile_UE),
    #        IMP_AMM = case_when(x_AMBITO == "FSC" ~ oc_impegni_ammessi_FSC,
    #                            x_AMBITO == "POC" ~ oc_impegni_ammessi_PAC,
    #                            x_AMBITO == "SNAI" ~ oc_impegni_ammessi_PAC,
    #                            TRUE ~ oc_impegni_ammessi_ue),
    #        PAG_AMM = case_when(x_AMBITO == "FSC" ~ oc_tot_pagamenti_FSC,
  #                            x_AMBITO == "POC" ~ oc_tot_pagamenti_PAC,
  #                            x_AMBITO == "SNAI" ~ oc_tot_pagamenti_PAC,
  #                            TRUE ~ oc_tot_pagamenti_rendicontab_ue)) %>%
  
  # map per ambito (REV TRASFERIMENTI)
  left_join(progetti %>%
              select(COD_LOCALE_PROGETTO, CUP_COD_NATURA),
            by = "COD_LOCALE_PROGETTO") %>%
    # CUP_COD_NATURA == "08"
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ costo_ammesso_FSC,
                               x_AMBITO == "POC" ~ costo_ammesso_NAZ,
                               x_AMBITO == "SNAI" ~ costo_ammesso_NAZ,
                               TRUE ~ costo_rendicontabile_UE),
           IMP_AMM = case_when(x_AMBITO == "FSC" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_FSC,
                               x_AMBITO == "FSC" ~ oc_impegni_ammessi_FSC,
                               x_AMBITO == "POC" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_PAC,
                               x_AMBITO == "POC" ~ oc_impegni_ammessi_PAC,
                               x_AMBITO == "SNAI" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_PAC,
                               x_AMBITO == "SNAI" ~ oc_impegni_ammessi_PAC,
                               CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_ue,
                               TRUE ~ oc_impegni_ammessi_ue),
           PAG_AMM = case_when(x_AMBITO == "FSC" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_FSC,
                               x_AMBITO == "FSC" ~ oc_tot_pagamenti_FSC,
                               x_AMBITO == "POC" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_PAC,
                               x_AMBITO == "POC" ~ oc_tot_pagamenti_PAC,
                               x_AMBITO == "SNAI" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_PAC,
                               x_AMBITO == "SNAI" ~ oc_tot_pagamenti_PAC,
                               CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_rendicontab_ue,
                               TRUE ~ oc_tot_pagamenti_rendicontab_ue)) %>%
    
    #   "oc_tot_pagamenti_rendicontab_ue"  "oc_tot_pagamenti_FSC"             "oc_tot_pagamenti_PAC"
    # [142] "oc_tot_pag_trasf_rendicontab_ue"  "oc_tot_pag_trasf_FSC"             "oc_tot_pag_trasf_PAC"
    # [145] "oc_impegni_ammessi_ue"            "oc_impegni_ammessi_FSC"           "oc_impegni_ammessi_PAC"
    
    # incrementa COE con finanziamento FSC e PAC
    # MEMO: per progetti "non multi" o "multi" di cui solo 1 programma FSC (per lasciare fuori i multi su 2 programmi FSC)
    left_join(progetti %>%
                # anti_join(progetti_multi, by = "COD_LOCALE_PROGETTO") %>%
                anti_join(progetti_multi_fsc_1420, by = "COD_LOCALE_PROGETTO") %>%
                select(COD_LOCALE_PROGETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO,
                       OC_FINANZ_STATO_FONDO_ROT_NETTO),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE = case_when(x_AMBITO == "FSC" & OC_FINANZ_STATO_FSC_NETTO > COS_AMM ~ OC_FINANZ_STATO_FSC_NETTO,
                           # MEMO: il caso di FSC1420:::FSC1420 è gestito a monte perché non importo OC_FINANZ_STATO_FSC_NETTO
                           # x_AMBITO == "POC" & OC_FINANZ_STATO_PAC_NETTO > COS_AMM ~ OC_FINANZ_STATO_PAC_NETTO,
                           # MEMO: OC_FINANZ_STATO_PAC_NETTO è 0 per 1420
                           # x_AMBITO == "POC" & OC_FINANZ_STATO_FONDO_ROT_NETTO > COS_AMM ~ OC_FINANZ_STATO_FONDO_ROT_NETTO,
                           # MEMO: questo non so valutare quanto funzioni bene...
                           TRUE ~ COS_AMM))
  
  
  
  
  # TODO:
  # misurare effetto incremento FSC
  
  # CHK:
  # controllo rispetto ai dati nel report: si riduce molto il POC
  # verificare effetti precisi di integrazione costo vs finanziamento FSC
  # verificare composizione di multi
  
  
  # chk
  if (debug == TRUE) {
    
    message("Debug dati 1420...")
    
    # chk trasferimenti
    # operazioni_1420 %>% filter(CUP_COD_NATURA == "08") %>% count(x_AMBITO)
    # x_AMBITO     n
    # <chr>    <int>
    # 1 FESR        92
    # 2 FSC          8
    # 3 FSE         12
    # 4 POC          1
    # 5 YEI          2
    
    chk <- operazioni_1420 %>%
      filter(CUP_COD_NATURA == "08") %>%
      group_by(x_AMBITO) %>%
      summarise(oc_impegni_trasf_ammessi_FSC = sum(oc_impegni_trasf_ammessi_FSC, na.rm = TRUE),
                oc_impegni_trasf_ammessi_PAC = sum(oc_impegni_trasf_ammessi_PAC, na.rm = TRUE),
                oc_impegni_trasf_ammessi_ue = sum(oc_impegni_trasf_ammessi_ue, na.rm = TRUE),
                oc_tot_pag_trasf_FSC = sum(oc_tot_pag_trasf_FSC, na.rm = TRUE),
                oc_tot_pag_trasf_PAC = sum(oc_tot_pag_trasf_PAC, na.rm = TRUE),
                oc_tot_pag_trasf_rendicontab_ue = sum(oc_tot_pag_trasf_rendicontab_ue, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_1420_trasf.csv"))
    
    # clean
    appo <- operazioni_1420 %>%
      select(COD_LOCALE_PROGETTO,
             OC_CODICE_PROGRAMMA,
             x_AMBITO,
             COE,
             FSC = OC_FINANZ_STATO_FSC_NETTO, # DEBUG
             PAC = OC_FINANZ_STATO_PAC_NETTO, # DEBUG
             FDR = OC_FINANZ_STATO_FONDO_ROT_NETTO, # DEBUG
             COS_AMM, # DEBUG
             COE_IMP = IMP_AMM,
             COE_PAG = PAG_AMM)
    
    chk <- appo %>%
      group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      left_join(octk::po_riclass %>%
                  distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
                by = "OC_CODICE_PROGRAMMA")
    
    write_csv2(chk, file.path(TEMP, "chk_1420.csv"))
    
    chk2 <- chk %>%
      group_by(x_AMBITO) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_1420_sum.csv"))
    
    # chk duplicati
    # chk <- operazioni_1420 %>% count(COD_LOCALE_PROGETTO, x_AMBITO) %>% filter(n > 1)
    # CHK:
    # verificare perché operazioni contiene altri dupli
    
    # chk totali per ambito
    # operazioni_1420 %>%
    #   group_by(x_AMBITO) %>%
    #   summarise(N = n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
    #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
    
    
  }
  
  # clean
  operazioni_1420 <- operazioni_1420 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM)
  
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2007-2013
  
  # chk
  # operazioni_713_raw %>% count(OC_COD_FONTE, QSN_FONDO_COMUNITARIO)
  # OC_COD_FONTE QSN_FONDO_COMUNITARIO      n
  # <chr>        <chr>                  <int>
  # 1 FS0713       FESR                  110693
  # 2 FS0713       FSE                   805181
  # 3 FSC0713      ""                     22530
  # 4 FSC1420      ""                        37 <- PIANO FSC DISSESTO IDROGEOLOGICO
  # 5 PAC          ""                     25353
  
  message("Preparazione dati 713...")
  
  chk <- operazioni_713_raw %>%
    filter(OC_FLAG_PAC == 1) %>%
    group_by(OC_COD_FONTE, QSN_FONDO_COMUNITARIO, OC_COD_PROGRAMMA) %>%
    summarise(N = n(),
              OC_TOT_PAGAMENTI_RENDICONTAB_UE = sum(OC_TOT_PAGAMENTI_RENDICONTAB_UE, na.rm = TRUE),
              OC_TOT_PAGAMENTI_FSC = sum(OC_TOT_PAGAMENTI_FSC, na.rm = TRUE),
              OC_TOT_PAGAMENTI_PAC = sum(OC_TOT_PAGAMENTI_PAC, na.rm = TRUE))
  
  # MEMO: dovrebbe esserci anche 2007SA002FA016 ma ha flag OC_FLAG_PAC solo per un progetto...
  
  # FIX: duplicazione di programmi PAC-FSC (es. direttrici ferroviarie)
  appo <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    
    # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FLAG_PAC),
    #           by = "COD_LOCALE_PROGETTO") %>%
    filter(OC_FLAG_PAC == 1) %>%
    # MEMO: OC_FLAG_PAC identifica i casi dove ci sono sia pagamenti FSC che PAC
    # TODO: verifica se OC_FLAG_PAC in operazioni_flt_ok coincide con quello in progetti
    # mutate(x_AMBITO = "POC:::FSC") %>%
    mutate(x_AMBITO = "PAC:::FSC") %>%
    separate_rows(x_AMBITO, sep = ":::")
  
  operazioni_713_raw_temp <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    mutate(x_AMBITO = NA) %>%
    anti_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    bind_rows(appo)
  
  dim(operazioni_713_raw_temp)[1] == dim(operazioni_713_raw)[1] + dim(appo)[1]/2
  
  # DEBUG:
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT005FAMG1")
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT001FA005")
  #
  
  # clean
  operazioni_713 <- operazioni_713_raw_temp %>%
    # fix per caratteri spuri
    fix_operazioni_713(.) %>%
    # creo ambito e ciclo
    mutate(x_AMBITO = case_when(!is.na(x_AMBITO) ~ x_AMBITO, # MEMO: serve a incorporare fix sopra
                                OC_COD_FONTE == "FS0713" ~ QSN_FONDO_COMUNITARIO,
                                OC_COD_FONTE == "FSC1420" ~ "FSC",
                                OC_COD_FONTE == "FSC0713" ~ "FSC",
                                OC_COD_FONTE == "PAC" ~ "PAC")) %>%
    # mutate(x_CICLO = case_when(OC_COD_FONTE == "FSC1420" ~ "2014-2020",
    #                            TRUE ~ "2007-2013")) %>%
    # map per ambito
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ 0,
                               x_AMBITO == "PAC" ~ 0,
                               TRUE ~ COSTO_RENDICONTABILE_UE),
           IMP_AMM = 0,
           PAG_AMM = case_when(x_AMBITO == "FSC" ~ OC_TOT_PAGAMENTI_FSC,
                               x_AMBITO == "PAC" ~ OC_TOT_PAGAMENTI_PAC,
                               TRUE ~ OC_TOT_PAGAMENTI_RENDICONTAB_UE)) %>%
    # OLD: integra variabili da progetti (l'aggancio crea valori duplicati)
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO, IMPEGNI),
    #           by = "COD_LOCALE_PROGETTO") %>%
    # mutate(COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
    #                        x_AMBITO == "POC" ~ OC_FINANZ_STATO_PAC_NETTO,
    #                        TRUE ~ COS_AMM),
    #        x = case_when(OC_FINANZ_TOT_PUB_NETTO > 0 ~ COE / OC_FINANZ_TOT_PUB_NETTO,
    #                      TRUE ~ 0), # CHK: con fabio avevo fatto un conto diverso ma non ho variabili
    #        COE_IMP = case_when(IMPEGNI <= OC_FINANZ_TOT_PUB_NETTO ~ IMPEGNI * x,
    #                            is.na(IMPEGNI) ~ 0, # FIX
  #                            IMPEGNI <= OC_FINANZ_TOT_PUB_NETTO ~  OC_FINANZ_TOT_PUB_NETTO * x,
  # ERROR: qui va ">"!!!
  #                            TRUE ~ 0)) # FIX
  # integra variabili da progetti (l'aggancio crea valori duplicati)
  left_join(progetti %>%
              select(COD_LOCALE_PROGETTO,
                     OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO,
                     FINANZ_TOTALE_PUBBLICO, FINANZ_STATO_FSC, FINANZ_STATO_PAC,
                     IMPEGNI, TOT_PAGAMENTI),
            by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "PAC" ~ OC_FINANZ_STATO_PAC_NETTO,
                           is.na(COS_AMM) ~ 0,
                           TRUE ~ COS_AMM),
           # MEMO: qui sopra non sto facendo MAX tra FTP e FTPN (è una tecnica che uso solo per riproporzionare impegni)
           # base_ftp = if_else(OC_FINANZ_TOT_PUB_NETTO > 0,
           #                    OC_FINANZ_TOT_PUB_NETTO,
           #                    FINANZ_TOTALE_PUBBLICO),
           base_ftp = case_when(COS_AMM > OC_FINANZ_TOT_PUB_NETTO ~ COS_AMM, # FIX per x > 1
                                COE > OC_FINANZ_TOT_PUB_NETTO ~ COE, # FIX per x > 1 >>> CHK: FORSE E' ININFLUENTE
                                OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_TOT_PUB_NETTO,
                                TRUE ~ FINANZ_TOTALE_PUBBLICO),
           base_coe = case_when(x_AMBITO == "FSC" & OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_STATO_FSC_NETTO,
                                x_AMBITO == "FSC" ~ FINANZ_STATO_FSC,
                                x_AMBITO == "PAC" & OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_STATO_PAC_NETTO,
                                x_AMBITO == "PAC" ~ FINANZ_STATO_PAC,
                                is.na(COS_AMM) ~ 0,
                                TRUE ~ COS_AMM), # per FS non posso considerare valore al netto di economie
           x = base_coe/base_ftp,
           # COE_PAG = PAG_AMM,
           
           # fix per pagamenti FSC 0
           PAG_AMM_2 = case_when(x == Inf ~ 0,
                                 # fix per caso di uguaglianza completa (non riproporziona anche se CP_N > COE)
                                 # round(TOT_PAGAMENTI, 0) == round(COE, 0) & round(IMPEGNI, 0) == round(COE, 0) ~ COE,
                                 # fix per caso di uguaglianza di pagamenti comunque con impegni > COE (non riproporziona)
                                 # round(TOT_PAGAMENTI, 0) == round(COE, 0) & round(IMPEGNI, 0) > round(COE, 0) ~ COE,
                                 
                                 TOT_PAGAMENTI <= base_ftp ~ TOT_PAGAMENTI * x,
                                 TOT_PAGAMENTI > base_ftp ~ base_ftp * x,
                                 is.na(TOT_PAGAMENTI) ~ 0,
                                 # is.na(COE) ~ 0, # CHK: capire se serve
                                 TRUE ~ 0),
           
           COE_PAG = case_when(x_AMBITO == "FSC" & PAG_AMM_2 > PAG_AMM ~ PAG_AMM_2,
                               x_AMBITO == "FSC" ~ PAG_AMM,
                               TRUE ~ PAG_AMM),
           # MEMO: uso massimo valore per FSC
           
           COE_IMP = case_when(x == Inf ~ 0,
                               # fix per caso di uguaglianza completa (non riproporziona anche se CP_N > COE)
                               round(COE_PAG, 0) == round(COE, 0) & round(IMPEGNI, 0) == round(COE, 0) ~ COE,
                               # fix per caso di uguaglianza di pagamenti comunque con impegni > COE (non riproporziona)
                               round(COE_PAG, 0) == round(COE, 0) & round(IMPEGNI, 0) > round(COE, 0) ~ COE,
                               
                               IMPEGNI <= base_ftp ~ IMPEGNI * x,
                               IMPEGNI > base_ftp ~ base_ftp * x,
                               is.na(IMPEGNI) ~ 0,
                               is.na(COE) ~ 0, # CHK: capire se serve
                               TRUE ~ 0))
  
  # CHK: per "giustizia celere" resta 0 su finanziamento FSC
  
  # TODO:
  # aggiungere misura di effetto su impegni
  
  # chk
  if (debug == TRUE) {
    
    message("Debug dati 1420...")
    
    # clean
    appo <- operazioni_713 %>%
      select(COD_LOCALE_PROGETTO,
             # OC_CODICE_PROGRAMMA,
             OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
             x_AMBITO,
             COE,
             COS_AMM, # DEBUG
             FSC_N = OC_FINANZ_STATO_FSC_NETTO, # DEBUG
             PAC_N = OC_FINANZ_STATO_PAC_NETTO, # DEBUG
             CP_N = OC_FINANZ_TOT_PUB_NETTO, # DEBUG
             FSC = FINANZ_STATO_FSC, # DEBUG
             PAC = FINANZ_STATO_PAC, # DEBUG
             CP = FINANZ_TOTALE_PUBBLICO, # DEBUG
             IMPEGNI, # DEBUG
             TOT_PAGAMENTI, # DEBUG
             base_ftp, # DEBUG
             base_coe, # DEBUG
             x, # DEBUG
             COE_IMP,
             PAG_AMM,
             PAG_AMM_2,
             COE_PAG)
    
    # appo %>%
    #   filter(x > 1) %>%
    #   arrange(desc(x))
    #
    # appo %>%
    #   filter(round(COE_PAG, 0) > round(COE_IMP, 0)) %>%
    #   count(x_AMBITO)
    # MEMO: prima del fix...
    # x_AMBITO     n
    #   <chr>    <int>
    # 1 FESR     28795
    # 2 FSC       8719
    # 3 FSE      83456
    # 4 POC       3146
    # MEMO: dopo il fix
    
    # chk <- appo %>%
    #   select(-x) %>%
    #   group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
    #   summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    #   left_join(octk::po_riclass %>%
    #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
    #             by = "OC_CODICE_PROGRAMMA")
    
    chk <- appo %>%
      mutate(TIPO_ELAB = case_when(round(IMPEGNI, 1) == round(COE_IMP, 1) ~ "no_elab",
                                   is.na(IMPEGNI) ~ "isna_to_zero",
                                   COE_IMP == 0 & IMPEGNI != 0 ~ "coe_to_zero",
                                   # x > 1 ~ "x_>_1",
                                   COE_IMP == COE & IMPEGNI != COE ~ "to_coe", # dovrebbe essere incluso sopra
                                   x_AMBITO == "FSC" & CP_N <= 0 ~ "fin_tot",
                                   x_AMBITO == "PAC" & CP_N <= 0 ~ "fin_tot",
                                   round(COE_IMP, 1) == round(COE, 1) ~ "imp_equal_coe",
                                   round(COE_IMP, 2) == round(COE, 2) ~ "imp_equal_coe_2",
                                   round(COE_IMP, 1) < round(COE, 1) & round(COE_IMP, 1) < round(IMPEGNI, 1) & x < 1 ~ "ripro")
      ) %>%
      # filter(is.na(TIPO_ELAB))
      group_by(TIPO_ELAB, x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                IMPEGNI = sum(IMPEGNI, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_0713.csv"))
    
    chk2 <- chk %>%
      group_by(TIPO_ELAB) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_0713_sum.csv"))
    
    # TIPO_ELAB            N          COE      COE_IMP      IMPEGNI
    # <chr>            <int>        <dbl>        <dbl>        <dbl>
    # 1 coe_to_zero       4314           0            0   1842261708.
    # 2 fin_tot              5           0       248194.     1230647. <<< QUESTI ANDREBBERO PORTATI A 0
    # 3 imp_equal_coe      311   112825686.   112825685.   272328584.
    # 4 imp_equal_coe_2     15     4312076.     4312076.    10258899.
    # 5 isna_to_zero      4148  3621360877.           0            0
    # 6 no_elab         892785 52937267517. 50383720429. 50383720423.
    # 7 ripro             2374  5823080565.  3645377826.  7726463907.
    # 8 to_coe           60944 16308254895. 16308254895. 28893817292.
    
    # chk totali per ambito
    # operazioni_713 %>%
    #   group_by(x_AMBITO) %>%
    #   summarise(N = n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
    #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
    
    chk <- appo %>%
      mutate(TIPO_ELAB = case_when(PAG_AMM == COE_PAG ~ "no_elab",
                                   # round(PAG_AMM, 1) == round(COE_PAG, 1) ~ "no_elab_round",
                                   is.na(TOT_PAGAMENTI) ~ "na_to_zero",
                                   x == Inf ~ "inf_to_zero",
                                   PAG_AMM == 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) & TOT_PAGAMENTI > base_ftp ~ "zero_ripro_base",
                                   PAG_AMM == 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) ~ "zero_ripro",
                                   PAG_AMM != 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) & TOT_PAGAMENTI > base_ftp ~ "ripro_base",
                                   PAG_AMM != 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) ~ "ripro"
                                   # round(PAG_AMM, 1) == round(COE_PAG, 1) ~ "no_elab_round",
      )) %>%
      # filter(is.na(TIPO_ELAB))
      group_by(TIPO_ELAB, x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                PAG_AMM = sum(PAG_AMM, na.rm = TRUE),
                PAG_AMM_2 = sum(PAG_AMM_2, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                TOT_PAGAMENTI = sum(TOT_PAGAMENTI, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_0713_pag.csv"))
    
    chk2 <- chk %>%
      filter(x_AMBITO == "FSC") %>%
      group_by(TIPO_ELAB) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_0713_pag_sum.csv"))
    
    # chk %>%
    #   filter(x_AMBITO != "FSC") %>%
    #   group_by(TIPO_ELAB) %>%
    #   summarise_if(is.numeric, sum, na.rm = TRUE)
    
    
  }
  
  # clean
  operazioni_713 <- operazioni_713 %>%
    select(COD_LOCALE_PROGETTO,
           # OC_CODICE_PROGRAMMA,
           OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_IMP,
           COE_PAG)
  
  
  # ----------------------------------------------------------------------------------- #
  # bind
  
  message("Unione dati...")
  
  operazioni <- operazioni_1420 %>%
    bind_rows(operazioni_713) %>%
    # passaggio equivalente a get_x_vars
    left_join(po %>%
                # MEMO: fix per doppio entry in po_riclass
                filter(!(OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" & x_CICLO == "2007-2013"),
                       !(OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" & x_CICLO == "2007-2013")) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = "OC_CODICE_PROGRAMMA") %>%
    # fix per dissesto
    mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
                                           TRUE ~ OC_CODICE_PROGRAMMA))
  # isola visualizzati
  # MEMO: questo sotto non prende ":::" su OC_CODICE_PROGRAMMA
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = "COD_LOCALE_PROGETTO")
  # left_join(progetti %>%
  #             select(COD_LOCALE_PROGETTO, OC_FLAG_VISUALIZZAZIONE),
  #           by = "COD_LOCALE_PROGETTO")
  
  # chk compatibile con Fabio x Stefano
  # operazioni %>% distinct(COD_LOCALE_PROGETTO, x_CICLO, x_AMBITO) %>% count(x_CICLO, x_AMBITO)
  
  # chk dupli per ciclo da po_riclass
  chk <- operazioni %>% count(COD_LOCALE_PROGETTO, x_CICLO)
  dim(chk)[1] == dim(progetti)[1]
  # MEMO: attenzione che a volte si importa progetti con i soli visualizzati e salta il chk sopra
  
  # chk
  operazioni %>%
    group_by(x_CICLO, x_AMBITO) %>%
    # group_by(x_CICLO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # programmi <- operazioni %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA) %>%
  #   summarise(N = n(),
  #             COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # fix per ERDF e ESF su 713
  operazioni <- operazioni %>%
    mutate(x_AMBITO = case_when(x_AMBITO == "ERDF" ~ "FESR",
                                x_AMBITO == "ESF" ~ "FSE",
                                TRUE ~ x_AMBITO))
  
  
  # ----------------------------------------------------------------------------------- #
  # debug multi
  
  if (debug == TRUE) {
    appo <- operazioni %>%
      semi_join(progetti_multi, by = "COD_LOCALE_PROGETTO") %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_COD_FONTE),
                by = "COD_LOCALE_PROGETTO")
    
    chk <- appo %>%
      distinct(COD_LOCALE_PROGETTO, x_CICLO, OC_COD_FONTE) %>%
      count(x_CICLO, OC_COD_FONTE) %>%
      rename(N_uni = n) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE) %>%
                  summarise(N_multi = n(),
                            COE = sum(COE, na.rm = TRUE),
                            COE_IMP = sum(COE_IMP, na.rm = TRUE),
                            COE_PAG = sum(COE_PAG, na.rm = TRUE)),
                by = c("x_CICLO", "OC_COD_FONTE")) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE, x_AMBITO) %>%
                  summarise(COE = sum(COE, na.rm = TRUE)) %>%
                  spread(x_AMBITO, COE, fill = 0),
                by = c("x_CICLO", "OC_COD_FONTE"))
    
    write_csv2(chk, file.path(TEMP, "chk_multi.csv"))
    
  }
  
  # x_CICLO   OC_COD_FONTE      N_uni N_multi         COE     COE_IMP     COE_PAG        FESR        FSC       FSE        POC
  # <chr>     <chr>             <int>   <int>       <dbl>       <dbl>       <dbl>       <dbl>      <dbl>     <dbl>      <dbl>
  # 1 2007-2013 FS0713:::FSC0713    356     712  162946290.  136472860.  158173021.  136919403.  22165005.  3861882.         0
  # 2 2007-2013 FS0713:::PAC       1739    3478 2073426131. 1903065658. 1406673744. 1539023360.         0  13682697. 520720073.
  # 3 2014-2020 FS1420:::FSC1420    934    1868  605882576.  403305710.   80546329.  320443835. 211799340. 73639401.         0
  # 4 2014-2020 FS1420:::PAC1420      1       2  356095180.  248854771.  125274997.  317851434.         0         0   38243746.
  # 5 2014-2020 FSC1420:::FSC1420    36      72  519178461   166280886.   44304843.          0  519178461         0          0
  # 6 2014-2020 FSC1420:::PAC1420     1       2    3500000           0           0           0    1550000         0    19
  
  
  
  # ----------------------------------------------------------------------------------- #
  # chk
  
  # chk <- progetti %>%
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO) %>%
  #   left_join(operazioni %>%
  #               distinct(COD_LOCALE_PROGETTO, x_CICLO),
  #             by = "COD_LOCALE_PROGETTO") %>%
  #   mutate(CHK = x_CICLO.x == x_CICLO.y)
  #
  # chk %>% filter(CHK == FALSE)
  
  
  # ----------------------------------------------------------------------------------- #
  # export
  
  message("Esce da workflow")
  
  return(operazioni)
}




#' Prepara perimetro operazioni con dati coesione
#'
#' Prepara perimetro operazioni con dati coesione
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
prep_perimetro_bimestre_coesione <- function(bimestre, usa_meuro=TRUE) {
  .Deprecated("load_operazioni")
  out <- load_operazioni(bimestre, usa_meuro)
  return(out)
}
# OLD:
# prep_perimetro_bimestre_coesione <- function(bimestre, usa_meuro=TRUE) {
#   
#   # loads
#   # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
#   perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)
#   
#   # fix per dissesto
#   # TODO: da spostare a monte nel workflow di operazioni
#   # perimetro <- perimetro %>%
#   #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
#   #                                          OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
#   #                                          TRUE ~ OC_CODICE_PROGRAMMA))
#   
#   # viz
#   perimetro <- perimetro %>%
#     filter(OC_FLAG_VISUALIZZAZIONE == 0)
#   
#   # meuro
#   if (usa_meuro == TRUE) {
#     perimetro <- perimetro %>%
#       mutate(COE = COE / 1000000,
#              COE_IMP = COE_IMP / 1000000,
#              COE_PAG = COE_PAG / 1000000)
#   } else {
#     perimetro <- perimetro %>%
#       mutate(COE = COE,
#              COE_IMP = COE_IMP,
#              COE_PAG = COE_PAG)
#   }
#   
#   # simply
#   perimetro <- get_simply_non_loc(perimetro)
#   
#   # refactor
#   perimetro <- refactor_ambito(perimetro)
#   perimetro <- refactor_ciclo(perimetro)
#   
#   return(perimetro)
# }



#' Prepara perimetro operazioni con dati coesione
#'
#' Prepara perimetro operazioni con dati coesione
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
load_operazioni <- function(bimestre, usa_meuro=TRUE) {
  
  # loads
  # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)
  
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
  perimetro <- perimetro %>%
    filter(OC_FLAG_VISUALIZZAZIONE == 0)
  
  # meuro
  if (usa_meuro == TRUE) {
    perimetro <- perimetro %>%
      mutate(COE = COE / 1000000,
             COE_IMP = COE_IMP / 1000000,
             COE_PAG = COE_PAG / 1000000)
  } else {
    perimetro <- perimetro %>%
      mutate(COE = COE,
             COE_IMP = COE_IMP,
             COE_PAG = COE_PAG)
  }
  
  # simply
  perimetro <- get_simply_non_loc(perimetro)
  
  # refactor
  perimetro <- refactor_ambito(perimetro)
  perimetro <- refactor_ciclo(perimetro)
  
  return(perimetro)
}




#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param add_totali Vuoi aggiungere valori calcolati in termini di costo pubblico?
#' @param use_cp2 Se add_totali == TRUE, vuoi raddoppiare i valori relativi ai progetti multi-programma?  
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_coesione <- function(perimetro, usa_meuro=FALSE, use_713=FALSE, use_flt=FALSE,
                                           add_totali=FALSE, use_cp2=FALSE, cut_no_risorse=FALSE,
                                           tipo_ciclo="CICLO_STRATEGIA",
                                           focus="report", export=FALSE, progetti=NULL, po_riclass=NULL) {
  
  # perimetro <- operazioni
  # DEBUG: use_713 <- TRUE
  programmi <- init_programmazione(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo) %>%
    rename(x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA)
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(OC_FLAG_MONITORAGGIO == 1 | OC_FLAG_MONITORAGGIO == 2)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }

  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # MEMO: patch per factor di x_AMBITO e x_CICLO
  # perimetro <- perimetro %>%
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  # perimetro <- refactor_ambito(perimetro)
  # perimetro <- refactor_ciclo(perimetro)
  
  # if (usa_meuro == TRUE) {
  #   programmi <- programmi %>%
  #     mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  # }
  
  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")
  
  spalla <- programmi %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
    #                                               "FEAD", "FAMI", "CTE", "ORD")),
    #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE))
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1))
  }
  
  if (is.null(po_riclass)) {
    po_riclass <- octk::po_riclass
  }
  
  
  # spalla <- po_riclass %>%
  #   filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
  #          x_CICLO != "2000-2006",
  #          x_AMBITO != "FEASR") %>%
  #   filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
  #   separate_rows(x_AMBITO, sep="-") %>%
  #   # MEMO: patch per factor di x_AMBITO e x_CICLO
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #   arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   left_join(programmi %>%
  #               # MEMO: patch per factor di x_AMBITO e x_CICLO
  #               mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                             "FEAD", "FAMI", "CTE", "ORD")),
  #                      x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
  
  # report
  out <- spalla %>%
    full_join(perimetro %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    left_join(po_riclass %>%
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, COE, COE_IMP, COE_PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)
  
  
  # aggiunge valori calcolati in termini di FTPN
  if (add_totali == TRUE) {
    
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, light = TRUE, refactor = TRUE)
    }
    
    progetti <- progetti %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.) %>%
      # MEMO: patch per factor di x_AMBITO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE", "ORD")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,
             CP = OC_FINANZ_TOT_PUB_NETTO,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
    
    # if (usa_meuro == TRUE) {
    #   progetti <- progetti %>%
    #     mutate(CP = CP / 1000000,
    #            IMP = IMP / 1000000,
    #            PAG = PAG / 1000000)
    # }

    out <- out %>%
      left_join(progetti %>%
                  group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                  summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE)),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>% # MEMO: qui perdo multi ":::" in progetti
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    
    # recupero multi
    appo <- progetti %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
      summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      anti_join(out,
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
      # riempie NA con 0
      # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # recupera x_vars
      left_join(po_riclass %>%
                  refactor_ambito(.) %>%
                  refactor_ciclo(.) %>%
                  # MEMO: patch per factor di x_AMBITO e x_CICLO
                  # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                  #                                               "FEAD", "FAMI", "CTE", "ORD")),
                  #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
                  select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO"))
    
    # bind di multi
    out <- out %>%
      bind_rows(appo) %>%
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    # CHK: QUI ACCODA RIGHE ":::" A QUELLE BASE
    
    # versione con cp2
    if (use_cp2 == TRUE) {
      
      appo <- out %>%
        filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        as_tibble(.) %>%
        select(OC_CODICE_PROGRAMMA, x_AMBITO_FSE_FESR = x_AMBITO, N_CLP, CP, IMP, PAG) %>%
        # CHK: forse N_CLP non va sommato...
        mutate(x_AMBITO_FSE_FESR = as.character(x_AMBITO_FSE_FESR)) %>%
        separate_rows(OC_CODICE_PROGRAMMA, sep = ":::") %>%
        # recupera x_vars
        left_join(po_riclass %>%
                    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO),
                  by = "OC_CODICE_PROGRAMMA") %>%
        # left_join(programmi %>%
        #             as_tibble(.) %>%
        #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
        #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
        # modifica x_AMBITO
        mutate(x_AMBITO = case_when(x_AMBITO == "FESR-FSE" ~ x_AMBITO_FSE_FESR, # MEMO: split per programmi pluri-fondo
                                    x_AMBITO == "YEI-FSE" ~ x_AMBITO_FSE_FESR,
                                    x_AMBITO == "FSC-POC" ~ "FSC",  # MEMO: forzo su FSC
                                    TRUE ~ x_AMBITO)) %>%
        refactor_ambito(.) %>%
        refactor_ciclo(.) %>%
        left_join(programmi %>%
                    as_tibble(.) %>%
                    distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
                  by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
        # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
        #                                               "FEAD", "FAMI", "CTE", "ORD")),
        #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
        # ripristina struttura per bindrows
        mutate(RISORSE = 0,
               N = 0,
               COE = 0,
               COE_IMP = 0,
               COE_PAG = 0,
               `Non avviato` = 0,
               `In avvio di progettazione` = 0,
               `In corso di progettazione` = 0,
               `In affidamento` = 0,
               `In esecuzione` = 0,
               `Eseguito` = 0) %>%
        select(names(out))
      
      # TODO: qui va aggiustata la parte PAC di direttrici ferroviarie e di giustizia digitale perché CP resta vuoto (o forse dentro report programmi)
      
      
      programmi_2 <- out %>%
        filter(!grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        bind_rows(appo)
      dim(out)[1] + dim(appo)[1]/2 == dim(programmi_2)[1]
      
      # sovrascrive out
      out <- programmi_2 %>%
        group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE)
      
      if (usa_meuro == TRUE) {
        out <- out %>%
          mutate(CP = round(CP / 1000000, 1),
                 IMP = round(IMP / 1000000, 1),
                 PAG = round(PAG / 1000000, 1))
      }
      
    }
  }
  
  if (cut_no_risorse == TRUE) {
    out <- out %>%
      filter(RISORSE > 0)
  }
  
  if (export == TRUE) {
    if (use_cp2 == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi.csv")), row.names = FALSE)
    }
  }
  
  
  return(out)
}




#' Integra report per Programmi con delta da un bimestre precedente (per coesione)
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
#' @note DA COMPLETARE
add_delta_programmi_coesione <- function(bimestre, programmi, last_bimestre, last_data_path=NULL,
                                         usa_meuro=FALSE, focus="delta", export=FALSE) {
  
  # bimestre precedente
  if (is.null(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }
  
  # loads
  last_perimetro <- read_csv2(file.path(last_data_path, paste0("operazioni_light_", last_bimestre, ".csv")), guess_max = 1000000)
  
  # switch variabile COE
  # last_perimetro <- last_progetti %>%
  #   mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #   mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
  #          COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
  #                          x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
  #                          x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
  #                          TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(COE = COE / 1000000,
             COE_IMP = COE_IMP / 1000000,
             COE_PAG = COE_PAG / 1000000)
  }
  
  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)
  
  appo <- last_perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                               "FEAD", "FAMI", "CTE")),
  #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  
  # report
  out <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
    #                                               "FEAD", "FAMI", "CTE")),
    #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    # refactor_ambito(.) %>%
    # refactor_ciclo(.) %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (COE_PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  
  # if (add_totali == TRUE) {
  #
  #   last_progetti <- load_progetti(bimestre = last_bimestre,
  #                                  data_path = last_data_path,
  #                                  visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE) %>%
  #     select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,
  #            CP = OC_FINANZ_TOT_PUB_NETTO,
  #            IMP = IMPEGNI,
  #            PAG = TOT_PAGAMENTI)
  #
  #   if (usa_meuro == TRUE) {
  #     last_progetti <- last_progetti %>%
  #       mutate(CP = CP / 1000000,
  #              IMP = IMP / 1000000,
  #              PAG = PAG / 1000000)
  #
  # TODO: aggiungere il calcolo
  #
  #   }
  # } 
  
  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}



#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_operazioni <- function(df) {
  
  df <- df %>%
    mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
                                      ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
                                      ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
                                        PO_FONDO == "FESR" ~ "FESR",
                                      ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
                                        PO_FONDO == "FESR FSE" & OC_CODICE_PROGRAMMA == "2014IT16M2OP002" ~ "FESR", #MEMO: problema per 42 progetti su 2014IT16M2OP002:::2016PATTIPUG
                                      oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
                                      oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG:::FSE" ~ "IOG",
                                      OC_CODICE_PROGRAMMA == "2019MATTMINA001" ~ "ORD",
                                      ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
                                        CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "NAZIONALE", # fix per 1MISEABIN-PSRA-89-153
                                      TRUE ~ ue_descr_fondo))
  
  # df <- df %>%
  #   mutate(COD_LOCALE_PROGETTO = case_when(grepl("^1MISE174", COD_LOCALE_PROGETTO) ~ "1MISE174",
  #                                          grepl("^1MISE397", COD_LOCALE_PROGETTO) ~ "1MISE397",
  #                                          grepl("^1MISE496", COD_LOCALE_PROGETTO) ~ "1MISE496",
  #                                          grepl("^1MISE608", COD_LOCALE_PROGETTO) ~ "1MISE608",
  #                                          TRUE ~ COD_LOCALE_PROGETTO))
  
  return(df)
}


#' Fix temporaneo per i dataset operazioni (versione per 713)
#'
#' Integra il dataset risolvendo il problema di caratteri spuri. Attenzione perché va fatto anche in "progetti".
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_operazioni_713 <- function(df) {
  
  
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


#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre
  
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
  
  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }
  
  return(report)
}




#' Workflow per creare il dataset operazioni
workflow_operazioni_ecomix <- function(bimestre, progetti, debug=FALSE) {
  
  # MEMO:
  # i programmi con ciclo sbagliato non sono ancora spostati nei file operazioni.sas
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass
  
  operazioni_1420_raw <- read_sas(file.path(DATA, "operazioni_pucok.sas7bdat"))
  message("Operazioni raw caricate per 1420")
  
  operazioni_713_raw <- read_sas(file.path(DATA, "operazioni_fltok.sas7bdat"))
  
  message("Operazioni raw caricate per 713")
  
  
  # ----------------------------------------------------------------------------------- #
  # prep
  
  # progetti in programmi multi con ":::"
  progetti_multi <- progetti %>%
    filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # progetti in programmi multi con ":::" in 2 programmi fsc
  progetti_multi_fsc_1420 <- progetti %>%
    # filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    filter(OC_COD_FONTE == "FSC1420:::FSC1420") %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2014-2020
  
  # chk
  # operazioni_1420_raw %>% count(oc_cod_fonte, ue_descr_fondo, CODICE_TIPOLOGIA_PROGRAMMA, STATO)
  # oc_cod_fonte ue_descr_fondo      n
  # <chr>        <chr>           <int>
  # 1 FS1420       ""                  1 <- ? 2014IT16RFOP010 POR LAZIO FESR (da eliminare)
  # 2 FS1420       FEAMP            6856
  # 3 FS1420       FESR            58220
  # 4 FS1420       FSE            304988
  # 5 FS1420       IOG            195158
  # 6 FS1420       PAC                25 <- ? SNAI
  # 7 FSC1420      FSC              8435
  # 8 PAC1420      PAC               858
  
  
  # clean
  operazioni_1420 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni(.) %>%
    # mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
    #                                     PO_FONDO == "FESR" ~ "FESR",
    #                                   oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
    #                                   TRUE ~ ue_descr_fondo)) %>%
    # creo ambito
    mutate(x_AMBITO = case_when(oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
                                oc_cod_fonte == "FSC1420" ~ "FSC",
                                oc_cod_fonte == "PAC1420" ~ "POC")) %>%
    # map per ambito
    # mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ costo_ammesso_FSC,
    #                            x_AMBITO == "POC" ~ costo_ammesso_NAZ,
    #                            x_AMBITO == "SNAI" ~ costo_ammesso_NAZ,
    #                            TRUE ~ costo_rendicontabile_UE),
    #        IMP_AMM = case_when(x_AMBITO == "FSC" ~ oc_impegni_ammessi_FSC,
    #                            x_AMBITO == "POC" ~ oc_impegni_ammessi_PAC,
    #                            x_AMBITO == "SNAI" ~ oc_impegni_ammessi_PAC,
    #                            TRUE ~ oc_impegni_ammessi_ue),
    #        PAG_AMM = case_when(x_AMBITO == "FSC" ~ oc_tot_pagamenti_FSC,
    #                            x_AMBITO == "POC" ~ oc_tot_pagamenti_PAC,
  #                            x_AMBITO == "SNAI" ~ oc_tot_pagamenti_PAC,
  #                            TRUE ~ oc_tot_pagamenti_rendicontab_ue)) %>%
  
  # map per ambito (REV TRASFERIMENTI)
  left_join(progetti %>%
              select(COD_LOCALE_PROGETTO, CUP_COD_NATURA),
            by = "COD_LOCALE_PROGETTO") %>%
    # CUP_COD_NATURA == "08"
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ costo_ammesso_FSC,
                               x_AMBITO == "POC" ~ costo_ammesso_NAZ,
                               x_AMBITO == "SNAI" ~ costo_ammesso_NAZ,
                               TRUE ~ costo_rendicontabile_UE),
           IMP_AMM = case_when(x_AMBITO == "FSC" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_FSC,
                               x_AMBITO == "FSC" ~ oc_impegni_ammessi_FSC,
                               x_AMBITO == "POC" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_PAC,
                               x_AMBITO == "POC" ~ oc_impegni_ammessi_PAC,
                               x_AMBITO == "SNAI" & CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_PAC,
                               x_AMBITO == "SNAI" ~ oc_impegni_ammessi_PAC,
                               CUP_COD_NATURA == "08" ~ oc_impegni_trasf_ammessi_ue,
                               TRUE ~ oc_impegni_ammessi_ue),
           PAG_AMM = case_when(x_AMBITO == "FSC" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_FSC,
                               x_AMBITO == "FSC" ~ oc_tot_pagamenti_FSC,
                               x_AMBITO == "POC" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_PAC,
                               x_AMBITO == "POC" ~ oc_tot_pagamenti_PAC,
                               x_AMBITO == "SNAI" & CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_PAC,
                               x_AMBITO == "SNAI" ~ oc_tot_pagamenti_PAC,
                               CUP_COD_NATURA == "08" ~ oc_tot_pag_trasf_rendicontab_ue,
                               TRUE ~ oc_tot_pagamenti_rendicontab_ue)) %>%
    
    # incrementa COE con finanziamento FSC (con mix tra netto e lordo economie) 
    # MEMO: per progetti "non multi" o "multi" di cui solo 1 programma FSC (per lasciare fuori i multi su 2 programmi FSC)
    left_join(progetti %>%
                # anti_join(progetti_multi, by = "COD_LOCALE_PROGETTO") %>%
                anti_join(progetti_multi_fsc_1420, by = "COD_LOCALE_PROGETTO") %>%
                select(COD_LOCALE_PROGETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO,
                       OC_FINANZ_STATO_FONDO_ROT_NETTO, FINANZ_STATO_FSC, OC_STATO_PROGETTO),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE = case_when(x_AMBITO == "FSC" & OC_STATO_PROGETTO == "Concluso" & OC_FINANZ_STATO_FSC_NETTO > COS_AMM ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSC" & OC_STATO_PROGETTO == "Liquidato" & OC_FINANZ_STATO_FSC_NETTO > COS_AMM ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSC" & FINANZ_STATO_FSC > COS_AMM ~ FINANZ_STATO_FSC,
                           TRUE ~ COS_AMM))
  
  
  # TODO:
  # misurare effetto incremento FSC
  
  # CHK:
  # controllo rispetto ai dati nel report: si riduce molto il POC
  # verificare effetti precisi di integrazione costo vs finanziamento FSC
  # verificare composizione di multi
  
  
  # chk
  if (debug == TRUE) {
    
    # chk trasferimenti
    # operazioni_1420 %>% filter(CUP_COD_NATURA == "08") %>% count(x_AMBITO)
    # x_AMBITO     n
    # <chr>    <int>
    # 1 FESR        92
    # 2 FSC          8
    # 3 FSE         12
    # 4 POC          1
    # 5 YEI          2
    
    chk <- operazioni_1420 %>%
      filter(CUP_COD_NATURA == "08") %>%
      group_by(x_AMBITO) %>%
      summarise(oc_impegni_trasf_ammessi_FSC = sum(oc_impegni_trasf_ammessi_FSC, na.rm = TRUE),
                oc_impegni_trasf_ammessi_PAC = sum(oc_impegni_trasf_ammessi_PAC, na.rm = TRUE),
                oc_impegni_trasf_ammessi_ue = sum(oc_impegni_trasf_ammessi_ue, na.rm = TRUE),
                oc_tot_pag_trasf_FSC = sum(oc_tot_pag_trasf_FSC, na.rm = TRUE),
                oc_tot_pag_trasf_PAC = sum(oc_tot_pag_trasf_PAC, na.rm = TRUE),
                oc_tot_pag_trasf_rendicontab_ue = sum(oc_tot_pag_trasf_rendicontab_ue, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_1420_trasf.csv"))
    
    # clean
    appo <- operazioni_1420 %>%
      select(COD_LOCALE_PROGETTO,
             OC_CODICE_PROGRAMMA,
             x_AMBITO,
             COE,
             FSC = OC_FINANZ_STATO_FSC_NETTO, # DEBUG
             PAC = OC_FINANZ_STATO_PAC_NETTO, # DEBUG
             FDR = OC_FINANZ_STATO_FONDO_ROT_NETTO, # DEBUG
             COS_AMM, # DEBUG
             COE_IMP = IMP_AMM,
             COE_PAG = PAG_AMM)
    
    chk <- appo %>%
      group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      left_join(octk::po_riclass %>%
                  distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
                by = "OC_CODICE_PROGRAMMA")
    
    write_csv2(chk, file.path(TEMP, "chk_1420.csv"))
    
    chk2 <- chk %>%
      group_by(x_AMBITO) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_1420_sum.csv"))
    
    # chk duplicati
    # chk <- operazioni_1420 %>% count(COD_LOCALE_PROGETTO, x_AMBITO) %>% filter(n > 1)
    # CHK:
    # verificare perché operazioni contiene altri dupli
    
    # chk totali per ambito
    # operazioni_1420 %>%
    #   group_by(x_AMBITO) %>%
    #   summarise(N = n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
    #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
    
    
  }
  
  # clean
  operazioni_1420 <- operazioni_1420 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM)
  
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2007-2013
  
  # chk
  # operazioni_713_raw %>% count(OC_COD_FONTE, QSN_FONDO_COMUNITARIO)
  # OC_COD_FONTE QSN_FONDO_COMUNITARIO      n
  # <chr>        <chr>                  <int>
  # 1 FS0713       FESR                  110693
  # 2 FS0713       FSE                   805181
  # 3 FSC0713      ""                     22530
  # 4 FSC1420      ""                        37 <- PIANO FSC DISSESTO IDROGEOLOGICO
  # 5 PAC          ""                     25353
  
  chk <- operazioni_713_raw %>%
    filter(OC_FLAG_PAC == 1) %>%
    group_by(OC_COD_FONTE, QSN_FONDO_COMUNITARIO, OC_COD_PROGRAMMA) %>%
    summarise(N = n(),
              OC_TOT_PAGAMENTI_RENDICONTAB_UE = sum(OC_TOT_PAGAMENTI_RENDICONTAB_UE, na.rm = TRUE),
              OC_TOT_PAGAMENTI_FSC = sum(OC_TOT_PAGAMENTI_FSC, na.rm = TRUE),
              OC_TOT_PAGAMENTI_PAC = sum(OC_TOT_PAGAMENTI_PAC, na.rm = TRUE))
  
  # MEMO: dovrebbe esserci anche 2007SA002FA016 ma ha flag OC_FLAG_PAC solo per un progetto...
  
  # FIX: duplicazione di programmi PAC-FSC (es. direttrici ferroviarie)
  appo <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    
    # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FLAG_PAC),
    #           by = "COD_LOCALE_PROGETTO") %>%
    filter(OC_FLAG_PAC == 1) %>%
    # MEMO: OC_FLAG_PAC identifica i casi dove ci sono sia pagamenti FSC che PAC
    # TODO: verifica se OC_FLAG_PAC in operazioni_flt_ok coincide con quello in progetti
    mutate(x_AMBITO = "POC:::FSC") %>%
    separate_rows(x_AMBITO, sep = ":::")
  
  operazioni_713_raw_temp <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    mutate(x_AMBITO = NA) %>%
    anti_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    bind_rows(appo)
  
  dim(operazioni_713_raw_temp)[1] == dim(operazioni_713_raw)[1] + dim(appo)[1]/2
  
  # DEBUG:
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT005FAMG1")
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT001FA005")
  #
  
  # clean
  operazioni_713 <- operazioni_713_raw_temp %>%
    # fix per caratteri spuri
    fix_operazioni_713(.) %>%
    # creo ambito e ciclo
    mutate(x_AMBITO = case_when(!is.na(x_AMBITO) ~ x_AMBITO, # MEMO: serve a incorporare fix sopra
                                OC_COD_FONTE == "FS0713" ~ QSN_FONDO_COMUNITARIO,
                                OC_COD_FONTE == "FSC1420" ~ "FSC",
                                OC_COD_FONTE == "FSC0713" ~ "FSC",
                                OC_COD_FONTE == "PAC" ~ "POC")) %>%
    # mutate(x_CICLO = case_when(OC_COD_FONTE == "FSC1420" ~ "2014-2020",
    #                            TRUE ~ "2007-2013")) %>%
    # map per ambito
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ 0,
                               x_AMBITO == "POC" ~ 0,
                               TRUE ~ COSTO_RENDICONTABILE_UE),
           IMP_AMM = 0,
           PAG_AMM = case_when(x_AMBITO == "FSC" ~ OC_TOT_PAGAMENTI_FSC,
                               x_AMBITO == "POC" ~ OC_TOT_PAGAMENTI_PAC,
                               TRUE ~ OC_TOT_PAGAMENTI_RENDICONTAB_UE)) %>%
    # integra variabili da progetti (l'aggancio crea valori duplicati)
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO,
                       OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO,
                       FINANZ_TOTALE_PUBBLICO, FINANZ_STATO_FSC, FINANZ_STATO_PAC,
                       IMPEGNI, TOT_PAGAMENTI, OC_STATO_PROGETTO),
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(COE = case_when(x_AMBITO == "FSC" & OC_STATO_PROGETTO == "Concluso" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSC" & OC_STATO_PROGETTO == "Liquidato" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSC" ~ FINANZ_STATO_FSC,
                           x_AMBITO == "POC" ~ OC_FINANZ_STATO_PAC_NETTO,
                           is.na(COS_AMM) ~ 0,
                           TRUE ~ COS_AMM),
           # MEMO: qui sopra non sto facendo MAX tra FTP e FTPN (è una tecnica che uso solo per riproporzionare impegni)
           # base_ftp = if_else(OC_FINANZ_TOT_PUB_NETTO > 0,
           #                    OC_FINANZ_TOT_PUB_NETTO,
           #                    FINANZ_TOTALE_PUBBLICO),
           base_ftp = case_when(COS_AMM > OC_FINANZ_TOT_PUB_NETTO ~ COS_AMM, # FIX per x > 1
                                COE > OC_FINANZ_TOT_PUB_NETTO ~ COE, # FIX per x > 1 >>> CHK: FORSE E' ININFLUENTE
                                OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_TOT_PUB_NETTO,
                                TRUE ~ FINANZ_TOTALE_PUBBLICO),
           base_coe = case_when(x_AMBITO == "FSC" & OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_STATO_FSC_NETTO,
                                x_AMBITO == "FSC" ~ FINANZ_STATO_FSC,
                                x_AMBITO == "POC" & OC_FINANZ_TOT_PUB_NETTO > 0 ~ OC_FINANZ_STATO_PAC_NETTO,
                                x_AMBITO == "POC" ~ FINANZ_STATO_PAC,
                                is.na(COS_AMM) ~ 0,
                                TRUE ~ COS_AMM), # per FS non posso considerare valore al netto di economie
           x = base_coe/base_ftp,
           # COE_PAG = PAG_AMM,
           
           # fix per pagamenti FSC 0
           PAG_AMM_2 = case_when(x == Inf ~ 0,
                                 # fix per caso di uguaglianza completa (non riproporziona anche se CP_N > COE)
                                 # round(TOT_PAGAMENTI, 0) == round(COE, 0) & round(IMPEGNI, 0) == round(COE, 0) ~ COE,
                                 # fix per caso di uguaglianza di pagamenti comunque con impegni > COE (non riproporziona)
                                 # round(TOT_PAGAMENTI, 0) == round(COE, 0) & round(IMPEGNI, 0) > round(COE, 0) ~ COE,
                                 
                                 TOT_PAGAMENTI <= base_ftp ~ TOT_PAGAMENTI * x,
                                 TOT_PAGAMENTI > base_ftp ~ base_ftp * x,
                                 is.na(TOT_PAGAMENTI) ~ 0,
                                 # is.na(COE) ~ 0, # CHK: capire se serve
                                 TRUE ~ 0),
           
           COE_PAG = case_when(x_AMBITO == "FSC" & PAG_AMM_2 > PAG_AMM ~ PAG_AMM_2,
                               x_AMBITO == "FSC" ~ PAG_AMM,
                               TRUE ~ PAG_AMM),
           # MEMO: uso massimo valore per FSC
           
           COE_IMP = case_when(x == Inf ~ 0,
                               # fix per caso di uguaglianza completa (non riproporziona anche se CP_N > COE)
                               round(COE_PAG, 0) == round(COE, 0) & round(IMPEGNI, 0) == round(COE, 0) ~ COE,
                               # fix per caso di uguaglianza di pagamenti comunque con impegni > COE (non riproporziona)
                               round(COE_PAG, 0) == round(COE, 0) & round(IMPEGNI, 0) > round(COE, 0) ~ COE,
                               
                               IMPEGNI <= base_ftp ~ IMPEGNI * x,
                               IMPEGNI > base_ftp ~ base_ftp * x,
                               is.na(IMPEGNI) ~ 0,
                               is.na(COE) ~ 0, # CHK: capire se serve
                               TRUE ~ 0))
  
  # CHK: per "giustizia celere" resta 0 su finanziamento FSC
  
  # TODO:
  # aggiungere misura di effetto su impegni
  
  # chk
  if (debug == TRUE) {
    
    # clean
    appo <- operazioni_713 %>%
      select(COD_LOCALE_PROGETTO,
             # OC_CODICE_PROGRAMMA,
             OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
             x_AMBITO,
             COE,
             COS_AMM, # DEBUG
             FSC_N = OC_FINANZ_STATO_FSC_NETTO, # DEBUG
             PAC_N = OC_FINANZ_STATO_PAC_NETTO, # DEBUG
             CP_N = OC_FINANZ_TOT_PUB_NETTO, # DEBUG
             FSC = FINANZ_STATO_FSC, # DEBUG
             PAC = FINANZ_STATO_PAC, # DEBUG
             CP = FINANZ_TOTALE_PUBBLICO, # DEBUG
             IMPEGNI, # DEBUG
             TOT_PAGAMENTI, # DEBUG
             base_ftp, # DEBUG
             base_coe, # DEBUG
             x, # DEBUG
             COE_IMP,
             PAG_AMM,
             PAG_AMM_2,
             COE_PAG)
    
    # appo %>%
    #   filter(x > 1) %>%
    #   arrange(desc(x))
    #
    # appo %>%
    #   filter(round(COE_PAG, 0) > round(COE_IMP, 0)) %>%
    #   count(x_AMBITO)
    # MEMO: prima del fix...
    # x_AMBITO     n
    #   <chr>    <int>
    # 1 FESR     28795
    # 2 FSC       8719
    # 3 FSE      83456
    # 4 POC       3146
    # MEMO: dopo il fix
    
    # chk <- appo %>%
    #   select(-x) %>%
    #   group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
    #   summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    #   left_join(octk::po_riclass %>%
    #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
    #             by = "OC_CODICE_PROGRAMMA")
    
    chk <- appo %>%
      mutate(TIPO_ELAB = case_when(round(IMPEGNI, 1) == round(COE_IMP, 1) ~ "no_elab",
                                   is.na(IMPEGNI) ~ "isna_to_zero",
                                   COE_IMP == 0 & IMPEGNI != 0 ~ "coe_to_zero",
                                   # x > 1 ~ "x_>_1",
                                   COE_IMP == COE & IMPEGNI != COE ~ "to_coe", # dovrebbe essere incluso sopra
                                   x_AMBITO == "FSC" & CP_N <= 0 ~ "fin_tot",
                                   x_AMBITO == "POC" & CP_N <= 0 ~ "fin_tot",
                                   round(COE_IMP, 1) == round(COE, 1) ~ "imp_equal_coe",
                                   round(COE_IMP, 2) == round(COE, 2) ~ "imp_equal_coe_2",
                                   round(COE_IMP, 1) < round(COE, 1) & round(COE_IMP, 1) < round(IMPEGNI, 1) & x < 1 ~ "ripro")
      ) %>%
      # filter(is.na(TIPO_ELAB))
      group_by(TIPO_ELAB, x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                IMPEGNI = sum(IMPEGNI, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_0713.csv"))
    
    chk2 <- chk %>%
      group_by(TIPO_ELAB) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_0713_sum.csv"))
    
    # TIPO_ELAB            N          COE      COE_IMP      IMPEGNI
    # <chr>            <int>        <dbl>        <dbl>        <dbl>
    # 1 coe_to_zero       4314           0            0   1842261708.
    # 2 fin_tot              5           0       248194.     1230647. <<< QUESTI ANDREBBERO PORTATI A 0
    # 3 imp_equal_coe      311   112825686.   112825685.   272328584.
    # 4 imp_equal_coe_2     15     4312076.     4312076.    10258899.
    # 5 isna_to_zero      4148  3621360877.           0            0
    # 6 no_elab         892785 52937267517. 50383720429. 50383720423.
    # 7 ripro             2374  5823080565.  3645377826.  7726463907.
    # 8 to_coe           60944 16308254895. 16308254895. 28893817292.
    
    # chk totali per ambito
    # operazioni_713 %>%
    #   group_by(x_AMBITO) %>%
    #   summarise(N = n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
    #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
    
    chk <- appo %>%
      mutate(TIPO_ELAB = case_when(PAG_AMM == COE_PAG ~ "no_elab",
                                   # round(PAG_AMM, 1) == round(COE_PAG, 1) ~ "no_elab_round",
                                   is.na(TOT_PAGAMENTI) ~ "na_to_zero",
                                   x == Inf ~ "inf_to_zero",
                                   PAG_AMM == 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) & TOT_PAGAMENTI > base_ftp ~ "zero_ripro_base",
                                   PAG_AMM == 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) ~ "zero_ripro",
                                   PAG_AMM != 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) & TOT_PAGAMENTI > base_ftp ~ "ripro_base",
                                   PAG_AMM != 0 & round(PAG_AMM_2, 1) == round(COE_PAG, 1) ~ "ripro"
                                   # round(PAG_AMM, 1) == round(COE_PAG, 1) ~ "no_elab_round",
      )) %>%
      # filter(is.na(TIPO_ELAB))
      group_by(TIPO_ELAB, x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                PAG_AMM = sum(PAG_AMM, na.rm = TRUE),
                PAG_AMM_2 = sum(PAG_AMM_2, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                TOT_PAGAMENTI = sum(TOT_PAGAMENTI, na.rm = TRUE))
    
    write_csv2(chk, file.path(TEMP, "chk_0713_pag.csv"))
    
    chk2 <- chk %>%
      filter(x_AMBITO == "FSC") %>%
      group_by(TIPO_ELAB) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)
    
    write_csv2(chk2, file.path(TEMP, "chk_0713_pag_sum.csv"))
    
    # chk %>%
    #   filter(x_AMBITO != "FSC") %>%
    #   group_by(TIPO_ELAB) %>%
    #   summarise_if(is.numeric, sum, na.rm = TRUE)
    
    
  }
  
  # clean
  operazioni_713 <- operazioni_713 %>%
    select(COD_LOCALE_PROGETTO,
           # OC_CODICE_PROGRAMMA,
           OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_IMP,
           COE_PAG)
  
  
  # ----------------------------------------------------------------------------------- #
  # bind
  
  operazioni <- operazioni_1420 %>%
    bind_rows(operazioni_713) %>%
    # passaggio equivalente a get_x_vars
    left_join(po %>%
                # MEMO: fix per doppio entry in po_riclass
                filter(!(OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" & x_CICLO == "2007-2013"),
                       !(OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" & x_CICLO == "2007-2013")) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = "OC_CODICE_PROGRAMMA") %>%
    # fix per dissesto
    mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
                                           TRUE ~ OC_CODICE_PROGRAMMA))
  # isola visualizzati
  # MEMO: questo sotto non prende ":::" su OC_CODICE_PROGRAMMA
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = "COD_LOCALE_PROGETTO")
  # left_join(progetti %>%
  #             select(COD_LOCALE_PROGETTO, OC_FLAG_VISUALIZZAZIONE),
  #           by = "COD_LOCALE_PROGETTO")
  
  # chk compatibile con Fabio x Stefano
  # operazioni %>% distinct(COD_LOCALE_PROGETTO, x_CICLO, x_AMBITO) %>% count(x_CICLO, x_AMBITO)
  
  # chk dupli per ciclo da po_riclass
  chk <- operazioni %>% count(COD_LOCALE_PROGETTO, x_CICLO)
  dim(chk)[1] == dim(progetti)[1]
  # MEMO: attenzione che a volte si importa progetti con i soli visualizzati e salta il chk sopra
  
  # chk
  operazioni %>%
    group_by(x_CICLO, x_AMBITO) %>%
    # group_by(x_CICLO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # programmi <- operazioni %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA) %>%
  #   summarise(N = n(),
  #             COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # fix per ERDF e ESF su 713
  operazioni <- operazioni %>%
    mutate(x_AMBITO = case_when(x_AMBITO == "ERDF" ~ "FESR",
                                x_AMBITO == "ESF" ~ "FSE",
                                TRUE ~ x_AMBITO))
  
  
  # ----------------------------------------------------------------------------------- #
  # debug multi
  
  if (debug == TRUE) {
    appo <- operazioni %>%
      semi_join(progetti_multi, by = "COD_LOCALE_PROGETTO") %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_COD_FONTE),
                by = "COD_LOCALE_PROGETTO")
    
    chk <- appo %>%
      distinct(COD_LOCALE_PROGETTO, x_CICLO, OC_COD_FONTE) %>%
      count(x_CICLO, OC_COD_FONTE) %>%
      rename(N_uni = n) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE) %>%
                  summarise(N_multi = n(),
                            COE = sum(COE, na.rm = TRUE),
                            COE_IMP = sum(COE_IMP, na.rm = TRUE),
                            COE_PAG = sum(COE_PAG, na.rm = TRUE)),
                by = c("x_CICLO", "OC_COD_FONTE")) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE, x_AMBITO) %>%
                  summarise(COE = sum(COE, na.rm = TRUE)) %>%
                  spread(x_AMBITO, COE, fill = 0),
                by = c("x_CICLO", "OC_COD_FONTE"))
    
    write_csv2(chk, file.path(TEMP, "chk_multi.csv"))
    
  }
  
  # x_CICLO   OC_COD_FONTE      N_uni N_multi         COE     COE_IMP     COE_PAG        FESR        FSC       FSE        POC
  # <chr>     <chr>             <int>   <int>       <dbl>       <dbl>       <dbl>       <dbl>      <dbl>     <dbl>      <dbl>
  # 1 2007-2013 FS0713:::FSC0713    356     712  162946290.  136472860.  158173021.  136919403.  22165005.  3861882.         0
  # 2 2007-2013 FS0713:::PAC       1739    3478 2073426131. 1903065658. 1406673744. 1539023360.         0  13682697. 520720073.
  # 3 2014-2020 FS1420:::FSC1420    934    1868  605882576.  403305710.   80546329.  320443835. 211799340. 73639401.         0
  # 4 2014-2020 FS1420:::PAC1420      1       2  356095180.  248854771.  125274997.  317851434.         0         0   38243746.
  # 5 2014-2020 FSC1420:::FSC1420    36      72  519178461   166280886.   44304843.          0  519178461         0          0
  # 6 2014-2020 FSC1420:::PAC1420     1       2    3500000           0           0           0    1550000         0    19
  
  
  
  # ----------------------------------------------------------------------------------- #
  # chk
  
  # chk <- progetti %>%
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO) %>%
  #   left_join(operazioni %>%
  #               distinct(COD_LOCALE_PROGETTO, x_CICLO),
  #             by = "COD_LOCALE_PROGETTO") %>%
  #   mutate(CHK = x_CICLO.x == x_CICLO.y)
  #
  # chk %>% filter(CHK == FALSE)
  
  
  # ----------------------------------------------------------------------------------- #
  # export
  
  return(operazioni)
}



#' Esporta report per ciclo, ambito e macroarea con dati coesione
#'
#' Report con apertura ciclo, ambito e macroarea.
#' E' costruito su operazioni e dati coesione.
#'
#' @param risorse Dataset proveniente da make_report_risorse()
#' @param perimetro Dataset di classe "operazioni"
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo, ambto e macroarea.
make_report_macroaree_coesione <- function(risorse=NULL, perimetro=NULL, use_meuro=TRUE, cut_no_risorse=FALSE,
                                           focus="report", export=TRUE) {
  
  if (is.null(risorse)) {
    # risorse <- make_report_risorse(use_meuro=TRUE, export=FALSE)
    risorse <- make_report_risorse(use_meuro=use_meuro, use_flt=TRUE, force_yei=TRUE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE)
    
  } 
  
  if (is.null(perimetro)) {
    perimetro <- prep_perimetro_bimestre_coesione(bimestre, usa_meuro=FALSE)
    # MEMO: qui fa usa_meuro=FALSE per deifnizione perché dopo viene aggregato
  }
  
  
  if (cut_no_risorse == TRUE) {
    appo <- init_programmazione(use_713 = TRUE, use_flt = TRUE) %>%
      filter(FINANZ_TOTALE_PUBBLICO > 0, OC_FLAG_MONITORAGGIO != 0, OC_FLAG_MONITORAGGIO != 9) %>%
      select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, FINANZ_TOTALE_PUBBLICO)
    
    # perimetro <- operazioni_2
    perimetro <- perimetro %>%
      semi_join(appo, by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
      # semi_join(appo, by = "OC_CODICE_PROGRAMMA")
    
    # # TEST
    # perimetro %>%
    #   filter(x_AMBITO != "CTE") %>%
    #   summarise(x = sum(COE, na.rm = T))
    # 
    # appo1 <- programmi_2 %>% 
    #   select(OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, RISORSE) %>%
    #   full_join(perimetro, by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
    #   group_by(OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO) %>%
    #   summarise(RISORSE = sum(RISORSE, na.rm = TRUE),
    #             COE = sum(COE, na.rm = TRUE))
    # 
    # # MOL_DEBITI_1	
    
    }
  
  appo <- perimetro %>% 
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              N = n())
  
  if (use_meuro == TRUE) {
    # MEMO: questo serve se non importo direttamente perimetro e ne uso uno esterno che potrebbe non essere in meuro
    appo <- appo %>%
      mutate(COE = round(COE/1000000, 1),
             COE_IMP = round(COE_IMP/1000000, 1),
             COE_PAG = round(COE_PAG/1000000, 1))
  }
  
  out <- risorse %>%
    full_join(appo, 
              by = c("x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    arrange(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    mutate_if(is.numeric, list(~replace(., is.na(.), 0)))
              
  
  if (export == TRUE) {
    # write.csv2(out, file.path(TEMP, "report_macroaree.csv"), row.names = FALSE)
    write.csv2(out, file.path(TEMP, paste0(focus, "_macroaree.csv")), row.names = FALSE)
  }

  return(out)
}



#' Varifica totale di RISORSE
#'
#' Report con apertura ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
#'
#' @param report Dataframe da make_report_bimestre_coesione()
#' @param programmi Dataframe da make_report_programmi_coesione()
#' @param report_macroaree Dataframe da make_report_macroaree_coesione()
#' @param risorse Dataframe da make_report_risorse().E' opzionale
#' @return Dataframe per ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
chk_allineamento_risorse <- function(report, programmi, report_macroaree, risorse=NULL) {
  
  if (is.null(risorse)) {
    risorse <- make_report_risorse(use_meuro=TRUE, use_flt=TRUE, use_eu=FALSE, force_yei=TRUE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE)
    message("Attenzione: 'risorse' è in meuro e potrebbe essere in valuta diversa dagli altri report confrontati")
  }
  
  out <- report %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    full_join(programmi %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(report_macroaree %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(risorse %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) 
  
  names(out) <- c("x_CICLO", "x_AMBITO", "report", "programmi", "report_macroaree", "risorse")
  
  return(out)
  
}


#' Varifica totale di COE
#'
#' Report con apertura ciclo e ambito con confronto del totale di COE in diversi report di octk.
#'
#' @param report Dataframe da make_report_bimestre_coesione()
#' @param programmi Dataframe da make_report_programmi_coesione()
#' @param report_macroaree Dataframe da make_report_macroaree_coesione()
#' @param perimetro Dataframe da prep_perimetro_bimestre_coesione().E' opzionale
#' @return Dataframe per ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
chk_allineamento_costo_coe <- function(report, programmi, report_macroaree, perimetro=NULL) {
  
  if (is.null(perimetro)) {
    perimetro <- prep_perimetro_bimestre_coesione(bimestre, usa_meuro=TRUE) 
    message("Attenzione: 'perimetro' è in meuro e potrebbe essere in valuta diversa dagli altri report confrontati")
  }
  
  out <- report %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(COE = sum(COE, na.rm = TRUE)) %>%
    full_join(programmi %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(report_macroaree %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(perimetro %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) 
  
  names(out) <- c("x_CICLO", "x_AMBITO", "report", "programmi", "report_macroaree", "perimetro")
  
  return(out)
  
}




#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param add_totali Vuoi aggiungere valori calcolati in termini di costo pubblico?
#' @param use_cp2 Se add_totali == TRUE, vuoi raddoppiare i valori relativi ai progetti multi-programma?  
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_macroaree_coesione <- function(perimetro, usa_meuro=FALSE, use_713=FALSE, use_flt=FALSE,
                                           add_totali=FALSE, use_cp2=FALSE, cut_no_risorse=FALSE,
                                           tipo_ciclo="CICLO_STRATEGIA",
                                           focus="report", export=FALSE, progetti=NULL, po_riclass=NULL) {
  
  # DEBUG: 
  # use_713 <- TRUE
  # use_flt <- TRUE
  # tipo_ciclo <- "CICLO_STRATEGIA"
  # perimetro <- operazioni
  
  programmi <- init_programmazione(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=TRUE) %>%
    rename(x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA)
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(OC_FLAG_MONITORAGGIO == 1 | OC_FLAG_MONITORAGGIO == 2)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }
  
  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # ricodifica x_MACROAREA
  # programmi <- ricodifica_macroaree(programmi)
  # MEMO: ora è spostato in init_programmazione()
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE))
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1))
  }
  
  if (is.null(po_riclass)) {
    # po_riclass <- octk::po_riclass
    # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
    
    po_riclass <- init_programmazione(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=FALSE) %>%
      rename(x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA) %>%
      distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
    
  }
  
  # refactor
  spalla <- refactor_macroarea(spalla)
  perimetro <- refactor_macroarea(perimetro)
  
  # report
  out <- spalla %>%
    full_join(perimetro %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea() %>%
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    left_join(po_riclass %>%
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, N, COE, COE_IMP, COE_PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)
  
  
  # aggiunge valori calcolati in termini di FTPN
  if (add_totali == TRUE) {
    
    message("Inizio integrazione totali")
    
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, light = TRUE, refactor = TRUE)
    }
    
    progetti <- progetti %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.) %>%
      # MEMO: patch per factor di x_AMBITO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE", "ORD")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA,
             CP = OC_FINANZ_TOT_PUB_NETTO,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
    
    
    # semplifica altro 
    # CHK: codifica diversa tra operazioni e progetti che compare solo qui
    progetti <- progetti %>%
      mutate(x_MACROAREA = as.character(x_MACROAREA)) %>%
      mutate(x_MACROAREA = case_when (x_MACROAREA == "Estero" ~ "Ambito nazionale",
                                      x_MACROAREA == "Trasversale" ~ "Ambito nazionale",
                                      x_MACROAREA == "Ambito nazionale" ~ "Ambito nazionale",
                                      TRUE ~ x_MACROAREA)) %>%
      refactor_macroarea()
    
    
    # if (usa_meuro == TRUE) {
    #   progetti <- progetti %>%
    #     mutate(CP = CP / 1000000,
    #            IMP = IMP / 1000000,
    #            PAG = PAG / 1000000)
    # }
    
    out <- out %>%
      left_join(progetti %>%
                  group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
                  summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE)),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>% # MEMO: qui perdo multi ":::" in progetti
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    
    # recupero multi
    appo <- progetti %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
      summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      anti_join(out,
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # recupera x_vars
      left_join(po_riclass %>%
                  refactor_ambito(.) %>%
                  refactor_ciclo(.) %>%
                  # MEMO: patch per factor di x_AMBITO e x_CICLO
                  # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                  #                                               "FEAD", "FAMI", "CTE", "ORD")),
                  #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
                  select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO"))
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    # left_join(po_riclass %>%
    #             refactor_ambito(.) %>%
    #             refactor_ciclo(.) %>%
    #             select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
    #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    #   as_tibble(.) %>%
    #   mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
    #          x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    #   select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    #   refactor_ambito(.) %>%
    #   refactor_ciclo(.)
      
    
    
    
    # bind di multi
    out <- out %>%
      bind_rows(appo) %>%
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    # CHK: QUI ACCODA RIGHE ":::" A QUELLE BASE
    
    # versione con cp2
    if (use_cp2 == TRUE) {
      
      message("Inizio calcolo cp2")
      
      appo <- out %>%
        filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        as_tibble(.) %>%
        select(OC_CODICE_PROGRAMMA, x_AMBITO_FSE_FESR = x_AMBITO, x_MACROAREA, N_CLP, CP, IMP, PAG) %>%
        # CHK: forse N_CLP non va sommato...
        mutate(x_AMBITO_FSE_FESR = as.character(x_AMBITO_FSE_FESR)) %>%
        separate_rows(OC_CODICE_PROGRAMMA, sep = ":::") %>%
        # recupera x_vars
        left_join(po_riclass %>%
                    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA, x_GRUPPO),
                  by = "OC_CODICE_PROGRAMMA") %>%
        # modifica x_AMBITO
        mutate(x_AMBITO = case_when(x_AMBITO == "FESR-FSE" ~ as.character(x_AMBITO_FSE_FESR), # MEMO: split per programmi pluri-fondo
                                    x_AMBITO == "YEI-FSE" ~ as.character(x_AMBITO_FSE_FESR),
                                    x_AMBITO == "FSC-POC" ~ "FSC",  # MEMO: forzo su FSC
                                    TRUE ~ as.character(x_AMBITO))) %>%
        refactor_ambito(.) %>%
        refactor_ciclo(.) %>%
        # left_join(programmi %>%
        #             refactor_macroarea(.) %>%
        #             # as_tibble(.) %>%
        #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA),
        #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
        # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
        #                                               "FEAD", "FAMI", "CTE", "ORD")),
        #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
        # ripristina struttura per bindrows
        mutate(RISORSE = 0,
               N = 0,
               COE = 0,
               COE_IMP = 0,
               COE_PAG = 0,
               `Non avviato` = 0,
               `In avvio di progettazione` = 0,
               `In corso di progettazione` = 0,
               `In affidamento` = 0,
               `In esecuzione` = 0,
               `Eseguito` = 0) %>%
        select(names(out))
      
      # TODO: qui va aggiustata la parte PAC di direttrici ferroviarie e di giustizia digitale perché CP resta vuoto (o forse dentro report programmi)
      
      
      programmi_2 <- out %>%
        filter(!grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        bind_rows(appo)
      dim(out)[1] + dim(appo)[1]/2 == dim(programmi_2)[1]
      
      
      # semplifica altro 
      # CHK: codifica diversa tra operazioni e progetti che compare solo qui
      # programmi_2 <- programmi_2 %>%
      #   mutate(x_MACROAREA = as.character(x_MACROAREA)) %>%
      #   mutate(x_MACROAREA = case_when (x_MACROAREA == "Estero" ~ "Ambito nazionale",
      #                                   x_MACROAREA == "Trasversale" ~ "Ambito nazionale",
      #                                   x_MACROAREA == "Ambito nazionale" ~ "Ambito nazionale",
      #                                   TRUE ~ x_MACROAREA)) %>%
      #   refactor_macroarea()
      
      # sovrascrive out
      out <- programmi_2 %>%
        group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE)
      
      if (usa_meuro == TRUE) {
        out <- out %>%
          mutate(CP = round(CP / 1000000, 1),
                 IMP = round(IMP / 1000000, 1),
                 PAG = round(PAG / 1000000, 1))
      }
      
    }
  }
  
  if (cut_no_risorse == TRUE) {
    # out <- out %>%
    #   filter(RISORSE > 0)
    # MEMO: questa versione esclude casi con RISORSE 0 per ambito nazionale e progetti presenti (e casi simili)
    
    appo <- init_programmazione(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=TRUE) %>%
      rename(x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA)
    
    if (use_flt == TRUE) {
      appo <- appo %>%
        filter(OC_FLAG_MONITORAGGIO == 1 | OC_FLAG_MONITORAGGIO == 2)
      # MEMO: in FSC resta anche tipo 9 che viene scartato
    }
    
    out <- out %>% 
      semi_join(appo, by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
    
  }
  
  if (export == TRUE) {
    if (use_cp2 == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree.csv")), row.names = FALSE)
    }
  }
  
  
  return(out)
}



#' Workflow per creare il dataset operazioni
workflow_operazioni_sito <- function(bimestre, progetti, debug=FALSE) {
  
  # MEMO:
  # questa versione prevede il calcolo delel risorse coesione direttamente nel flusso di Fabio per il sito
  # i programmi con ciclo sbagliato non sono ancora spostati nei file operazioni.sas
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass
  
  # if (is.null(progetti)) {
  #   # progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  #   progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, light = FALSE)
  # }
  
  # MEMO: ora è dentro in pre-esteso
  # recupera finanziamenti lordi (se assente in df)
  # if (!(any(names(progetti) == "FINANZ_TOTALE_PUBBLICO"))) {
  #
  #   temp <- paste0("progetti_esteso_", bimestre, ".csv")
  #   progetti_ext <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
  #
  #   progetti <- progetti %>%
  #     left_join(progetti_ext %>%
  #                 select(COD_LOCALE_PROGETTO, FINANZ_TOTALE_PUBBLICO, FINANZ_STATO_FSC, FINANZ_STATO_PAC),
  #               by = "COD_LOCALE_PROGETTO")
  #
  #   rm(progetti_ext)
  # }
  
  operazioni_1420_raw <- read_sas(file.path(DATA, "oper_pucok_preesteso.sas7bdat"))
  message("Operazioni raw caricate per 1420")
  
  operazioni_713_raw <- read_sas(file.path(DATA, "oper_fltok_preesteso.sas7bdat"))
  message("Operazioni raw caricate per 713")
  
  
  # ----------------------------------------------------------------------------------- #
  # prep
  
  # progetti in programmi multi con ":::"
  progetti_multi <- progetti %>%
    filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # progetti in programmi multi con ":::" in 2 programmi fsc
  progetti_multi_fsc_1420 <- progetti %>%
    # filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
    filter(OC_COD_FONTE == "FSC1420:::FSC1420") %>%
    select(COD_LOCALE_PROGETTO)
  
  
  # chk
  # progetti %>%
  #   filter(OC_COD_FONTE %in% c("FS0713:::FSC0713", "FS0713:::PAC")) %>%
  #   group_by(OC_COD_FONTE, FONDO_COMUNITARIO) %>%
  #   summarise(N = n(),
  #             FSC = sum(OC_FINANZ_STATO_FSC_NETTO, na.rm = TRUE),
  #             PAC = sum(OC_FINANZ_STATO_PAC_NETTO, na.rm = TRUE),
  #             UE = sum(COSTO_RENDICONTABILE_UE, na.rm = TRUE),
  #             CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2014-2020
  
  # chk
  # operazioni_1420_raw %>% count(oc_cod_fonte, ue_descr_fondo, CODICE_TIPOLOGIA_PROGRAMMA, STATO)
  # oc_cod_fonte ue_descr_fondo      n
  # <chr>        <chr>           <int>
  # 1 FS1420       ""                  1 <- ? 2014IT16RFOP010 POR LAZIO FESR (da eliminare)
  # 2 FS1420       FEAMP            6856
  # 3 FS1420       FESR            58220
  # 4 FS1420       FSE            304988
  # 5 FS1420       IOG            195158
  # 6 FS1420       PAC                25 <- ? SNAI
  # 7 FSC1420      FSC              8435
  # 8 PAC1420      PAC               858
  
  # chk su ""
  # chk <- operazioni_1420_raw %>% filter(ue_descr_fondo == "", STATO == 1)
  
  # chk su STATO
  # chk <- operazioni_1420_raw %>% filter(STATO == 2)
  # 96 operazioni a giugno
  # sum(chk$costo_ammesso_FSC, na.rm = TRUE)
  # 6800000 > sono interamente riassorbiti da incremento via finanziamenti
  # sum(chk$costo_rendicontabile_UE, na.rm = TRUE)
  # 325273535
  
  # chk su multi
  # operazioni_1420_raw %>%
  #   rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
  #   left_join(progetti_multi %>%
  #               mutate(MULTI = TRUE),
  #             by = "COD_LOCALE_PROGETTO") %>%
  #     count(oc_cod_fonte, ue_descr_fondo, MULTI)
  
  
  message("Preparazione dati 1420...")
  
  # clean
  operazioni_1420 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni(.) %>%
    # mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
    #                                     PO_FONDO == "FESR" ~ "FESR",
    #                                   oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
    #                                   TRUE ~ ue_descr_fondo)) %>%
    # creo ambito
    mutate(x_AMBITO = case_when(oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "IPA" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
                                oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
                                  CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "SNAI",
                                oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
                                oc_cod_fonte == "FSC1420" ~ "FSC",
                                oc_cod_fonte == "PAC1420" ~ "POC")) %>%
    # variabili coesione
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  # chk
  operazioni_1420 %>%
    count(x_AMBITO, oc_ambito)
  
  # clean
  operazioni_1420 <- operazioni_1420 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM)
  
  
  
  # ----------------------------------------------------------------------------------- #
  #  variabili COE per 2007-2013
  
  # chk
  # operazioni_713_raw %>% count(OC_COD_FONTE, QSN_FONDO_COMUNITARIO)
  # OC_COD_FONTE QSN_FONDO_COMUNITARIO      n
  # <chr>        <chr>                  <int>
  # 1 FS0713       FESR                  110693
  # 2 FS0713       FSE                   805181
  # 3 FSC0713      ""                     22530
  # 4 FSC1420      ""                        37 <- PIANO FSC DISSESTO IDROGEOLOGICO
  # 5 PAC          ""                     25353
  
  message("Preparazione dati 713...")
  
  chk <- operazioni_713_raw %>%
    filter(OC_FLAG_PAC == 1) %>%
    group_by(OC_COD_FONTE, QSN_FONDO_COMUNITARIO, OC_COD_PROGRAMMA) %>%
    summarise(N = n())
  
  # MEMO: dovrebbe esserci anche 2007SA002FA016 ma ha flag OC_FLAG_PAC solo per un progetto...
  
  # FIX: duplicazione di programmi PAC-FSC (es. direttrici ferroviarie)
  appo <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    
    # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FLAG_PAC),
    #           by = "COD_LOCALE_PROGETTO") %>%
    filter(OC_FLAG_PAC == 1) %>%
    # MEMO: OC_FLAG_PAC identifica i casi dove ci sono sia pagamenti FSC che PAC
    # TODO: verifica se OC_FLAG_PAC in operazioni_flt_ok coincide con quello in progetti
    # mutate(x_AMBITO = "POC:::FSC") %>%
    mutate(x_AMBITO = "PAC:::FSC") %>%
    separate_rows(x_AMBITO, sep = ":::")
  
  operazioni_713_raw_temp <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    mutate(x_AMBITO = NA) %>%
    anti_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    bind_rows(appo)
  
  dim(operazioni_713_raw_temp)[1] == dim(operazioni_713_raw)[1] + dim(appo)[1]/2
  
  # DEBUG:
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT005FAMG1")
  # chk <- operazioni_713_raw_temp %>% filter(OC_CODICE_PROGRAMMA == "2007IT001FA005")
  #
  
  # clean
  operazioni_713 <- operazioni_713_raw_temp %>%
    # fix per caratteri spuri
    fix_operazioni_713(.) %>%
    # creo ambito e ciclo
    mutate(x_AMBITO = case_when(!is.na(x_AMBITO) ~ x_AMBITO, # MEMO: serve a incorporare fix sopra
                                OC_COD_FONTE == "FS0713" ~ QSN_FONDO_COMUNITARIO,
                                OC_COD_FONTE == "FSC1420" ~ "FSC",
                                OC_COD_FONTE == "FSC0713" ~ "FSC",
                                OC_COD_FONTE == "PAC" ~ "PAC")) %>%
    # fix per ERDF e ESF su 713
    mutate(x_AMBITO = case_when(x_AMBITO == "ERDF" ~ "FESR",
                                x_AMBITO == "ESF" ~ "FSE",
                                TRUE ~ x_AMBITO)) %>%
    # map per ambito
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  # chk
  if (debug == TRUE) {
    operazioni_713 %>%
      count(x_AMBITO, oc_ambito)
  }
  
  # clean
  operazioni_713 <- operazioni_713 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM)
  
  
  # ----------------------------------------------------------------------------------- #
  # bind
  
  message("Unione dati...")
  
  operazioni <- operazioni_1420 %>%
    bind_rows(operazioni_713) %>%
    # passaggio equivalente a get_x_vars
    left_join(po %>%
                # MEMO: fix per doppio entry in po_riclass
                filter(!(OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" & x_CICLO == "2007-2013"),
                       !(OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" & x_CICLO == "2007-2013")) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = "OC_CODICE_PROGRAMMA") %>%
    # fix per dissesto
    mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
                                           TRUE ~ OC_CODICE_PROGRAMMA))
  # isola visualizzati
  # MEMO: questo sotto non prende ":::" su OC_CODICE_PROGRAMMA
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
  # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = "COD_LOCALE_PROGETTO")
  # left_join(progetti %>%
  #             select(COD_LOCALE_PROGETTO, OC_FLAG_VISUALIZZAZIONE),
  #           by = "COD_LOCALE_PROGETTO")
  
  # chk compatibile con Fabio x Stefano
  # operazioni %>% distinct(COD_LOCALE_PROGETTO, x_CICLO, x_AMBITO) %>% count(x_CICLO, x_AMBITO)
  
  # chk dupli per ciclo da po_riclass
  chk <- operazioni %>% count(COD_LOCALE_PROGETTO, x_CICLO)
  dim(chk)[1] - dim(progetti)[1]
  
  # chk dupli da caratteri spuri
  chk %>% count(COD_LOCALE_PROGETTO) %>% filter(n > 1)
  # operazioni %>%
  #   anti_join(progetti, by = "COD_LOCALE_PROGETTO") %>%
  #   write_csv2(file.path(TEMP, "chk_operazioni_duple_missing"))
  # progetti %>%
  #   anti_join(operazioni, by = "COD_LOCALE_PROGETTO") %>%
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_AMBITO, X_CICLO, OC_FLAG_VISUALIZZAZIONE) %>% 
  #   write_csv2(file.path(TEMP, "chk_progetti_missing"))
  # MEMO: attenzione che a volte si importa progetti con i soli visualizzati e salta il chk sopra

  # chk
  operazioni %>%
    group_by(x_CICLO, x_AMBITO) %>%
    # group_by(x_CICLO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # programmi <- operazioni %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA) %>%
  #   summarise(N = n(),
  #             COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
  # # fix per ERDF e ESF su 713
  # operazioni <- operazioni %>%
  #   mutate(x_AMBITO = case_when(x_AMBITO == "ERDF" ~ "FESR",
  #                               x_AMBITO == "ESF" ~ "FSE",
  #                               TRUE ~ x_AMBITO))
  
  
  # ----------------------------------------------------------------------------------- #
  # debug multi
  
  if (debug == TRUE) {
    appo <- operazioni %>%
      semi_join(progetti_multi, by = "COD_LOCALE_PROGETTO") %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_COD_FONTE),
                by = "COD_LOCALE_PROGETTO")
    
    chk <- appo %>%
      distinct(COD_LOCALE_PROGETTO, x_CICLO, OC_COD_FONTE) %>%
      count(x_CICLO, OC_COD_FONTE) %>%
      rename(N_uni = n) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE) %>%
                  summarise(N_multi = n(),
                            COE = sum(COE, na.rm = TRUE),
                            COE_IMP = sum(COE_IMP, na.rm = TRUE),
                            COE_PAG = sum(COE_PAG, na.rm = TRUE)),
                by = c("x_CICLO", "OC_COD_FONTE")) %>%
      left_join(appo %>%
                  group_by(x_CICLO, OC_COD_FONTE, x_AMBITO) %>%
                  summarise(COE = sum(COE, na.rm = TRUE)) %>%
                  spread(x_AMBITO, COE, fill = 0),
                by = c("x_CICLO", "OC_COD_FONTE"))
    
    write_csv2(chk, file.path(TEMP, "chk_multi.csv"))
    
  }
  
  # x_CICLO   OC_COD_FONTE      N_uni N_multi         COE     COE_IMP     COE_PAG        FESR        FSC       FSE        POC
  # <chr>     <chr>             <int>   <int>       <dbl>       <dbl>       <dbl>       <dbl>      <dbl>     <dbl>      <dbl>
  # 1 2007-2013 FS0713:::FSC0713    356     712  162946290.  136472860.  158173021.  136919403.  22165005.  3861882.         0
  # 2 2007-2013 FS0713:::PAC       1739    3478 2073426131. 1903065658. 1406673744. 1539023360.         0  13682697. 520720073.
  # 3 2014-2020 FS1420:::FSC1420    934    1868  605882576.  403305710.   80546329.  320443835. 211799340. 73639401.         0
  # 4 2014-2020 FS1420:::PAC1420      1       2  356095180.  248854771.  125274997.  317851434.         0         0   38243746.
  # 5 2014-2020 FSC1420:::FSC1420    36      72  519178461   166280886.   44304843.          0  519178461         0          0
  # 6 2014-2020 FSC1420:::PAC1420     1       2    3500000           0           0           0    1550000         0    19
  
  
  
  # ----------------------------------------------------------------------------------- #
  # chk
  
  # chk <- progetti %>%
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO) %>%
  #   left_join(operazioni %>%
  #               distinct(COD_LOCALE_PROGETTO, x_CICLO),
  #             by = "COD_LOCALE_PROGETTO") %>%
  #   mutate(CHK = x_CICLO.x == x_CICLO.y)
  #
  # chk %>% filter(CHK == FALSE)
  
  
  # ----------------------------------------------------------------------------------- #
  # export
  
  message("Esce da workflow")
  
  return(operazioni)
}



