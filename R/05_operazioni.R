# funzioni del blocco "coesione"


#' Crea il dataset operazioni
#'
#' Crea il dataset operazioni a partire da sottoprodotti del workflow SAS.
#'
#' @param bimestre Bimestre di riferimento
#' @param progetti Dataset con un perimetro in formato "progetti".
#' @param operazioni_713 File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_1420 File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_extra File di tipo operazioni da flusso sas/dataiku.
#' @param use_fix Vuoi applicare fix_progetti() prima di esecuzione? Non impatta su x_AMBITO!
#' @return Il dataset operazioni con le variabili coesione calcolate: COE, COE_IMP e COE_PAG.
#' @note La modalità **debug** esporta diversi csv in TEMP La modalità **export** esporta operazioni_light.csv in DATA.
setup_operazioni <- function(bimestre, progetti, operazioni_713, operazioni_1420, operazioni_extra, export=FALSE, use_fix=FALSE, use_ecomix=FALSE, use_sito=FALSE, debug=FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {
    
    # if (is.null(progetti)) {
    #   progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, light = FALSE)
    #   message("Dataset progetti caricato")
    # }
    # MEMO: da qui veniva l'esclusione dei non visualizzati da operazioni? no perché sotto c'è left_join
    
    if (use_fix == TRUE) {
      progetti <- fix_progetti(progetti)
      message("Fix su progetti effettuati")
    }
    


    # TODO: rinominare

    operazioni <- workflow_operazioni(bimestre, progetti, operazioni_713, operazioni_1420, operazioni_extra, debug=debug)
    
    # export
    if (export == TRUE) {
      
      # ----------------------------------------------------------------------------------- #
      #  export light
      
      # macroarea
      # operazioni_light <- get_macroarea(operazioni, progetti, real_reg=TRUE)
      operazioni_light <- get_macroarea_oc(operazioni, progetti)
      operazioni_light <- get_regione_simply(operazioni_light, progetti)
      
      # variabili
      operazioni_light <- operazioni_light %>%
        # integra CP (che viene duplicato in modalità "cp2")
        left_join(progetti %>%
                    select(COD_LOCALE_PROGETTO, 
                           CP = OC_FINANZ_TOT_PUB_NETTO, 
                           IMP = IMPEGNI, 
                           PAG = TOT_PAGAMENTI),
                  by = "COD_LOCALE_PROGETTO") %>% 
        # integra natura per gestione trasferimenti
        left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, CUP_COD_NATURA),
                by = "COD_LOCALE_PROGETTO") %>%
        # TODO: decidere se spostare dentreo workflow
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
                           # OLD: ora articolazioni vengono da operazioni.sas
                           # OC_COD_ARTICOLAZ_PROGRAMMA,
                           # OC_DESCR_ARTICOLAZ_PROGRAMMA,
                           # OC_COD_SUBARTICOLAZ_PROGRAMMA,
                           # OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
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
                  by = "COD_LOCALE_PROGETTO") %>% 
        # NEW
        mutate(COD_SEZIONE = x_COD_LIVELLO_0, 
               DESCR_SEZIONE = x_DES_LIVELLO_0, 
               OC_COD_ARTICOLAZ_PROGRAMMA = x_COD_LIVELLO_1,
               OC_DESCR_ARTICOLAZ_PROGRAMMA = x_DES_LIVELLO_1,
               OC_COD_SUBARTICOLAZ_PROGRAMMA = x_COD_LIVELLO_2,
               OC_DESCR_SUBARTICOLAZ_PROGRAMMA = x_DES_LIVELLO_2)
      
      # x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
      # x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
      # x_COD_LIVELLO_2, x_DES_LIVELLO_2
      
      # TODO: serve un solo export
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
workflow_operazioni <- function(bimestre, progetti, operazioni_713, operazioni_1420, operazioni_extra, debug=FALSE) {
  
  # MEMO:
  # questa versione mantiene il calcolo delle risorse coesione direttamente nel flusso di Fabio per il sito
  # cambia l'associazione al ciclo perché i psc sono programmi divisi su più cicli
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass%>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
  
  
  operazioni_extra_raw <- operazioni_extra
  
  operazioni_1420_raw <- operazioni_1420
  
  operazioni_713_raw <- operazioni_713
  
  
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
  #  variabili COE per extra
  
  # chk
  # operazioni_1420_raw %>% count(oc_cod_fonte, ue_descr_fondo)
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
  
  message("Preparazione dati extra...")
  # dati extrasistema----
  
  # clean
  operazioni_extra <- operazioni_extra_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni_1420(.) %>%
    # mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
    #                                     PO_FONDO == "FESR" ~ "FESR",
    #                                   oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
    #                                   TRUE ~ ue_descr_fondo)) %>%
    #fix ACCOESBAILICATA
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))%>%
    # creo ambito
    # TODO: togliere casi 1420
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
                                oc_cod_fonte == "NAZORD" ~ "SNAI",
                                oc_cod_fonte == "FS2127" ~ oc_ambito,
                                oc_cod_fonte == "FDR2127" ~ oc_ambito,
                                oc_cod_fonte == "FSC2127" ~ oc_ambito)) %>%
    # articolazioni
    # TODO: togliere casi 1420
    mutate(x_COD_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sezione,
                                       x_AMBITO == "FDR" & !is.na(psc_sezione) ~ psc_sezione,
                                       TRUE ~ NA_character_),
           x_DES_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sezione,
                                       x_AMBITO == "FDR" & !is.na(psc_sezione) ~ psc_descr_sezione,
                                       TRUE ~ NA_character_),
           x_COD_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_asse_prioritario,
                                       x_AMBITO == "FSE" ~ ue_asse_prioritario,
                                       x_AMBITO == "YEI" ~ ue_asse_prioritario,
                                       x_AMBITO == "CTE" ~ ue_asse_prioritario,
                                       x_AMBITO == "ENI" ~ ue_asse_prioritario,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_area_tematica,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_area_tematica,
                                       x_AMBITO == "FSC" ~ fsc_settore_strategico,
                                       x_AMBITO == "POC" ~ pac_asse_tematico,
                                       x_AMBITO == "SNAI" ~ pac_asse_tematico,
                                       x_AMBITO == "FEASR" ~ cod_misura_feasr,
                                       x_AMBITO == "FDR" ~ psc_area_tematica,
                                       x_AMBITO == "JTF" ~ ue_asse_prioritario,),
           x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "FSE" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "YEI" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "CTE" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "ENI" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_area_tematica,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_area_tematica,
                                       x_AMBITO == "FSC" ~ fsc_descr_settore_strategico,
                                       x_AMBITO == "POC" ~ pac_descr_asse_tematico,
                                       x_AMBITO == "SNAI" ~ pac_descr_asse_tematico,
                                       x_AMBITO == "FEASR" ~ descr_misura_feasr,
                                       x_AMBITO == "FDR" ~ psc_descr_area_tematica,
                                       x_AMBITO == "JTF" ~ ue_descr_asse_prioritario),
           x_COD_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_ob_specifico,
                                       x_AMBITO == "FSE" ~ ue_ob_specifico,
                                       x_AMBITO == "YEI" ~ ue_ob_specifico,
                                       x_AMBITO == "CTE" ~ ue_ob_specifico,
                                       x_AMBITO == "ENI" ~ ue_ob_specifico,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sett_interv,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_sett_interv,
                                       x_AMBITO == "FSC" ~ fsc_asse_tematico,
                                       x_AMBITO == "POC" ~ pac_lineazione,
                                       x_AMBITO == "SNAI" ~ pac_lineazione,
                                       x_AMBITO == "FEASR" ~ cod_submisura_feasr,
                                       x_AMBITO == "JTF" ~ ue_ob_specifico,
                                       x_AMBITO == "FDR" ~ psc_sett_interv),
           x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "FSE" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "YEI" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "CTE" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "ENI" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sett_interv,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_sett_interv,
                                       x_AMBITO == "FSC" ~ fsc_descr_asse_tematico,
                                       x_AMBITO == "POC" ~ pac_descr_lineazione,
                                       x_AMBITO == "SNAI" ~ pac_descr_lineazione,
                                       x_AMBITO == "FEASR" ~ descr_submisura_feasr,
                                       x_AMBITO == "FDR" ~ psc_descr_sett_interv,
                                       x_AMBITO == "JTF" ~ ue_descr_ob_specifico)) %>% 
    # TODO: valutare se lasciare FDR
    mutate(x_AMBITO = ifelse(x_AMBITO == "FDR", "POC", x_AMBITO))%>%
    # variabili coesione
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  
  # operazioni_1420 %>% count(x_AMBITO, oc_cod_fonte, ue_descr_fondo)
  # operazioni_1420 %>% filter(oc_cod_fonte == "FSC0713") %>% count(OC_CODICE_PROGRAMMA)
  
  chk <- operazioni_extra %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2) %>% 
    filter(is.na(x_COD_LIVELLO_1))
  
  # chk
  operazioni_extra %>%
    count(x_AMBITO, oc_ambito)
  
  # clean
  operazioni_extra <- operazioni_extra %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM,
           costo_ammesso_MZ, costo_ammesso_CN, #MEMO: presenti solo per SIE
           imp_ammesso_MZ, imp_ammesso_CN, imp_trasf_ammesso_MZ, imp_trasf_ammesso_CN,
           pag_ammesso_MZ, pag_ammesso_CN, pag_trasf_ammesso_MZ, pag_trasf_ammesso_CN,
           x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
           x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
           x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  
  
  
  
  
  message("Preparazione dati 1420...")
  # dati bdu 1420----
  
  # DEBUG: (post fix_operazioni_1420())
  # operazioni_1420 %>% count(oc_cod_fonte, ue_descr_fondo)
  # operazioni_1420 %>% filter(oc_cod_fonte == "NAZORD", ue_descr_fondo == "PAC") %>% count(OC_CODICE_PROGRAMMA, oc_descrizione_programma)
  # operazioni_1420 %>% filter(oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE") %>% count(CODICE_TIPOLOGIA_PROGRAMMA)
  
  # clean
  operazioni_1420 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni_1420(.) %>%
    # mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
    #                                   ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
    #                                     PO_FONDO == "FESR" ~ "FESR",
    #                                   oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
    #                                   TRUE ~ ue_descr_fondo)) %>%
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
    mutate(x_COD_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_sezione,
                                       TRUE ~ NA_character_),
           x_DES_LIVELLO_0 = case_when(x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sezione,
                                       TRUE ~ NA_character_),
           x_COD_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_asse_prioritario,
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
           x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "FSE" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "YEI" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "CTE" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "ENI" ~ ue_descr_asse_prioritario,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_area_tematica,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_area_tematica,
                                       x_AMBITO == "FSC" ~ fsc_descr_settore_strategico,
                                       x_AMBITO == "POC" ~ pac_descr_asse_tematico,
                                       x_AMBITO == "SNAI" ~ pac_descr_asse_tematico,
                                       x_AMBITO == "FEASR" ~ descr_misura_feasr),
           x_COD_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_ob_specifico,
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
           x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "FSE" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "YEI" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "CTE" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "ENI" ~ ue_descr_ob_specifico,
                                       x_AMBITO == "FSC" & !is.na(psc_sezione) ~ psc_descr_sett_interv,
                                       x_AMBITO == "FSC" & psc_sezione != "" ~ psc_descr_sett_interv,
                                       x_AMBITO == "FSC" ~ fsc_descr_asse_tematico,
                                       x_AMBITO == "POC" ~ pac_descr_lineazione,
                                       x_AMBITO == "SNAI" ~ pac_descr_lineazione,
                                       x_AMBITO == "FEASR" ~ descr_submisura_feasr)) %>% 
    # variabili coesione
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  
  # operazioni_1420 %>% count(x_AMBITO, oc_cod_fonte, ue_descr_fondo)
  # operazioni_1420 %>% filter(oc_cod_fonte == "FSC0713") %>% count(OC_CODICE_PROGRAMMA)
  
  chk <- operazioni_1420 %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2) %>% 
    filter(is.na(x_COD_LIVELLO_1))
  
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
           COE_PAG = PAG_AMM,
           costo_ammesso_MZ, costo_ammesso_CN, #MEMO: presenti solo per SIE
           imp_ammesso_MZ, imp_ammesso_CN, imp_trasf_ammesso_MZ, imp_trasf_ammesso_CN,
           pag_ammesso_MZ, pag_ammesso_CN, pag_trasf_ammesso_MZ, pag_trasf_ammesso_CN,
           x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
           x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
           x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  
  
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
  # dati bdu 713----
  
  operazioni_713_raw <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_COD_PROGRAMMA = oc_cod_programma)
  
  chk <- operazioni_713_raw %>% 
    filter(OC_FLAG_PAC == 1) %>%
    group_by(OC_COD_FONTE, QSN_FONDO_COMUNITARIO, ) %>%
    summarise(N = n())
  
  # MEMO: dovrebbe esserci anche 2007SA002FA016 ma ha flag OC_FLAG_PAC solo per un progetto...
  
  
  
  # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FA
  
  # TODO: verificare se serve ancora (sono interamente dentro PSC?) 
  # in teoria sono da cancellare, ma va gestita rappresentazione su PAC...
  # es. https://opencoesione.gov.it/it/dati/progetti/1mise788/
  # es. https://opencoesione.gov.it/it/dati/progetti/?q=&programma=2007IT001FA005&selected_facets=fonte:PAC0713&r=1
  # TODO: semplificare perché troppo contorto
  # FIX: duplicazione di programmi PAC-FSC (es. direttrici ferroviarie)
  appo <- operazioni_713_raw %>%
    # rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    # filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005")) %>%
    filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005", "2007SA002FA016")) %>%
    # left_join(progetti %>%
    #             select(COD_LOCALE_PROGETTO, OC_FLAG_PAC),
    #           by = "COD_LOCALE_PROGETTO") %>%
    filter(OC_FLAG_PAC == 1) %>%
    # MEMO: OC_FLAG_PAC identifica i casi dove ci sono sia pagamenti FSC che PAC
    # TODO: verifica se OC_FLAG_PAC in operazioni_flt_ok coincide con quello in progetti
    # mutate(x_AMBITO = "POC:::FSC") %>%
    mutate(x_AMBITO = "PAC:::FSC") %>%
    separate_rows(x_AMBITO, sep = ":::") %>%
    # ricalcolo variabili coe (sovrascive fabio)
    fix_operazioni_713(.) %>%
    # map per ambito
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ 0,
                               x_AMBITO == "PAC" ~ 0,
                               TRUE ~ COSTO_RENDICONTABILE_UE),
           IMP_AMM = 0,
           PAG_AMM = case_when(x_AMBITO == "FSC" ~ OC_TOT_PAGAMENTI_FSC,
                               x_AMBITO == "PAC" ~ OC_TOT_PAGAMENTI_PAC,
                               TRUE ~ OC_TOT_PAGAMENTI_RENDICONTAB_UE)) %>%
    # integra variabili da progetti (l'aggancio crea valori duplicati)
    select(-TOT_PAGAMENTI) %>%
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO,
                       OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO=OC_FINANZ_Stato_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO=OC_FINANZ_Stato_PAC_NETTO,
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
                               TRUE ~ 0)) %>%
    # DEBUG:
    # appo1 <- appo %>% select(COD_LOCALE_PROGETTO, OC_COD_PROGRAMMA, x_AMBITO,
    #                          OC_FINANZ_TOT_PUB_NETTO, OC_FINANZ_STATO_FSC_NETTO, OC_FINANZ_STATO_PAC_NETTO, oc_costo_coesione, COE,
    #                          OC_TOT_PAGAMENTI_FSC, OC_TOT_PAGAMENTI_PAC, oc_tot_pagamenti_coesione, COE_PAG)
    # write_csv2(appo1, file.path(TEMP, "chk_multi_fas_pac_713.csv"))
    # map per ambito
    mutate(oc_costo_coesione = COE,
           oc_impegni_coesione = COE_IMP,
           oc_tot_pagamenti_coesione = COE_PAG) %>% 
    # clean
    select(names(operazioni_713_raw), x_AMBITO)
  
  # fix codice programma
  appo <- appo %>% 
    mutate(OC_COD_PROGRAMMA = case_when(x_AMBITO == "PAC" & OC_COD_PROGRAMMA == "2007SA002FA016" ~ "TEMP_MIT_SAR",
                                        TRUE ~ OC_COD_PROGRAMMA))
  
  # chk <- appo %>%
  #   select(OC_COD_PROGRAMMA, x_AMBITO, COD_LOCALE_PROGETTO, oc_costo_coesione, COE, 
  #          oc_impegni_coesione, COE_IMP, oc_tot_pagamenti_coesione, COE_PAG
  
  operazioni_713_raw_temp <- operazioni_713_raw %>%
    # rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
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
    # articolazioni
    mutate(x_COD_LIVELLO_0 = NA_character_,
           x_DES_LIVELLO_0 = NA_character_) %>% 
    mutate(COD_LINEA = as.character(COD_LINEA),
           COD_AZIONE = as.character(COD_AZIONE)) %>% 
    mutate(x_COD_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ PO_CODICE_ASSE,
                                       x_AMBITO == "FSE" ~ PO_CODICE_ASSE,
                                       x_AMBITO == "FSC" ~ COD_LINEA,
                                       x_AMBITO == "PAC" ~ COD_LINEA),
           x_DES_LIVELLO_1 = case_when(x_AMBITO == "FESR" ~ PO_DENOMINAZIONE_ASSE,
                                       x_AMBITO == "FSE" ~ PO_DENOMINAZIONE_ASSE,
                                       x_AMBITO == "FSC" ~ descr_linea,
                                       x_AMBITO == "PAC" ~ descr_linea),
           x_COD_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ PO_COD_OBIETTIVO_OPERATIVO,
                                       x_AMBITO == "FSE" ~ PO_COD_OBIETTIVO_OPERATIVO,
                                       x_AMBITO == "FSC" ~ COD_AZIONE,
                                       x_AMBITO == "PAC" ~ COD_AZIONE),
           x_DES_LIVELLO_2 = case_when(x_AMBITO == "FESR" ~ PO_OBIETTIVO_OPERATIVO,
                                       x_AMBITO == "FSE" ~ PO_OBIETTIVO_OPERATIVO,
                                       x_AMBITO == "FSC" ~ descr_azione,
                                       x_AMBITO == "PAC" ~ descr_azione)
    ) %>%
    # map per ambito
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  # chk
  if (debug == TRUE) {
    operazioni_713 %>%
      count(x_AMBITO, oc_ambito)
  }
  
  
  chk <- operazioni_713 %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2) %>% 
    filter(is.na(x_COD_LIVELLO_1))
  
  # clean
  operazioni_713 <- operazioni_713 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA = OC_COD_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM, 
           x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
           x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
           x_COD_LIVELLO_2, x_DES_LIVELLO_2) %>% 
    mutate(costo_ammesso_MZ = 0, 
           costo_ammesso_CN = 0,
           imp_ammesso_MZ = 0, 
           imp_ammesso_CN = 0, 
           pag_ammesso_MZ = 0, 
           pag_ammesso_CN = 0)
  
  
  # ----------------------------------------------------------------------------------- #
  # bind----
  
  message("Unione dati...")
  
  # fix psc
  psc <- po %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0)
  appo <- operazioni_1420 %>% 
    semi_join(psc, by = "OC_CODICE_PROGRAMMA")
  # appo %>% count(COD_LOCALE_PROGETTO) %>% filter(n>1)
  operazioni_psc <- appo %>% 
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_CICLO, OC_COD_CICLO), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(x_CICLO = case_when(OC_COD_CICLO == 1 ~ "2007-2013",
                               OC_COD_CICLO == 2 ~ "2014-2020",
                               OC_COD_CICLO == 3 ~ "2021-2027",
                               OC_COD_CICLO == 9 ~ "2000-2006",
                               # TODO: controllare true finale, va bene ancora 1420? c'è diff con fabio su 1420
                               TRUE ~ "2014-2020")) # MEMO: fix per anomalie di pre-esteso
  
  # DEBUG:
  # operazioni_psc %>% count(X_CICLO, OC_COD_CICLO, x_CICLO)
  # operazioni_psc %>% filter(is.na(x_CICLO)) %>% count(OC_CODICE_PROGRAMMA)
  # OC_CODICE_PROGRAMMA     n
  # <chr>               <int>
  # 1 PSCCAMPANIA            17
  # 2 PSCEMILROMAGNA         53
  # 3 PSCLAZIO                1
  # 4 PSCSVILECONOM         137
  # 5 PSCUNIVRICERCA          2
  # operazioni_psc %>% filter(is.na(OC_COD_CICLO)) %>% count(OC_CODICE_PROGRAMMA)
  # OC_CODICE_PROGRAMMA     n
  # <chr>               <int>
  # 1 PSCCAMPANIA           177
  # 2 PSCEMILROMAGNA         53
  # 3 PSCLAZIO                1
  # 4 PSCMOLISE               1
  # 5 PSCPUGLIA               6
  # 6 PSCSVILECONOM         178
  # 7 PSCUNIVRICERCA          2
  
  # operazioni_psc %>% filter(OC_CODICE_PROGRAMMA == "PSCLOMBARDIA") %>% count(X_CICLO)
  
  operazioni <- operazioni_1420 %>%
    bind_rows(operazioni_713) %>%
    bind_rows(operazioni_extra) %>%
    # passaggio equivalente a get_x_vars
    left_join(po %>%
                # MEMO: fix per doppio entry in po_riclass
                filter(!(OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" & x_CICLO == "2007-2013"),
                       !(OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" & x_CICLO == "2007-2013")) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = "OC_CODICE_PROGRAMMA") %>%
    # fix per psc 
    left_join(operazioni_psc %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(x_CICLO = if_else(is.na(x_CICLO.y), x_CICLO.x, x_CICLO.y)) %>% 
    select(-x_CICLO.x, -x_CICLO.y) %>% 
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
  
  # DEBUG:
  # chk ciclo
  # operazioni %>% count(x_CICLO)
  # operazioni %>% filter(is.na(x_CICLO)) %>% count(x_AMBITO)
  
  # fix ciclo
  operazioni <- operazioni %>%
    mutate(x_CICLO = case_when(OC_CODICE_PROGRAMMA == "TEMP_MIT_SAR" ~ "2007-2013",
                               OC_CODICE_PROGRAMMA == "2007PI004MA007" ~ "2000-2006", 
                               TRUE ~ x_CICLO))
  
  # chk compatibile con Fabio x Stefano
  # operazioni %>% distinct(COD_LOCALE_PROGETTO, x_CICLO, x_AMBITO) %>% count(x_CICLO, x_AMBITO)
  
  # DEBUG:
  # chk dupli per ciclo da po_riclass
  # chk <- operazioni %>% count(COD_LOCALE_PROGETTO, x_CICLO)
  # dim(chk)[1] - dim(progetti)[1]
  
  # chk dupli da caratteri spuri
  # chk %>% count(COD_LOCALE_PROGETTO) %>% filter(n > 1)
  # operazioni %>%
  #   anti_join(progetti, by = "COD_LOCALE_PROGETTO") %>%
  #   write_csv2(file.path(TEMP, "chk_operazioni_duple_missing"))
  # progetti %>%
  #   anti_join(operazioni, by = "COD_LOCALE_PROGETTO") %>%
  #   select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_AMBITO, X_CICLO, OC_FLAG_VISUALIZZAZIONE) %>% 
  #   write_csv2(file.path(TEMP, "chk_progetti_missing"))
  # MEMO: attenzione che a volte si importa progetti con i soli visualizzati e salta il chk sopra
  
  # chk
  # operazioni %>%
  #   group_by(x_CICLO, x_AMBITO) %>%
  #   # group_by(x_CICLO) %>%
  #   summarise(N = n(),
  #             COE = sum(COE, na.rm = TRUE),
  #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
  #             COE_PAG = sum(COE_PAG, na.rm = TRUE))
  
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
  
  # TODO: da buttare
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


#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_operazioni_1420 <- function(df) {
  
  # TODO: da spostare in workflow quando creo x_AMBITO
  
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
fix_operazioni_713 <- function(df) {
  
  # TODO: capire se va fatto ancora
  
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


#' Prepara perimetro operazioni con dati coesione
#'
#' Prepara perimetro operazioni con dati coesione
#'
#' @param perimetro Dataset di classe perimetro.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
load_operazioni <- function(bimestre, visualizzati=TRUE) {
  
  col_types <- cols(
    COD_LOCALE_PROGETTO = col_character(),
    OC_CODICE_PROGRAMMA = col_character(),
    x_AMBITO = col_character(),
    COE = col_double(),
    COE_IMP = col_double(),
    COE_PAG = col_double(),
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
    x_COD_LIVELLO_0 = col_character(),
    x_DES_LIVELLO_0 = col_character(),
    x_COD_LIVELLO_1 = col_character(),
    x_DES_LIVELLO_1 = col_character(),
    x_COD_LIVELLO_2 = col_character(),
    x_DES_LIVELLO_2 = col_character(),
    x_GRUPPO = col_character(),
    x_PROGRAMMA = col_character(),
    x_REGNAZ = col_character(),
    x_CICLO = col_character(),
    OC_MACROAREA = col_character(),
    x_MACROAREA = col_character(),
    COD_REGIONE = col_character(),
    DEN_REGIONE = col_character(),
    COD_PROVINCIA = col_character(),
    x_REGIONE = col_character(),
    CP = col_double(),
    IMP = col_double(),
    PAG = col_double(),
    COE_SUD = col_double(),
    COE_CN = col_double(),
    COE_IMP_SUD = col_double(),
    COE_IMP_CN = col_double(),
    COE_PAG_SUD = col_double(),
    COE_PAG_CN = col_double(),
    CUP = col_character(),
    OC_TITOLO_PROGETTO = col_character(),
    OC_SINTESI_PROGETTO = col_character(),
    OC_COD_TEMA_SINTETICO = col_character(),
    FONDO_COMUNITARIO = col_character(),
    OC_DESCRIZIONE_PROGRAMMA = col_character(),
    COD_RISULTATO_ATTESO = col_character(),
    DESCR_RISULTATO_ATTESO = col_character(),
    OC_COD_CATEGORIA_SPESA = col_character(),
    OC_DESCR_CATEGORIA_SPESA = col_character(),
    COD_STRUMENTO = col_character(),
    DESCR_STRUMENTO = col_character(),
    DESCR_TIPO_STRUMENTO = col_character(),
    COD_PROGETTO_COMPLESSO = col_character(),
    DESCRIZIONE_PROGETTO_COMPLESSO = col_character(),
    COD_TIPO_COMPLESSITA = col_character(),
    DESCR_TIPO_COMPLESSITA = col_character(),
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
    OC_STATO_PROGETTO = col_character(),
    OC_STATO_PROCEDURALE = col_character(),
    OC_COD_FASE_CORRENTE = col_character(),
    OC_DESCR_FASE_CORRENTE = col_character(),
    COD_PROCED_ATTIVAZIONE = col_character(),
    DESCR_PROCED_ATTIVAZIONE = col_character(),
    OC_CODFISC_BENEFICIARIO = col_character(),
    OC_DENOM_BENEFICIARIO = col_character(),
    OC_FLAG_VISUALIZZAZIONE = col_integer(),
    COD_SEZIONE = col_character(),
    DES_SEZIONE = col_character(),
    OC_COD_ARTICOLAZ_PROGRAMMA = col_character(),
    OC_DESCR_ARTICOLAZ_PROGRAMMA = col_character(),
    OC_COD_SUBARTICOLAZ_PROGRAMMA = col_character(),
    OC_DESCR_SUBARTICOLAZ_PROGRAMMA = col_character()
  )
  
  # loads
  # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  # perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)
  DATA <- file.path(dirname(DATA), bimestre)
  perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), col_types = col_types)
  
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

