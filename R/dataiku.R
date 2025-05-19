# coesione post passaggio a dataiku


#' Workflow per creare il dataset operazioni
workflow_operazioni_dataiku <- function(bimestre, progetti, operazioni_713, operazioni_1420, debug=FALSE) {
  
  # MEMO:
  # questa versione mantiene il calcolo delle risorse coesione direttamente nel flusso di Fabio per il sito
  # cambia l'associazione al ciclo perché i psc sono programmi divisi su più cicli
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  message("Entro in workflow operazioni")
  
  po <- octk::po_riclass
  
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
  #  variabili COE per 2014-2020
  
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
  
  
  message("Preparazione dati 1420...")
  
  # DEBUG: (post fix_operazioni_dataiku())
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
    fix_operazioni_dataiku(.) %>%
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
  
  operazioni_713_raw <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_COD_PROGRAMMA = oc_cod_programma)
  
  chk <- operazioni_713_raw %>% 
    filter(OC_FLAG_PAC == 1) %>%
    group_by(OC_COD_FONTE, QSN_FONDO_COMUNITARIO, ) %>%
    summarise(N = n())
  
  # MEMO: dovrebbe esserci anche 2007SA002FA016 ma ha flag OC_FLAG_PAC solo per un progetto...
  
  
  
  # filter(OC_CODICE_PROGRAMMA %in% c("2007IT005FA
  
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
  # bind
  
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
                               TRUE ~ "2014-2020")) # MEMO: fix per anomalie di pre-esteso
  operazioni_psc %>% count(X_CICLO, OC_COD_CICLO, x_CICLO)
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
  
  # chk ciclo
  operazioni %>% count(x_CICLO)
  operazioni %>% filter(is.na(x_CICLO)) %>% count(x_AMBITO)
  
  # fix ciclo
  operazioni <- operazioni %>%
    mutate(x_CICLO = case_when(OC_CODICE_PROGRAMMA == "TEMP_MIT_SAR" ~ "2007-2013",
                               OC_CODICE_PROGRAMMA == "2007PI004MA007" ~ "2000-2006", 
                               TRUE ~ x_CICLO))
  
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


#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_operazioni_dataiku <- function(df) {
  
  # df <- df %>%
  #   mutate(ue_descr_fondo = case_when(ue_descr_fondo == "" & oc_cod_fonte == "FSC1420" ~ "FSC",
  #                                     ue_descr_fondo == "" & oc_cod_fonte == "PAC1420" ~ "PAC",
  #                                     ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
  #                                       PO_FONDO == "FESR" ~ "FESR",
  #                                     ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
  #                                       PO_FONDO == "FESR FSE" & OC_CODICE_PROGRAMMA == "2014IT16M2OP002" ~ "FESR", #MEMO: problema per 42 progetti su 2014IT16M2OP002:::2016PATTIPUG
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG:::FSE" ~ "IOG",
  #                                     OC_CODICE_PROGRAMMA == "2019MATTMINA001" ~ "ORD",
  #                                     ue_descr_fondo == "" & oc_cod_fonte == "FS1420" &
  #                                       CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "NAZIONALE", # fix per 1MISEABIN-PSRA-89-153
  #                                     # fix temporanei
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I.:::F.S.E." ~ "IOG",
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "F.S.E." ~ "FSE",
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "F.E.S.R." ~ "FESR",
  #                                     oc_cod_fonte == "FS1420" & ue_descr_fondo == "F.E.A.S.R." ~ "FEASR",
  #                                     TRUE ~ ue_descr_fondo))
  
  df <- df %>%
    mutate(ue_descr_fondo = case_when(ue_descr_fondo == "EAFRD" ~ "FEASR",
                                      ue_descr_fondo == "ESF" ~ "FSE",
                                      ue_descr_fondo == "IOG:::FSE" ~ "IOG",
                                      TRUE ~ ue_descr_fondo))
  
  return(df)
}


#' Carica il dataset progetti
#'
#' Carica il file progetti_esteso_$BIMESTRE.csv dal folder DATI. Versione post dataiku.
#'
#' @param bimestre Stringa in formato "20180630" come da standard per le date in OC.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param debug Logico. Vuoi vedere i totali di progetti e costo pubblico per controllo sul portale OC?
#' @param light Logico. Vuoi usare la versione light di "progetti.csv"?
#' @return Il dataset viene caricato come "progetti" nel Global Environment. Se "progetti" è gia presente compare una notifica.
load_progetti_dataiku <- function(bimestre, data_path=NULL, visualizzati=TRUE, debug=FALSE, light=FALSE, refactor=FALSE)
{
  # if (exists("progetti", envir = .GlobalEnv)) {
  #   print("Progetti esteso è gia caricato")
  #   progetti <- progetti
  #
  # } else {
  
  # switch di filename per progetti_light
  if (light == TRUE) {
    temp <- paste0("progetti_light_", bimestre, ".csv")
    
    col_types <- cols(
      COD_LOCALE_PROGETTO = col_character(),
      CUP = col_character(),
      OC_TITOLO_PROGETTO = col_character(),
      OC_SINTESI_PROGETTO = col_character(),
      OC_COD_TEMA_SINTETICO = col_character(),
      OC_COD_FONTE = col_character(),
      FONDO_COMUNITARIO = col_character(),
      OC_CODICE_PROGRAMMA = col_character(),
      OC_DESCRIZIONE_PROGRAMMA = col_character(),
      COD_RISULTATO_ATTESO = col_character(),
      DESCR_RISULTATO_ATTESO = col_character(),
      OC_COD_CATEGORIA_SPESA = col_character(),
      OC_DESCR_CATEGORIA_SPESA = col_character(),
      OC_COD_ARTICOLAZ_PROGRAMMA = col_character(),
      OC_DESCR_ARTICOLAZ_PROGRAMMA = col_character(),
      OC_COD_SUBARTICOLAZ_PROGRAMMA = col_character(),
      OC_DESCR_SUBARTICOLAZ_PROGRAMMA = col_character(),
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
      COD_REGIONE = col_character(),
      DEN_REGIONE = col_character(),
      COD_PROVINCIA = col_character(),
      DEN_PROVINCIA = col_character(),
      COD_COMUNE = col_character(),
      DEN_COMUNE = col_character(),
      OC_FINANZ_UE_NETTO = col_double(),
      OC_FINANZ_STATO_FONDO_ROT_NETTO = col_double(),
      OC_FINANZ_STATO_FSC_NETTO = col_double(),
      OC_FINANZ_STATO_PAC_NETTO = col_double(),
      OC_FINANZ_TOT_PUB_NETTO = col_double(),
      IMPEGNI = col_double(),
      TOT_PAGAMENTI = col_double(),
      COSTO_REALIZZATO = col_double(),
      OC_COSTO_COESIONE = col_double(),
      OC_IMPEGNI_COESIONE = col_double(),
      OC_PAGAMENTI_COESIONE = col_double(),
      OC_STATO_PROGETTO = col_character(),
      OC_STATO_PROCEDURALE = col_character(),
      OC_COD_FASE_CORRENTE = col_character(),
      OC_DESCR_FASE_CORRENTE = col_character(),
      COD_PROCED_ATTIVAZIONE = col_character(),
      DESCR_PROCED_ATTIVAZIONE = col_character(),
      OC_CODFISC_BENEFICIARIO = col_character(),
      OC_DENOM_BENEFICIARIO = col_character(),
      OC_COD_FORMA_GIU_BENEFICIARIO = col_character(),
      OC_FLAG_VISUALIZZAZIONE = col_double(),
      OC_FLAG_BENICONF = col_double(),
      OC_FLAG_COVID = col_character(),
      OC_MACROAREA = col_character(),
      SNAI = col_double(),
      IMPORTO_AGGIUDICATO = col_double(),
      IMPORTO_AGGIUDICATO_NODATA = col_double(),
      IMPORTO_AGGIUDICATO_BANDITO = col_double(),
      QSN_CODICE_OBIETTIVO_SPECIFICO = col_double(),
      QSN_DESCR_OBIETTIVO_SPECIFICO = col_character(),
      COD_AREA_INT = col_character(),
      AREA_INTERNA = col_character(),
      x_CICLO = col_character(),
      x_AMBITO = col_character(),
      x_GRUPPO = col_character(),
      x_PROGRAMMA = col_character(),
      x_REGNAZ = col_character(),
      x_MACROAREA = col_character(),
      x_REGIONE = col_character()
    )
    
  } else {
    if (as.numeric(bimestre) <= 20181231) {
      temp <- paste0("progetti_esteso_", bimestre, ".csv")
      
      col_types <- cols()
      
    } else {
      temp <- "PROGETTI_PREESTESO.csv"
      
      col_types <- cols(
        db = col_character(),
        COD_LOCALE_PROGETTO = col_character(),
        CUP = col_character(),
        OC_TITOLO_PROGETTO = col_character(),
        OC_SINTESI_PROGETTO = col_character(),
        OC_COD_CICLO = col_double(),
        OC_DESCR_CICLO = col_character(),
        OC_COD_TEMA_SINTETICO = col_character(),
        OC_TEMA_SINTETICO = col_character(),
        COD_GRANDE_PROGETTO = col_character(),
        DESCRIZIONE_GRANDE_PROGETTO = col_character(),
        COD_PROGETTO_COMPLESSO = col_character(),
        DESCRIZIONE_PROGETTO_COMPLESSO = col_character(),
        COD_TIPO_COMPLESSITA = col_character(),
        DESCR_TIPO_COMPLESSITA = col_character(),
        x_ciclo = col_character(),
        x_ambito = col_character(),
        x_gruppo = col_character(),
        x_regnaz = col_character(),
        x_programma = col_character(),
        OC_COD_FONTE = col_character(),
        OC_DESCR_FONTE = col_character(),
        FONDO_COMUNITARIO = col_character(),
        OC_CODICE_PROGRAMMA = col_character(),
        OC_DESCRIZIONE_PROGRAMMA = col_character(),
        COD_OB_TEMATICO = col_character(),
        DESCR_OB_TEMATICO = col_character(),
        COD_RISULTATO_ATTESO = col_character(),
        DESCR_RISULTATO_ATTESO = col_character(),
        OC_COD_CATEGORIA_SPESA = col_character(),
        OC_DESCR_CATEGORIA_SPESA = col_character(),
        OC_ARTICOLAZIONE_PROGRAMMA = col_character(),
        OC_SUBARTICOLAZIONE_PROGRAMMA = col_character(),
        OC_COD_ARTICOLAZ_PROGRAMMA = col_character(),
        OC_DESCR_ARTICOLAZ_PROGRAMMA = col_character(),
        OC_COD_SUBARTICOLAZ_PROGRAMMA = col_character(),
        OC_DESCR_SUBARTICOLAZ_PROGRAMMA = col_character(),
        COD_STRUMENTO = col_character(),
        DESCR_STRUMENTO = col_character(),
        DESCR_TIPO_STRUMENTO = col_character(),
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
        COD_ATECO = col_character(),
        DESCRIZIONE_ATECO = col_character(),
        OC_COD_TIPO_AIUTO = col_character(),
        OC_DESCR_TIPO_AIUTO = col_character(),
        COD_REGIONE = col_character(),
        DEN_REGIONE = col_character(),
        COD_PROVINCIA = col_character(),
        DEN_PROVINCIA = col_character(),
        COD_COMUNE = col_character(),
        DEN_COMUNE = col_character(),
        OC_MACROAREA = col_character(),
        OC_MAREA = col_character(),
        oc_macroarea_sas = col_character(),
        oc_marea_sas = col_character(),
        oc_cod_sll = col_character(),
        OC_DENOMINAZIONE_SLL = col_character(),
        OC_COD_AI = col_character(),
        OC_DENOM_AI = col_character(),
        FINANZ_UE = col_double(),
        FINANZ_STATO_FONDO_DI_ROTAZIONE = col_double(),
        FINANZ_STATO_FSC = col_double(),
        FINANZ_STATO_PAC = col_double(),
        FINANZ_PRIVATO = col_double(),
        FINANZ_DA_REPERIRE = col_double(),
        FINANZ_TOTALE_PUBBLICO = col_double(),
        ECONOMIE_TOTALI = col_double(),
        ECONOMIE_TOTALI_PUBBLICHE = col_double(),
        ECONOMIE_PRIVATO = col_double(),
        ECONOMIE_DA_REPERIRE = col_double(),
        OC_FINANZ_UE_NETTO = col_double(),
        OC_FINANZ_UE_FESR_NETTO = col_double(),
        OC_FINANZ_UE_FSE_NETTO = col_double(),
        OC_FINANZ_UE_FEASR_NETTO = col_double(),
        OC_FINANZ_UE_FEAMP_NETTO = col_double(),
        OC_FINANZ_UE_IOG_NETTO = col_double(),
        OC_FINANZ_Stato_Fondo_Rot_NETTO = col_double(),
        OC_FINANZ_Stato_FSC_NETTO = col_double(),
        OC_FINANZ_Stato_PAC_NETTO = col_double(),
        OC_FINANZ_Stato_compl_NETTO = col_double(),
        OC_FINANZ_Stato_altri_prov_NETTO = col_double(),
        OC_FINANZ_Regione_NETTO = col_double(),
        OC_FINANZ_Provincia_NETTO = col_double(),
        OC_FINANZ_Comune_NETTO = col_double(),
        OC_FINANZ_Risorse_liberate_NETTO = col_double(),
        OC_FINANZ_Altro_pubblico_NETTO = col_double(),
        OC_FINANZ_Stato_estero_NETTO = col_double(),
        OC_FINANZ_Privato_NETTO = col_double(),
        OC_FINANZ_TOT_PUB_NETTO = col_double(),
        OC_COSTO_COESIONE = col_double(),
        IMPEGNI = col_double(),
        OC_IMPEGNI_GIURID_VINCOLANTI = col_double(),
        OC_IMPEGNI_TRASFERIMENTI = col_double(),
        OC_IMPEGNI_COESIONE = col_double(),
        TOT_PAGAMENTI = col_double(),
        OC_TOT_PAGAMENTI_BENEFICIARI = col_double(),
        OC_TOT_PAGAMENTI_TRASFERIMENTI = col_double(),
        OC_PAGAMENTI_COESIONE = col_double(),
        COSTO_REALIZZATO = col_double(),
        IMPORTO_AGGIUDICATO = col_double(),
        IMPORTO_AGGIUDICATO_NODATA = col_double(),
        IMPORTO_AGGIUDICATO_BANDITO = col_double(),
        COSTO_RENDICONTABILE_UE = col_double(),
        OC_TOT_PAGAMENTI_RENDICONTAB_UE = col_double(),
        OC_TOT_PAGAMENTI_FSC = col_double(),
        OC_TOT_PAGAMENTI_PAC = col_double(),
        OC_DATA_INIZIO_PROGETTO = col_integer(),
        OC_DATA_FINE_PROGETTO_PREVISTA = col_integer(),
        OC_DATA_FINE_PROGETTO_EFFETTIVA = col_integer(),
        DATA_INIZIO_PREV_STUDIO_FATT = col_integer(),
        DATA_INIZIO_EFF_STUDIO_FATT = col_integer(),
        DATA_FINE_PREV_STUDIO_FATT = col_integer(),
        DATA_FINE_EFF_STUDIO_FATT = col_integer(),
        DATA_INIZIO_PREV_PROG_PREL = col_integer(),
        DATA_INIZIO_EFF_PROG_PREL = col_integer(),
        DATA_FINE_PREV_PROG_PREL = col_integer(),
        DATA_FINE_EFF_PROG_PREL = col_integer(),
        DATA_INIZIO_PREV_PROG_DEF = col_integer(),
        DATA_INIZIO_EFF_PROG_DEF = col_integer(),
        DATA_FINE_PREV_PROG_DEF = col_integer(),
        DATA_FINE_EFF_PROG_DEF = col_integer(),
        DATA_INIZIO_PREV_PROG_ESEC = col_integer(),
        DATA_INIZIO_EFF_PROG_ESEC = col_integer(),
        DATA_FINE_PREV_PROG_ESEC = col_integer(),
        DATA_FINE_EFF_PROG_ESEC = col_integer(),
        DATA_INIZIO_PREV_AGG_BANDO = col_integer(),
        DATA_INIZIO_EFF_AGG_BANDO = col_integer(),
        DATA_FINE_PREV_AGG_BANDO = col_integer(),
        DATA_FINE_EFF_AGG_BANDO = col_integer(),
        DATA_INIZIO_PREV_STIP_ATTRIB = col_integer(),
        DATA_INIZIO_EFF_STIP_ATTRIB = col_integer(),
        DATA_FINE_PREV_STIP_ATTRIB = col_integer(),
        DATA_FINE_EFF_STIP_ATTRIB = col_integer(),
        DATA_INIZIO_PREV_ESECUZIONE = col_integer(),
        DATA_INIZIO_EFF_ESECUZIONE = col_integer(),
        DATA_FINE_PREV_ESECUZIONE = col_integer(),
        DATA_FINE_EFF_ESECUZIONE = col_integer(),
        DATA_INIZIO_PREV_COLLAUDO = col_integer(),
        DATA_INIZIO_EFF_COLLAUDO = col_integer(),
        DATA_FINE_PREV_COLLAUDO = col_integer(),
        DATA_FINE_EFF_COLLAUDO = col_integer(),
        OC_STATO_FINANZIARIO = col_character(),
        OC_STATO_PROGETTO = col_character(),
        OC_STATO_PROCEDURALE = col_character(),
        OC_STATO_PROCEDURALE_OGV = col_character(),
        OC_COD_FASE_CORRENTE = col_character(),
        OC_DESCR_FASE_CORRENTE = col_character(),
        COD_PROCED_ATTIVAZIONE = col_character(),
        DESCR_PROCED_ATTIVAZIONE = col_character(),
        COD_TIPO_PROCED_ATTIVAZIONE = col_character(),
        DESCR_TIPO_PROCED_ATTIVAZIONE = col_character(),
        OC_CODFISC_PROGRAMMATORE = col_character(),
        OC_DENOM_PROGRAMMATORE = col_character(),
        OC_COD_FORMA_GIU_PROGRAMMATORE = col_character(),
        OC_DESCR_FORMA_GIU_PROGRAMMATORE = col_character(),
        OC_TOTALE_PROGRAMMATORI = col_double(),
        OC_CODFISC_attuatore = col_character(),
        OC_DENOM_attuatore = col_character(),
        OC_COD_FORMA_GIU_attuatore = col_character(),
        OC_DESCR_FORMA_GIU_attuatore = col_character(),
        OC_TOTALE_ATTUATORI = col_double(),
        OC_CODFISC_BENEFICIARIO = col_character(),
        OC_DENOM_BENEFICIARIO = col_character(),
        OC_COD_FORMA_GIU_BENEFICIARIO = col_character(),
        OC_DESCR_FORMA_GIU_BENEFICIARIO = col_character(),
        OC_TOTALE_BENEFICIARI = col_double(),
        OC_CODFISC_realizzatorE = col_character(),
        OC_DENOM_realizzatorE = col_character(),
        OC_COD_FORMA_GIU_realizzatorE = col_character(),
        OC_DESCR_FORMA_GIU_realizzatorE = col_character(),
        OC_TOTALE_realizzatori = col_double(),
        OC_FLAG_REGIONE_UNICA = col_double(),
        OC_FLAG_VISUALIZZAZIONE = col_double(),
        OC_FLAG_PAC = col_double(),
        OC_FLAG_TAG_BENICONF = col_double(),
        COVID = col_character(),
        pnrr = col_double(),
        SNAI = col_character(),
        DATA_AGGIORNAMENTO = col_double(),
        OC_FOCUS = col_character(),
        x_fondo = col_character(),
        OC_FLAG_AGGREGATO = col_integer(),
        OC_PROGETTO_AGGREGATO = col_character()
      )
    }
  }
  
  # switch
  if (!is.null(data_path)) {
    DATA <- data_path
    # MEMO: sovrascrive data_path a DATA
  } else {
    # #OLD: questo non aveva senso perché era pleonastico perché risultava "/home/antonio/dati/oc/20210228/../20210228"
    # data_path=file.path(DATA, "..", bimestre)
    # DATA <- data_path
    # # MEMO: questo serve per puntare a bimestre specifico senza modificare data_path
    DATA <- DATA
  }
  
  # load progetti
  if (visualizzati == TRUE) {
    # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000) %>%
    #   filter(OC_FLAG_VISUALIZZAZIONE == 0)
    # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1200000) %>%
    #   filter(OC_FLAG_VISUALIZZAZIONE == 0)
    # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000) %>%
    #   filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9) # include progetti FEASR per SNAI
    progetti <- read_csv2(file.path(DATA, temp), col_types = col_types) %>%
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9 | OC_FLAG_VISUALIZZAZIONE == 10) # include progetti FEASR per SNAI
    # CHK: progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 9) %>% count(X_AMBITO)
  } else {
    # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
    # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000)
    progetti <- read_csv2(file.path(DATA, temp), col_types = col_types)
    
    # MEMO: qui prende anche non visualizzati
  }
  
  # analisi tipologia colonne
  # sapply(names(progetti), function(x) {print(paste0(x, " = ", class(progetti[[x]])))})
  
  # refactor
  # MEMO: si applica solo a light
  if (light == TRUE & refactor == TRUE) {
    progetti <- refactor_progetti(progetti)
  }
  
  # debug
  if (debug == TRUE) {
    msg <- progetti %>%
      summarise(N = n(),
                CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
    message(paste0("Progetti esteso contiene ", format(msg$N, big.mark = ".", decimal.mark = ","),
                   " progetti per un costo pubblico totale di ",
                   format(round(msg$CP/1000000000, 1), big.mark = ".", decimal.mark = ","),
                   " miliardi di euro."))
  }
  return(progetti)
  # }
}


#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param progetti dataset di tipo "progetti" che serve per integrare CP, impegni e pagamenti totali.
#' @return Un file csv con apertura per programma, con RISORSE, COE, COE_IMPe, COE_PAG e con COE per fase procedurale.
make_report_programmi_coesione_dataiku <- function(perimetro, usa_meuro=FALSE, show_cp=FALSE, use_eu=FALSE, use_flt=TRUE, 
                                                   use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                   export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  po <- octk::po_riclass
  
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  message("programmi caricato")
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
  }
  
  # patch YEI su programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI su attuazione
  perimetro <- perimetro %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # programmazione
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  
  # attuazione
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`) %>%
    filter(RISORSE > 0)
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    out <- out %>% 
      select(-RISORSE_UE)
  } 
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_cp2.xlsx"))
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi.xlsx"))
    }
  }
  
  return(out)
}




#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx in OUTPUT?
#' @param progetti dataset di tipo "progetti" da utilizzare per show_cp == TRUE
#' @return Un file csv con apertura per programma e fase procedurale.
#' @note Nel report restano per definizione righe con risorse 0, che derivano da errate imputazioni di macroaree.
make_report_programmi_macroaree_coesione_dataiku <- function(perimetro, usa_meuro=FALSE, use_eu=FALSE,
                                                             use_flt=FALSE, use_cicli_psc=FALSE,
                                                             use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                             show_cp=FALSE, export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  # OLD:
  # po <- octk::po_riclass
  # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
  
  # NEW:
  po <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
  
  # programmazione
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
    
  }
  
  # patch YEI programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # refactor macroarea
  programmi <- refactor_macroarea(programmi)
  perimetro <- refactor_macroarea(perimetro)
  
  # patch YEI attuazione 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  # DEV: qui raddoppia per ogni progetto su più macroaree
  
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea(.)%>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, 
           RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP,IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`)
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  }
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree_cp2.xlsx"), rowNames = FALSE)
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree.xlsx"), rowNames = FALSE)
    }
  }
  
  return(out)
}

#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono come in 'programmi', vale solo se 'programmi' è in euro
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione_dataiku <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre
  
  # fix
  programmi <- programmi %>%
    mutate(N_CLP = N) #,
  # IMP = 0,
  # PAG = 0)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE),
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
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
    
  } else {
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
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
  }
  
  
  
  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }
  
  return(report)
}


#' Esporta report per ciclo, ambito e macroarea con dati coesione
#'
#' Report con apertura ciclo, ambito e macroarea.
#' E' costruito su operazioni e dati coesione.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_macroaree_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo, ambto e macroarea.
make_report_macroaree_coesione_dataiku <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEBUG:
  # programmi <- programmi_macroaree
  
  report <- programmi  %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    arrange(x_CICLO, x_AMBITO, x_MACROAREA)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N) 
      
    }
  } else {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N) 
      
    }
  }
  
  
  
  
  out <- report %>% 
    filter(# x_AMBITO != "FEAMP", 
      x_AMBITO != "FEASR") %>% 
    arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA)
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "report_macroaree.csv"))
  }
  
  return(out)
}



#' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_programmi_coesione_dataiku <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                      use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO", "x_PROGRAMMA"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}


' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_sintesi_coesione_dataiku <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                    use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(x_CICLO, x_AMBITO)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_sintesi_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}