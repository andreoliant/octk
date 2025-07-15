# funzioni del blocco "macroaree"




#' Crea il dataset macroaree
#'
#' Crea il dataset macroaree con operazioni per articolazioni e macroaree territoriali.
#'
#' @param bimestre Bimestre di riferimento
#' @param progetti Dataset con un perimetro in formato "progetti".
#' @param operazioni_713 File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_1420 File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_extra File di tipo operazioni da flusso sas/dataiku.
#' @param use_fix Vuoi applicare fix_progetti() prima di esecuzione? Non impatta su x_AMBITO!
#' @param export Vuoi esportare in DATA?
#' @return Il dataset macroaree con le variabili coesione calcolate per articolazione e macroarea territoriale.
#' @note La modalità **debug** esporta diversi csv in TEMP La modalità **export** esporta macroaree_light.csv in DATA.
setup_macroaree <- function(bimestre, progetti, operazioni_713, operazioni_1420, operazioni_extra, export=FALSE, use_fix=FALSE, debug=FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {
    
    # if (is.null(progetti)) {
    #   progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, light = FALSE)
    #   message("Dataset progetti caricato")
    # }

    if (use_fix == TRUE) {
      progetti <- fix_progetti(progetti)
      message("Fix su progetti effettuati")
    }
    
    macroaree <- workflow_macroaree(bimestre, progetti, operazioni_713=operazioni_713, operazioni_1420=operazioni_1420, operazioni_extra=operazioni_extra, debug=debug)
    # macroaree <- operazioni_1
    
    macroaree_2 <- get_regione_simply(macroaree, progetti=progetti)


    
    # variabili
    macroaree_light <- macroaree_2 %>%
      # integra dati finanziari per progetto (sono duplicati)
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, 
                         CP = OC_FINANZ_TOT_PUB_NETTO, 
                         IMP = IMPEGNI, 
                         PAG = TOT_PAGAMENTI),
                by = "COD_LOCALE_PROGETTO") %>%
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
             DES_SEZIONE = x_DES_LIVELLO_0, 
             OC_COD_ARTICOLAZ_PROGRAMMA = x_COD_LIVELLO_1,
             OC_DESCR_ARTICOLAZ_PROGRAMMA = x_DES_LIVELLO_1,
             OC_COD_SUBARTICOLAZ_PROGRAMMA = x_COD_LIVELLO_2,
             OC_DESCR_SUBARTICOLAZ_PROGRAMMA = x_DES_LIVELLO_2)
    
    
    sum(macroaree$COE, na.rm=TRUE) - sum(macroaree_light$COE, na.rm=TRUE)
    
    
    # export
    if (export == TRUE) {
      write.csv2(macroaree_light, file.path(DATA, paste0("macroaree_light_", bimestre, ".csv")), row.names = FALSE)
    }
  } else {
    message("Non hai definito il folder DATA. Carica 'oc' ed inizializza 'oc_init()'.")
  }
}


#' Fix temporaneo per i dataset operazioni
#'
#' Integra il dataset.
#'
#' @param df Dataset in formato standard.
#' @return Il dataset operazioni integrato.
fix_operazioni_macroaree <- function(df) {
  
  
  # patch per caratteri spuri
  # MEMO: ripristino quelli in progetti
  df <- df %>%
    mutate(COD_LOCALE_PROGETTO = case_when(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607\t" ~ "5IGRUEC291_12208_660607",
                                           TRUE ~ COD_LOCALE_PROGETTO))
  
  
  
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
  
  # df <- df %>%
  #   mutate(COD_LOCALE_PROGETTO = case_when(grepl("^1MISE174", COD_LOCALE_PROGETTO) ~ "1MISE174",
  #                                          grepl("^1MISE397", COD_LOCALE_PROGETTO) ~ "1MISE397",
  #                                          grepl("^1MISE496", COD_LOCALE_PROGETTO) ~ "1MISE496",
  #                                          grepl("^1MISE608", COD_LOCALE_PROGETTO) ~ "1MISE608",
  #                                          TRUE ~ COD_LOCALE_PROGETTO))
  
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
fix_operazioni_713_macroaree <- function(df) {
  
  
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



#' Integra la macroarea da localizzazioni
#'
#' Integra in x_MACROAREA la macroarea dalle localizzazioni di PREESTESO partendo da OC_MACROAREA.
#'
#' @param df Dataset con un perimetro in formato "progetti".
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @param debug_mode Dataset in formato "progetti".
#' @return Il dataset con la variabile x_MACROAREA, come factor con levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero").
get_macroarea_localizzazioni <- function(df, progetti, debug_mode=FALSE) {
  # versione da dati di ottobre 2021 con OC_MACROAREA.
  
  # DEBUG:
  # df <- progetti
  
  if (!any(names(df) == "OC_MACROAREA")) {
    if (missing(progetti)) {
      progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = TRUE)
    }
    df <- df %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_MACROAREA, OC_MAREA),
                by = "COD_LOCALE_PROGETTO")
  }
  
  df <- df %>%
    mutate(x_MACROAREA = case_when(OC_MACROAREA == "Mezzogiorno" ~ "Mezzogiorno",
                                   OC_MACROAREA == "Centro-Nord" ~ "Centro-Nord",
                                   OC_MACROAREA == "Ambito Nazionale" ~ "Ambito nazionale",
                                   OC_MACROAREA ==  "Altro" ~ "Trasversale",
                                   OC_MACROAREA == "Estero" ~ "Estero",
                                   OC_MAREA == "CN"~ "Centro-Nord",
                                   OC_MAREA == "MZ"~ "Mezzogiorno",
                                   is.na(OC_MAREA) ~ "Ambito nazionale")) %>%
    # mutate(x_MACROAREA = factor(x_MACROAREA, levels = c("Mezzogiorno", "Centro-Nord", "Ambito nazionale", "Trasversale", "Estero")))
    refactor_macroarea(.)
  
  return(df)
}


#' Workflow per creare il dataset macroaree
#'
#' Workflow per creare il dataset macroaree
#'
#' @param bimestre Bimestre di rifeirimento da oc_init().
#' @param progetti Dataset "progetti" in formato PREESTESO per integrazione.
#' @param operazioni_713 File di tipo operazioni da flusso sas/dataiku.
#' @param operazioni_1420 File di tipo operazioni da flusso sas/dataiku.
#' @param riprop_yei Vuoi riproporzionare yei dove mancano dati finanziari per livelli gerarchici? Default su FALSE.
#' @param debug_mode Esporta i file relativi a [...]
#' @return Il dataset con la variabile x_MACROAREA, da usare all'interno di setup_macroare().
workflow_macroaree <- function(bimestre, progetti, operazioni_713, operazioni_1420, operazioni_extra, riprop_yei=FALSE, debug=FALSE) {
  
  # ----------------------------------------------------------------------------------- #
  # loads----
  
  # DEBUG:
  # tipo_input="sas"
  # riprop_yei=FALSE
  # debug=FALSE
  
  message("Entro in workflow operazioni")
  
  # NEXT: da eliminare e spostare su DBCOE
  po <- octk::po_riclass%>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
  
  # if (tipo_input == "csv") {
  #   operazioni_1420_raw <- read_csv2(file.path(DATA, "oper_pucok_preesteso.csv"))
  #   message("Operazioni raw caricate per 1420 (da csv)")
  #   
  #   operazioni_713_raw <- read_csv2(file.path(DATA, "oper_fltok_preesteso.csv")) 
  #   message("Operazioni raw caricate per 713 (da csv)")
  #   
  # } else if (tipo_input == "sas") {
  #   operazioni_1420_raw <- read_sas(file.path(DATA, "oper_pucok_preesteso.sas7bdat"))
  #   message("Operazioni raw caricate per 1420 (da sas)")
  #   
  #    <- read_sas(file.path(DATA, "oper_fltok_preesteso.sas7bdat")) 
  #   message("Operazioni raw caricate per 713 (da sas)")
  #   
  # } else {
  #   message("Tipo input non applicabile")
  #   exit
  # }
  
  operazioni_713_raw <- operazioni_713
  operazioni_1420_raw <- operazioni_1420
  operazioni_extra_raw <- operazioni_extra
  
  
  
  # ----------------------------------------------------------------------------------- #
  # prep----
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni extra----
  

  message("Preparazione dati extra...")
  
  # clean
  operazioni_extra <- operazioni_extra_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni_macroaree(.) %>%
    # creo ambito
    # mutate(x_AMBITO = case_when(oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "IPA" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "SNAI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "INA" ~ "SNAI", # oc_cod_programma == "2020PCDPCINA001"
    #                             oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
    #                             oc_cod_fonte == "FSC1420" ~ "FSC",
    #                             oc_cod_fonte == "PAC1420" ~ "POC",
    #                             # new
    #                             oc_cod_fonte == "FSC2127" ~ "FSC",
    #                             oc_cod_fonte == "FSC0713" ~ "FSC", # parte di PSC migrata da 713
    #                             oc_cod_fonte == "NAZORD" ~ "SNAI")) %>%
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
                                oc_cod_fonte == "FSC2127" ~ oc_ambito,)) %>%
    # articolazioni
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
                                       x_AMBITO == "JTF" ~ ue_descr_ob_specifico,)) %>% 
    # variabili coesione
    mutate(COS_AMM = oc_costo_coesione,
           IMP_AMM = oc_impegni_coesione,
           PAG_AMM = oc_tot_pagamenti_coesione)
  
  
  # operazioni_1420 %>% count(x_AMBITO, oc_cod_fonte, ue_descr_fondo)
  # operazioni_1420 %>% filter(oc_cod_fonte == "FSC0713") %>% count(OC_CODICE_PROGRAMMA)
  
  # chk
  chk <- operazioni_extra %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  chk %>% filter(is.na(x_COD_LIVELLO_1))
  chk %>% filter(grepl(":::", x_COD_LIVELLO_1)) %>% count(x_COD_LIVELLO_1, x_COD_LIVELLO_2)
  
  # DEV: qui va messo nuovo workflow per separare righe con ":::" che deve riproporzionare COE, COE_IMP e COE_PAG
  
  # clean
  operazioni_extra <- operazioni_extra %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM,
           costo_ammesso_MZ, costo_ammesso_CN, 
           imp_ammesso_MZ, imp_ammesso_CN, imp_trasf_ammesso_MZ, imp_trasf_ammesso_CN,
           pag_ammesso_MZ, pag_ammesso_CN, pag_trasf_ammesso_MZ, pag_trasf_ammesso_CN,
           x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
           x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
           x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  # chk
  # operazioni_1420 %>% 
  #   group_by(x_AMBITO) %>% 
  #   summarise_if(is.numeric, sum, na.rm=TRUE)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # macroaree extra----
  
  # verifica dove sono presenti dati finanziari aperti per macroaree
  chk <- operazioni_extra  %>%
    # integra x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = c("OC_CODICE_PROGRAMMA")) %>% 
    group_by(x_AMBITO, x_GRUPPO, x_CICLO) %>% 
    summarise_if(is.numeric, sum, na.rm=TRUE)
  # SIE 1420, PSC, anticipazioni accordi
  
  chk2 <- operazioni_extra  %>%
    # integra x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = c("OC_CODICE_PROGRAMMA")) %>% 
    group_by(x_AMBITO, x_GRUPPO, x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_COD_LIVELLO_1) %>% 
    summarise_if(is.numeric, sum, na.rm=TRUE) %>% 
    mutate(chk_delta = COE - costo_ammesso_MZ - costo_ammesso_CN)
  # assi react + assi YEI
  
  # integra macroarea da localizzazione
  operazioni_extra_1 <- get_macroarea_localizzazioni(operazioni_extra, progetti)
  
  # chk NA su macroarea
  # operazioni_1420_1 %>% count(x_MACROAREA)
  # progetti %>% count(OC_MACROAREA)
  # operazioni_1420 %>% anti_join(progetti, by = "COD_LOCALE_PROGETTO") %>% select(COD_LOCALE_PROGETTO)
  # 
  # progetti %>% filter(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607\t")
  # progetti %>% filter(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607")
  # operazioni_1420_raw %>% filter(cod_locale_progetto == "5IGRUEC291_12208_660607\t")
  # operazioni_1420_raw %>% filter(cod_locale_progetto == "5IGRUEC291_12208_660607")
  
  # chk valle d'aosta
  # operazioni_1420_1 %>% filter(OC_CODICE_PROGRAMMA == "PSCVALLEAOSTA") %>% count(x_MACROAREA)
  # progetti %>% filter(OC_CODICE_PROGRAMMA == "PSCVALLEAOSTA") %>% count(OC_MACROAREA)
  
  operazioni_extra_2 <- operazioni_extra_1 %>%
    # integra natura per gestione trasferimenti
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
    select(-CUP_COD_NATURA)
  
  # DEV: OPT1
  
  # sovrascrive macroarea sie da livelli gerarchici
  # perimetro_sie <- workflow_macroaree_sub_sie(operazioni = operazioni_1420_2, riprop_yei=riprop_yei, debug=debug)
  
  # # DEV: chk YEI
  # temp <- operazioni_1420_2 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>%
  #   mutate(tot = COE_SUD + COE_CN, chk = COE - tot) %>% 
  #   filter(is.na(COE_SUD) & is.na(COE_CN) | COE_SUD == 0 & COE_CN == 0 | abs(chk)>1)
  # 
  # perimetro_sie_1 <- workflow_macroaree_sub_sie(operazioni = temp, riprop_yei=TRUE, debug=debug)
  # perimetro_sie_2 <- workflow_macroaree_sub_sie(operazioni = temp, riprop_yei=FALSE, debug=debug)
  # 
  # temp %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # temp %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_1 %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_1 %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_2 %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_2 %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # 
  # 
  # sum(perimetro_sie_1$COE, na.rm = TRUE) - sum(perimetro_sie_2$COE, na.rm = TRUE)
  #   # 
  # perimetro_sie_1 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(perimetro_sie_2 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  
  # operazioni_1420_3 <- operazioni_1420_2 %>% 
  #   # scarta le righe sie di cui uso dati da livelli gerarchici
  #   anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
  #   # accoda nuove righe sie
  #   bind_rows(perimetro_sie)
  
  # chk
  # operazioni_1420_2 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  # perimetro_sie %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  # operazioni_1420_3 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  
  # chk
  # operazioni_1420_3 %>% count(x_MACROAREA)
  # 
  # sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_3$COE, na.rm = TRUE)
  # # 0.08996582
  # 
  # operazioni_1420 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(operazioni_1420_3 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  
  # perimetro_psc <- workflow_macroaree_sub_psc(operazioni = operazioni_1420_3)
  
  # operazioni_1420_4 <- operazioni_1420_3 %>% 
  #   # scarta le righe psc di cui uso dati da livelli gerarchici
  #   anti_join(perimetro_psc, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
  #   # accoda nuove righe sie
  #   bind_rows(perimetro_psc) 
  
  # operazioni_1420_4 %>% count(x_MACROAREA)
  # 
  # # riordina
  # operazioni_1420_5 <- operazioni_1420_4 %>% 
  #   select(names(operazioni_1420), x_MACROAREA)
  # 
  # operazioni_1420_5 %>% count(x_MACROAREA)
  
  # chk
  # sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_5$COE, na.rm = TRUE)
  # CHK: 0.09002686
  
  # operazioni_1420 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(operazioni_1420_5 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  # # OC_CODICE_PROGRAMMA       COE.x       COE.y    chk
  # # <chr>                     <dbl>       <dbl>  <dbl>
  # # 2014IT05M9OP001     2016666050. 2016666050. 0.0900
  # message("DEBUG: controlla YEI")
  
  # DEV: OPT2
  
  
  po_react <- c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05SFOP001", "2014IT05SFOP001",  "2014IT05SFOP002", 
                "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003")
  po_yei <- c("2014IT05M9OP001")
  po_psc <- octk::po_riclass %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  po_ant <- octk::po_riclass %>% filter(x_CICLO == "2021-2027", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  
  
  # studio casistiche
  chk <- operazioni_extra_2 %>%
    select(-costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -imp_trasf_ammesso_MZ, -imp_trasf_ammesso_CN,
           -pag_ammesso_MZ, -pag_ammesso_CN, -pag_trasf_ammesso_MZ, -pag_trasf_ammesso_CN) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # mutate_if(is.numeric, round, digits=2) %>% 
    mutate(COE = round(COE, 2),
           COE_SUD = round(COE_SUD, 2),
           COE_CN = round(COE_CN, 2)) %>% 
    mutate(chk_coe = COE - COE_SUD - COE_CN,
           chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>% 
    mutate(chk_coe = round(chk_coe, 2)) %>%
    mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
                              OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
                              OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
                              OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
                              TRUE ~ "")) %>% 
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
                               TRUE ~ "chk"))
  
  chk %>% 
    group_by(CHK_COE) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE))  %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  chk2 <- chk %>% 
    group_by(CLASSE, CHK_COE) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE))  %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  chk3 <- chk %>% filter(CHK_COE == "negativi") %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN, chk_coe)
  
  # mapping variabili
  operazioni_extra_3 <- operazioni_extra_2 %>%
    select(-costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -imp_trasf_ammesso_MZ, -imp_trasf_ammesso_CN,
           -pag_ammesso_MZ, -pag_ammesso_CN, -pag_trasf_ammesso_MZ, -pag_trasf_ammesso_CN) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # fix per anomalie floating
    # mutate_if(is.numeric, round, digits=2) %>% 
    mutate(COE = round(COE, 2),
           COE_SUD = round(COE_SUD, 2),
           COE_CN = round(COE_CN, 2),
           
           COE_IMP = round(COE_IMP, 2),
           COE_IMP_SUD = round(COE_IMP_SUD, 2),
           COE_IMP_CN = round(COE_IMP_CN, 2),
           
           COE_PAG = round(COE_PAG, 2),
           COE_PAG_SUD = round(COE_PAG_SUD, 2),
           COE_PAG_CN = round(COE_PAG_CN, 2)) %>% 
    mutate(chk_coe = COE - COE_SUD - COE_CN,
           chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>% 
    mutate(chk_coe = round(chk_coe, 2),
           chk_coe_imp = round(chk_coe_imp, 2),
           chk_coe_pag = round(chk_coe_pag, 2)) %>% 
    mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
                              OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
                              OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
                              OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
                              TRUE ~ "")) %>% 
    mutate(COE_SUD = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD + chk_coe, #"tutto sud localizzazioni e livelli"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD + chk_coe, #"tutto sud ma manca una parte da livelli"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD + chk_coe, #"tutto sud solo localizzazioni"
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"tutto cn localizzazioni e livelli"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"tutto cn ma manca una parte da livelli"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"tutto cn solo localizzazioni"
                               
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                               
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"ambito nazionale puro"
                               
                               COE == 0 ~ COE_SUD, #"fin nullo"
                               COE < 1 ~ COE_SUD, #"fin quasi nullo"
                               TRUE ~ 0),
           
           COE_CN = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"tutto sud localizzazioni e livelli"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"tutto sud ma manca una parte da livelli"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"tutto sud solo localizzazioni"
                              
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                              
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN + chk_coe, #"tutto cn localizzazioni e livelli"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN + chk_coe, #"tutto cn ma manca una parte da livelli"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN + chk_coe, #"tutto cn solo localizzazioni"
                              
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                              
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"ambito nazionale puro"
                              
                              COE == 0 ~ COE_CN, #"fin nullo"
                              COE < 1 ~ COE_CN, #"fin quasi nullo"
                              TRUE ~ 0),
           
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
                              
                              COE == 0 ~ chk_coe, #"fin nullo"
                              COE < 1 ~ chk_coe, #"fin quasi nullo"
                              TRUE ~ 0)) %>% 
    
    #impegni  
    mutate(COE_IMP_SUD = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud localizzazioni e livelli"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud ma manca una parte da livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud solo localizzazioni"
                                   
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                   
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"tutto cn localizzazioni e livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"tutto cn ma manca una parte da livelli"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"tutto cn solo localizzazioni"
                                   
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                   
                                   COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"ambito nazionale puro"
                                   
                                   COE_IMP == 0 ~ COE_IMP_SUD, #"fin nullo"
                                   COE_IMP < 1 ~ COE_IMP_SUD, #"fin quasi nullo"
                                   TRUE ~ 0),
           
           COE_IMP_CN = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"tutto sud localizzazioni e livelli"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"tutto sud ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"tutto sud solo localizzazioni"
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn localizzazioni e livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn ma manca una parte da livelli"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn solo localizzazioni"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"ambito nazionale puro"
                                  
                                  COE_IMP == 0 ~ COE_IMP_CN, #"fin nullo"
                                  COE_IMP < 1 ~ COE_IMP_CN, #"fin quasi nullo"
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
                                  
                                  COE_IMP == 0 ~ chk_coe_imp, #"fin nullo"
                                  COE_IMP < 1 ~ chk_coe_imp, #"fin quasi nullo"
                                  TRUE ~ 0)) %>% 
    
    # pagamenti 
    mutate(COE_PAG_SUD = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud localizzazioni e livelli"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud ma manca una parte da livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud solo localizzazioni"
                                   
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                   
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"tutto cn localizzazioni e livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"tutto cn ma manca una parte da livelli"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"tutto cn solo localizzazioni"
                                   
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                   
                                   COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                   COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"ambito nazionale puro"
                                   
                                   COE_PAG == 0 ~ COE_PAG_SUD, #"fin nullo"
                                   COE_PAG < 1 ~ COE_PAG_SUD, #"fin quasi nullo"
                                   TRUE ~ 0),
           
           COE_PAG_CN = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"tutto sud localizzazioni e livelli"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"tutto sud ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"tutto sud solo localizzazioni"
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                                  
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn localizzazioni e livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn ma manca una parte da livelli"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn solo localizzazioni"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (con delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                                  
                                  COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                                  COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"ambito nazionale puro"
                                  
                                  COE_PAG == 0 ~ COE_PAG_CN, #"fin nullo"
                                  COE_PAG < 1 ~ COE_PAG_CN, #"fin quasi nullo"
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
                                  
                                  COE_PAG == 0 ~ chk_coe_pag, #"fin nullo"
                                  COE_PAG < 1 ~ chk_coe_pag, #"fin quasi nullo"
                                  TRUE ~ 0)) %>% 
    
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2) %>% 
    # fix per anomalie floating
    mutate_if(is.numeric, round, digits=2) 
  
  
  
  operazioni_extra_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE),
              COE_ND = sum(COE_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2)
  
  
  operazioni_extra_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_IMP_SUD = sum(COE_IMP_SUD, na.rm = TRUE),
              COE_IMP_CN = sum(COE_IMP_CN, na.rm = TRUE),
              COE_IMP_ND = sum(COE_IMP_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_IMP_SUD + COE_IMP_CN + COE_IMP_ND,
           chk2 = COE_IMP - tot2)
  
  
  operazioni_extra_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE_PAG = sum(COE_PAG, na.rm = TRUE),
              COE_PAG_SUD = sum(COE_PAG_SUD, na.rm = TRUE),
              COE_PAG_CN = sum(COE_PAG_CN, na.rm = TRUE),
              COE_PAG_ND = sum(COE_PAG_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_PAG_SUD + COE_PAG_CN + COE_PAG_ND,
           chk2 = COE_PAG - tot2)
  
  operazioni_extra_3 %>% 
    filter(abs(chk2) > 0) %>% 
    left_join(po %>% 
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA), 
              by = "OC_CODICE_PROGRAMMA") %>% 
    group_by(CLASSE, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE),
              COE_ND = sum(COE_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2)
  
  # # chk pon imprese 
  # chk <- operazioni_1420_3 %>% 
  #   filter(chk2 > 0) %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2014IT16RFOP003")
  # # 46 progetti con due assi (IV:::VI, solo l'asse VI è react)
  
  # # chk pon yei
  # chk <- operazioni_1420_3 %>% 
  #   filter(chk2 > 0) %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001")
  # # progetti con COE_CN > 0, COE_SUD = 0 e x_MACROAREA = "Mezzogiorno" o simili (vedi mail a fabio)
  
  
  chk <- operazioni_extra_3 %>% 
    filter(abs(chk2) > 0) %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN, COE_ND, chk2, tot2) %>% 
    arrange(desc(COE))
  
  operazioni_extra_3 %>% 
    semi_join(chk, by = "COD_LOCALE_PROGETTO") %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN)  %>% 
    arrange(desc(COE))
  
  
  
  operazioni_extra_4 <- workflow_macroaree_sub_pivot_evo(operazioni_extra_3) 
  
  sum(operazioni_extra$COE, na.rm = TRUE) - sum(operazioni_extra_4$COE, na.rm = TRUE)
  sum(operazioni_extra$COE_IMP, na.rm = TRUE) - sum(operazioni_extra_4$COE_IMP, na.rm = TRUE)
  sum(operazioni_extra$COE_PAG, na.rm = TRUE) - sum(operazioni_extra_4$COE_PAG, na.rm = TRUE)
  # 0
  
  operazioni_extra %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE, na.rm = TRUE)) %>%
    full_join(operazioni_extra_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  operazioni_extra %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE_IMP, na.rm = TRUE)) %>%
    full_join(operazioni_extra_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE_IMP, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  operazioni_extra %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE_PAG, na.rm = TRUE)) %>%
    full_join(operazioni_extra_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE_PAG, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  # pulisce dupli per macroaree vuote
  operazioni_extra_5 <- operazioni_extra_4 %>% 
    filter(COE != 0)
  
  
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni 1420----
  
  # chk
  # operazioni_1420_raw %>% count(oc_cod_fonte, ue_descr_fondo)
  # operazioni_1420_raw %>% count(STATO)
  
  message("Preparazione dati 1420...")
  
  # clean
  operazioni_1420 <- operazioni_1420_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_CODICE_PROGRAMMA = oc_cod_programma) %>%
    # elimina duplicati anomali (solo per 1420)
    filter(STATO == 1) %>%
    # fix per anomalie (cambiano nei diversi bimestri ma sono abbastanza generiche)
    fix_operazioni_macroaree(.) %>%
    # creo ambito
    # mutate(x_AMBITO = case_when(oc_cod_fonte == "FS1420" & ue_descr_fondo == "IOG" ~ "YEI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "PAC" ~ "SNAI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "FESR" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "IPA" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "CTE" ~ "CTE",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "ARI" ~ "SNAI",
    #                             oc_cod_fonte == "FS1420" & ue_descr_fondo == "NAZIONALE" &
    #                               CODICE_TIPOLOGIA_PROGRAMMA == "INA" ~ "SNAI", # oc_cod_programma == "2020PCDPCINA001"
    #                             oc_cod_fonte == "FS1420" ~ ue_descr_fondo,
    #                             oc_cod_fonte == "FSC1420" ~ "FSC",
    #                             oc_cod_fonte == "PAC1420" ~ "POC",
    #                             # new
    #                             oc_cod_fonte == "FSC2127" ~ "FSC",
    #                             oc_cod_fonte == "FSC0713" ~ "FSC", # parte di PSC migrata da 713
    #                             oc_cod_fonte == "NAZORD" ~ "SNAI")) %>%
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
  
  # chk
  chk <- operazioni_1420 %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  chk %>% filter(is.na(x_COD_LIVELLO_1))
  chk %>% filter(grepl(":::", x_COD_LIVELLO_1)) %>% count(x_COD_LIVELLO_1, x_COD_LIVELLO_2)
  
  # DEV: qui va messo nuovo workflow per separare righe con ":::" che deve riproporzionare COE, COE_IMP e COE_PAG
  
  # clean
  operazioni_1420 <- operazioni_1420 %>%
    select(COD_LOCALE_PROGETTO,
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE = COS_AMM,
           COE_IMP = IMP_AMM,
           COE_PAG = PAG_AMM,
           costo_ammesso_MZ, costo_ammesso_CN, 
           imp_ammesso_MZ, imp_ammesso_CN, imp_trasf_ammesso_MZ, imp_trasf_ammesso_CN,
           pag_ammesso_MZ, pag_ammesso_CN, pag_trasf_ammesso_MZ, pag_trasf_ammesso_CN,
           x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
           x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
           x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  # chk
  # operazioni_1420 %>% 
  #   group_by(x_AMBITO) %>% 
  #   summarise_if(is.numeric, sum, na.rm=TRUE)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # macroaree 1420----
  
  # verifica dove sono presenti dati finanziari aperti per macroaree
  chk <- operazioni_1420  %>%
    # integra x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = c("OC_CODICE_PROGRAMMA")) %>% 
    group_by(x_AMBITO, x_GRUPPO, x_CICLO) %>% 
    summarise_if(is.numeric, sum, na.rm=TRUE)
  # SIE 1420, PSC, anticipazioni accordi
  
  chk2 <- operazioni_1420  %>%
    # integra x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = c("OC_CODICE_PROGRAMMA")) %>% 
    group_by(x_AMBITO, x_GRUPPO, x_CICLO, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_COD_LIVELLO_1) %>% 
    summarise_if(is.numeric, sum, na.rm=TRUE) %>% 
    mutate(chk_delta = COE - costo_ammesso_MZ - costo_ammesso_CN)
  # assi react + assi YEI
  
  # integra macroarea da localizzazione
  operazioni_1420_1 <- get_macroarea_localizzazioni(operazioni_1420, progetti)

  # chk NA su macroarea
  # operazioni_1420_1 %>% count(x_MACROAREA)
  # progetti %>% count(OC_MACROAREA)
  # operazioni_1420 %>% anti_join(progetti, by = "COD_LOCALE_PROGETTO") %>% select(COD_LOCALE_PROGETTO)
  # 
  # progetti %>% filter(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607\t")
  # progetti %>% filter(COD_LOCALE_PROGETTO == "5IGRUEC291_12208_660607")
  # operazioni_1420_raw %>% filter(cod_locale_progetto == "5IGRUEC291_12208_660607\t")
  # operazioni_1420_raw %>% filter(cod_locale_progetto == "5IGRUEC291_12208_660607")
  
  # chk valle d'aosta
  # operazioni_1420_1 %>% filter(OC_CODICE_PROGRAMMA == "PSCVALLEAOSTA") %>% count(x_MACROAREA)
  # progetti %>% filter(OC_CODICE_PROGRAMMA == "PSCVALLEAOSTA") %>% count(OC_MACROAREA)
  
  operazioni_1420_2 <- operazioni_1420_1 %>%
    # integra natura per gestione trasferimenti
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
    select(-CUP_COD_NATURA)
  
  # DEV: OPT1
  
  # sovrascrive macroarea sie da livelli gerarchici
  # perimetro_sie <- workflow_macroaree_sub_sie(operazioni = operazioni_1420_2, riprop_yei=riprop_yei, debug=debug)
  
  # # DEV: chk YEI
  # temp <- operazioni_1420_2 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>%
  #   mutate(tot = COE_SUD + COE_CN, chk = COE - tot) %>% 
  #   filter(is.na(COE_SUD) & is.na(COE_CN) | COE_SUD == 0 & COE_CN == 0 | abs(chk)>1)
  # 
  # perimetro_sie_1 <- workflow_macroaree_sub_sie(operazioni = temp, riprop_yei=TRUE, debug=debug)
  # perimetro_sie_2 <- workflow_macroaree_sub_sie(operazioni = temp, riprop_yei=FALSE, debug=debug)
  # 
  # temp %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # temp %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_1 %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_1 %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_2 %>% group_by(x_MACROAREA) %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # perimetro_sie_2 %>% summarise(COE=sum(COE,na.rm=TRUE),COE_SUD=sum(COE_SUD,na.rm=TRUE),COE_CN=sum(COE_CN,na.rm=TRUE)) %>% mutate(tot = COE_SUD + COE_CN, chk = COE - tot)
  # 
  # 
  # sum(perimetro_sie_1$COE, na.rm = TRUE) - sum(perimetro_sie_2$COE, na.rm = TRUE)
  #   # 
  # perimetro_sie_1 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(perimetro_sie_2 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  
  # operazioni_1420_3 <- operazioni_1420_2 %>% 
  #   # scarta le righe sie di cui uso dati da livelli gerarchici
  #   anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
  #   # accoda nuove righe sie
  #   bind_rows(perimetro_sie)
  
  # chk
  # operazioni_1420_2 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  # perimetro_sie %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  # operazioni_1420_3 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm = TRUE))
  
  # chk
  # operazioni_1420_3 %>% count(x_MACROAREA)
  # 
  # sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_3$COE, na.rm = TRUE)
  # # 0.08996582
  # 
  # operazioni_1420 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(operazioni_1420_3 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  
  # perimetro_psc <- workflow_macroaree_sub_psc(operazioni = operazioni_1420_3)
  
  # operazioni_1420_4 <- operazioni_1420_3 %>% 
  #   # scarta le righe psc di cui uso dati da livelli gerarchici
  #   anti_join(perimetro_psc, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
  #   # accoda nuove righe sie
  #   bind_rows(perimetro_psc) 
  
  # operazioni_1420_4 %>% count(x_MACROAREA)
  # 
  # # riordina
  # operazioni_1420_5 <- operazioni_1420_4 %>% 
  #   select(names(operazioni_1420), x_MACROAREA)
  # 
  # operazioni_1420_5 %>% count(x_MACROAREA)
  
    # chk
  # sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_5$COE, na.rm = TRUE)
  # CHK: 0.09002686
  
  # operazioni_1420 %>% 
  #   group_by(OC_CODICE_PROGRAMMA) %>% 
  #   summarise(COE = sum(COE, na.rm = TRUE)) %>% 
  #   full_join(operazioni_1420_5 %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(COE = sum(COE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(chk = COE.x-COE.y) %>% 
  #   filter(chk != 0)
  # # OC_CODICE_PROGRAMMA       COE.x       COE.y    chk
  # # <chr>                     <dbl>       <dbl>  <dbl>
  # # 2014IT05M9OP001     2016666050. 2016666050. 0.0900
  # message("DEBUG: controlla YEI")
  
  # DEV: OPT2
  
  
  po_react <- c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05SFOP001", "2014IT05SFOP001",  "2014IT05SFOP002", 
                "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003")
  po_yei <- c("2014IT05M9OP001")
  po_psc <- octk::po_riclass %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  po_ant <- octk::po_riclass %>% filter(x_CICLO == "2021-2027", x_GRUPPO == "PSC", TIPO == 0) %>% .$OC_CODICE_PROGRAMMA
  

  # studio casistiche
  chk <- operazioni_1420_2 %>%
    select(-costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -imp_trasf_ammesso_MZ, -imp_trasf_ammesso_CN,
           -pag_ammesso_MZ, -pag_ammesso_CN, -pag_trasf_ammesso_MZ, -pag_trasf_ammesso_CN) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # mutate_if(is.numeric, round, digits=2) %>% 
    mutate(COE = round(COE, 2),
           COE_SUD = round(COE_SUD, 2),
           COE_CN = round(COE_CN, 2)) %>% 
    mutate(chk_coe = COE - COE_SUD - COE_CN,
           chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>% 
    mutate(chk_coe = round(chk_coe, 2)) %>%
    mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
                              OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
                              OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
                              OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
                              TRUE ~ "")) %>% 
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
                               TRUE ~ "chk"))
  
  chk %>% 
    group_by(CHK_COE) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE))  %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  chk2 <- chk %>% 
    group_by(CLASSE, CHK_COE) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE))  %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  chk3 <- chk %>% filter(CHK_COE == "negativi") %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN, chk_coe)
  
  # mapping variabili
  operazioni_1420_3 <- operazioni_1420_2 %>%
    select(-costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -imp_trasf_ammesso_MZ, -imp_trasf_ammesso_CN,
           -pag_ammesso_MZ, -pag_ammesso_CN, -pag_trasf_ammesso_MZ, -pag_trasf_ammesso_CN) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # fix per anomalie floating
    # mutate_if(is.numeric, round, digits=2) %>% 
    mutate(COE = round(COE, 2),
           COE_SUD = round(COE_SUD, 2),
           COE_CN = round(COE_CN, 2),
           
           COE_IMP = round(COE_IMP, 2),
           COE_IMP_SUD = round(COE_IMP_SUD, 2),
           COE_IMP_CN = round(COE_IMP_CN, 2),
           
           COE_PAG = round(COE_PAG, 2),
           COE_PAG_SUD = round(COE_PAG_SUD, 2),
           COE_PAG_CN = round(COE_PAG_CN, 2)) %>% 
    mutate(chk_coe = COE - COE_SUD - COE_CN,
           chk_coe_imp = COE_IMP - COE_IMP_SUD - COE_IMP_CN,
           chk_coe_pag = COE_PAG - COE_PAG_SUD - COE_PAG_CN) %>% 
    mutate(chk_coe = round(chk_coe, 2),
           chk_coe_imp = round(chk_coe_imp, 2),
           chk_coe_pag = round(chk_coe_pag, 2)) %>% 
    mutate(CLASSE = case_when(OC_CODICE_PROGRAMMA %in% po_react & COE_SUD == 0 & COE_CN == 0 ~ "react", #MEMO: altrimenti prende anche assi non react
                              OC_CODICE_PROGRAMMA %in% po_yei ~ "yei",
                              OC_CODICE_PROGRAMMA %in% po_psc ~ "psc",
                              OC_CODICE_PROGRAMMA %in% po_ant ~ "ant",
                              TRUE ~ "")) %>% 
    mutate(COE_SUD = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD + chk_coe, #"tutto sud localizzazioni e livelli"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD + chk_coe, #"tutto sud ma manca una parte da livelli"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD + chk_coe, #"tutto sud solo localizzazioni"
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"tutto cn localizzazioni e livelli"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"tutto cn ma manca una parte da livelli"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"tutto cn solo localizzazioni"
                               
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                               
                               COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_SUD, #"ambito nazionale puro"
                               
                               COE == 0 ~ COE_SUD, #"fin nullo"
                               COE < 1 ~ COE_SUD, #"fin quasi nullo"
                               TRUE ~ 0),
           
           COE_CN = case_when(COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"tutto sud localizzazioni e livelli"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"tutto sud ma manca una parte da livelli"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"tutto sud solo localizzazioni"
                              
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                              
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN + chk_coe, #"tutto cn localizzazioni e livelli"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN + chk_coe, #"tutto cn ma manca una parte da livelli"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN + chk_coe, #"tutto cn solo localizzazioni"
                              
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe == 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe > 0 ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                              
                              COE_SUD > 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD > 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD == 0 & COE_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_SUD == 0 & COE_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_CN, #"ambito nazionale puro"
                              
                              COE == 0 ~ COE_CN, #"fin nullo"
                              COE < 1 ~ COE_CN, #"fin quasi nullo"
                              TRUE ~ 0),
           
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
                              
                              COE == 0 ~ chk_coe, #"fin nullo"
                              COE < 1 ~ chk_coe, #"fin quasi nullo"
                              TRUE ~ 0)) %>% 
  
    #impegni  
    mutate(COE_IMP_SUD = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud localizzazioni e livelli"
                               COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud ma manca una parte da livelli"
                               COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD + chk_coe_imp, #"tutto sud solo localizzazioni"
                               
                               COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"tutto cn localizzazioni e livelli"
                               COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"tutto cn ma manca una parte da livelli"
                               COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"tutto cn solo localizzazioni"
                               
                               COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                               COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                               
                               COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_SUD, #"ambito nazionale puro"
                               
                               COE_IMP == 0 ~ COE_IMP_SUD, #"fin nullo"
                               COE_IMP < 1 ~ COE_IMP_SUD, #"fin quasi nullo"
                               TRUE ~ 0),
           
           COE_IMP_CN = case_when(COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"tutto sud localizzazioni e livelli"
                              COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"tutto sud ma manca una parte da livelli"
                              COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"tutto sud solo localizzazioni"
                              
                              COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                              COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                              
                              COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn localizzazioni e livelli"
                              COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn ma manca una parte da livelli"
                              COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN + chk_coe_imp, #"tutto cn solo localizzazioni"
                              
                              COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp == 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                              COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_imp > 0 ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                              
                              COE_IMP_SUD > 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_IMP_SUD > 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_IMP_SUD == 0 & COE_IMP_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_IMP_SUD == 0 & COE_IMP_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_IMP_CN, #"ambito nazionale puro"
                              
                              COE_IMP == 0 ~ COE_IMP_CN, #"fin nullo"
                              COE_IMP < 1 ~ COE_IMP_CN, #"fin quasi nullo"
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
                              
                              COE_IMP == 0 ~ chk_coe_imp, #"fin nullo"
                              COE_IMP < 1 ~ chk_coe_imp, #"fin quasi nullo"
                              TRUE ~ 0)) %>% 
    
    # pagamenti 
    mutate(COE_PAG_SUD = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud localizzazioni e livelli"
                               COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud ma manca una parte da livelli"
                               COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD + chk_coe_pag, #"tutto sud solo localizzazioni"
                               
                               COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)",
                               COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)",
                               
                               COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"tutto cn localizzazioni e livelli"
                               COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"tutto cn ma manca una parte da livelli"
                               COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"tutto cn solo localizzazioni"
                               
                               COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (no delta)"
                               COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza livelli vs localizzazioni (con delta)"
                               COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (no delta)"
                               COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (con delta)"
                               
                               COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                               COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_SUD, #"ambito nazionale puro"
                               
                               COE_PAG == 0 ~ COE_PAG_SUD, #"fin nullo"
                               COE_PAG < 1 ~ COE_PAG_SUD, #"fin quasi nullo"
                               TRUE ~ 0),
           
           COE_PAG_CN = case_when(COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"tutto sud localizzazioni e livelli"
                              COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"tutto sud ma manca una parte da livelli"
                              COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"tutto sud solo localizzazioni"
                              
                              COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)",
                              COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Mezzogiorno" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)",
                              
                              COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn localizzazioni e livelli"
                              COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn ma manca una parte da livelli"
                              COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN + chk_coe_pag, #"tutto cn solo localizzazioni"
                              
                              COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (no delta)"
                              COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza livelli vs localizzazioni (con delta)"
                              COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag == 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (no delta)"
                              COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA == "Centro-Nord" & chk_coe_pag > 0 ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (con delta)"
                              
                              COE_PAG_SUD > 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_PAG_SUD > 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_PAG_SUD == 0 & COE_PAG_CN > 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"divergenza parziale livelli vs localizzazioni (ambito nazionale)"
                              COE_PAG_SUD == 0 & COE_PAG_CN == 0 & x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero") ~ COE_PAG_CN, #"ambito nazionale puro"
                              
                              COE_PAG == 0 ~ COE_PAG_CN, #"fin nullo"
                              COE_PAG < 1 ~ COE_PAG_CN, #"fin quasi nullo"
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
                              
                              COE_PAG == 0 ~ chk_coe_pag, #"fin nullo"
                              COE_PAG < 1 ~ chk_coe_pag, #"fin quasi nullo"
                              TRUE ~ 0)) %>% 
    
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2) %>% 
    # fix per anomalie floating
    mutate_if(is.numeric, round, digits=2) 
  

  
  operazioni_1420_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE),
              COE_ND = sum(COE_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2)
  
  
  operazioni_1420_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_IMP_SUD = sum(COE_IMP_SUD, na.rm = TRUE),
              COE_IMP_CN = sum(COE_IMP_CN, na.rm = TRUE),
              COE_IMP_ND = sum(COE_IMP_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_IMP_SUD + COE_IMP_CN + COE_IMP_ND,
           chk2 = COE_IMP - tot2)
  
  
  operazioni_1420_3 %>%
    group_by(CLASSE) %>% 
    summarise(COE_PAG = sum(COE_PAG, na.rm = TRUE),
              COE_PAG_SUD = sum(COE_PAG_SUD, na.rm = TRUE),
              COE_PAG_CN = sum(COE_PAG_CN, na.rm = TRUE),
              COE_PAG_ND = sum(COE_PAG_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_PAG_SUD + COE_PAG_CN + COE_PAG_ND,
           chk2 = COE_PAG - tot2)
  
  operazioni_1420_3 %>% 
    filter(abs(chk2) > 0) %>% 
    left_join(po %>% 
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA), 
              by = "OC_CODICE_PROGRAMMA") %>% 
    group_by(CLASSE, OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% 
    summarise(N =n(),
              COE = sum(COE, na.rm = TRUE),
              COE_SUD = sum(COE_SUD, na.rm = TRUE),
              COE_CN = sum(COE_CN, na.rm = TRUE),
              COE_ND = sum(COE_ND, na.rm = TRUE)) %>% 
    mutate(tot2 = COE_SUD + COE_CN + COE_ND,
           chk2 = COE - tot2)
  
  # # chk pon imprese 
  # chk <- operazioni_1420_3 %>% 
  #   filter(chk2 > 0) %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2014IT16RFOP003")
  # # 46 progetti con due assi (IV:::VI, solo l'asse VI è react)
  
  # # chk pon yei
  # chk <- operazioni_1420_3 %>% 
  #   filter(chk2 > 0) %>% 
  #   filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001")
  # # progetti con COE_CN > 0, COE_SUD = 0 e x_MACROAREA = "Mezzogiorno" o simili (vedi mail a fabio)
  
  
  chk <- operazioni_1420_3 %>% 
    filter(abs(chk2) > 0) %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN, COE_ND, chk2, tot2) %>% 
    arrange(desc(COE))
  
  operazioni_1420_3 %>% 
    semi_join(chk, by = "COD_LOCALE_PROGETTO") %>% 
    select(COD_LOCALE_PROGETTO, COE, COE_SUD, COE_CN)  %>% 
    arrange(desc(COE))
  
  
  
  operazioni_1420_4 <- workflow_macroaree_sub_pivot_evo(operazioni_1420_3) 
  
  sum(operazioni_1420$COE, na.rm = TRUE) - sum(operazioni_1420_4$COE, na.rm = TRUE)
  sum(operazioni_1420$COE_IMP, na.rm = TRUE) - sum(operazioni_1420_4$COE_IMP, na.rm = TRUE)
  sum(operazioni_1420$COE_PAG, na.rm = TRUE) - sum(operazioni_1420_4$COE_PAG, na.rm = TRUE)
  # 0
  
  operazioni_1420 %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE, na.rm = TRUE)) %>%
    full_join(operazioni_1420_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  operazioni_1420 %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE_IMP, na.rm = TRUE)) %>%
    full_join(operazioni_1420_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE_IMP, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  operazioni_1420 %>%
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(COE = sum(COE_PAG, na.rm = TRUE)) %>%
    full_join(operazioni_1420_4 %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(COE = sum(COE_PAG, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(chk = COE.x-COE.y) %>%
    filter(chk != 0)
  
  # pulisce dupli per macroaree vuote
  operazioni_1420_5 <- operazioni_1420_4 %>% 
    filter(COE != 0)
  
  
  # ----------------------------------------------------------------------------------- #
  #  operazioni 713----
  
  # chk
  # operazioni_713_raw %>% count(OC_COD_FONTE, QSN_FONDO_COMUNITARIO)
  
  message("Preparazione dati 713...")
  
  # rename
  operazioni_713_raw <- operazioni_713_raw %>%
    rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
           OC_COD_PROGRAMMA = oc_cod_programma)
  
  # DEV: qui sotto dovrebbe rimanere solo giustizia civile perché gli altri programmi si disattivano post migrazione
  
  # duplicazione di programmi PAC-FSC (es. direttrici ferroviarie)
  appo <- operazioni_713_raw %>%
    filter(OC_COD_PROGRAMMA %in% c("2007IT005FAMG1", "2007IT001FA005", "2007SA002FA016")) %>%
    filter(OC_FLAG_PAC == 1) %>%
    mutate(x_AMBITO = "PAC:::FSC") %>%
    separate_rows(x_AMBITO, sep = ":::") %>%
    # ricalcolo variabili coe (sovrascive fabio)
    fix_operazioni_713_macroaree(.) %>%
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
    mutate(oc_costo_coesione = COE,
           oc_impegni_coesione = COE_IMP,
           oc_tot_pagamenti_coesione = COE_PAG) %>% 
    # clean
    select(names(operazioni_713_raw), x_AMBITO)
  
  # fix codice programma
  appo <- appo %>% 
    mutate(OC_COD_PROGRAMMA = case_when(x_AMBITO == "PAC" & OC_COD_PROGRAMMA == "2007SA002FA016" ~ "TEMP_MIT_SAR",
                                        TRUE ~ OC_COD_PROGRAMMA))
  # TODO: rivedere codifica per questo programma CIS SSOT
  
  # integra nuove righe da sopra
  operazioni_713_raw_temp <- operazioni_713_raw %>%
    # rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>%
    mutate(x_AMBITO = NA) %>%
    anti_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    bind_rows(appo)
  
  dim(operazioni_713_raw_temp)[1] == dim(operazioni_713_raw)[1] + dim(appo)[1]/2
  
  # clean
  operazioni_713 <- operazioni_713_raw_temp %>%
    # fix per caratteri spuri
    fix_operazioni_713_macroaree(.) %>%
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
  chk <- operazioni_713 %>% 
    count(x_AMBITO, oc_descrizione_programma, 
          x_COD_LIVELLO_0, x_DES_LIVELLO_0, 
          x_COD_LIVELLO_1, x_DES_LIVELLO_1, 
          x_COD_LIVELLO_2, x_DES_LIVELLO_2)
  
  chk %>% filter(is.na(x_COD_LIVELLO_1))
  chk %>% filter(grepl(":::", x_COD_LIVELLO_1)) %>% count(x_COD_LIVELLO_1, x_COD_LIVELLO_2)
  # MEMO: non esitono ":::" per definizione PUC BDU 713
  
  # chk
  if (debug == TRUE) {
    operazioni_713 %>%
      count(x_AMBITO, oc_ambito)
  }
  
  # DEV: non esistono dati finanziari per macroarea per definizione PUC, posso usare solo localizzaioni

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
  
  
  #macroaree
  operazioni_713_1 <- get_macroarea_localizzazioni(operazioni_713, progetti)
  operazioni_713_1 %>% count(x_MACROAREA)
  
  
  # ----------------------------------------------------------------------------------- #
  # bind----
  
  message("Unione dati...")
  
  operazioni <- operazioni_1420_5 %>%
    bind_rows(operazioni_713_1) %>%
    bind_rows(operazioni_extra_5) %>%
    # intega x_vars
    left_join(po %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
              by = "OC_CODICE_PROGRAMMA")%>%
    select(-OC_MAREA)
  
  operazioni_1420_5 %>% count(x_MACROAREA)
  operazioni_713_1 %>% count(x_MACROAREA)
  operazioni_extra_5 %>% count(x_MACROAREA)
  operazioni %>% count(x_MACROAREA)
  
  
  sum(operazioni_1420_5$COE, na.rm=TRUE) - sum(operazioni_1420$COE, na.rm=TRUE)
  sum(operazioni_713_1$COE, na.rm=TRUE) - sum(operazioni_713$COE, na.rm=TRUE)
  sum(operazioni_extra_5$COE, na.rm=TRUE) - sum(operazioni_extra$COE, na.rm=TRUE)
  
  
  # ----------------------------------------------------------------------------------- #
  # fix ciclo psc----
  
  message("Fix ciclo progetti PSC...")
  

  # chk ciclo vuoto
  # operazioni_1420%>% 
  #   semi_join(po %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0), by = "OC_CODICE_PROGRAMMA") %>% 
  #   left_join(progetti %>%
  #               select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_CICLO, OC_COD_CICLO), 
  #             by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
  #   mutate(x_CICLO = case_when(OC_COD_CICLO == 1 ~ "2007-2013",
  #                              OC_COD_CICLO == 2 ~ "2014-2020",
  #                              OC_COD_CICLO == 3 ~ "2021-2027",
  #                              OC_COD_CICLO == 9 ~ "2000-2006",
  #                              TRUE ~ "")) %>% 
  #   count(X_CICLO, OC_COD_CICLO, x_CICLO)
  # X_CICLO   OC_COD_CICLO x_CICLO         n
  # <chr>            <dbl> <chr>       <int>
  # 1 2014-2020            1 "2007-2013" 21223
  # 2 2014-2020            2 "2014-2020" 43232
  # 3 2014-2020            9 "2000-2006" 14715
  # 4 NA                  NA ""            547

  operazioni_psc <- operazioni_1420 %>% 
    semi_join(po %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0),
              by = "OC_CODICE_PROGRAMMA") %>% 
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_CICLO, OC_COD_CICLO), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(x_CICLO = case_when(OC_COD_CICLO == 1 ~ "2007-2013",
                               OC_COD_CICLO == 2 ~ "2014-2020",
                               OC_COD_CICLO == 3 ~ "2021-2027",
                               OC_COD_CICLO == 9 ~ "2000-2006",
                               TRUE ~ "2014-2020")) # MEMO: fix per anomalie di pre-esteso
  
  # operazioni_psc %>% filter(is.na(OC_COD_CICLO)) %>% count(OC_CODICE_PROGRAMMA)
  # OC_CODICE_PROGRAMMA     n
  # <chr>               <int>
  # 1 PSCCALABRIA             1
  # 2 PSCCAMPANIA           263
  # 3 PSCCIMETFIRENZE         1
  # 4 PSCEMILROMAGNA          2
  # 5 PSCLAZIO                1
  # 6 PSCLIGURIA              1
  # 7 PSCMOLISE              10
  # 8 PSCPUGLIA              16
  # 9 PSCSICILIA             72
  # 10 PSCSVILECONOM         178
  # 11 PSCUNIVRICERCA          2
  
  # fix ciclo per psc (che da get_x_vars sarebbe 1420)
  operazioni_1 <- operazioni %>%
    left_join(operazioni_psc %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>% 
    mutate(x_CICLO = if_else(is.na(x_CICLO.y), x_CICLO.x, x_CICLO.y)) %>% 
    select(-x_CICLO.x, -x_CICLO.y)
 
  
  dim(operazioni)[1] == dim(operazioni_1)[1]
  
  sum(operazioni_1$COE, na.rm=TRUE) - sum(operazioni_1420_5$COE, na.rm=TRUE) -sum(operazioni_713_1$COE, na.rm=TRUE)
  sum(operazioni_1$COE, na.rm=TRUE) - sum(operazioni$COE, na.rm=TRUE)

  
  
  # ----------------------------------------------------------------------------------- #
  # chk vari----
  
  # chk ciclo
  operazioni_1 %>% count(x_CICLO)
  operazioni_1 %>% filter(is.na(x_CICLO)) %>% count(x_AMBITO, OC_CODICE_PROGRAMMA)
  
  
  # ----------------------------------------------------------------------------------- #
  # debug----
  
  if (debug == TRUE) {
    
    # DO SOMETHING
    
  }
  
  
  # ----------------------------------------------------------------------------------- #
  # export----
  
  message("Esce da workflow")
  
  out <- operazioni_1
  return(out)
}


#' Apre operazioni sie per macroarea
#'
#' Apre operazioni per macroarea, per progetti sie non react e non yei, all'interno del flusso di workflow_macroaree(), in setup_macroaree().
#' Per react e yei usa macroarea da localizzazioni.
#'
#' @param operazioni Dataset di classe operazioni interno al flusso di workflow_macroaree().
#' @param riprop_yei Vuoi riproporzionare yei dove mancano dati finanziari per livelli gerarchici? Default su FALSE.
#' @param debug vuoi salvare il file?
#' @return Dataset macroaree psc per il workflow.
workflow_macroaree_sub_sie <- function(operazioni, riprop_yei=FALSE, debug=FALSE) {
  
  # DEBUG:
  # operazioni <- operazioni_1420_2
  # riprop_yei = FALSE
  # debug = FALSE
  
  po_react <- c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05SFOP001", "2014IT05SFOP001",  "2014IT05SFOP002", 
                "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003")
  po_yei <- "2014IT05M9OP001"
  
  appo <- operazioni %>% 
    filter(x_AMBITO %in% c("FSE", "FESR", "YEI"))
  
  # chk
  # appo %>% filter(x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero")) %>% summarise(COE = sum(COE))
  
  # isola progetti sie senza dati finanziari da livelli gerarchici
  react <- appo %>% 
    # isola programmi potenzialmente interessati
    # OLD:
    # filter(OC_CODICE_PROGRAMMA %in% c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05M9OP001", "2014IT05SFOP001", "2014IT05SFOP001",
    #                                   "2014IT05SFOP002", "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003"))  %>%
    filter(OC_CODICE_PROGRAMMA %in% po_react |
             OC_CODICE_PROGRAMMA ==  po_yei) %>%
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot) %>% 
    # isola progetti degli assi senza dati finanziari da livelli gerarchici
    filter(is.na(COE_SUD) & is.na(COE_CN) | COE_SUD == 0 & COE_CN == 0 | abs(chk)>1)
    # filter(!(is.na(COE_SUD) & is.na(COE_CN)) & !(COE_SUD == 0 & COE_CN == 0) & abs(chk)>1)
    # MEMO: ci sono progetti chedistribuiti su più livelli gerarchici, in parte con macroaree e in parte no (da YEI 2014IT05M9OP001 ma anche da PON Imprese 2014IT16RFOP003)
  
  # chk
  # react %>% filter(x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero")) %>% summarise(COE = sum(COE))
  
  # mapping 
  # sovrascrive dati finanziari da livelli gerarchici usando macroaree da localizzazioni
  # con riproporzione per yei
  if (riprop_yei == TRUE) {
    react1 <- react  %>% 
      mutate(COE_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE,
                                 x_MACROAREA == "Ambito nazionale" & OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE * 0.4903,
                                 TRUE ~ 0),
             COE_IMP_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE_IMP,
                                     x_MACROAREA == "Ambito nazionale" & OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE_IMP * 0.4903,
                                     TRUE ~ 0),
             COE_PAG_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE_PAG, 
                                     x_MACROAREA == "Ambito nazionale" &  OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE_PAG * 0.4903,
                                     TRUE ~ 0),
             COE_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE, 
                                x_MACROAREA == "Ambito nazionale" & OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE * 0.5097,
                                TRUE ~ 0),
             COE_IMP_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE_IMP, 
                                    x_MACROAREA == "Ambito nazionale" & OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE_IMP * 0.5097,
                                    TRUE ~ 0),
             COE_PAG_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE_PAG, 
                                    x_MACROAREA == "Ambito nazionale" & OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & x_AMBITO == "YEI" ~ COE_PAG * 0.5097,
                                    TRUE ~ 0))
  } else {
    react1 <- react %>% 
      mutate(COE_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE,
                                 TRUE ~ 0),
             COE_IMP_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE_IMP,
                                     TRUE ~ 0),
             COE_PAG_SUD = case_when(x_MACROAREA == "Mezzogiorno" ~ COE_PAG, 
                                     TRUE ~ 0),
             COE_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE, 
                                TRUE ~ 0),
             COE_IMP_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE_IMP, 
                                    TRUE ~ 0),
             COE_PAG_CN = case_when(x_MACROAREA == "Centro-Nord" ~ COE_PAG, 
                                    TRUE ~ 0))
  }
  
  # chk
  # react %>% filter(x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero")) %>% summarise(COE = sum(COE))
  # react1 %>% filter(x_MACROAREA %in% c("Ambito nazionale", "Trasversale", "Estero")) %>% summarise(COE = sum(COE))
  
  # chk prima del mapping
  react %>%
    filter(x_MACROAREA == "Ambito nazionale") %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA), 
              by = "OC_CODICE_PROGRAMMA") %>%
    group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO)  %>%
    summarise(COE = sum(COE, na.rm = T),
              COE_SUD = sum(COE_SUD, na.rm = T),
              COE_CN = sum(COE_CN, na.rm = T))  %>%
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  # chk dopo il mapping
  react1 %>%
    filter(x_MACROAREA == "Ambito nazionale") %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA), 
              by = "OC_CODICE_PROGRAMMA") %>%
    group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO)  %>%
    summarise(COE = sum(COE, na.rm = T),
              COE_SUD = sum(COE_SUD, na.rm = T),
              COE_CN = sum(COE_CN, na.rm = T))  %>%
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  # MEMO: tot deve valere 0
  
  # CMQ YEI VA TRATTATO A PARTE
  
  appo1 <- appo %>% 
    anti_join(react1, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
    bind_rows(react1)
  
  # appo1 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm=TRUE))
  # appo1 %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% group_by(x_MACROAREA) %>% summarise(COE = sum(COE, na.rm=TRUE))

  dim(appo1)[1] == dim(appo)[1]
  sum(appo1$COE, na.rm = TRUE) - sum(appo$COE, na.rm = TRUE)
  
  # pivot macroaree
  pivo <- workflow_macroaree_sub_pivot(appo1)
  # MEMO: qui passo tutti i progetti, ma i casi in "react" escono con COE pari a 0 (li recupero in setup_macroaree())
  
  # pivo %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm=TRUE))
  # pivo %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% group_by(x_MACROAREA) %>% summarise(COE = sum(COE, na.rm=TRUE))
  
  out <- pivo %>% 
    left_join(appo1 %>% 
                select(-COE, -COE_IMP, -COE_PAG, -x_MACROAREA, 
                       -costo_ammesso_MZ, -costo_ammesso_CN, -imp_ammesso_MZ, -imp_ammesso_CN, -pag_ammesso_MZ, -pag_ammesso_CN), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) 
  
  # out %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% summarise(COE = sum(COE, na.rm=TRUE))
  # out %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% group_by(x_MACROAREA) %>% summarise(COE = sum(COE, na.rm=TRUE))
  
  nrow(pivo) - nrow(out)
  sum(pivo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  sum(appo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  # CHK:
  # 0.08999634
  
  # pulisce dupli per macroaree vuote
  out <- out %>% 
    filter(COE != 0)
  
  if (debug == TRUE) {
    write_csv2(out, file.path(TEMP, "operazioni_macroaree_sie.csv"))
  }
  
  return(out)
}



#' Apre operazioni psc per macroarea
#'
#' Apre operazioni per macroarea, per programmi psc, all'interno del flusso di workflow_macroaree(), in setup_macroaree().
#'
#' @param operazioni Dataset di classe operazioni interno al flusso di workflow_macroaree().
#' @param debug vuoi salvare il file?
#' @return Dataset macroaree psc per il workflow.
workflow_macroaree_sub_psc <- function(operazioni, debug=FALSE) {
  
  # DEBUG:
  # operazioni <- operazioni_1420_2
  
  psc <- octk::po_riclass %>% filter(x_CICLO == "2014-2020", x_GRUPPO == "PSC", TIPO == 0)
  
  appo <- operazioni %>% 
    semi_join(psc, by = "OC_CODICE_PROGRAMMA")
  
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
  # 58453354286. 36363640886. 43686580830. 14766773456. 25980770057. 10382870828. 0.00000191 -0.00000191
  
  pivo <- workflow_macroaree_sub_pivot(appo)
  
  out <- pivo %>% 
    # select(-QUOTA_SUD) %>% 
    left_join(appo %>% 
                select(-COE, -COE_IMP, -COE_PAG, -x_MACROAREA), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) 
  
  nrow(pivo) - nrow(out)
  sum(pivo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  sum(appo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  sum(appo$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  # CHK:
  # 0
  
  # pulisce dupli per macroaree vuote
  out <- out %>%
    filter(COE != 0)
  # MEMO: vanno escluse per non duplicare le altre variabili
  
  if (debug == TRUE) {
    write_csv2(out, file.path(TEMP, "operazioni_macroaree_psc.csv"))
  }
  
  return(out)
}


#' Workflow per creazione file di base per macroaree
#'
#' Workflow per creazione file di base per macroaree.
#' Funziona con input specifici per sie non react e per psc migrati.
#'
#' @param perimetro Dataset di classe operazioni
#' @return Dataset pivot con macroaree per setup_macroaree_sie e setup_macroaree_psc
workflow_macroaree_sub_pivot <- function(operazioni) {
  
  # DEBUG:
  # operazioni <- appo1
  
  operazioni_1420 <- operazioni
  
  # chk <- operazioni_1420_raw %>%
  #   anti_join(react, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
  #   # filter(!(OC_CODICE_PROGRAMMA %in% c("2014IT05M2OP001", "2014IT05M2OP002", "2014IT05M9OP001", "2014IT05SFOP001", "2014IT05SFOP001",
  #   #                                   "2014IT05SFOP002", "2014IT16M2OP003", "2014IT16M2OP004", "2014IT16M2OP005", "2014IT16RFOP003"))) %>%
  #   
  #   mutate_if(is.numeric, replace_na, replace = 0) %>%
  #   # filter(is.na(COE_SUD) & is.na(COE_CN) | COE_SUD == 0 & COE_CN == 0)
  #   mutate(tot = COE_SUD + COE_CN,
  #          chk = COE - tot) %>%
  #   filter(chk != 0) %>% 
  #   select(1:28, "chk")
  # 
  # write.xlsx(chk, file.path(TEMP, "test.xlsx"))
  
  # chk %>%
  #   left_join(octk::po_riclass %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA)) %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
  #   summarise(COE = sum(COE, na.rm = T),
  #             COE_SUD = sum(COE_SUD, na.rm = T),
  #             COE_CN = sum(COE_CN, na.rm = T))  %>%
  #   mutate(tot = COE_SUD + COE_CN,
  #          chk = COE - tot)
  # # OC_CODICE_PROGRAMMA x_PROGRAMMA           COE COE_SUD COE_CN   tot   chk
  # # <chr>               <chr>               <dbl>   <dbl>  <dbl> <dbl> <dbl>
  # # 2014IT16M2OP002     POR PUGLIA FESR-FSE     0       0      0     0     0 #MEMO: contiene NA su COE
  
  appo_costo <- operazioni_1420  %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_SUD,
           COE_CN) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_SUD + COE_CN,
           chk = COE - tot)
  
  appo_costo %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  # chk 
  # chk <- appo_costo %>% 
  #   filter(is.na(COE_SUD) & is.na(COE_CN))
  # chk <- appo_costo %>%
  #   filter(chk != 0 | is.na(chk)) %>%
  #   filter(COE > 0) 
  # chk %>%
  #   left_join(octk::po_riclass %>% 
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA)) %>% 
  #   group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_costo <- appo_costo %>% 
    select(-COE, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_SUD, COE_CN), names_to = "x_MACROAREA", values_to = "COE") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_CN" ~ "Centro-Nord",
                                   TRUE ~ "CHK"))
  
  
  sum(appo_costo$COE, na.rm = TRUE) - sum(operazioni_1420$COE, na.rm = TRUE)
  sum(pivo_costo$COE, na.rm = TRUE) - sum(operazioni_1420$COE, na.rm = TRUE)
  
  appo_imp <- operazioni_1420 %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_IMP_SUD + COE_IMP_CN,
           chk = COE_IMP - tot)
  
  appo_imp %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  pivo_imp <- appo_imp %>% 
    select(-COE_IMP, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_IMP_SUD, COE_IMP_CN), names_to = "x_MACROAREA", values_to = "COE_IMP") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_IMP_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_IMP_CN" ~ "Centro-Nord",
                                   TRUE ~ "CHK"))
  
  appo_pag <- operazioni_1420 %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_PAG_SUD + COE_PAG_CN,
           chk = COE_PAG - tot)
  
  appo_pag %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_pag <- appo_pag %>% 
    select(-COE_PAG, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_PAG_SUD, COE_PAG_CN), names_to = "x_MACROAREA", values_to = "COE_PAG") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_PAG_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_PAG_CN" ~ "Centro-Nord",
                                   TRUE ~ "CHK"))
  
  pivo <- pivo_costo %>% 
    full_join(pivo_imp, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_MACROAREA")) %>% 
    full_join(pivo_pag, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_MACROAREA"))
  
  # chk
  sum(pivo$COE, na.rm = TRUE) - (sum(appo_costo$COE_SUD, na.rm = TRUE) + sum(appo_costo$COE_CN, na.rm = TRUE))
  sum(pivo$COE_IMP, na.rm = TRUE) - (sum(appo_imp$COE_IMP_SUD, na.rm = TRUE) + sum(appo_imp$COE_IMP_CN, na.rm = TRUE))
  sum(pivo$COE_PAG, na.rm = TRUE) - (sum(appo_pag$COE_PAG_SUD, na.rm = TRUE) + sum(appo_pag$COE_PAG_CN, na.rm = TRUE))
  
  return(pivo)
  
}

#' Workflow per creazione file di base per macroaree
#'
#' Workflow per creazione file di base per macroaree.
#' Funziona con input specifici per sie non react e per psc migrati.
#'
#' @param perimetro Dataset di classe operazioni
#' @return Dataset pivot con macroaree per setup_macroaree_sie e setup_macroaree_psc
workflow_macroaree_sub_pivot_evo <- function(operazioni) {
  
  # DEBUG:
  # operazioni <- appo1
  
  operazioni_1420 <- operazioni
  
  appo_costo <- operazioni_1420  %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE,
           COE_SUD,
           COE_CN,
           COE_ND) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_SUD + COE_CN + COE_ND,
           chk = COE - tot)
  
  appo_costo %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  # chk 
  # chk <- appo_costo %>% 
  #   filter(is.na(COE_SUD) & is.na(COE_CN))
  # chk <- appo_costo %>%
  #   filter(chk != 0 | is.na(chk)) %>%
  #   filter(COE > 0) 
  # chk %>%
  #   left_join(octk::po_riclass %>% 
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA)) %>% 
  #   group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_costo <- appo_costo %>% 
    select(-COE, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_SUD, COE_CN, COE_ND), names_to = "x_MACROAREA", values_to = "COE") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_CN" ~ "Centro-Nord",
                                   x_MACROAREA == "COE_ND" ~ "Ambito nazionale",
                                   TRUE ~ "CHK"))
  
  
  sum(appo_costo$COE, na.rm = TRUE) - sum(operazioni_1420$COE, na.rm = TRUE)
  sum(pivo_costo$COE, na.rm = TRUE) - sum(operazioni_1420$COE, na.rm = TRUE)
  
  appo_imp <- operazioni_1420 %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE_IMP,
           COE_IMP_SUD,
           COE_IMP_CN,
           COE_IMP_ND) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_IMP_SUD + COE_IMP_CN + COE_IMP_ND,
           chk = COE_IMP - tot)
  
  appo_imp %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  pivo_imp <- appo_imp %>% 
    select(-COE_IMP, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_IMP_SUD, COE_IMP_CN, COE_IMP_ND), names_to = "x_MACROAREA", values_to = "COE_IMP") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_IMP_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_IMP_CN" ~ "Centro-Nord",
                                   x_MACROAREA == "COE_IMP_ND" ~ "Ambito nazionale",
                                   TRUE ~ "CHK"))
  
  appo_pag <- operazioni_1420 %>% 
    select(COD_LOCALE_PROGETTO, 
           OC_CODICE_PROGRAMMA,
           x_AMBITO,
           COE_PAG,
           COE_PAG_SUD,
           COE_PAG_CN,
           COE_PAG_ND) %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(tot = COE_PAG_SUD + COE_PAG_CN + COE_PAG_ND,
           chk = COE_PAG - tot)
  
  appo_pag %>%
    group_by(x_AMBITO) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)
  
  pivo_pag <- appo_pag %>% 
    select(-COE_PAG, -tot, -chk) %>% 
    pivot_longer(cols = c(COE_PAG_SUD, COE_PAG_CN, COE_PAG_ND), names_to = "x_MACROAREA", values_to = "COE_PAG") %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "COE_PAG_SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "COE_PAG_CN" ~ "Centro-Nord",
                                   x_MACROAREA == "COE_PAG_ND" ~ "Ambito nazionale",
                                   TRUE ~ "CHK"))
  
  pivo <- pivo_costo %>% 
    full_join(pivo_imp, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_MACROAREA")) %>% 
    full_join(pivo_pag, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_MACROAREA"))
  
  # chk
  sum(pivo$COE, na.rm = TRUE) - (sum(appo_costo$COE_SUD, na.rm = TRUE) + sum(appo_costo$COE_CN, na.rm = TRUE) + sum(appo_costo$COE_ND, na.rm = TRUE))
  sum(pivo$COE_IMP, na.rm = TRUE) - (sum(appo_imp$COE_IMP_SUD, na.rm = TRUE) + sum(appo_imp$COE_IMP_CN, na.rm = TRUE) + sum(appo_imp$COE_IMP_ND, na.rm = TRUE))
  sum(pivo$COE_PAG, na.rm = TRUE) - (sum(appo_pag$COE_PAG_SUD, na.rm = TRUE) + sum(appo_pag$COE_PAG_CN, na.rm = TRUE) + sum(appo_pag$COE_PAG_ND, na.rm = TRUE))
  
  return(pivo)
  
}
#' Carica file "macroaree"
#'
#' Carica il file di base di classe "macroaree", proveniente da setup_macroaree() e salvato in DATA.
#'
#' @param bimestre Bimestre di riferimento.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
load_macroaree <- function(bimestre, visualizzati=TRUE, DATA) {
  
  col_types <- cols(
    COD_LOCALE_PROGETTO = col_character(),
    OC_CODICE_PROGRAMMA = col_character(),
    x_AMBITO = col_character(),
    x_MACROAREA = col_character(),
    COE = col_double(),
    COE_IMP = col_double(),
    COE_PAG = col_double(),
    x_COD_LIVELLO_0 = col_character(),
    x_DES_LIVELLO_0 = col_character(),
    x_COD_LIVELLO_1 = col_character(),
    x_DES_LIVELLO_1 = col_character(),
    x_COD_LIVELLO_2 = col_character(),
    x_DES_LIVELLO_2 = col_character(),
    costo_ammesso_MZ = col_double(),
    costo_ammesso_CN = col_double(),
    imp_ammesso_MZ = col_double(),
    imp_ammesso_CN = col_double(),
    pag_ammesso_MZ = col_double(),
    pag_ammesso_CN = col_double(),
    OC_MACROAREA = col_character(),
    x_GRUPPO = col_character(),
    x_PROGRAMMA = col_character(),
    x_REGNAZ = col_character(),
    x_CICLO = col_character(),
    COD_REGIONE = col_character(),
    DEN_REGIONE = col_character(),
    COD_PROVINCIA = col_character(),
    x_REGIONE = col_character(),
    CP = col_double(),
    IMP = col_double(),
    PAG = col_double(),
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
  # perimetro <- read_csv2(file.path(DATA, paste0("macroaree_light_", bimestre, ".csv")), guess_max = 1000000)
  perimetro <- read_csv2(file.path(DATA, paste0("macroaree_light_", bimestre, ".csv")), col_types = col_types)
  
  
  # visualizzati
  if (visualizzati == TRUE) {
    perimetro <- perimetro %>%
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9 | OC_FLAG_VISUALIZZAZIONE == 10) # include progetti FEASR per SNAI
  }
  
  # simply
  perimetro <- get_simply_non_loc(perimetro)
  
  # refactor
  perimetro <- refactor_ambito(perimetro)
  perimetro <- refactor_ciclo(perimetro)
  
  return(perimetro)
}


#' Crea il dataset operazioni
#'
#' Crea il dataset operazioni a partire dal dataset macroaree_light.
#'
#' @param bimestre Bimestre di riferimento
#' @param macroaree Dataset di tipo "macroaree".
#' @param export Vuoi esportare in DATA?
#' @return Il dataset operazioni con le variabili coesione calcolate: COE, COE_IMP e COE_PAG.
#' @note La modalità **export** esporta operazioni_light.csv in DATA.
setup_operazioni_macroaree <- function(bimestre, macroaree, export=FALSE) {
  
  appo <- macroaree %>% 
    group_by(COD_LOCALE_PROGETTO, CUP, OC_TITOLO_PROGETTO,
             OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA,
             x_COD_LIVELLO_0, x_DES_LIVELLO_0, x_COD_LIVELLO_1, x_DES_LIVELLO_1, x_COD_LIVELLO_2, x_DES_LIVELLO_2,
             COD_SEZIONE, DES_SEZIONE, OC_COD_ARTICOLAZ_PROGRAMMA, OC_DESCR_ARTICOLAZ_PROGRAMMA, OC_COD_SUBARTICOLAZ_PROGRAMMA, OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             x_REGNAZ, x_REGIONE,
             OC_FLAG_VISUALIZZAZIONE) %>% 
    summarise(COE = sum(COE, na.rm=TRUE),
              COE_IMP = sum(COE_IMP, na.rm=TRUE),
              COE_PAG = sum(COE_PAG, na.rm=TRUE))
  
  write.csv2(appo, file.path(DATA, paste0("operazioni_macroaree_light_", bimestre, ".csv")), row.names = FALSE)
  
}


#' Carica file operazioni macroaree
#'
#' Carica file operazioni sintetizzato dal file macroaree
#'
#' @param bimestre Bimestre di riferimento.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
load_operazioni_macroaree <- function(bimestre, visualizzati=TRUE, DATA) {
  
  col_types <- cols(
    
  )
  
  # loads
  perimetro <- read_csv2(file.path(DATA, paste0("operazioni_macroaree_light_", bimestre, ".csv")), col_types = col_types)

  # viz
  if (visualizzati == TRUE) {
    perimetro <- perimetro %>%
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9 | OC_FLAG_VISUALIZZAZIONE == 10) # include progetti FEASR per SNAI
  }
  
  # simply
  # perimetro <- get_simply_non_loc(perimetro)
  
  # refactor
  perimetro <- refactor_ambito(perimetro)
  perimetro <- refactor_ciclo(perimetro)
  
  return(perimetro)
}
