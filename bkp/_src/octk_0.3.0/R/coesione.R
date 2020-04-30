# funzioni del blocco "coesione"


#' Crea il dataset operazioni
#'
#' Crea il dataset operazioni a partire da sottoprodotti del workflow SAS.
#'
#' @param bimestre Bimestre di riferimento
#' @param progetti Dataset con un perimetro in formato "progetti".
#' @return Il dataset operazioni con le variabili coesione calcolate: COE, COE_IMP e COE_PAG.
#' @note La modalità **debug** esporta diversi csv in TEMP La modalità **export** esporta operazioni_light.csv in DATA.
setup_operazioni <- function(bimestre, progetti=NULL, export=FALSE, debug=FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {

    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
    }

    operazioni <- workflow_operazioni(bimestre, progetti, debug=debug)

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
      write.csv2(operazioni_light, file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)
      # write.csv2(operazioni_light, file.path(TEMP, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)

    } else {
      return(operazioni_light)
    }

  } else {
    message("Non hai definito il folder DATA. Carica 'oc' ed inizializza 'oc_init()'.")
  }
}


#' Workflow per creare il dataset operazioni
workflow_operazioni <- function(bimestre, progetti=NULL, debug=FALSE, use_fix=FALSE) {

  # MEMO:
  # i programmi con ciclo sbagliato non sono ancora spostati nei file operazioni.sas

  # ----------------------------------------------------------------------------------- #
  # loads

  po <- octk::po_riclass

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }
  if (use_fix == TRUE) {
    progetti <- fix_progetti(progetti)
  }

  # recupera finanziamenti lordi (se assente in df)
  if (!(any(names(progetti) == "FINANZ_TOTALE_PUBBLICO"))) {

    temp <- paste0("progetti_esteso_", bimestre, ".csv")
    progetti_ext <- read_csv2(file.path(DATA, temp), guess_max = 1000000)

    progetti <- progetti %>%
      left_join(progetti_ext %>%
                  select(COD_LOCALE_PROGETTO, FINANZ_TOTALE_PUBBLICO, FINANZ_STATO_FSC, FINANZ_STATO_PAC),
                by = "COD_LOCALE_PROGETTO")

    rm(progetti_ext)
  }

  operazioni_1420_raw <- read_sas(file.path(DATA, "operazioni_pucok.sas7bdat"))
  operazioni_713_raw <- read_sas(file.path(DATA, "operazioni_fltok.sas7bdat"))


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
    mutate(COS_AMM = case_when(x_AMBITO == "FSC" ~ costo_ammesso_FSC,
                               x_AMBITO == "POC" ~ costo_ammesso_NAZ,
                               x_AMBITO == "SNAI" ~ costo_ammesso_NAZ,
                               TRUE ~ costo_rendicontabile_UE),
           IMP_AMM = case_when(x_AMBITO == "FSC" ~ oc_impegni_ammessi_FSC,
                               x_AMBITO == "POC" ~ oc_impegni_ammessi_PAC,
                               x_AMBITO == "SNAI" ~ oc_impegni_ammessi_PAC,
                               TRUE ~ oc_impegni_ammessi_ue),
           PAG_AMM = case_when(x_AMBITO == "FSC" ~ oc_tot_pagamenti_FSC,
                               x_AMBITO == "POC" ~ oc_tot_pagamenti_PAC,
                               x_AMBITO == "SNAI" ~ oc_tot_pagamenti_PAC,
                               TRUE ~ oc_tot_pagamenti_rendicontab_ue)) %>%
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
                       IMPEGNI),
              by = "COD_LOCALE_PROGETTO") %>%
      mutate(COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                             x_AMBITO == "POC" ~ OC_FINANZ_STATO_PAC_NETTO,
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
                                  TRUE ~ COS_AMM), # per FS non posso considerare valore al netto di economie
             x = base_coe/base_ftp,
             COE_PAG = PAG_AMM,
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
             base_ftp, # DEBUG
             base_coe, # DEBUG
             x, # DEBUG
             COE_IMP,
             COE_PAG)

    appo %>%
      filter(x > 1) %>%
      arrange(desc(x))

    appo %>%
      filter(round(COE_PAG, 0) > round(COE_IMP, 0)) %>%
      count(x_AMBITO)
    # MEMO: prima del fix...
    # x_AMBITO     n
    #   <chr>    <int>
    # 1 FESR     28795
    # 2 FSC       8719
    # 3 FSE      83456
    # 4 POC       3146
    # MEMO: dopo il fix

    chk <- appo %>%
      select(-x) %>%
      group_by(x_AMBITO, OC_CODICE_PROGRAMMA) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      left_join(octk::po_riclass %>%
                  distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
                by = "OC_CODICE_PROGRAMMA")

    write_csv2(chk, file.path(TEMP, "chk_0713.csv"))

    chk2 <- chk %>%
      group_by(x_AMBITO) %>%
      summarise_if(is.numeric, sum, na.rm = TRUE)

    write_csv2(chk2, file.path(TEMP, "chk_0713_sum.csv"))

    # chk totali per ambito
    # operazioni_713 %>%
    #   group_by(x_AMBITO) %>%
    #   summarise(N = n(),
    #             COE = sum(COE, na.rm = TRUE),
    #             COE_IMP = sum(COE_IMP, na.rm = TRUE),
    #             COE_PAG = sum(COE_PAG, na.rm = TRUE))

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
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ)) %>%
    # isola visualizzati
    # MEMO: questo sotto non prende ":::" su OC_CODICE_PROGRAMMA
    # semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA"))
    semi_join(progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 0), by = "COD_LOCALE_PROGETTO")

  # chk compatibile con Fabio x Stefano
  operazioni %>% distinct(COD_LOCALE_PROGETTO, x_CICLO, x_AMBITO) %>% count(x_CICLO, x_AMBITO)

  # chk dupli per ciclo da po_riclass
  chk <- operazioni %>% count(COD_LOCALE_PROGETTO, x_CICLO)
  dim(chk)[1] == dim(progetti)[1]

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


#' Prepara perimetro operazioni con dati coesione
#'
#' Prepara perimetro operazioni con dati coesione
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
prep_perimetro_bimestre_coesione <- function(bimestre, usa_meuro=TRUE) {

  # loads
  # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  perimetro <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)

  # fix per dissesto
  # TODO: da spostare a monte nel workflow di operazioni
  perimetro <- perimetro %>%
    mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016ABAMPSAP01" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016EMAMPSAP02" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LIAMPSAP03" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016LOAMPSAP06" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016SAAMPSAP04" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016TOAMPSAP05" ~ "2016XXAMPSAP00",
                                           OC_CODICE_PROGRAMMA == "2016VEAMPSAP07" ~ "2016XXAMPSAP00",
                                           TRUE ~ OC_CODICE_PROGRAMMA))

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

  return(perimetro)
}


#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param add_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_coesione <- function(perimetro, usa_meuro=FALSE, add_713=FALSE,
                                           add_totali=FALSE, focus="report", export=FALSE) {

  temp <- paste0(focus, "_programmi.csv")

  # DEBUG: add_713 <- TRUE
  programmi <- init_programmazione(usa_temi=FALSE, add_713=add_713, export=FALSE)

  perimetro <- perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))

  if (usa_meuro == TRUE) {
    programmi <- programmi %>%
      mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  }

  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")

  spalla <- octk::po_riclass %>%
    filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
           x_CICLO != "2000-2006",
           x_AMBITO != "FEASR") %>%
    filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>%
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
    separate_rows(x_AMBITO, sep="-") %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    left_join(programmi %>%
                # MEMO: patch per factor di x_AMBITO e x_CICLO
                mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                              "FEAD", "FAMI", "CTE")),
                       x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))

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
                #                                               "FEAD", "FAMI", "CTE")),
                #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, COE, COE_IMP, COE_PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)


  if (add_totali == TRUE) {
    progetti <- load_progetti(bimestre, light = TRUE, refactor = TRUE) %>%
      select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,
             CP = OC_FINANZ_TOT_PUB_NETTO,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)

    if (usa_meuro == TRUE) {
      progetti <- progetti %>%
        mutate(CP = CP / 1000000,
               IMP = IMP / 1000000,
               PAG = PAG / 1000000)
    }

    out <- out %>%
      left_join(progetti %>%
                  group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                  summarise(N_CLP =n(), # MEMO: questa serve per contare il numero netto di progetti
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE)),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>% # MEMO: qui perdo multi ":::" in progetti
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))

    # recupero multi
    appo <- progetti %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
      summarise(N_CLP =n(), # MEMO: questa serve per contare il numero netto di progetti
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      anti_join(out,
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
      # riempie NA con 0
      # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # recupera x_vars
      left_join(octk::po_riclass %>%
                  # MEMO: patch per factor di x_AMBITO e x_CICLO
                  mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                                "FEAD", "FAMI", "CTE")),
                         x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
                  select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO"))

    # bind di multi
    out <- out %>%
      bind_rows(appo) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))



  }

  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
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
add_delta_programmi_coesione <- function(bimestre, programmi, last_bimestre, last_data_path,
                                         usa_meuro=FALSE, focus="delta", export=FALSE) {

  # bimestre precedente
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }

  # loads
  last_perimetro <- read_csv2(file.path(TEMP, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)

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
             IMP_COE = IMP_COE / 1000000,
             PAG_COE = PAG_COE / 1000000)
  }

  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)

  appo <- last_perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))

  # report
  out <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG_COE, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (PAG_COE - PAG_LAST) / PAG_LAST) %>%
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
                                      oc_cod_fonte == "FS1420" & ue_descr_fondo == "Y.E.I." ~ "IOG",
                                      TRUE ~ ue_descr_fondo))

  return(df)
}


#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione <- function(programmi=NULL, export=TRUE) {

  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre

  report <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
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

  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }

  return(report)
}

