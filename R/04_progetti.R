# OC > Progetti

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


#' Carica il dataset progetti
#'
#' Carica il file progetti_esteso_$BIMESTRE.csv dal folder DATI.
#'
#' @param bimestre Stringa in formato "20180630" come da standard per le date in OC.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param debug Logico. Vuoi vedere i totali di progetti e costo pubblico per controllo sul portale OC?
#' @param light Logico. Vuoi usare la versione light di "progetti.csv"?
#' @return Il dataset viene caricato come "progetti" nel Global Environment. Se "progetti" è gia presente compare una notifica.
load_progetti <- function(bimestre, data_path=NULL, visualizzati=TRUE, debug=FALSE, light=FALSE, refactor=FALSE)
{
  # if (exists("progetti", envir = .GlobalEnv)) {
  #   print("Progetti esteso è gia caricato")
  #   progetti <- progetti
  #
  # } else {

    # switch di filename per progetti_light
    if (light == TRUE) {
      temp <- paste0("progetti_light_", bimestre, ".csv")

    } else {
      if (as.numeric(bimestre) <= 20181231) {
        temp <- paste0("progetti_esteso_", bimestre, ".csv")

      } else {
        temp <- "PROGETTI_PREESTESO.csv"
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
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000) %>%
        filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9 | OC_FLAG_VISUALIZZAZIONE == 10) # include progetti FEASR per SNAI e i progetti visualizzati ma accorpati codice 10
      # CHK: progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 9) %>% count(X_AMBITO)
    } else {
      # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000)
      # progetti <- read_csv2(file.path(DATA, temp), col_types = col_types)
      
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





#' Refactor per perimetro di progetti
#'
#' Integra un perimetro di progetti appena caricato con i factor per x_MACROAREA, x_CICLO, x_AMBITO e OC_STATO_PROCEDURALE.
#'
#' @param perimetro Dataset in formato standard.
#' @return Il dataset integrato.
refactor_progetti <- function(perimetro) {

  perimetro <- perimetro %>%
    mutate(# x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
           # x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
           #                                        "FEAD", "FAMI", "CTE")),
           # x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")),
           OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE, levels = c("Non avviato",
                                                                          "In avvio di progettazione",
                                                                          "In corso di progettazione",
                                                                          "In affidamento",
                                                                          "In esecuzione",
                                                                          "Eseguito",
                                                                          "Non determinabile"))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea(.)
  
  return(perimetro)
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

