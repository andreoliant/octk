# OC > Toolkit
# Utility per aggiornare gli oc_asset in data/

# MEMO: funzione eseguite in "dev.R" per inizializzare i complementi al package per il bimestre
# WARNING: richiede oc_init


# ----------------------------------------------------------------------------------- #
# chk progetti

# TODO: creare diagnostico generale


# ----------------------------------------------------------------------------------- #
# progetti_light

#' Crea progetti_light.csv
#'
#' Crea un file progetti da progetti_esteso.csv, con le sole variabili fondamentali. Aggiunge blocco x_var e x_MACROAREA.
#'
#' @param bimestre Bimestre di riferimento.
#' @return Il dataset viene salvato in DATA e può essere caricato con load_progetti(light = TRUE).
setup_light <- function(bimestre, fix = FALSE, ...) {
  if (exists("DATA", envir = .GlobalEnv)) {
    # loads
    # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = FALSE)
    progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE, light = FALSE) # MEMO: versione con SNAI-FEASR

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
             OC_FINANZ_STATO_FONDO_ROT_NETTO, # DA RIPRISTINARE
             OC_FINANZ_STATO_FSC_NETTO, # DA RIPRISTINARE
             OC_FINANZ_STATO_PAC_NETTO, # DA RIPRISTINARE
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
             # OC_COD_FORMA_GIU_BENEFICIARIO,  # DA RIPRISTINARE
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
             SNAI # ,
             # COD_AREA_INT, # MEMO: queste entrano dopo
             # AREA_INTERNA
      )

    # add QSN
    operazioni_713_raw <- read_sas(file.path(DATA, "oper_fltok_preesteso.sas7bdat"))
    
     appo <- operazioni_713_raw %>%
      distinct(COD_LOCALE_PROGETTO = cod_locale_progetto,
             QSN_CODICE_OBIETTIVO_SPECIFICO = qsn_codice_obiettivo_specifico,
             QSN_DESCR_OBIETTIVO_SPECIFICO = qsn_descr_obiettivo_specifico)
    
    progetti_light <- progetti_light %>%
      left_join(appo, by = "COD_LOCALE_PROGETTO")
    
    
    # clean & fix
    if (fix == TRUE) {
      progetti_light <- fix_progetti(progetti_light, ...)
    }
    progetti_light <- get_x_vars(progetti_light)
    # progetti_light <- get_macroarea(progetti_light, progetti_light, real_reg=TRUE)
    progetti_light <- get_macroarea_oc(progetti_light, progetti_light)
    progetti_light <- get_regione_simply(progetti_light, progetti_light)

    # chk
    progetti_light %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N = n(),
                CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
    # MEMO: confronta con recap_preesteso.xlsx
    
    # ripristina solo visualizzati (inclusa SNAI su FEASR)
    progetti_light <- progetti_light %>%
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9)

    # export
    write.csv2(progetti_light, file.path(DATA, paste0("progetti_light_", bimestre, ".csv")), row.names = FALSE)

  } else {
    message("Non hai definito il folder DATA. Carica 'oc' ed inizializza 'oc_init()'.")
  }
}
# TODO: inserire gestione per 'get_macroarea()' e 'get_regione_simply()' in uso per 'futura'


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
  out <- oc::po_riclass %>%
    bind_rows(programmi %>%
                anti_join(po_riclass, by = "OC_CODICE_PROGRAMMA"))

  file.rename(file.path(getwd(), "setup", "data-raw", "po_riclass.csv"),
              file.path(getwd(), "setup", "data-raw", "po_riclass_OLD.csv"))

  write_delim(out, file.path("data-raw", "po_riclass_NEW.csv"), delim = ";", na = "")
}

# confronta po_riclass e isola delta
chk_delta_po_riclass <- function(da="NEW") {
  old <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_riclass_OLD.csv"))
  new <- read_csv2(file.path(getwd(), "setup", "data-raw","po_riclass_NEW.csv"))
  if (da == "OLD") {
    delta <- old %>% anti_join(new, by = "OC_CODICE_PROGRAMMA")
  } else {
    delta <- new %>% anti_join(old, by = "OC_CODICE_PROGRAMMA")
  }
  return(delta)
}







# ----------------------------------------------------------------------------------- #
# query

# TODO:
# queste dovrebbero popolare in automatico il file "query_template.xlsx" (ora in "inst" con altri template xls)
# creare wrapper che riepie direttamente il template

# crea nuuovo matrix programmi_linee_azioni
make_matrix_po <- function(bimestre) {

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }

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

  file.rename(file.path(getwd(), "setup", "data-raw", "po_linee_azioni.csv"),
              file.path(getwd(), "setup", "data-raw", "po_linee_azioni_OLD.csv"))

  # write_delim(out, file.path(getwd(), "setup", "data-raw", "po_linee_azioni_NEW.csv"), delim = ";", na = "")
  write_delim(out, file.path(getwd(), "setup", "data-raw", "po_linee_azioni.csv"), delim = ";", na = "")
}

# confronta programmi_linee_azioni e isola delta
chk_delta_po <- function(da="NEW") {
  old <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_linee_azioni_OLD.csv"))
  new <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_linee_azioni_NEW.csv"))
  if (da == "OLD") {
    delta <- old %>% anti_join(new, by = c("OC_CODICE_PROGRAMMA",
                                           "OC_COD_ARTICOLAZ_PROGRAMMA",
                                           "OC_COD_SUBARTICOLAZ_PROGRAMMA"))
  } else {
    delta <- new %>% anti_join(old, by = c("OC_CODICE_PROGRAMMA",
                                           "OC_COD_ARTICOLAZ_PROGRAMMA",
                                           "OC_COD_SUBARTICOLAZ_PROGRAMMA"))
  }
  return(delta)
}



# crea matrix temi UE
# make_matrix_ue <- function(DATA) {
#   # MEMO: questo parte da file custom di Luca (il "Campo" non è più presente nei dati pubblicati)
#
#   require("readr")
#
#   # appo_tema <- read_csv2(file.path(DATA, "clp_tema_campointervento.csv")) %>%
#   #   mutate(CICLO = case_when(TIPO == "CAMPO" ~ 2,
#   #                            TIPO == "TEMA" ~ 1))
#
#   if (is.null(progetti)) {
#     progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)
#   }
#
#   appo_tema <- progetti %>%
#     select(CICLO = OC_COD_CICLO,
#            COD_TEMA_CAMPO = OC_COD_CATEGORIA_SPESA,
#            DESCR_TEMA_CAMPO = OC_DESCR_CATEGORIA_SPESA)
#
#   out <- appo_tema %>%
#     distinct(CICLO,
#              COD_TEMA_CAMPO,
#              DESCR_TEMA_CAMPO) %>%
#     # select(OC_COD_CICLO, COD_TEMA_CAMPO, DESCR_TEMA_CAMPO) %>%
#     # MEMO: elimina collapse
#     filter(!grepl(":::", COD_TEMA_CAMPO)) %>%
#     # MEMO: fix encoding
#     mutate(DESCR_TEMA_CAMPO = gsub("Ã\\s", "à", DESCR_TEMA_CAMPO)) %>%
#     mutate(DESCR_TEMA_CAMPO = gsub("Ã©", "é", DESCR_TEMA_CAMPO)) %>%
#     mutate(QUERY = 0,
#            NOTE = NA) %>%
#     arrange(desc(OC_COD_CICLO), COD_TEMA_CAMPO)
#
#   write_delim(out, file.path("data-raw", "categorie_ue.csv"), delim = ";", na = "")
#
# }
#
# MEMO: va creato staticamente sulla base delle tabelle di contesto

# crea matrix strumenti attuativi
make_matrix_strum <- function(bimestre, file_name="strum_att.csv") {

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }

  out <- progetti %>%
    distinct(COD_STRUMENTO, DESCR_STRUMENTO, DESCR_TIPO_STRUMENTO, OC_CODICE_PROGRAMMA) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(QUERY = 0,
           NOTE = NA) %>% 
    filter(!is.na(COD_STRUMENTO))

  write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
}


# crea matrix delibere cipe
make_matrix_cipe <- function(bimestre, file_name="delib_cipe.csv") {

  # load finanziamenti
  # temp <- paste0("finanziamenti_esteso_", bimestre, ".csv")
  # delibere <- read_csv2(file.path(DATA, temp), guess_max = 5000)
  temp <- file.path(DATA, "finanziamenti_preesteso.sas7bdat")
  delibere <- read_sas(temp)

  out <- delibere %>%
    # distinct(COD_DEL_CIPE, NUMERO_DEL_CIPE, ANNO_DEL_CIPE, TIPO_QUOTA, DESCRIZIONE_QUOTA, IMPORTO) %>%
    # distinct(NUMERO_DEL_CIPE, ANNO_DEL_CIPE, DESCRIZIONE_QUOTA) %>%
    rename(NUMERO_DEL_CIPE = numero_del_cipe,
           ANNO_DEL_CIPE = anno_del_cipe) %>%
    distinct(NUMERO_DEL_CIPE, ANNO_DEL_CIPE, DESCRIZIONE_QUOTA) %>%
    mutate(QUERY = 0,
           NOTE = NA)

  write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
}


# crea matrix progetti complessi
make_prog_comp <- function(bimestre, file_name="prog_comp.csv") {

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }

  out <- progetti %>%
    distinct(COD_PROGETTO_COMPLESSO, DESCRIZIONE_PROGETTO_COMPLESSO, COD_TIPO_COMPLESSITA, DESCR_TIPO_COMPLESSITA,
             OC_CODICE_PROGRAMMA) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(QUERY = 0,
           NOTE = NA) %>% 
    filter(!is.na(COD_PROGETTO_COMPLESSO))

  write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
}


# crea matrix patt
make_patt <- function(bimestre, file_name="patt.csv") {

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
  }

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
    
    print(paste0(x, ": ", nrow(chk)))
    
    # DEBUG:
    # cat(chk$DESCR_PROCED_ATTIVAZIONE)
    
    # DEBUG:
    # fileConn <- file(file.path(TEMP, "chk.txt"))
    # writeLines(chk$DESCR_PROCED_ATTIVAZIONE, con = fileConn, sep = "\n\r")
    # close(fileConn)
    
  }

  # export
  write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
}


# crea matrix comuni
make_comuni <- function(file_name="matrix_comuni.csv") {
  
  # library("haven")
  
  message("Ricodati di chiedere a Luca se ha cambiato il file!")
  message("Ricodati di convertire a mano il csv di Luca in UTF8")
  
  # out <- read_sas(file.path(getwd(), "setup", "data-raw", "variazioni_comuni.sas7bdat"))  %>%
    out <- read_csv2(file.path(getwd(), "setup", "data-raw", "variazioni_comuni.csv"))  %>%
    rename(ANNO_VARIAZIONE = anno,
           TIPO_VARIAZIONE = tipo,
           COD_COMUNE_OLD = id_old,
           DEN_COMUNE_OLD = name_old,
           COD_COMUNE = id_new, 
           DEN_COMUNE = name_new) %>% 
    mutate(QUERY = 0,
           AMBITO = NA,
           AMBITO_SUB = NA)

  write_delim(out, file.path(getwd(), "setup", "data-raw", file_name), delim = ";", na = "")
}



#' Fix temporaneo per il dataset progetti in vestione preesteso
#'
#' Integra il dataset.
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_progetti <- function(progetti, path_snai=NULL) {
  
  # fix temporaneo per matera
  # progetti <- progetti %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(is.na(OC_CODICE_PROGRAMMA) ~ "2018MATERAFSC",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA),
  #          OC_DESCRIZIONE_PROGRAMMA = case_when(is.na(OC_DESCRIZIONE_PROGRAMMA) ~ "MATERA CAPITALE DELLA CULTURA 2019",
  #                                               TRUE ~ OC_DESCRIZIONE_PROGRAMMA))
  # # progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))
  #
  # progetti <- progetti %>%
  #   mutate(DEN_REGIONE = case_when(COD_REGIONE == "002" ~ "VALLE D'AOSTA", # fix per denominazione bilingue
  #                                  COD_REGIONE == "004" ~ "TRENTINO-ALTO ADIGE",
  #                                  DEN_REGIONE == "EMILIA" ~ "EMILIA-ROMAGNA", # fix per denominazione doppia
  #                                  DEN_REGIONE == "FRIULI" ~ "FRIULI-VENEZIA GIULIA",
  #                                  TRUE ~ DEN_REGIONE))
  
  # progetti <- progetti %>%
  #   mutate(FONDO_COMUNITARIO = case_when(OC_CODICE_PROGRAMMA == "2014IT16M2OP006" & is.na(FONDO_COMUNITARIO) ~ "FESR",
  #                                        # MEMO: forzo su FESR ma c'è anche FSE
  #                                        TRUE ~ FONDO_COMUNITARIO))
  
  # fix temporaneo per YEI
  # progetti <- progetti %>%
  #   mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI",
  #                                        TRUE ~ FONDO_COMUNITARIO))
  
  # fix temporaneo per IOG>YEI
  progetti <- progetti %>%
    mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "IOG" ~ "YEI",
                                         OC_CODICE_PROGRAMMA == "2014IT05M9OP001" & is.na(FONDO_COMUNITARIO) ~ "YEI",
                                         TRUE ~ FONDO_COMUNITARIO))
  
  # # fix temporaneo per ":::OC_CODICE_PROGRAMMA"
  # progetti <- progetti %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == ":::2014IT16RFOP007" ~ "2014IT16RFOP007",
  #                                          OC_CODICE_PROGRAMMA == ":::2016POCIMPRESE1" ~ "2016POCIMPRESE1",
  #                                          OC_CODICE_PROGRAMMA == ":::2017FSCRICERCA" ~ "2017FSCRICERCA",
  #                                          OC_CODICE_PROGRAMMA == ":::2017POCRICERCA1" ~ "2017POCRICERCA1",
  #                                          OC_CODICE_PROGRAMMA == ":::2017POIMPCOMFSC" ~ "2017POIMPCOMFSC",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  
  # fix temporaneo per ":::OC_CODICE_PROGRAMMA" (su dati 20291031)
  # progetti <- progetti %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == ":::2014IT16RFOP007" ~ "2014IT16RFOP007",
  #                                          OC_CODICE_PROGRAMMA == ":::2016POCIMPRESE1" ~ "2014IT16RFOP003", # cambia!
  #                                          OC_CODICE_PROGRAMMA == ":::2017FSCRICERCA" ~ "2014IT16M2OP005",  # cambia!
  #                                          OC_CODICE_PROGRAMMA == ":::2017POCRICERCA1" ~ "2014IT16M2OP005", # cambia!
  #                                          OC_CODICE_PROGRAMMA == ":::2017POIMPCOMFSC" ~ "2017POIMPCOMFSC",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  
  # fix temporaneo per ":::OC_CODICE_PROGRAMMA" (su dati 20291231)
  # MEMO: sposto su programma SIE
  # progetti <- progetti %>%
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2016POCIMPRESE1" & OC_COD_FONTE == "FS1420" ~ "2014IT16RFOP003",
  #                                          OC_CODICE_PROGRAMMA == "2017FSCRICERCA" & OC_COD_FONTE == "FS1420" ~ "2014IT16M2OP005",
  #                                          OC_CODICE_PROGRAMMA == "2017POCRICERCA1" & OC_COD_FONTE == "FS1420" ~ "2014IT16M2OP005",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  # 
  # progetti <- progetti %>%
  #   mutate(COD_LOCALE_PROGETTO = case_when(grepl("^1MISE174", COD_LOCALE_PROGETTO) ~ "1MISE174",
  #                                          grepl("^1MISE397", COD_LOCALE_PROGETTO) ~ "1MISE397",
  #                                          grepl("^1MISE496", COD_LOCALE_PROGETTO) ~ "1MISE496",
  #                                          grepl("^1MISE608", COD_LOCALE_PROGETTO) ~ "1MISE608",
  #                                          TRUE ~ COD_LOCALE_PROGETTO))
  
  # fix di progetti senza FONDO_COMUNITARIO
  # progetti <- progetti %>%
  #   mutate(FONDO_COMUNITARIO = case_when(OC_CODICE_PROGRAMMA == "2014IT16M2OP002:::2016PATTIPUG" & is.na(FONDO_COMUNITARIO) ~ "FESR",
  #                                        TRUE ~ FONDO_COMUNITARIO))
  
  # fix snai
  if (!(is.null(path_snai))) {
    progetti <- fix_snai(progetti, path_snai)
  }
  
  return(progetti)
}



#' Fix variabili SNAI
#'
#' Fix temporaneo per integrare le variabili SNAI (COD_AREA_INT e AREA_INTERNA) dal file di Andrea
#'
#' @param progetti Dataset in formato standard.
#' @return Il dataset progetti integrato.
fix_snai <- function(progetti, path_snai) {
  
  # path_snai <- "ELAB/20211031/SNAI/snai/V.01/output/perimetro_snai.xlsx"
  snai <- read_xlsx(file.path(DRIVE, path_snai)) %>% 
    select(COD_LOCALE_PROGETTO, SNAI_OC, COD_AREA_INT, AREA_INTERNA) %>% 
    filter(SNAI_OC == 1) %>% 
    select(-SNAI_OC)
  
  # fix temporaneo per IOG>YEI
  progetti <- progetti %>%
    left_join(snai, by = "COD_LOCALE_PROGETTO")
  
  return(progetti)
}


