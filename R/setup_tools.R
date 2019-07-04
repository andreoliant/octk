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
setup_light <- function(bimestre, fix = FALSE) {
  if (exists("DATA", envir = .GlobalEnv)) {
    # loads
    progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = FALSE)

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
             # OC_COD_FONTE,
             # OC_DESCR_FONTE,
             FONDO_COMUNITARIO,
             OC_CODICE_PROGRAMMA,
             # OC_DESCRIZIONE_PROGRAMMA,
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
             # COSTO_REALIZZATO,
             COSTO_RENDICONTABILE_UE,
             OC_TOT_PAGAMENTI_RENDICONTAB_UE,
             OC_TOT_PAGAMENTI_FSC,
             OC_TOT_PAGAMENTI_PAC,
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
             OC_DESCR_FORMA_GIU_BENEFICIARIO,  # DA RIPRISTINARE
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
             OC_FLAG_VISUALIZZAZIONE #,
             # OC_FLAG_PAC,
             # DATA_AGGIORNAMENTO,
             # OC_FOCUS
      )

    # clean & fix
    if (fix == TRUE) {
      progetti_light <- fix_progetti(progetti_light)
    }
    progetti_light <- get_x_vars(progetti_light)
    progetti_light <- get_macroarea(progetti_light, real_reg=TRUE)
    progetti_light <- get_regione_simply(progetti_light)

    # chk
    progetti_light %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N = n(),
                CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
    # MEMO: confronta con recap_preesteso.xlsx

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

  write_delim(out, file.path(getwd(), "setup", "data-raw", "po_linee_azioni_NEW.csv"), delim = ";", na = "")
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
    distinct(COD_STRUMENTO, DESCR_STRUMENTO, DESCR_TIPO_STRUMENTO) %>%
    mutate(QUERY = 0,
           NOTE = NA)

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



