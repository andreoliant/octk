# OC > Explorer > Perimetri
# Export finale

#' Export per il dataset finale (DEPRECATA)
#'
#' Popola pseudo con una lista di variabili.
#'
#' @param pseudo Dataset "pseudo".
#' @param focus Nome file da salvare in OUTPUT.
#' @param bimestre Bimestre di riferimento (utilizzato per la composizone del nome file in OUTPUT.
#' @param var_ls Elenco delle variabili di base da esportare. Se nullo usa get_default_vars.
#' @param var_add Elenco delle ulteriori variabili da esportare oltre a quelle di var_ls (se si usa get_default_vars)).
#' @param export Vuoi salvare?
#' @return Un file "[focus]_[bimestre].csv".
export_data <- function(pseudo, focus, bimestre, var_ls=NULL, var_add=NULL, export=TRUE) {
  # DEV: aggiungere progetti in scope

  # merge con progetti
  perimetro <- pseudo %>%
    # isola scarti
    filter(PERI == 1) %>%
    select(-CHK, -PERI) # %>%
    # merge variabili anagrafiche (da progetti)
    # left_join(progetti %>%
    #             select("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO", "OC_CODICE_PROGRAMMA"),
    #           by = "COD_LOCALE_PROGETTO")

  # x_vars
  # perimetro <- get_x_vars(perimetro, progetti = progetti)

  # macroarea
  # perimetro <- get_macroarea(perimetro)

  # regione
  # perimetro <- get_regione_simply(perimetro)

  # var_ls
  if (is.null(var_ls)) {
    # var_ls <- c("CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
    #             "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
    #             "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
    #             "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE",
    #             "CUP_COD_NATURA", "CUP_DESCR_NATURA",
    #             "COD_REGIONE",
    #             # "DEN_REGIONE", "COD_PROVINCIA", "DEN_PROVINCIA", "COD_COMUNE", "DEN_COMUNE",
    #             # "OC_COD_SLL", "OC_DENOMINAZIONE_SLL",
    #             "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
    var_ls <- get_default_vars()
  }
  perimetro <- perimetro %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls),
              by = "COD_LOCALE_PROGETTO") # %>%
    # fix per case in natura CUP
    # mutate(CUP_DESCR_NATURA = ifelse(is.na(CUP_DESCR_NATURA), "NON CLASSIFICATO", toupper(CUP_DESCR_NATURA)))

  # var_add
  # aggiunge ulteriori spefiche varibili
  if (!is.null(var_add)) {
    perimetro <- perimetro %>%
      left_join(progetti %>%
                  select("COD_LOCALE_PROGETTO", var_add),
                by = "COD_LOCALE_PROGETTO")
  }

  # Dimensione finanziaria
  perimetro <- get_dimensione_fin(perimetro)
    # MEMO: versione più fine rispetto a quella di OC

  # Stato di attuazione
  # perimetro <- get_stato_attuazione(df = perimetro, chk_today = "20180531")
  # perimetro <- perimetro %>%
  #   left_join(progetti %>%
  #               select("COD_LOCALE_PROGETTO", "OC_STATO_FASI"),
  #             by = "COD_LOCALE_PROGETTO")

  # export
  if (export == TRUE) {
    temp <- paste0(paste(focus, bimestre, sep = "_"), ".csv")
    write.csv2(perimetro, file.path(TEMP, temp), na = "", row.names = FALSE)
  }

  return(perimetro)
}



#' Export per il dataset finale in formato excel
#'
#' Esporta perimetro da export_data() in excel.
#'
#' @param perimetro Dataset "perimetro" creato con export_data().
#' @param focus Nome file da salvare in OUTPUT.
#' @param bimestre Bimestre di riferimento (utilizzato per la composizone del nome file in OUTPUT.
#' @param use_template Vuoi usare un template?
#' @return Un file "[focus]_[bimestre].csv".
export_data_xls <- function(perimetro, focus, bimestre, use_template=FALSE) {

  # library("openxlsx")
  temp <- paste0(paste(focus, bimestre, sep = "_"), ".xlsx")

  if (use_template == TRUE) {
    message("DA IMPLEMENTARE")
    # wb <- loadWorkbook(system.file("extdata", "template.xlsx", package = "oc", mustWork = TRUE))
    # removeTable(wb = wb, sheet = "dati", table = getTables(wb, sheet = "dati"))
    # writeDataTable(wb, sheet = "dati", x = perimetro, stack = TRUE)
    # saveWorkbook(wb, file = file.path(OUTPUT, temp), overwrite = TRUE)

    # OLD: FORSE DA BUTTARE
    # for (i in seq_along(tab_ls)) {
    #   print(names(tab_ls)[i])
    #   removeTable(wb = wb, sheet = names(tab_ls)[i], table = getTables(wb, sheet = names(tab_ls)[i]))
    #   writeDataTable(wb, sheet = names(tab_ls)[i], x = tab_ls[[i]], stack = TRUE)
    # }
    #

  } else {
    tab_ls <- list(perimetro = perimetro)
    write.xlsx(tab_ls, file = file.path(OUTPUT, temp), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
  }



  # wb <- loadWorkbook(file.path(src_path, "template.xlsx"))
  # removeTable(wb = wb, sheet = "dati", table = getTables(wb, sheet = "dati"))
  # writeDataTable(wb, sheet = "dati", x = perimetro, stack = TRUE)
  # saveWorkbook(wb, file = file.path(dat_path, paste0(paste(this_path, oc_ver, sep = "_"), ".xlsx")), overwrite = TRUE)

}





# ----------------------------------------------------------------------------------- #
# reload
reload_perimetro <- function(focus=NULL, bimestre=NULL, livelli_classe) {

  # load
  # perimetro <- read_csv2(file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")))
  temp <- paste0(paste(focus, bimestre, sep = "_"), ".csv")
  if (file.exists(temp)) {
    perimetro <- read_csv2(file.path(TEMP, temp))
  } else {
    perimetro <- read_csv2(file.path(TEMP, "dati.csv"))
  }


  # etc
  # reg_cn <- c("001", "002", "003", "004", "005", "006",
  #             "007", "008", "009", "010", "011", "012")
  # names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
  #                    "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  #
  # reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  # names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  #
  # temp <- c(names(reg_cn[1:3]), "PA TRENTO", "PA BOLZANO", names(reg_cn[5:12]), names(reg_sud), "ALTRO TERRITORIO")

  # refactor
  out <- perimetro %>%
    refactor_progetti(.) %>%
    mutate(# CLASSE_FIN = factor(CLASSE_FIN, levels=c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty")),
           # MACROAREA = factor(MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
           # STATO_PROCED = factor(STATO_PROCED, levels = c("Programmazione", "Avvio", "Progettazione", "Affidamento", "Esecuzione", "Esercizio")),
           # CUP_DESCR_NATURA = factor(CUP_DESCR_NATURA,
           #                           levels=c("REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)",
           #                                    "ACQUISTO DI BENI",
           #                                    "ACQUISTO O REALIZZAZIONE DI SERVIZI",
           #                                    "CONCESSIONE DI INCENTIVI AD UNITA' PRODUTTIVE",
           #                                    "CONCESSIONE DI CONTRIBUTI AD ALTRI SOGGETTI (DIVERSI DA UNITA' PRODUTTIVE)",
           #                                    "NON CLASSIFICATO")),
           # DEN_REGIONE = factor(DEN_REGIONE, levels = temp),
           CLASSE = factor(CLASSE, levels = livelli_classe))

  return(out)

}



#' Export perimetro to SAS
#'
#' Crea file da passare a SAS per il popolmaneto della variabile FOCUS.
#'
#' @param perimetro Dataset "perimetro" da \link[octk]{make_perimetro_edit}o \link[octk]{make_perimetro_std.} 
#' @param focus Nome del file da salvare.
#' @param use_drive Logico. Stai salvano in Drive (TRUE) o in locale (FALSE)?
#' @param keep_classe Logico. Vuoi tenere la variabile CLASSE?
#' @param split_classe Logico. Vuoi separare N file in base alla variabile CLASSE?
#' @return Un file di tipo "[focus]_clp.csv". Viene salvato in PERIMETRI/OUTPUT_SAS se use_drive == TRUE.
export_sas <- function(perimetro, focus="perimetro", use_drive=TRUE, keep_classe=FALSE, split_classe=FALSE) {
  # funzione di esportazione
  if (use_drive == TRUE) {
    export_fun <- function(df, focus) {
      # salva in WORK
      temp_filename <- paste0(focus, "_clp.csv")
      write.csv2(df, file.path(OUTPUT, temp_filename), na = "", row.names = FALSE)
      
      # salva copia ridondante per SAS
      OUTPUT_SAS <- file.path(dirname(dirname(WORK)), "_OUTPUT_SAS")
      write.csv2(df, file.path(OUTPUT_SAS, temp_filename), na = "", row.names = FALSE)
      message("Copia salvata anche in OUTPUT_SAS")
    }
  } else {
    export_fun <- function(df, focus) {
      # salva in WORK locale
      temp_filename <- paste0(focus, "_clp.csv")
      write.csv2(df, file.path(OUTPUT, temp_filename), na = "", row.names = FALSE)
      message("Ricordati di aggiornare anche OUTPUT_SAS!")
    }
  }
  
  # gestione dei casi
  if (split_classe == TRUE) {
    temp <- unique(perimetro$CLASSE)
    for (x in temp) {
      appo <- perimetro %>%
        filter(CLASSE == x) %>%
        select(COD_LOCALE_PROGETTO)
      export_fun(df = appo, focus = x)
    }
  } else {
    if (keep_classe == TRUE) {
      appo <- perimetro %>%
        select(COD_LOCALE_PROGETTO, CLASSE)
      export_fun(df = appo, focus = focus)
    } else {
      appo <- perimetro %>%
        select(COD_LOCALE_PROGETTO)
      export_fun(df = appo, focus = focus)
    }
  }
} 


# internal utility to list exported variables
get_default_vars <- function() {
  out <- c(
    'COD_LOCALE_PROGETTO',
    'CUP',
    'OC_TITOLO_PROGETTO',
    'OC_SINTESI_PROGETTO',
    'x_CICLO',
    'x_AMBITO',
    'OC_CODICE_PROGRAMMA',
    'x_PROGRAMMA',
    'COD_RISULTATO_ATTESO',
    'DESCR_RISULTATO_ATTESO',
    'OC_COD_CATEGORIA_SPESA',
    'OC_DESCR_CATEGORIA_SPESA',
    'OC_COD_ARTICOLAZ_PROGRAMMA',
    'OC_DESCR_ARTICOLAZ_PROGRAMMA',
    'OC_COD_SUBARTICOLAZ_PROGRAMMA',
    'OC_DESCR_SUBARTICOLAZ_PROGRAMMA',
    'COD_STRUMENTO',
    'DESCR_STRUMENTO',
    'DESCR_TIPO_STRUMENTO',
    'COD_PROGETTO_COMPLESSO',
    'DESCRIZIONE_PROGETTO_COMPLESSO',
    'COD_TIPO_COMPLESSITA',
    'DESCR_TIPO_COMPLESSITA',
    'CUP_COD_NATURA',
    'CUP_DESCR_NATURA',
    'CUP_COD_TIPOLOGIA',
    'CUP_DESCR_TIPOLOGIA',
    'CUP_COD_SETTORE',
    'CUP_DESCR_SETTORE',
    'CUP_COD_SOTTOSETTORE',
    'CUP_DESCR_SOTTOSETTORE',
    'CUP_COD_CATEGORIA',
    'CUP_DESCR_CATEGORIA',
    'x_REGIONE',
    'x_MACROAREA',
    'COD_PROVINCIA',
    'DEN_PROVINCIA',
    'COD_COMUNE',
    'DEN_COMUNE',
    'OC_FINANZ_UE_NETTO',
    'OC_FINANZ_TOT_PUB_NETTO',
    'IMPEGNI',
    'TOT_PAGAMENTI',
    'OC_COSTO_COESIONE',
    'OC_IMPEGNI_COESIONE',
    'OC_PAGAMENTI_COESIONE',
    'OC_STATO_PROGETTO',
    'OC_STATO_PROCEDURALE',
    'OC_COD_FASE_CORRENTE',
    'OC_DESCR_FASE_CORRENTE',
    'COD_PROCED_ATTIVAZIONE',
    'DESCR_PROCED_ATTIVAZIONE',
    'OC_CODFISC_BENEFICIARIO',
    'OC_DENOM_BENEFICIARIO'
    )
  return(out)
}

