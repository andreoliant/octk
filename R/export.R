# OC > Explorer > Perimetri
# Export finale
# Focus: turismo

#' Export per il dataset finale
#'
#' Popola pseudo con una lista di variabili.
#'
#' @param pseudo Dataset "pseudo".
#' @param focus Nome file da salvare in OUTPUT.
#' @param bimestre Bimestre di riferimento (utilizzato per la composizone del nome file in OUTPUT.
#' @param var_ls Elenco delle variabili di base da esportare.
#' @param var_add Elenco delle ulteriori variabili da esportare (oltre a quelle di base).
#' @param export Vuoi salvare?
#' @return Un file "[focus]_[bimestre].csv".
export_data <- function(pseudo, focus, bimestre, var_ls=NULL, var_add=NULL, export=TRUE) {
  # DEV: aggiungere progetti in scope

  # merge con progetti
  perimetro <- pseudo %>%
    # isola scarti
    filter(PERI == 1) %>%
    select(-CHK, -PERI) %>%
    # merge variabili anagrafiche (da progetti)
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO", "OC_CODICE_PROGRAMMA"),
              by = "COD_LOCALE_PROGETTO")

  # x_vars
  perimetro <- get_x_vars(perimetro, progetti = progetti)

  # macroarea
  perimetro <- get_macroarea(perimetro)

  # regione
  perimetro <- get_regione_simply(perimetro)

  # var_ls
  if (is.null(var_ls)) {
    var_ls <- c("CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
                "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
                "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
                "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE",
                "CUP_COD_NATURA", "CUP_DESCR_NATURA",
                # "COD_REGIONE", "DEN_REGIONE", "COD_PROVINCIA", "DEN_PROVINCIA", "COD_COMUNE", "DEN_COMUNE",
                # "OC_COD_SLL", "OC_DENOMINAZIONE_SLL",
                "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
  }
  perimetro <- perimetro %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls),
              by = "COD_LOCALE_PROGETTO") %>%
    # fix per case in natura CUP
    mutate(CUP_DESCR_NATURA = ifelse(is.na(CUP_DESCR_NATURA), "NON CLASSIFICATO", toupper(CUP_DESCR_NATURA)))

  # var_add
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
  perimetro <- perimetro %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "OC_STATO_FASI"),
              by = "COD_LOCALE_PROGETTO")


  # export
  if (export == TRUE) {
    temp <- paste0(paste(focus, bimestre, sep = "_"), ".csv")
    write.csv2(perimetro, file.path(OUTPUT, temp), na = "", row.names = FALSE)
  }

  return(perimetro)
}



# ----------------------------------------------------------------------------------- #
# Export to xls

# FARE A MANO PERCHE NON FUNZIA...

# library("openxlsx")
#
# # MEMO: usato al primo giro per creare template (poi integrato a mano)
# tab_ls <- list(perimetro = perimetro)
# # write.xlsx(perimetro, file = file.path(tmp_path, "prova.xlsx"), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
# write.xlsx(tab_ls, file = file.path(tmp_path, "prova.xlsx"), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
#
# wb <- loadWorkbook(file.path(src_path, "template.xlsx"))
# removeTable(wb = wb, sheet = "dati", table = getTables(wb, sheet = "dati"))
# writeDataTable(wb, sheet = "dati", x = perimetro, stack = TRUE)
# saveWorkbook(wb, file = file.path(dat_path, paste0(paste(this_path, oc_ver, sep = "_"), ".xlsx")), overwrite = TRUE)
#
#


# ----------------------------------------------------------------------------------- #
# reload
reload_perimetro <- function(focus, bimestre, livelli_classe) {

  # load
  perimetro <- read_csv2(file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")))

  # etc
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")

  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")

  temp <- c(names(reg_cn[1:3]), "PA TRENTO", "PA BOLZANO", names(reg_cn[5:12]), names(reg_sud), "ALTRO TERRITORIO")

  # refactor
  out <- perimetro %>%
    mutate(CLASSE_FIN = factor(CLASSE_FIN, levels=c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty")),
           MACROAREA = factor(MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
           STATO_PROCED = factor(STATO_PROCED, levels = c("Programmazione", "Avvio", "Progettazione", "Affidamento", "Esecuzione", "Esercizio")),
           CUP_DESCR_NATURA = factor(CUP_DESCR_NATURA,
                                     levels=c("REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)",
                                              "ACQUISTO DI BENI",
                                              "ACQUISTO O REALIZZAZIONE DI SERVIZI",
                                              "CONCESSIONE DI INCENTIVI AD UNITA' PRODUTTIVE",
                                              "CONCESSIONE DI CONTRIBUTI AD ALTRI SOGGETTI (DIVERSI DA UNITA' PRODUTTIVE)",
                                              "NON CLASSIFICATO")),
           DEN_REGIONE = factor(DEN_REGIONE, levels = temp),
           CLASSE = factor(CLASSE, levels = livelli_classe))

  return(out)

}


