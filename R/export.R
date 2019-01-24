# OC > Explorer > Perimetri
# Export finale
# Focus: turismo

export_data <- function(pseudo, focus, bimestre, var_ls, var_add=NULL, chk_today="20180731", export=TRUE, debug=FALSE) {

  # setting
  var_ls1 <- c("CUP", "OC_TITOLO_PROGETTO",
               "OC_CODICE_PROGRAMMA",
               "OC_COD_CICLO", "OC_COD_FONTE", "FONDO_COMUNITARIO")

  var_ls2 <- c("CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
               "OC_DESCRIZIONE_PROGRAMMA",
               # "OC_CODICE_PROGRAMMA",
               "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_SUBARTICOLAZ_PROGRAMMA",
               "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA")

  var_ls3 <- c("CUP_COD_NATURA", "CUP_DESCR_NATURA",

               "COD_ATECO", "DESCRIZIONE_ATECO", # NEW ITEM

               "COD_REGIONE", "DEN_REGIONE", "COD_PROVINCIA", "DEN_PROVINCIA", "COD_COMUNE", "DEN_COMUNE",
               "OC_COD_SLL", "OC_DENOMINAZIONE_SLL")

  var_ls4 <- c("OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI", "OC_STATO_PROGETTO",
               "COSTO_RENDICONTABILE_UE",
               "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE")

  # merge con progetti
  perimetro <- pseudo %>%
    # isola scarti
    filter(PERI == 1) %>%
    select(-CHK, -PERI) %>%
    # merge variabili anagrafiche (da progetti)
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls1),
              by = "COD_LOCALE_PROGETTO") %>%
    # merge con con po_riclass
    # MEMO:
    # fa casino se ci sono NA su OC_COD_PROGRAMMA sia in po_riclass che in pseudo/progetti
    # ad es. per non progetti da add_old_turismo che poi sono diventati non visualizzati
    left_join(po_riclass,
              by = "OC_CODICE_PROGRAMMA") %>%
    # merge variabili di classificazione (da progetti)
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls2),
              by = "COD_LOCALE_PROGETTO") %>%
    # merge con categorie UE
    # OLD:
    # left_join(temp_tema,
    #           by = "COD_LOCALE_PROGETTO") %>%
    # NEW:
    # get_categorie_UE(.) %>%


    # merge variabili di localizzazione (da progetti)
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls3),
              by = "COD_LOCALE_PROGETTO") %>%
    # fix natura CUP
    mutate(CUP_DESCR_NATURA = ifelse(is.na(CUP_DESCR_NATURA), "NON CLASSIFICATO", toupper(CUP_DESCR_NATURA))) %>%
    # merge variabili avanzamento
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls4),
              by = "COD_LOCALE_PROGETTO")

  # Dimensione finanziaria
  # MEMO: versione più fine rispetto a quella di OC
  perimetro <- get_dimensione_fin(perimetro)

  # Macroarea
  perimetro <- get_macroarea(perimetro)

  # Stato di attuazione
  # perimetro <- get_stato_attuazione(df = perimetro, chk_today = "20180531")
  # perimetro <- get_stato_attuazione(df = perimetro, chk_today = chk_today)


  # ----------------------------------------------------------------------------------- #
  # Export

    # chk
  # dim(pseudo %>% filter(PERI == 1))[1] == dim(perimetro)[1]
  # length(unique(perimetro$COD_LOCALE_PROGETTO)) == dim(perimetro)[1]

  if (export == TRUE) {
    # export
    write.csv2(perimetro,
               file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")),
               na = "", row.names = FALSE)
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
           DEN_REGIONE = factor(DEN_REGIONE, levels = c(names(reg_cn), names(reg_sud), "ALTRO TERRITORIO")),
           CLASSE = factor(CLASSE, levels = livelli_classe))

}


