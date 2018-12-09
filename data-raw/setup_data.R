# Aggiorna dati generali in data/

# setup
# library("usethis")
# usethis::use_data_raw()

matrix_comuni <- read_csv2("data-raw/matrix_comuni.csv")
usethis::use_data(matrix_comuni, overwrite = TRUE)

po_riclass <- read_csv2("data-raw/po_riclass.csv") %>%
  # MEMO: raw contiene NA per i programmi POC 2014-2020 > fanno casino perché join con progetti è sempre su OC_COD_PROGRAMMA
  filter(!is.na(OC_CODICE_PROGRAMMA))
usethis::use_data(po_riclass, overwrite = TRUE)

monithon_clp <- read_csv2("data-raw/monithon_clp.csv")
usethis::use_data(monithon_clp, overwrite = TRUE)

# progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)
# usethis::use_data(progetti, overwrite = TRUE)


# ----------------------------------------------------------------------------------- #
# Setup per query
# TODO:
# queste dovrebbero popolare in automatico il file "query_template.xlsx" (ora in "inst" con altri template xls)
# creare wrapper che riepie direttamente il template
# MEMO: il blocco va eseguito una volta sola in update di intero package oc (ma richiede upload di progetti)

# crea matrix programmi > linee > azioni
make_matrix_po <- function() {

  progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)

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

  write_delim(out, file.path("data-raw", "po_linee_azioni.csv"), delim = ";", na = "")
}

# crea matrix temi UE
make_matrix_ue <- function() {
  # MEMO: questo parte da file custom di Luca (il "Campo" non è più presente nei dati pubblicati)

  require("readr")

  appo_tema <- read_csv2(file.path(DATA, "clp_tema_campointervento.csv")) %>%
    mutate(CICLO = case_when(TIPO == "CAMPO" ~ 2,
                             TIPO == "TEMA" ~ 1))

  out <- appo_tema %>%
    distinct(OC_COD_CICLO = CICLO,
             COD_TEMA_CAMPO,
             DESCR_TEMA_CAMPO) %>%
    select(OC_COD_CICLO, COD_TEMA_CAMPO, DESCR_TEMA_CAMPO) %>%
    # MEMO: fix encoding
    mutate(DESCR_TEMA_CAMPO = gsub("Ã\\s", "à", DESCR_TEMA_CAMPO)) %>%
    mutate(DESCR_TEMA_CAMPO = gsub("Ã©", "é", DESCR_TEMA_CAMPO)) %>%
    mutate(QUERY = 0,
           NOTE = NA) %>%
    arrange(desc(OC_COD_CICLO), COD_TEMA_CAMPO)

  write_delim(out, file.path("data-raw", "categorie_ue.csv"), delim = ";", na = "")

}

# workflow
categorie_cup <- read_csv2("data-raw/categorie_cup.csv")
usethis::use_data(categorie_cup, overwrite = TRUE)

make_matrix_po() # po_linee_azioni.csv
po_linee_azioni <- read_csv2("data-raw/po_linee_azioni.csv")
usethis::use_data(po_linee_azioni, overwrite = TRUE)

make_matrix_ue() # categorie_ue.csv
categorie_ue <- read_csv2("data-raw/categorie_ue.csv")
usethis::use_data(categorie_ue, overwrite = TRUE)


