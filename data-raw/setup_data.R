# OC > Toolkit
# Utility per aggiornare gli oc_asset in data/

# MEMO:
# il blocco va eseguito una volta sola in update di intero package oc
# richiede load di progetti!

# setup
# library("usethis")
# usethis::use_data_raw()
# progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)
# usethis::use_data(progetti, overwrite = TRUE)


# ----------------------------------------------------------------------------------- #
# etc

# po_riclass
po_riclass <- read_csv2("data-raw/po_riclass.csv") %>%
  # MEMO: raw contiene NA per i programmi POC 2014-2020 > fanno casino perché join con progetti è sempre su OC_COD_PROGRAMMA
  filter(!is.na(OC_CODICE_PROGRAMMA))
usethis::use_data(po_riclass, overwrite = TRUE)

# moniton_clp
monithon_clp <- read_csv2("data-raw/monithon_clp.csv")
usethis::use_data(monithon_clp, overwrite = TRUE)


# ----------------------------------------------------------------------------------- #
# matrix

# matrix_comuni
matrix_comuni <- read_csv2("data-raw/matrix_comuni.csv")
usethis::use_data(matrix_comuni, overwrite = TRUE)
# MEMO: questo viene da uno script a parte che va integrato in oc

# matrix_op
matrix_op <- read_csv2("data-raw/matrix_op.csv")
usethis::use_data(matrix_op, overwrite = TRUE)

# matrix_opos
matrix_opos <- read_csv2("data-raw/matrix_opos.csv")
usethis::use_data(matrix_opos, overwrite = TRUE)

# matrix_ra_opos
matrix_ra_opos <- read_csv2("data-raw/matrix_ra_opos.csv")
usethis::use_data(matrix_ra_opos, overwrite = TRUE)

# matrix_ra_temi_fsc
matrix_ra_temi_fsc <- read_csv2("data-raw/matrix_ra_temi_fsc.csv")
usethis::use_data(matrix_ra_temi_fsc, overwrite = TRUE)


# ----------------------------------------------------------------------------------- #
# query

# TODO:
# queste dovrebbero popolare in automatico il file "query_template.xlsx" (ora in "inst" con altri template xls)
# creare wrapper che riepie direttamente il template

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


# categorie_cup
categorie_cup <- read_csv2("data-raw/categorie_cup.csv")
usethis::use_data(categorie_cup, overwrite = TRUE)

# po_linee_azioni
make_matrix_po()
po_linee_azioni <- read_csv2("data-raw/po_linee_azioni.csv")
usethis::use_data(po_linee_azioni, overwrite = TRUE)

# categorie_ue
make_matrix_ue()
categorie_ue <- read_csv2("data-raw/categorie_ue.csv")
usethis::use_data(categorie_ue, overwrite = TRUE)

# ra
ra <- read_csv2("data-raw/ra.csv")
usethis::use_data(ra, overwrite = TRUE)

# aree_temi_fsc
aree_temi_fsc <- read_csv2("data-raw/aree_temi_fsc.csv")
usethis::use_data(aree_temi_fsc, overwrite = TRUE)


# ----------------------------------------------------------------------------------- #
# fixlist

# stoplist
stoplist <- read_csv2("data-raw/template_stoplist.csv")
usethis::use_data(stoplist, overwrite = TRUE)

# safelist
safelist <- read_csv2("data-raw/template_safelist.csv")
usethis::use_data(safelist, overwrite = TRUE)

# fixlist
fixlist <- read_csv2("data-raw/template_fixlist.csv")
usethis::use_data(fixlist, overwrite = TRUE)








