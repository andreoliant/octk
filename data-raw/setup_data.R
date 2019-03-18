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

# DEV: sta roba deve tutta lavorare per delta!

# confronta po_riclass
make_po_riclass <- function(bimestre) {

  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)
  }

  programmi <- progetti %>% distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)

  # require("readr")

  out <- po_riclass %>%
    bind_rows(programmi %>%
                anti_join(po_riclass))

  file.rename(file.path("data-raw", "po_riclass.csv"), file.path("data-raw", "po_riclass_OLD.csv"))

  write_delim(out, file.path("data-raw", "po_riclass_NEW.csv"), delim = ";", na = "")
}

make_po_riclass(bimestre="20181231")
# HAND: fare aggiornamento a mano

chk_delta_po_riclass <- function(da="NEW") {
  old <- read_csv2(file.path("data-raw", "po_riclass_OLD.csv"))
  new <- read_csv2(file.path("data-raw", "po_riclass_NEW.csv"))
  if (da == "OLD") {
    delta <- old %>% anti_join(new, by = "OC_CODICE_PROGRAMMA")
  } else {
    delta <- new %>% anti_join(old, by = "OC_CODICE_PROGRAMMA")
  }
  return(delta)
}

chk_delta_po_riclass()
chk_delta_po_riclass("OLD")
# HAND: fare aggiornamento a mano e rinominare in "po_riclass.csv"

# chk <- progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))
# chk <- progetti %>% filter(OC_CODICE_PROGRAMMA == "COMMTARANTOFSC")
# sum(chk$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)

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
make_matrix_po <- function(bimestre, file_name="po_linee_azioni_NEW.csv") {

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

  file.rename(file.path("data-raw", "po_linee_azioni.csv"), file.path("data-raw", "po_linee_azioni_OLD.csv"))

  write_delim(out, file.path("data-raw", file_name), delim = ";", na = "")
}

make_matrix_po(bimestre="20181231")
# HAND: fare aggiornamento a mano


chk_delta_po <- function(da="NEW") {
  old <- read_csv2(file.path("data-raw", "po_linee_azioni_OLD.csv"))
  new <- read_csv2(file.path("data-raw", "po_linee_azioni_NEW.csv"))
  if (da == "OLD") {
    delta <- old %>% anti_join(new)
  } else {
    delta <- new %>% anti_join(old)
  }
  return(delta)
}

chk <- chk_delta_po()
chk_delta_po("OLD")
# HAND: fare aggiornamento a mano e rinominare in "po_riclass.csv"





# crea matrix temi UE
make_matrix_ue <- function(DATA) {
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


# crea matrix strumenti attuativi
make_matrix_strum <- function(bimestre, file_name="strum_att.csv") {

  progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)

  out <- progetti %>%
    distinct(COD_STRUMENTO, DESCR_STRUMENTO, DESCR_TIPO_STRUMENTO) %>%
    mutate(QUERY = 0,
           NOTE = NA)

  write_delim(out, file.path("data-raw", file_name), delim = ";", na = "")
}


# crea matrix delibere cipe
make_matrix_cipe <- function(bimestre, file_name="delib_cipe.csv") {

  # load finanziamenti
  temp <- paste0("finanziamenti_esteso_", bimestre, ".csv")
  delibere <- read_csv2(file.path(DATA, temp), guess_max = 5000)

  out <- delibere %>%
    # distinct(COD_DEL_CIPE, NUMERO_DEL_CIPE, ANNO_DEL_CIPE, TIPO_QUOTA, DESCRIZIONE_QUOTA, IMPORTO) %>%
    distinct(NUMERO_DEL_CIPE, ANNO_DEL_CIPE, DESCRIZIONE_QUOTA) %>%
    mutate(QUERY = 0,
           NOTE = NA)

  write_delim(out, file.path("data-raw", file_name), delim = ";", na = "")
}



# categorie_cup
categorie_cup <- read_csv2("data-raw/categorie_cup.csv")
usethis::use_data(categorie_cup, overwrite = TRUE)

# po_linee_azioni
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

# strum_att
make_matrix_strum(bimestre="20181031")
strum_att <- read_csv2("data-raw/strum_att.csv")
usethis::use_data(strum_att, overwrite = TRUE)

# delib_cipe
make_matrix_cipe(bimestre="20181031")
delib_cipe <- read_csv2("data-raw/delib_cipe.csv")
usethis::use_data(delib_cipe, overwrite = TRUE)



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








