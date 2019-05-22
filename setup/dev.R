# OC > Toolkit
# development platform

# rm(list=ls())
library("devtools")


# getwd()
# "/Users/aa/coding/oc"



# ----------------------------------------------------------------------------------- #
# workflow

# Faqre una nuova versione per ogni nuovo bimestre di monitoraggio.
# Per la nuova versione:
# - modifica "Z" in DESCRIPTION (es. X.Y.Z); solo in caso di altre modifiche rilevanti
#   salire a livello di "Y" o "X"
# - ...
# - DO SOMETHING ABOUT po_riclass.csv
# - per per data (OLD: "data-raw/setup_data.R")
# - setup data
# - setup di progetti_light.csv
# -
# - aggiungere tag in git con $: git tag vX.Y.Z
# - build e invia al team


# ----------------------------------------------------------------------------------- #
# build as bundle

# build
devtools::build(pkg = ".", path = "/Users/aa/coding/oc/bkp")
# MEMO: build to boundle "oc_0.1.0.tar.gz"

# install
install.packages("/Users/aa/coding/oc/bkp/oc_0.1.0.tar.gz", repos = NULL, type="source")

# build as binary
# devtools::build(path = "/Users/aa/coding/oc", binary = TRUE)
# MEMO: build to binary "oc_0.1.0.tgz"
# WARNING: it is platform specific!


# ----------------------------------------------------------------------------------- #
# init

# libs
# library("oc")
# devtools::load_all(path = "/Users/aa/coding/oc")
devtools::load_all(path = ".")
# check(path = "/Users/aa/coding/oc")

# CHK:
# con console in setup.R la workarea viene creata di default qui e persiste:
# oc::workarea
# "/private/var/folders/9l/1v3xv9w13_lghflpn0lhsb7m0000gn/T/RtmpPZLaEA/R.INSTALL7c8304103db/oc"

# setup
bimestre <- "20181231"
# focus <- "test"
data_path <- "/Users/aa/dati/oc"
# workarea <- NA # MEMO: in DEV_MODE non c'è workarea

# setup
oc_init()


# ----------------------------------------------------------------------------------- #
# prep di po_riclass

# load
progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)

# workflow
make_po_riclass(bimestre)
chk_delta_po_riclass()
chk_delta_po_riclass("OLD")
# HAND: fare aggiornamento a mano e rinominare in "po_riclass_NEW.csv" in "po_riclass.csv"
# WARN: eliminare colonna "OC_DESCR_PROGRAMMA" altrimenti fa conflitto con get_x_vars

# chk
# chk <- progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))
# chk <- progetti %>% filter(OC_CODICE_PROGRAMMA == "COMMTARANTOFSC")
# sum(chk$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)

# TODO: manca la stessa cosa su "_ext"


# ----------------------------------------------------------------------------------- #
# altri prep di data

# TODO:...

# workflow
# make_matrix_po(bimestre)
# chk <- chk_delta_po()
# chk %>% count(OC_DESCRIZIONE_PROGRAMMA)
# chk_delta_po("OLD")
# HAND: fare aggiornamento a mano e rinominare "po_linee_azioni_NEW.csv" in "po_linee_azioni.csv


# make_matrix_cipe(bimestre)
# make_matrix_strum(bimestre)


# ----------------------------------------------------------------------------------- #
# data

# load in package as .rda
source(file.path(getwd(), "setup", "setup_data.R"))


# ----------------------------------------------------------------------------------- #
# progetti_light
# https://readr.tidyverse.org/articles/readr.html#column-specification

setup_light(bimestre = "20181231")




# ----------------------------------------------------------------------------------- #
# documents

# CHK: non ricordo come funziona!!!

# usethis::use_vignette("oc")
devtools::document()


