# OC > Toolkit
# development platform

# rm(list=ls())
library("devtools")


# getwd()
# "/Users/aa/coding/oc"


# ----------------------------------------------------------------------------------- #
# workflow generale
# RStudio + GitHub

# inizializzazione progetto
# $:
# git commit -m "first commit"
# git remote add origin https://github.com/andreoliant/oc.git
# git push -u origin master

# gestione modifiche
# $:
# git add .
# git commit -m "Something"
# git push -u origin master
# ...
# git pull origin master
# MEMO: su può fare direttamente da RStudio


# ----------------------------------------------------------------------------------- #
# workflow per bimestre

# Fare una versione del package per ogni nuovo *bimestre* di monitoraggio.
# Per la nuova versione:
# - modifica "X" in DESCRIPTION (es. 0.1.X); solo in caso di altre modifiche rilevanti
#   salire a livello superiori
# - prep per aggiornare setup/data-raw/po_riclass.csv (con step di confronto manuale con SAS) [non serve "preeteso"]
# - ...
# - prep per aggiornare altro in data (OLD: "data-raw/setup_data.R")
# - setup data nel package
# - setup di progetti_light.csv >>> upload in Drive
# - DO SOMETHING WITH DB PROGARAMMAZIONE
# - ...
# - aggiungere tag in git con $: git tag v0.1.X
# - integra bkp/versioni.csv
# - build, carica su google drive e invia al team


# ----------------------------------------------------------------------------------- #
# init

# libs
# library("octk")
devtools::load_all(path = ".")

# setup
bimestre <- "19000101"
data_path <- "/Users/aa/dati/oc"
# MEMO: in DEV_MODE workarea e focus sono in test

# setup
oc_init()

# CHK:
# con console in setup.R la workarea viene creata di default qui e persiste:
# oc::workarea
# "/private/var/folders/9l/1v3xv9w13_lghflpn0lhsb7m0000gn/T/RtmpPZLaEA/R.INSTALL7c8304103db/oc"


# ----------------------------------------------------------------------------------- #
# prep di po_riclass
# MEMO: parte da "oc_programmi.sas7bdat" e verifica variazioni
# MEMO: parte dalla versione di sviluppo di "octk::po_riclass" che si assume aggiornata al bimestre precedente
# TODO: implementare direttamente da SAD

# TIPO
# 0: programma normale
# 1: programma misto con ":::"
# 2: programma duplicato lato IGRUE (2 programmi con CCI diversi ma uno è vuoto)
# 3: progetti/programma da accorpare (e programma/unione post accorpamento)
# 4: programma censito lato programmazione e ancora da caricare in BDU
# 9: programma disattivato in BDU

library("haven")

# laod da sas
ROOT <- "/Volumes/GoogleDrive/Drive del team"
path <- file.path(ROOT, "DATI", bimestre, "DASAS/DATABASE/oc_programmi.sas7bdat")
po_sas <- read_sas(path)

# chk
chk_match(octk::po_riclass, po_sas, id = "OC_CODICE_PROGRAMMA")
#   area         obs obs_na obs_n  id_n obs_m  id_m
# 1 left         646      0   646   646     0     0
# 2 right        633      0   633   619    27    13
# 3 inner        613      0   613   599    27    13
# 4 semi_left    599      0   599   599     0     0
# 5 anti_left     47      0    47    47     0     0
# 6 semi_right   613      0   613   599    27    13
# 7 anti_right    20      0    20    20     0     0
# MEMO: valori ante consolidamento

#   area         obs obs_na obs_n  id_n obs_m  id_m
# 1 left         663      0   663   663     0     0
# 2 right        633      0   633   619    27    13
# 3 inner        633      0   633   619    27    13
# 4 semi_left    619      0   619   619     0     0
# 5 anti_left     44      0    44    44     0     0
# 6 semi_right   633      0   633   619    27    13
# 7 anti_right     0      0     0     0     0     0
# MEMO: valori post consolidamento

# chk scarti da DB
chk_left <- octk::po_riclass %>%

  anti_join(po_sas %>%
              select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
            by = "OC_CODICE_PROGRAMMA")
write_csv(chk_left, file.path(TEMP, "chk_left.csv"))

# chk nuovi da SAS
chk_right <- po_sas %>%
  select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ) %>%
  anti_join(octk::po_riclass,
            by = "OC_CODICE_PROGRAMMA")
write_csv(chk_right, file.path(TEMP, "chk_right.csv"))

# HAND:
# Integrare a mano a "po_riclass.csv" con:
# - righe di "chk_right" con ":::" >>> entrano tutte dritte
# - altre righe di "chk_right" >>> verificare sovrapposizione con righe gia censite ed eventualmente integrare il DB Programmazione
# - le righe di "chk_lef" restano ma non sono ricomprese in SAS

# OLD:
# progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE, light = FALSE)
# make_po_riclass(bimestre)
# chk_delta_po_riclass("NEW")
# chk_delta_po_riclass("OLD")
# HAND: fare aggiornamento a mano di "po_riclass_NEW.csv" e rinominare in "po_riclass.csv"
# chk <- progetti %>% filter(is.na(OC_CODICE_PROGRAMMA))
# chk <- progetti %>% filter(OC_CODICE_PROGRAMMA == "COMMTARANTOFSC")
# sum(chk$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)


# ----------------------------------------------------------------------------------- #
# altri prep di data

# TODO: verificare se i nomi sono troncati...

# workflow
# make_matrix_po(bimestre)
# chk <- chk_delta_po()
# chk %>% count(OC_DESCRIZIONE_PROGRAMMA)
# chk_delta_po("OLD")
# HAND: fare aggiornamento a mano e rinominare "po_linee_azioni_NEW.csv" in "po_linee_azioni.csv

# TODO: voglio sapere cosa manca in po_linee_azioni rispetto al DB programmazione

# make_matrix_cipe(bimestre)
# make_matrix_strum(bimestre)


# ----------------------------------------------------------------------------------- #
# data

# load in package as .rda
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# progetti_light
# https://readr.tidyverse.org/articles/readr.html#column-specification

setup_light(bimestre = "20181231")




# ----------------------------------------------------------------------------------- #
# documents

# CHK: non ricordo come funziona!!!

# usethis::use_vignette("oc")
devtools::document()



# ----------------------------------------------------------------------------------- #
# build as bundle

# build
# devtools::check(path = "/Users/aa/coding/oc")
devtools::build(pkg = ".", path = "/Users/aa/coding/oc/bkp")
# MEMO: build to boundle "oc_0.1.0.tar.gz"

# install
install.packages("/Users/aa/coding/oc/bkp/octk_0.1.0.tar.gz", repos = NULL, type="source")

# build as binary
# devtools::build(path = "/Users/aa/coding/oc", binary = TRUE)
# MEMO: build to binary "oc_0.1.0.tgz"
# WARNING: it is platform specific!
