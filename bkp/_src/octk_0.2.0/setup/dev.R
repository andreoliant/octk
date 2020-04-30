# OC > Toolkit
# development platform

# rm(list=ls())
library("devtools")


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
# git pull origin master
# ...
# git add .
# git commit -m "Something"
# git push -u origin master
# MEMO: su può fare direttamente da RStudio

# clona in altro folder fino a un tag
# git clone /Users/aa/coding/oc --branch v0.1.0 new_folder
# git clone <orign_path> --branch <tag_name> <dest_path>


# ----------------------------------------------------------------------------------- #
# add packages

# usethis::use_package("dplyr") # 'tidyverse' is a meta-package and it is rarely a good idea to depend on it
# usethis::use_package("haven")
# usethis::use_package("readxl")


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
# - build con ...
# - ...
# - aggiungere tag in git con $: git tag v0.1.X
# - copia in bkp/_src/octk_0.1.X e fai zip octk_0.1.X.zip
# - integra bkp/versioni.csv
# - carica su google drive
# - invia mail al team


# ----------------------------------------------------------------------------------- #
# init

# libs
devtools::load_all(path = ".")

# setup
oc_init(
  bimestre = "20190228",
  db_ver = "20190620",
  data_path = "/Users/aa/dati/oc",
  use_drive=TRUE,
  DEV_MODE=TRUE
  )

# /Users/aa/dati/oc

# oc_init(
#   bimestre = "20181231",
#   db_ver = "20190620",
#   data_path = "/Users/aa/dati/oc",
#   use_drive=TRUE,
#   DEV_MODE=TRUE
# )
# MEMO: per il setup bimestrale la workarea è in locale oc/test e i dati sono in GoogleDrive

# copy data from GoogleDrive to local
oc_init_data(
  bimestre = "20190228",
  data_path = "/Users/aa/dati/oc"
)
# TODO: serve versione con package "googledrive"


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

# laod da sas
# path <- "/Volumes/GoogleDrive/Drive condivisi/DATI/20190228/DASAS/DATABASE/oc_programmi.sas7bdat"
path <- file.path(DATA, "oc_programmi.sas7bdat")
po_sas <- read_sas(path)
# MEMO: oc_programmi.sas7bdat scaricato a mano e messo in DATA


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
# chk non visualizzati e delta

# loads
bimestre_old <- "20181231"
# OLD: data_path_old <- file.path(dirname(dirname(dirname(DATA))), bimestre_old, "DASAS", "DATAMART")
data_path_old <- file.path(dirname(DATA), bimestre_old)
progetti_all_old <- load_progetti(bimestre = bimestre_old,
                                  data_path = data_path_old,
                                  visualizzati = FALSE, debug = TRUE)

progetti_all <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE)

# variazione non visualizzati
chk <- progetti_all_old %>%
  get_x_vars(progetti) %>%
  count(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
  full_join(progetti_all %>%
              get_x_vars(progetti) %>%
              count(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO),
            by = c("OC_FLAG_VISUALIZZAZIONE", "x_CICLO", "x_AMBITO"), suffix = c(".old", ".new")) %>%
  mutate(chk = n.old - n.new)

write.csv2(chk, file.path(OUTPUT, "chk_delta_noviz.csv"), row.names = FALSE)

# verifica anomalia 20181231
# delta_old <- read_csv2(file.path(INPUT, "chk_delta_preesteso.csv"))
# delta_old %>% semi_join(progetti)
# # CHK: capire perché questi "non visualizzati" a dicembre sono stati recuperati
#
# delta_old %>%
#   # semi_join(progetti_all) %>%
#   semi_join(progetti) %>%
#   filter(X_PROGRAMMA == "PS AREE METROPOLITANE LOMBARDIA")
# # MEMO: questi 2 sono stati eliminati

rm(progetti_all, progetti_all_old)


# ----------------------------------------------------------------------------------- #
# verifica x_vars

progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = FALSE)

# verifica x_vars
# progetti <- fix_progetti(progetti)
appo <- get_x_vars(progetti)
# appo <- get_macroarea(appo, real_reg=TRUE)
# appo <- get_regione_simply(appo)
# appo <- refactor_progetti(appo)
appo %>%
  count(x_CICLO, X_CICLO, x_AMBITO, X_AMBITO)

chk <- appo %>%
  filter(x_AMBITO == "FSC", X_AMBITO == "FESR")

# verifica calabria
# chk <- appo %>%
#   filter(is.na(x_AMBITO))
# chk %>%
#   count(OC_CODICE_PROGRAMMA, x_PROGRAMMA)
# write.csv2(chk, file.path(OUTPUT, "anomalie_calabria.csv"))

# A tibble: 1 x 3
# OC_CODICE_PROGRAMMA x_PROGRAMMA               n
# 2014IT16M2OP006     POR CALABRIA FESR-FSE    27

# HAND: aggiorna po_riclass.csv e ricarica con script sotto
# HAND: aggiorna fix_progetti in loaders.R

chk <- appo %>%
  count(x_REGNAZ, X_REGNAZ)

chk <- appo %>%
  filter(x_REGNAZ == "NAZ", X_REGNAZ != "NAZ")
# MEMO: qui va aggiornato lato SAS

chk %>%
  count(x_PROGRAMMA)

# test
appo <- get_macroarea(appo, real_reg=TRUE)
appo <- get_regione_simply(appo)

chk <- appo %>%
  count(x_MACROAREA, x_REGNAZ, x_REGIONE)


# ----------------------------------------------------------------------------------- #
# verifica data quality

# HAND:: verifica numero di progetti su mail per Stefano
appo %>%
  # count(x_CICLO, x_AMBITO)
  count(x_CICLO)
# x_CICLO        n
# 2007-2013 949928
# 2014-2020 296934 >>> 296924 nella mail per Stefano

appo %>%
  group_by(x_CICLO) %>%
  summarise(CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))

appo %>%
  filter(is.na(OC_CODICE_PROGRAMMA)) %>%
  count(x_CICLO, x_AMBITO)


chk <- appo %>%
  count(DEN_REGIONE)

# verifica mascheramenti
appo %>%
  mutate(MASK = grepl("\\*INDIVIDUO\\*", toupper(OC_TITOLO_PROGETTO))) %>%
  count(CUP_COD_NATURA, CUP_DESCR_NATURA, MASK) %>%
  spread(MASK, n)

chk <- appo %>%
  mutate(MASK = grepl("\\*INDIVIDUO\\*", toupper(OC_TITOLO_PROGETTO))) %>%
  filter(CUP_COD_NATURA == "03" & MASK == TRUE) %>%
  select(OC_TITOLO_PROGETTO, CUP_DESCR_CATEGORIA)

chk %>%
  count(CUP_DESCR_CATEGORIA)


# verifica cambiamenti nomi variabili
# "OC_COD_CATEGORIA_SPESA" "OC_DESCR_CATEGORIA_SPESA" >>> peri_query.R >>> query_ue()
# "COD_RISULTATO_ATTESO" "DESCR_RISULTATO_ATTESO"

appo %>%
  filter(x_AMBITO != "FEASR") %>%
  count(x_CICLO, OC_STATO_PROCEDURALE)







# ----------------------------------------------------------------------------------- #
# prep di dataset in octk

# TODO: verificare se i nomi sono troncati...

# po_linee_azioni.csv
make_matrix_po(bimestre = "20190228")
chk <- chk_delta_po("NEW")
chk %>% count(OC_DESCRIZIONE_PROGRAMMA)
chk <- chk_delta_po("OLD")
# HAND: rinominare "po_linee_azioni_NEW.csv" in "po_linee_azioni.csv

# TODO: voglio sapere cosa manca in po_linee_azioni rispetto al DB programmazione

# strum_att.csv
make_matrix_strum(bimestre = "20190228")

# delib_cipe.csv
make_matrix_cipe(bimestre = "20190228")

# TODO: inserire matrix per PATT e per progetto complesso


# ----------------------------------------------------------------------------------- #
# data

# load in package as .rda
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# progetti_light
# https://readr.tidyverse.org/articles/readr.html#column-specification

# setup_light(bimestre = "20181231")
setup_light(bimestre = "20190228")
# TODO: aggiungere nuove variabili di progetti

#  oggetto "OC_FINANZ_UE_NETTO" non trovato

# setup_progetti_descr(bimestre = "20181231")

# analisi peso variabili
# progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = FALSE)
# for (var in names(progetti)) {
#   appo <- progetti %>% select(var)
#   print(paste0(var, ": ", object.size(appo)))
#   write.csv2(appo, file.path(TEMP, "prova_peso", paste0(var, ".csv")), row.names = FALSE)
# }

# ----------------------------------------------------------------------------------- #
# documents

# usethis::use_vignette("oc")
devtools::document()


# ----------------------------------------------------------------------------------- #
# build as bundle

# build
devtools::load_all(path = ".")
devtools::check(path = "/Users/aa/coding/oc")
devtools::build(pkg = ".", path = "/Users/aa/coding/oc/bkp")
# MEMO: build to boundle "oc_0.2.0.tar.gz"

# install
install.packages("/Users/aa/coding/oc/bkp/oc_0.2.0.tar.gz", repos = NULL, type="source")

# build as binary
# devtools::build(path = "/Users/aa/coding/oc", binary = TRUE)
# MEMO: build to binary "oc_0.1.0.tgz"
# WARNING: it is platform specific!


# ----------------------------------------------------------------------------------- #
# backup source

# $:
# VERS="octk_0.2.0"
# cp oc.Rproj bkp/_src/$VERS/
# cp README.md bkp/_src/$VERS/
# cp DESCRIPTION bkp/_src/$VERS/
# cp NAMESPACE bkp/_src/$VERS/
# cp -r setup bkp/_src/$VERS/
# cp -r R bkp/_src/$VERS/
# cp -r data bkp/_src/$VERS/
# cp -r vignettes bkp/_src/$VERS/
# cp -r man bkp/_src/$VERS/
# cp -r inst bkp/_src/$VERS/


# ----------------------------------------------------------------------------------- #
# google drive sync

# $:
# DEV_BKP="/Users/aa/coding/oc/bkp/"
# GOOGLE='/Volumes/GoogleDrive/Drive condivisi/TOOLS/OCTK'
# rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"




