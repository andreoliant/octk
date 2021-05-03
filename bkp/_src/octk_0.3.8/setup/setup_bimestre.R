# OC > Toolkit
# development platform
# workflow per update dati bimestrali


# ----------------------------------------------------------------------------------- #
# workflow per bimestre

# this: 0.2.1 > v0.2.1

# Fare una versione del package per ogni nuovo *bimestre* di monitoraggio.
# Per la nuova versione:
# - modifica "X" in DESCRIPTION (es. 0.1.X); solo in caso di altre modifiche rilevanti
#   salire a livello superiori
# - HAND: aggiorna DB per coerenza
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
# prep di dataset in octk


progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = FALSE)
progetti <- fix_progetti(progetti)

# po_linee_azioni.csv
make_matrix_po(bimestre)
chk <- chk_delta_po("NEW")
chk %>% count(OC_DESCRIZIONE_PROGRAMMA)
chk <- chk_delta_po("OLD")
# HAND: rinominare "po_linee_azioni_NEW.csv" in "po_linee_azioni.csv

# TODO: voglio sapere cosa manca in po_linee_azioni rispetto al DB programmazione

# strum_att.csv
make_matrix_strum(bimestre)

# delib_cipe.csv
make_matrix_cipe(bimestre)

# prog_comp.csv
make_prog_comp(bimestre)

# patt.csv
make_patt(bimestre)


# TODO: estendere lista RA

# progetti %>%
#   count(COD_RISULTATO_ATTESO, DESCR_RISULTATO_ATTESO) %>%
#   filter(!(grepl(":::", COD_RISULTATO_ATTESO))) %>%
#   write_csv2(file.path(TEMP, "ra.csv"))


# ----------------------------------------------------------------------------------- #
# data

# load in package as .rda
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")



# ----------------------------------------------------------------------------------- #
# fix per variabili covid

# MEMO: il file "" in DATA viene dalla versione di "PROGETTI_PREESTESO.csv" del 05/03/2021
# appo <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE)
# 
# appo1 <- appo %>%
#   distinct(COD_LOCALE_PROGETTO, OC_FLAG_TAG_BENICONF, COVID)
# 
# write.csv2(appo1, file.path(DATA, "flag_beniconf_covid.csv"), row.names = FALSE)
# 
# # MEMO: la versione di "PROGETTI_PREESTESO.csv" del 05/03/2021 poi è stata cancellata per ripristino della precedente del 22/02/2021
# progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE, light = FALSE)
# appo1 <- read_csv2(file.path(DATA, "flag_beniconf_covid.csv"), guess_max = 1200000)
# 
# progetti_2 <- progetti %>%
#   left_join(appo1, by = "COD_LOCALE_PROGETTO")
# 
# write.csv2(progetti_2, file.path(DATA, "PROGETTI_PREESTESO.csv"), row.names = FALSE)


# ----------------------------------------------------------------------------------- #
# progetti_light e operazioni
# https://readr.tidyverse.org/articles/readr.html#column-specification

# progetti light
setup_light(bimestre, fix = TRUE)
# setup_light(bimestre, fix = FALSE)

# operazioni light
# progetti <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE, light = FALSE)
# progetti <- fix_progetti(progetti)
# setup_operazioni(bimestre, progetti, export=TRUE, debug=TRUE)
setup_operazioni(bimestre, use_sito=TRUE, export=TRUE, debug=TRUE)

# chk vuoti
rm(progetti)
progetti <- read_csv2(file.path(DATA, paste0("progetti_light_", bimestre, ".csv")), guess_max = 1000000)
progetti %>% count(x_CICLO, x_AMBITO)
sum(progetti$OC_FINANZ_TOT_PUB_NETTO, na.rm=TRUE)

operazioni <- read_csv2(file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), guess_max = 1000000)
operazioni %>% count(x_CICLO, x_AMBITO)


# chk mismatch progetti vs operazioni
chk <- progetti %>%
  # get_x_vars(.) %>%
  select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  full_join(operazioni %>%
              select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO),
            by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA")) %>%
  mutate(CHK = x_AMBITO.x == x_AMBITO.y) %>%
  filter(CHK == FALSE)

chk %>%
  count(OC_CODICE_PROGRAMMA, x_AMBITO.x, x_AMBITO.y)
  # OC_CODICE_PROGRAMMA x_AMBITO.x x_AMBITO.y     n
# <chr>               <fct>      <chr>      <int>
# 1 2007IT001FA005      FSC        POC            6
# 2 2007IT005FAMG1      FSC        POC            4
# MEMO: se sono solo questi sopra è ok per si tratta di sdoppiamenti forzati per direttrici ferroviarie e giustizia civile

write_csv2(chk, file.path(TEMP, "chk_mismatch_progetti_operazioni.csv"))

# MEMO:
# una parte del problema è direttrici ferroviarie e giustizia civile
# poi però ci sono altre anomalie che non comprendo
# però quelli del fix sul CCI sembrano corretti

# patch operazioni in "2014IT16M2OP002:::2016PATTIPUG" senza FONDO_COMUNITARIO
# chk <- operazioni %>%
#   filter(is.na(x_AMBITO))
# chk %>% count(FONDO_COMUNITARIO, OC_CODICE_PROGRAMMA)
# 
# operazioni_rev <- operazioni %>%
#   mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT16M2OP002" & is.na(FONDO_COMUNITARIO) ~ "FESR",
#                               TRUE ~ x_AMBITO))
# write.csv2(operazioni_rev, file.path(DATA, paste0("operazioni_light_", bimestre, ".csv")), row.names = FALSE)


# ----------------------------------------------------------------------------------- #
# chk 

chk <- progetti %>%
  count(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, OC_CODICE_PROGRAMMA)


