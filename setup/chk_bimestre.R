# OC > Toolkit
# development platform
# workflow per update dati bimestrali (verifica data quality)




# ----------------------------------------------------------------------------------- #
# prep di po_riclass
# MEMO: parte da "oc_programmi.sas7bdat" e verifica variazioni
# MEMO: parte dalla versione di sviluppo di "octk::po_riclass" che si assume aggiornata al bimestre precedente
# TODO: implementare direttamente da SAD

# TIPO
# 0: programma normale
# 1: programma misto con ":::"
# 2: programma duplicato lato IGRUE (2 programmi con CCI diversi ma uno è vuoto)
# 3: progetti/programma da accorpare (e programma/unione post accorpamento fino a 0.2.5)
# 4: programma censito lato programmazione e ancora da caricare in BDU
# 5: programma/unione post accorpamento (sostituisce casi con 3 da 0.2.6)
# 6: programma fittizio per completamenti
# 8: programma da verificare
# 9: programma disattivato in BDU

# load da DB programmazione
po <- octk::po_riclass %>%
  filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disattivati
         x_CICLO != "2000-2006",
         x_AMBITO != "FEASR") %>%
  filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))

# DEV: do something here!!!!!! per ora vedi sotto


# ----------------------------------------------------------------------------------- #
#  OLD: verifica po_riclass su DB 

programmi <- init_programmazione(add_713 = TRUE, export = FALSE) %>%
  count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)

# chk
chk_match(po, programmi, id = "OC_CODICE_PROGRAMMA")

# chk nuovi da OCTK
chk_left <- po %>%
  select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ) %>%
  anti_join(programmi,
            by = "OC_CODICE_PROGRAMMA")
write_csv(chk_left, file.path(TEMP, "chk_left.csv"))

# chk scarti da DB
chk_right <- programmi %>%
  anti_join(po %>%
              select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
            by = "OC_CODICE_PROGRAMMA")
write_csv(chk_right, file.path(TEMP, "chk_right.csv"))


# load in package as .rda
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# OLD: verifica po_riclass su sas

# laod da sas
# path <- "/Volumes/GoogleDrive/Drive condivisi/DATI/20190831/DASAS/DATABASE/oc_programmi.sas7bdat"
path <- file.path(DATA, "oc_programmi.sas7bdat")
po_sas <- read_sas(path)
# MEMO: oc_programmi.sas7bdat scaricato a mano e messo in DATA


# chk
chk_match(po, po_sas, id = "OC_CODICE_PROGRAMMA")
# area         obs obs_na obs_n  id_n obs_m  id_m
# <chr>      <int>  <int> <int> <int> <int> <int>
# 1 left         367      0   367   365     4     2
# 2 right        652      0   652   638    27    13
# 3 inner        303      0   303   287    31    15
# 4 semi_left    289      0   289   287     4     2
# 5 anti_left     78      0    78    78     0     0
# 6 semi_right   301      0   301   287    27    13
# 7 anti_right   351      0   351   351     0     0

# chk scarti da DB
chk_left <- po %>%
  anti_join(po_sas %>%
              select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
            by = "OC_CODICE_PROGRAMMA")
write_csv(chk_left, file.path(TEMP, "chk_left.csv"))

# chk nuovi da SAS
chk_right <- po_sas %>%
  select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ) %>%
  anti_join(po,
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

# Legenda per po_riclass$TIPO:
# 0: programma normale
# 1: programma misto con ":::"
# 2: programma duplicato lato IGRUE (2 programmi con CCI diversi ma uno è vuoto)
# 3: progetti/programma da accorpare (e programma/unione post accorpamento)
# 4: programma censito lato programmazione e ancora da caricare in BDU
# 9: programma disattivato in BDU


# ----------------------------------------------------------------------------------- #
# chk non visualizzati e delta da bimestre precedente

# loads
bimestre_old <- "20200630"
# bimestre_old <- "20191031"
# OLD: data_path_old <- file.path(dirname(dirname(dirname(DATA))), bimestre_old, "DASAS", "DATAMART")
data_path_old <- file.path(dirname(DATA), bimestre_old)
progetti_all_old <- load_progetti(bimestre = bimestre_old,
                                  data_path = data_path_old,
                                  visualizzati = FALSE, debug = TRUE)

progetti_all <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE)


# chk disattivati e nuovi
chk_match(progetti_all_old, progetti_all, id = "COD_LOCALE_PROGETTO")

# test nuovi programmi assenti in po_riclass (FIX da sopra)
progetti_all %>% 
  count(OC_CODICE_PROGRAMMA) %>%
  anti_join(po, by = "OC_CODICE_PROGRAMMA") %>%
  filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))

source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")

# non visualizzati
chk <- progetti_all_old %>%
  # mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI", # MEMO: patch per 20190630
  #                                      TRUE ~ FONDO_COMUNITARIO)) %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  group_by(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
  summarise(N = n(),
            CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)) %>%
  full_join(progetti_all %>%
              # mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI", # MEMO: patch per 20190630
              #                                      TRUE ~ FONDO_COMUNITARIO)) %>%
              fix_progetti(.) %>%
              get_x_vars(.) %>%
              group_by(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
              summarise(N = n(),
                        CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)),
            by = c("OC_FLAG_VISUALIZZAZIONE", "x_CICLO", "x_AMBITO"), suffix = c(".old", ".new")) %>%
  mutate(N.chk = N.new - N.old,
         CP.chk = CP.new - CP.old) %>%
  select(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO,
         N.old, N.new, N.chk,
         CP.old, CP.new, CP.chk)

write.csv2(chk, file.path(TEMP, paste0("chk_delta_noviz_", bimestre, ".csv")), row.names = FALSE)



# singoli progetti
chk2 <- progetti_all_old %>%
  # mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI", # MEMO: patch per 20190630
  #                                      TRUE ~ FONDO_COMUNITARIO)) %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  # group_by(OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO) %>%
  select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO, CP = OC_FINANZ_TOT_PUB_NETTO) %>%
  full_join(progetti_all %>%
              # mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI",
              #                                      TRUE ~ FONDO_COMUNITARIO)) %>%
              fix_progetti(.) %>%
              get_x_vars(.) %>%
              select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_FLAG_VISUALIZZAZIONE, x_CICLO, x_AMBITO, CP = OC_FINANZ_TOT_PUB_NETTO),
            by = "COD_LOCALE_PROGETTO", suffix = c(".old", ".new")) %>%
  mutate(CP.chk = CP.new - CP.old)

# ricerca progetti persi
temp <- chk2 %>% filter(is.na(x_AMBITO.old), is.na(x_AMBITO.new))
temp <- chk2 %>% filter(!is.na(OC_CODICE_PROGRAMMA.new), x_AMBITO.old != "FEAMP", is.na(x_AMBITO.new))
temp %>% filter(is.na(x_AMBITO.old))
write.csv2(temp, file.path(TEMP, paste0("chk_ambito_na.csv")), row.names = FALSE)

# chk vari
progetti_all_old %>%
  filter(is.na(OC_CODICE_PROGRAMMA))

temp <- progetti_all_old %>% 
  filter(COD_LOCALE_PROGETTO == "7PUA1006.392")

temp2 <- progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.)

temp2 %>% count(x_AMBITO)

temp3 <- temp2 %>% 
  filter(is.na(x_AMBITO)) %>%
  select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, FONDO_COMUNITARIO, OC_FLAG_VISUALIZZAZIONE)

write.csv2(temp3, file.path(TEMP, paste0("chk_fondo_comunitario_na.csv")), row.names = FALSE)


# chk2 %>% arrange(desc(CP.chk))
# chk2 %>% filter(is.na(x_AMBITO.new), OC_FLAG_VISUALIZZAZIONE.new == 0) %>% count(OC_CODICE_PROGRAMMA.new)
# chk2 %>% filter(x_AMBITO.new == "FESR", OC_FLAG_VISUALIZZAZIONE.new == 0) %>% count(OC_FLAG_VISUALIZZAZIONE.old)
#
# temp <- chk2 %>% filter(is.na(x_CICLO.old), is.na(x_CICLO.new))
#
#                         # == "2007-2013", x_AMBITO.new == "FSC", OC_FLAG_VISUALIZZAZIONE.new == 0, CP.chk > 0)
#
# temp <- chk2 %>% filter(x_CICLO.new == "2007-2013", x_AMBITO.new == "FSC", OC_FLAG_VISUALIZZAZIONE.new == 0, CP.chk > 0)
# write_csv2(temp, file.path(TEMP, "chk_fsc713.csv"))



# chk mismatch con po_riclass
temp <- progetti_all %>%
  # mutate(FONDO_COMUNITARIO = case_when(FONDO_COMUNITARIO == "Y.E.I"~ "YEI", # MEMO: patch per 20190630 e 20190831
  #                                      TRUE ~ FONDO_COMUNITARIO)) %>%
  get_x_vars(.) %>%
  filter(is.na(x_CICLO))

write_csv2(temp, file.path(TEMP, "chk_missing.csv"))



# OC_CODICE_PROGRAMMA FONDO_COMUNITARIO      n
# <chr>               <chr>              <int>
# 1 2014IT05M9OP001     Y.E.I             129868
# 2 2014IT14MFOP001     NA                    34

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

# chk anomalie ":::CCI"
# chk <- progetti %>%
#   filter(grepl("^:::2017POIMPCOMFSC", OC_CODICE_PROGRAMMA))

# chk %>%
#   get_x_vars(.) %>%
#   count(OC_CODICE_PROGRAMMA, x_CICLO, X_CICLO, x_AMBITO, X_AMBITO)


# verifica x_vars
# progetti <- fix_progetti(progetti) # MEMO: questo va escluso prima e aggiunto poi per testare se il fix funziona ancora o va integrato
appo <- get_x_vars(progetti)
# appo <- get_macroarea(appo, real_reg=TRUE)
# appo <- get_regione_simply(appo)
# appo <- refactor_progetti(appo)
appo %>%
  count(x_CICLO, X_CICLO, x_AMBITO, X_AMBITO)

# chk mismatch su ambito
chk <- appo %>%
  filter(x_AMBITO == "POC", X_AMBITO == "FESR-FSE") %>%
  mutate(CHK = "poc>fesr-fse") %>%
  bind_rows(appo %>%
              filter(x_AMBITO == "POC", X_AMBITO == "FESR") %>%
              mutate(CHK = "poc>fesr")) %>%
  bind_rows(appo %>%
              filter(x_AMBITO == "FSC", X_AMBITO == "FESR-FSE") %>%
              mutate(CHK = "fsc>fesr-fse"))

write_csv2(chk, file.path(TEMP, paste0("chk_mismatch_ambito_", bimestre, ".csv")))

chk %>%
  count(OC_CODICE_PROGRAMMA, x_CICLO, X_CICLO, x_AMBITO, X_AMBITO, OC_COD_FONTE)
# OC_CODICE_PROGRAMMA x_CICLO   X_CICLO   x_AMBITO X_AMBITO OC_COD_FONTE     n
# <chr>               <chr>     <chr>     <fct>    <chr>    <chr>        <int>
# 1 2016POCIMPRESE1     2014-2020 2014-2020 POC      FESR     FS1420          10
# 2 2017FSCRICERCA      2014-2020 2014-2020 FSC      FESR-FSE FS1420          42
# 3 2017POCRICERCA1     2014-2020 2014-2020 POC      FESR-FSE FS1420          30


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
# MEMO: qui va aggiornato lato SAS # ok per 20190630 e seguenti

chk %>%
  count(x_PROGRAMMA)

# test
appo <- get_macroarea(appo, real_reg=TRUE)
appo <- get_regione_simply(appo)

chk <- appo %>%
  count(x_MACROAREA, x_REGNAZ, x_REGIONE)

rm(appo, progetti)







