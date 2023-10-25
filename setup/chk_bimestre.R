# OC > Toolkit
# development platform
# workflow per update dati bimestrali (verifica data quality)




# ----------------------------------------------------------------------------------- #
# prep di po_riclass
# MEMO: parte dalla versione di sviluppo di "octk::po_riclass" che si assume aggiornata al bimestre precedente

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
         # x_AMBITO != "FEASR",
         x_CICLO != "2000-2006") %>%
  filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))


# TODO: qui va inserito sistema per rigenerare po_riclass direttamente dal DB

# ----------------------------------------------------------------------------------- #
# # verifica po_riclass su DB programmazione
# 
# 
# programmi <- init_programmazione_dati(use_713 = TRUE, use_po_psc = TRUE) %>%
#   rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA) %>%
#   count(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
# 
# # write_csv2(programmi, file.path(TEMP = "chk_po_riclass.csv"))
# 
# # chk
# chk_match(po, programmi, id = "OC_CODICE_PROGRAMMA")
# 
# # chk nuovi da OCTK
# chk_left <- po %>%
#   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ) %>%
#   anti_join(programmi,
#             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO"))
# write_csv(chk_left, file.path(TEMP, "chk_left.csv"))
# 
# # chk scarti da DB
# chk_right <- programmi %>%
#   anti_join(po %>%
#               select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ),
#             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO"))
# write_csv(chk_right, file.path(TEMP, "chk_right.csv"))
# 
# 
# # HAND:
# # Integrare a mano a "po_riclass.csv" con:
# # - righe di "chk_right" con ":::" >>> entrano tutte dritte
# # - altre righe di "chk_right" >>> verificare sovrapposizione con righe gia censite ed eventualmente integrare il DB Programmazione
# # - le righe di "chk_lef" restano ma non sono ricomprese in programmazione ()
# 
# 
# # load in package as .rda
# source(file.path(getwd(), "setup", "setup_data.R"))
# devtools::load_all(path = ".")
# 
# 
# ----------------------------------------------------------------------------------- #
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
# chk nuovi programmi da preesteso


#load
progetti_all <- load_progetti(bimestre = bimestre, visualizzati = FALSE, debug = TRUE)


# chk mismatch con po_riclass (trova nuovi programmi)
progetti_all %>% 
  count(OC_CODICE_PROGRAMMA) %>%
  anti_join(octk::po_riclass, by = "OC_CODICE_PROGRAMMA") %>%
  filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))
# MEMO: qui non controlla i programmi ":::" che sono esclusi a monte in po

# progetti_all %>% 
#   filter(OC_CODICE_PROGRAMMA == "POCMOLISE") %>% 
#   count(OC_MACROAREA, DEN_REGIONE)

# chk mismatch con get_x_vars per i programmi misti e anomalie
progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  filter(is.na(x_CICLO) | is.na(x_AMBITO)) %>%
  count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
# MEMO: qui dovrennero rimanere come problematici solo i programmi ":::"

# CHK:
# OC_CODICE_PROGRAMMA OC_DESCRIZIONE_PROGRAMMA                n
# <chr>               <chr>                               <int>
# 2014IT05M9OP001     PON INIZIATIVA OCCUPAZIONE GIOVANI 214837

# CHK YEI:
progetti_all %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>% count(FONDO_COMUNITARIO)
# FONDO_COMUNITARIO      n
# <chr>              <int>
# 1 FSE                  256
# 2 IOG                  887
# 3 IOG::             214837 #prima era NA
progetti_all %>% fix_progetti() %>% filter(OC_CODICE_PROGRAMMA == "2014IT05M9OP001") %>% count(FONDO_COMUNITARIO)
# FONDO_COMUNITARIO      n
# <chr>              <int>
# 1 FSE                  256
# 2 YEI               215724

# chk fondo comunitario
progetti_all %>% filter(OC_CODICE_PROGRAMMA == "2014IT16M2OP006") %>% count(FONDO_COMUNITARIO)

# chk dupli
progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  count(COD_LOCALE_PROGETTO) %>%
  filter(n > 1)

# HAND: integra po_riclass.csv (e DB in caso di codifiche assenti)
# HAND: integra fix_progetti() per gestire anomalie

# reload
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# controllo su DBCOE

# TODO: inserire di nuovo use_po_psc = TRUE (ma serve prima sviluppo) altrimenti ci sono troppi programmi (es. PRA)
# DB <- file.path(dirname(DB), "20230630.00")

programmi <- init_programmazione_dati(use_713 = TRUE, use_po_psc = FALSE) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA) %>%
    count(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)

chk <- progetti_all %>% 
  count(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, X_CICLO, X_AMBITO) %>%
  anti_join(programmi, by = "OC_CODICE_PROGRAMMA") %>%
  filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))

# HAND: integra po_riclass.csv (e DB in caso di codifiche assenti)
# HAND: integra fix_progetti() per gestire anomalie

# reload
source(file.path(getwd(), "setup", "setup_data.R"))
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# chk non visualizzati e delta da bimestre precedente

# loads
bimestre_old <- "20230630"
# OLD: data_path_old <- file.path(dirname(dirname(dirname(DATA))), bimestre_old, "DASAS", "DATAMART")
data_path_old <- file.path(dirname(DATA), bimestre_old)
progetti_all_old <- load_progetti(bimestre = bimestre_old,
                                  data_path = data_path_old,
                                  visualizzati = FALSE, debug = TRUE)



# chk disattivati e nuovi
# chk_match(progetti_all_old, progetti_all, id = "COD_LOCALE_PROGETTO")


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

# write.csv2(chk, file.path(TEMP, paste0("chk_delta_noviz_", bimestre, ".csv")), row.names = FALSE)
write.xlsx(chk, file.path(TEMP, paste0("chk_delta_noviz_", bimestre, ".xlsx")))



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
# temp <- chk2 %>% filter(!is.na(OC_CODICE_PROGRAMMA.new), x_AMBITO.old != "FEAMP", is.na(x_AMBITO.new))
# temp <- chk2 %>% filter(is.na(x_AMBITO.old))
# temp %>% filter(x_AMBITO.new == "FSC")
temp <- chk2 %>% filter(x_AMBITO.old == "PAC", x_AMBITO.new == "FESR")
write.xlsx(temp, file.path(TEMP, paste0("chk_nuovi_progetti.xlsx")), row.names = FALSE)


temp <- chk2 %>% filter(abs(CP.chk) > 1000000)
write.xlsx(temp, file.path(TEMP, paste0("chk_variazione_progetti.xlsx")), row.names = FALSE)

chk3 <- progetti_all %>% 
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  filter(x_AMBITO == "FESR" | x_AMBITO == "FSE") %>% 
  filter(x_CICLO == "2014-2020") %>% 
  group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, FONDO_COMUNITARIO) %>% 
  summarise(N = n(),
            FTP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))

# progetti_all %>% 
#   filter(OC_CODICE_PROGRAMMA == "2014IT05M2OP002") %>% 
#   count(FONDO_COMUNITARIO)

chk <- progetti_all %>% 
  filter(FONDO_COMUNITARIO == "FESR:")

  
# ----------------------------------------------------------------------------------- #
# verifica x_vars


chk <- progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  count(x_CICLO, X_CICLO, x_AMBITO, X_AMBITO)

# chk <- progetti_all %>%
#   fix_progetti(.) %>%
#   get_x_vars(.) %>%
#   filter(x_AMBITO == "FSC", X_AMBITO == "FESR")
# MEMO: progetti migrati su PSC, vince x_AMBITO

chk <- progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  filter(x_CICLO == "2021-2027", X_CICLO == "2014-2020")
# MEMO: cis sisma, vince x_CICLO

progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  count(x_REGNAZ, X_REGNAZ) %>%
  filter(x_REGNAZ != X_REGNAZ)

# chk <- progetti_all %>%
#   fix_progetti(.) %>%
#   get_x_vars(.) %>%
#   filter(x_REGNAZ == "NAZ", X_REGNAZ == "SARDEGNA")

# x_REGNAZ X_REGNAZ     n
# <chr>    <chr>    <int>
# 1 LAZIO    NAZ          1 # POR vs Piano Impresa (vince POR)
# 2 NAZ      ABRUZZO      4 # GSSI
# 3 NAZ      CAMPANIA    27 # METANIZZAZIONE vs PATTO (vince METANIZZAZIONE)
# 4 NAZ      MOLISE      47 # CIS MOLISE (va a coumuni e non a regione)
# 5 TOSCANA  NAZ          2 # Piombino

appo <- progetti_all %>%
  fix_progetti(.) %>%
  get_x_vars(.) %>%
  get_macroarea(., progetti_all, real_reg=TRUE) %>%
  get_regione_simply(., progetti_all) 

chk <- appo %>%
  count(x_MACROAREA, x_REGNAZ, x_REGIONE) %>%
  filter(x_REGNAZ != x_REGIONE)

appo %>%
  filter(x_REGNAZ == "PA BOLZANO", x_REGIONE == "PA TRENTO", x_MACROAREA == "Centro-Nord") %>%
  count(x_CICLO, x_AMBITO, x_PROGRAMMA, x_MACROAREA, x_REGNAZ, x_REGIONE)
# CHK: per i programmi 713 che forzo su Mezzogiorno restano disallineamenti con x_REGIONE 


# chk macroarea
appo %>% 
  mutate(OC_MACROAREA = case_when(OC_MACROAREA == "Ambito Nazionale" ~ "Ambito nazionale",
                                  OC_MACROAREA ==	"Non definibile" ~ "Trasversale",
                                  TRUE ~ OC_MACROAREA)) %>% 
  count(x_MACROAREA, OC_MACROAREA) %>% 
  write.xlsx(., file.path(TEMP, "chk_macroarea.xlsx"))

appo %>% 
  mutate(OC_MACROAREA = case_when(OC_MACROAREA == "Ambito Nazionale" ~ "Ambito nazionale",
                                  OC_MACROAREA ==	"Non definibile" ~ "Trasversale",
                                  TRUE ~ OC_MACROAREA)) %>% 
  filter(x_MACROAREA != OC_MACROAREA) %>% 
  count(x_AMBITO, x_CICLO, x_PROGRAMMA, x_REGIONE, x_MACROAREA, DEN_REGIONE, OC_MACROAREA) %>% 
  write.xlsx(., file.path(TEMP, "chk_macroarea_progetti.xlsx"))

rm(progetti_all, progetti_all_old, appo)


