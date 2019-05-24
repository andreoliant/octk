# OC > Futura
# Tools per analisi su 2021-2027

# Sovrascrive COD_REGIONE per programmi regionali
# get_real_reg <- function(df, debug_mode=FALSE) {
#
#   # df <- progetti[1000000:1000200, c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "COD_REGIONE")]
#   # df <- get_x_vars(df)
#
#   # MEMO: richiede e sovrascrive COD_REGIONE
#
#   appo <- po_riclass_ext %>%
#     filter(!is.na(OC_CODICE_PROGRAMMA)) %>%
#     select(OC_CODICE_PROGRAMMA, FLT) %>%
#     mutate(COD_REGIONE_NEW =
#              case_when(FLT == "PIEMONTE" ~ "001",
#                        FLT == "VALLE D'AOSTA" ~ "002", # "VALLE D'AOSTA"
#                        FLT == "LOMBARDIA" ~ "003",
#                        FLT == "PA TRENTO" ~ "004", # "TRENTINO-ALTO ADIGE"
#                        FLT == "PA BOLZANO" ~ "004", # "TRENTINO-ALTO ADIGE"
#                        FLT == "VENETO" ~ "005",
#                        FLT == "FRIULI-VENEZIA GIULIA" ~ "006", # "FRIULI-VENEZIA GIULIA"
#                        FLT == "LIGURIA" ~ "007",
#                        FLT == "EMILIA-ROMAGNA" ~ "008", # "EMILIA-ROMAGNA"
#                        FLT == "TOSCANA" ~ "009",
#                        FLT == "UMBRIA" ~ "010",
#                        FLT == "MARCHE" ~ "011",
#                        FLT == "LAZIO" ~ "012",
#                        FLT == "ABRUZZO" ~ "013",
#                        FLT == "MOLISE" ~ "014",
#                        FLT == "SARDEGNA" ~ "020",
#                        FLT == "CAMPANIA" ~ "015",
#                        FLT == "PUGLIA" ~ "016",
#                        FLT == "BASILICATA" ~ "017",
#                        FLT == "CALABRIA" ~ "018",
#                        FLT == "SICILIA" ~ "019",
#                        FLT == "NAZ" ~ "",
#                        TRUE ~ "CHK"))
#
#   out <- df %>%
#     left_join(appo, by = "OC_CODICE_PROGRAMMA") %>%
#     mutate(COD_REGIONE = case_when(COD_REGIONE_NEW == "" ~  COD_REGIONE,
#                                    TRUE ~ COD_REGIONE_NEW)) %>%
#     select(-FLT, -COD_REGIONE_NEW)
#
#   return(out)
# }
#
#
# # Aggiunge categoriaz di regione UE
# get_catreg_UE2127 <- function(df, debug_mode=FALSE) {
#   # DEBUG:
#   # df <- perimetro
#   # DEV: da implementare via progetti e con debug_mode (come per "get_stato_attuazione.R")
#
#   # load progetti
#   # source("loader.R")
#
#   # NEW BLOCK
#   if (!any(names(df) == "COD_REGIONE")) {
#     df <- df %>%
#       left_join(progetti %>%
#                   select(COD_LOCALE_PROGETTO, COD_REGIONE),
#                 by = "COD_LOCALE_PROGETTO")
#   }
#
#   # NEW BLOCK (DA AGGIUNGERE ANCHE PER MACROAREA)
#   df <- get_real_reg(df)
#   # MEMO: sovrascrive COD_REGIONE
#
#   # fix per macroarea
#   rs <- c("001", "002", "003", "004", "005", "006",
#           "007", "008", "009", "010", "011", "012")
#   names(rs) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
#                  "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
#
#   rt <- c("013", "014", "020")
#   names(rt) <- c("ABRUZZO", "MOLISE", "SARDEGNA")
#
#   rms <- c("015", "016", "017", "018", "019")
#   names(rms) <- c("CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA")
#
#   chk_regione <- function(data_vector, test_vector) {
#     # DEBUG:
#     # temp <- c("001:::002", "001:::003", "001:::020")
#     # chk_regione(temp, reg_cn)
#     sapply(data_vector, function(x) {all(unlist(str_split(x, pattern = ":::")) %in% test_vector)})
#   }
#
#   df <- df %>%
#     mutate(x_CATREG = case_when(COD_REGIONE %in% rs ~ "RS",
#                               COD_REGIONE %in% rt ~ "RT",
#                               COD_REGIONE %in% rms ~ "RMS",
#                               COD_REGIONE == "000" ~ "Nazionale", # AMBITO NAZIONALE
#                               grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, rs) == TRUE ~ "RS",
#                               grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, rt) == TRUE ~ "RT",
#                               grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, rms) == TRUE ~ "RMS",
#                               grepl(":::", COD_REGIONE) ~ "Trasversale", # MEMO: multi-regionale su più macroaree
#                               TRUE ~ "Estero")) %>%
#     mutate(x_CATREG = factor(x_CATREG, levels = c("RS", "RT", "RMS", "Trasversale", "Nazionale", "Estero")))
#
#   return(df)
#
#
# }
#
#
# # Semplifica regione
# get_regione_simply_real_reg <- function(df, progetti=NULL) {
#   # MEMO: deve avere struttura di progetti
#
#   if (is.null(progetti)) {
#     progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = TRUE)
#   }
#
#   # NEW BLOCK
#   if (!any(names(df) == "COD_REGIONE")) {
#     df <- df %>%
#       left_join(progetti %>%
#                   select(COD_LOCALE_PROGETTO, COD_REGIONE, DEN_REGIONE, COD_PROVINCIA),
#                 by = "COD_LOCALE_PROGETTO")
#
#   } else if (!any(names(df) == "DEN_REGIONE")) {
#     df <- df %>%
#       left_join(progetti %>%
#                   select(COD_LOCALE_PROGETTO, DEN_REGIONE, COD_PROVINCIA),
#                 by = "COD_LOCALE_PROGETTO")
#   }
#
#   # NEW BLOCK (DA AGGIUNGERE ANCHE PER MACROAREA)
#   df <- get_real_reg(df)
#   # MEMO: sovrascrive COD_REGIONE
#
#   reg_cn <- c("001", "002", "003", "004", "005", "006",
#               "007", "008", "009", "010", "011", "012")
#   names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
#                      "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
#
#   reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
#   names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
#
#   temp <- c(names(reg_cn[1:3]), "PA TRENTO", "PA BOLZANO", names(reg_cn[5:12]), names(reg_sud), "ALTRO TERRITORIO")
#
#   # regioni
#   df <- df %>%
#     mutate(x_REGIONE =
#              case_when(
#                COD_REGIONE == "001" ~ "PIEMONTE",
#                COD_REGIONE == "002" ~ "VALLE D'AOSTA",
#                COD_REGIONE == "003" ~ "LOMBARDIA",
#                COD_PROVINCIA == "004021" ~ "PA BOLZANO",
#                COD_PROVINCIA == "004022" ~ "PA TRENTO",
#                COD_PROVINCIA == "004000" & COD_LOCALE_PROGETTO == "2BO5-1a-237" ~ "PA BOLZANO", # MEMO: progetto del POR PA Bolzano
#                COD_PROVINCIA == "004000" & COD_LOCALE_PROGETTO == "1ML1279" ~ "PA TRENTO", # MEMO: progetto del PON AdS FSE forzato qui
#                COD_REGIONE == "004" ~ "PA BOLZANO", # "TRENTINO-ALTO ADIGE"
#                COD_REGIONE == "005" ~ "VENETO",
#                COD_REGIONE == "006" ~ "FRIULI-VENEZIA GIULIA", # "FRIULI-VENEZIA GIULIA"
#                COD_REGIONE == "007" ~ "LIGURIA",
#                COD_REGIONE == "008" ~ "EMILIA-ROMAGNA", # "EMILIA-ROMAGNA"
#                COD_REGIONE == "009" ~ "TOSCANA",
#                COD_REGIONE == "010" ~ "UMBRIA",
#                COD_REGIONE == "011" ~ "MARCHE",
#                COD_REGIONE == "012" ~ "LAZIO",
#                COD_REGIONE == "013" ~ "ABRUZZO",
#                COD_REGIONE == "014" ~ "MOLISE",
#                COD_REGIONE == "020" ~ "SARDEGNA",
#                COD_REGIONE == "015" ~ "CAMPANIA",
#                COD_REGIONE == "016" ~ "PUGLIA",
#                COD_REGIONE == "017" ~ "BASILICATA",
#                COD_REGIONE == "018" ~ "CALABRIA",
#                COD_REGIONE == "019" ~ "SICILIA",
#                TRUE ~ "ALTRO TERRITORIO")) %>%
#     mutate(x_REGIONE = factor(x_REGIONE, levels = temp))
#
#   return(df)
# }
#
# # Aggiunge macroarea
# get_macroarea_real_reg <- function(df, progetti=NULL, debug_mode=FALSE) {
#   # DEBUG:
#   # df <- perimetro
#   # DEV: da implementare via progetti e con debug_mode (come per "get_stato_attuazione.R")
#
#   # load progetti
#   # source("loader.R")
#
#   if (is.null(progetti)) {
#     progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = TRUE)
#   }
#
#   # NEW BLOCK
#   if (!any(names(df) == "COD_REGIONE")) {
#     df <- df %>%
#       left_join(progetti %>%
#                   select(COD_LOCALE_PROGETTO, COD_REGIONE),
#                 by = "COD_LOCALE_PROGETTO")
#   }
#
#   # NEW BLOCK (DA AGGIUNGERE ANCHE PER MACROAREA)
#   df <- get_real_reg(df)
#   # MEMO: sovrascrive COD_REGIONE
#
#   # fix per macroarea
#   reg_cn <- c("001", "002", "003", "004", "005", "006",
#               "007", "008", "009", "010", "011", "012")
#   names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
#                      "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
#
#   reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
#   names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
#
#   chk_regione <- function(data_vector, test_vector) {
#     # DEBUG:
#     # temp <- c("001:::002", "001:::003", "001:::020")
#     # chk_regione(temp, reg_cn)
#     sapply(data_vector, function(x) {all(unlist(str_split(x, pattern = ":::")) %in% test_vector)})
#   }
#
#   df <- df %>%
#     mutate(x_MACROAREA = case_when(COD_REGIONE %in% reg_cn ~ "Centro-Nord",
#                                    COD_REGIONE %in% reg_sud ~ "Sud",
#                                    COD_REGIONE == "000" ~ "Nazionale", # AMBITO NAZIONALE
#                                    grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_cn) == TRUE ~ "Centro-Nord",
#                                    grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_sud) == TRUE ~ "Sud",
#                                    grepl(":::", COD_REGIONE) ~ "Trasversale", # MEMO: multi-regionale su più macroaree
#                                    TRUE ~ "Estero")) %>%
#     mutate(x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")))
#
#   return(df)
#
# }
