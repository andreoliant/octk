# workflow demo

rm(list=ls())

# library("devtools")
devtools::load_all(path = "/Users/aa/coding/oc")
# library("oc")

# setup
bimestre <- "20181231"
# focus <- "test"
data_path <- "/Users/aa/dati/oc"
# workarea <- NULL # MEMO: in DEV_MODE non c'è workarea

# setup
oc_init()
# pryr::mem_used()


# etc
# var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
#             "x_AMBITO", "...",
#             "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
#             "OC_DESCRIZIONE_PROGRAMMA", "OC_CODICE_PROGRAMMA",
#             "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
#             "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")







# ------------------------------------- #

# load

system.time(
  progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light=TRUE)
)


# chk su totali
# MEMO: confronta con recap_preesteso.xlsx
sum(progetti$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)
sum(progetti$TOT_PAGAMENTI, na.rm = TRUE)

# chk su programmi per po_riclass.csv
# HAND: task manuali da setup_data.R

# chk su articolazioni per po_linee_azioni.csv
# HAND: task manuali da setup_data.R
# MEMO: per i nuovi delta dei perimetri va rifatto aggancio al vecchio input delle query

# chk vari
progetti %>%
  filter(COD_REGIONE == "002" | COD_REGIONE == "004" | COD_REGIONE == "006" | COD_REGIONE == "008") %>%
  count(COD_REGIONE, DEN_REGIONE)

# A tibble: 7 x 3
# COD_REGIONE DEN_REGIONE               n
# <chr>       <chr>                 <int>
# 1 002         VALLE D'AOSTA          8942
# 2 004         TRENTINO                 65
# 3 004         TRENTINO-ALTO ADIGE    8886
# 4 006         FRIULI                   36
# 5 006         FRIULI-VENEZIA GIULIA 67513
# 6 008         EMILIA                   27
# 7 008         EMILIA-ROMAGNA        24810

# chk mismatch di x_vars
appo <- progetti %>%
  select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, X_CICLO, X_AMBITO, X_PROGRAMMA)

appo <- get_x_vars(appo)

chk <- appo %>%
  mutate(ciclo = X_CICLO == x_CICLO,
         ambito = X_AMBITO == x_AMBITO,
         programma = X_PROGRAMMA == x_PROGRAMMA) %>%
  filter(ciclo == FALSE | ambito == FALSE | programma == FALSE)

chk %>%
  filter(ambito == FALSE) %>%
  count(X_AMBITO, x_AMBITO)
# X_AMBITO x_AMBITO     n
# <chr>    <fct>    <int>
# 1 FESR     FSE        189
# 2 FESR-FSE FESR     22186
# 3 FESR-FSE FSE      24667
# 4 PAC      POC      23405

chk2 <- chk %>%
  filter(X_AMBITO == "FESR", x_AMBITO == "FSE")
# SAS assegna FESR a interventi FSE (che vede anche PAC) di "POR CRO FSE PA BOLZANO" e "POR CONV FSE CAMPANIA"

# MEMO:
# per ambito:
# SAS usa "PAC" e "FESR-FSE"
# R usa "POC" (anche su 713) e split di "FESR"/"FSE"
# per programma:
# SAS vede PAC insieme a "POR CONV FESR SICILIA" (es. 1SI14187) ma le altre variabili sono corrette (si vede anche in altri programmi)



# ------------------------------------- #
# workflow

# loads
progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE)
progetti <- fix_progetti(progetti)

# gets
progetti <- progetti %>%
  select(-X_CICLO, -X_FONDO, -X_AMBITO, -X_GRUPPO, -X_PROGRAMMA)
# MEMO: devo cancellare prima quelle presenti in progetti
progetti <- get_x_vars(progetti)
progetti <- get_macroarea(progetti)


# TO DO FROM HERE


# query template
write.csv2(categorie_cup, file.path(INPUT, "categorie_cup.csv"), row.names = FALSE)
write.csv2(categorie_ue, file.path(INPUT, "categorie_ue.csv"), row.names = FALSE)
write.csv2(po_linee_azioni, file.path(INPUT, "po_linee_azioni.csv"), row.names = FALSE)
write.csv2(ra, file.path(INPUT, "ra.csv"), row.names = FALSE)
write.csv2(aree_temi_fsc, file.path(INPUT, "aree_temi_fsc.csv"), row.names = FALSE)



# query
# peri_cup <- query_cup(progetti)
# peri_po <- query_po(progetti)
# peri_ue <- query_ue(progetti)
peri_ra <- query_ra(progetti)
peri_atp <- query_atp(progetti)

# chk <- progetti %>%
#   filter(COD_SETTORE_STRATEGICO_FSC == 2, COD_ASSE_TEMATICO_FSC == 3)


# query con wrapper
pseudo <- make_pseudo_edit(progetti, query_ls=c("query_cup", "query_po", "query_atp", "query_ra"), export=TRUE)
chk_match(peri_cup, peri_po, id = "COD_LOCALE_PROGETTO")

# OR:
pseudo <- make_pseudo_std(progetti)

# integra (solo per turismo)
pseudo <- add_old_turismo(pseudo, export=TRUE)
# CHK: perché sono diventati 319 gli "old"? prima erano "309"
# DEV: questo serve ancora? come funziona ora?

# QUI VA TOOL PER L'ANALISI DEI DATI!

write.csv2(stoplist, file.path(INPUT, "stoplist.csv"), row.names = FALSE)
write.csv2(safelist, file.path(INPUT, "safelist.csv"), row.names = FALSE)

pseudo <- read_csv2(file.path(TEMP, "pseudo.csv"))

pseudo <- make_perimetro_edit(pseudo, export=TRUE, debug=TRUE)
# DEV: debug=TRUE richiede var_ls e progetti
# HAND: blocco su analisi (serve tool...)
pseudo <- make_classi(pseudo,
                      classe_jolly="Turismo",
                      livelli_classe = c("Natura", "Cultura", "Turismo"),
                      export=TRUE, debug=FALSE)
# DEV: migliora codice e verifica se funziona anche con altre classi...
# DEV: qui c'è tutto il tema delle keywords
perimetro <- export_data(pseudo, focus, bimestre)
# DEV: inserire debug
# DEV: migliorare parametri anche per sottofunzioni nel gruppo gets
# DEV: inserire selezione di variabili addizionali con relativi parametri
# DEV: implementare template
# OLD: perimetro <- read_csv2(file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")))
# RESTORE:
# perimetro <- reload_perimetro(focus, bimestre, livelli_classe)
export_report(perimetro, use_template=TRUE)
# DEV: deve scegliere quali report
# DEV: wizard implementa automaticamente la cartella report
# CHK: forse in dissesto aveveo una versione più completa con anche FSC
# CHK: There were 19 warnings (use warnings() to see them)


export_data_xls(perimetro, focus, bimestre, use_template=TRUE)


OLD <- "/Users/aa/coding/oc_explorer/turismo/dat/turismo_20180430.csv"
# DEV: da spostare in setup
delta <- make_delta(perimetro, path_to_old = OLD, debug=TRUE)
# chk_match(perimetro, delta, id="COD_LOCALE_PROGETTO")
# chk_match(perimetro, perim_old, id="COD_LOCALE_PROGETTO")
chk_delta(perimetro, path_to_old = OLD, debug=TRUE)
make_delta_scarti(pseudo, perimetro, path_to_old = OLD, debug=TRUE, var_ls, min_cp=2000000)
# DEV: qui va introdotto un puntamento a pseudo_old (ora sovrascritto...)
# DEV: introdurre logica di versioning (con salvataggio di pseudo e folder "input")

# A tibble: 3 x 8
# COD_LOCALE_PROGETTO  QUERY_CUP QUERY_PO QUERY_UE TIPO_QUERY   CHK  PERI CLASSE
# <chr>                    <dbl>    <dbl>    <dbl> <chr>      <dbl> <dbl> <fct>
# 1 2AGCOABBT-PSRA-84-02         0        1        0 po             1     1 NA
# 2 4BA23/2017/0167              0        1        0 po             1     1 NA
# 3 7PUA0608.49                  0        1        0 po             1     1 NA
# chk <- progetti %>%
#   semi_join(pseudo %>%
#               filter(PERI == 1, is.na(CLASSE))) %>%
#   select(var_ls)

# cancellati <- perim_old %>%
#   anti_join(perimetro, by = "COD_LOCALE_PROGETTO")
# cancellati %>%
#   write.csv2(file.path(TEMP, "delta_cancellati.csv"), na = "", row.names = FALSE)
# DEV: DA INSERIRE IN delta.R

# DEV CHK:
# verifica se ho preso da dissesto ultima versione di get_stato_proc

# NEW CHK:
# escono 3 NA su classe >>> "Altro"?

# CHK:
# modificare X per MISTO
# NA su CUP
# valore CP negativo su progettu con stato "non determinabile"

# TODO:
# INPUT/OUTPUT sono in tests ma andranno fuori...SERVE WIZARD PER GENERARE MAIN FOLDER!
# WIzARD DEVE ANCHE CREARE I TEMPLATE PER "QUERY" E "CLASSE"
# DATA è fisso ma deve diventare parametro in input durante la prima installazione...
# TEMP forse non serve perché R ha tempdir()
# inserire riepilogo post importazione in load_progetti (ora non funziona)
# creare strumento per analisi "delta"
# scrivere funzione di chk
# file.path(DATA, "clp_tema_campointervento.csv")

appo <- load_progetti(bimestre = "20180430", visualizzati=TRUE)
# DEV: errore da setup con bimestre dentro
chk_match(progetti, appo, id="COD_LOCALE_PROGETTO")

pseudo %>%
  count(COD_LOCALE_PROGETTO) %>%
  filter(n > 1)

perimetro %>%
  count(COD_LOCALE_PROGETTO) %>%
  filter(n > 1)

chk <- progetti %>%
  semi_join(perimetro %>%
              count(COD_LOCALE_PROGETTO) %>%
              filter(n > 1)) %>%
  select(var_ls)

# peri_cup %>%
#   left_join(progetti %>%
#               select(var_ls),
#             by = "COD_LOCALE_PROGETTO") %>%
#   group_by(CUP_COD_CATEGORIA, CUP_DESCR_CATEGORIA) %>%
#   summarise(N = n(),
#             FTP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
#             PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
#   arrange(desc(FTP))

# # explore
# peri_ue %>%
#   left_join(progetti %>%
#               select(var_ls),
#             by = "COD_LOCALE_PROGETTO") %>%
#   group_by(OC_COD_CICLO, OC_COD_FONTE, OC_DESCRIZIONE_PROGRAMMA) %>%
#   # MEMO: le variabili per ategoria UE vanno aggiunte
#   summarise(N = n(),
#             FTP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
#             PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
#   arrange(desc(FTP))


