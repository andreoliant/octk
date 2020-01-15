# OC > Toolkit
# Loader

#' Definizione del perimetro (versione standard per Turismo)
#'
#' Ricerca progetti per settore, sotto-settore e categoria CUP a partire da input in "categorie_cup.csv".
#'
#' @param pseudo Dataset "pseudo".
#' @param export Vuoi salvare pseduo.csv in TEMP?
#' @param stoplist Dataset con l'elenco dei COD_LOCALE_PROGETTO da eliminare.
#' @param safelist Dataset con l'elenco dei COD_LOCALE_PROGETTO da conservare anche se sarebbero scartati.
#' @param debug Vuoi salvare scarti.csv in TEMP, con i progetti non considerati?
#' @param var_ls Varibili da integrare in caso di debug.
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe "pseudo" con le variabili addizionali "CHK" e "PERI"
#' #' @section Warning:
#' Al momento è disponibile solo la versione con QUERY_CUP, QUERY_PO e QUERY_UE.
make_perimetro_std <- function(pseudo, export=TRUE,
                           stoplist=NULL, safelist=NULL,
                           debug=FALSE, progetti=NULL, var_ls=NULL) {

  # forzo
  pseudo <- pseudo %>%
    mutate(CHK = case_when(QUERY_CUP == 2 & QUERY_PO == 0 & QUERY_UE == 0 ~ 0,
                           QUERY_CUP == 0 & QUERY_PO == 2 & QUERY_UE == 0 ~ 0,
                           QUERY_CUP == 0 & QUERY_PO == 0 & QUERY_UE == 2 ~ 0,
                           TRUE ~ 1))

  # loads
  if (missing(stoplist)) {
    stoplist <- read_csv2(file.path(INPUT, "stoplist.csv")) %>%
      filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
      # select(COD_LOCALE_PROGETTO, COD_LOCALE_PROGETTO) %>%
      .$COD_LOCALE_PROGETTO
  }

  if (missing(safelist)) {
    safelist <- read_csv2(file.path(INPUT, "safelist.csv")) %>%
      filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
      # select(COD_LOCALE_PROGETTO)
      .$COD_LOCALE_PROGETTO
  }

  # definisce perimetro
  pseudo <- pseudo  %>%
    mutate(PERI = case_when(CHK == 1 ~ 1,
                            # CHK == 2 ~ 0, # DEV: QUESTO NON SI APPLICA PIU...
                            CHK == 0 ~ 0)) %>%
    mutate(PERI = case_when(COD_LOCALE_PROGETTO %in% stoplist ~ 0,
                            COD_LOCALE_PROGETTO %in% safelist ~ 1,
                            TRUE ~ PERI))
  # gestione scarti
  if (debug == TRUE) {
    # defaults
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE)
    }
    if (is.null(var_ls)) {
      var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
                  "OC_COD_CICLO", "OC_COD_FONTE", "FONDO_COMUNITARIO",
                  "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
                  "OC_DESCRIZIONE_PROGRAMMA", "OC_CODICE_PROGRAMMA",
                  "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
                  "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
                  "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
    }
    # filter
    scarti <- pseudo %>%
      filter(PERI == 0) %>%
      select(-PERI) %>%
      left_join(progetti %>%
                  select(var_ls),
                by = "COD_LOCALE_PROGETTO")
    # if ("QUERY_UE" %in% names(pseudo)) {
    #   # aggiunge categorie UE
    #   scarti <- get_categorie_UE(scarti)
    #   # DEV: serve solo se query_ue è nell'elenco delle query
    # }
    # export
    write.csv2(scarti, file.path(TEMP, "scarti_perim.csv"), na = "", row.names = FALSE)
  }

  if (export == TRUE) {
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(pseudo)
}




make_perimetro_edit <- function(pseudo, export=TRUE,
                           stoplist=NULL, safelist=NULL,
                           debug=FALSE, progetti=NULL, var_ls=NULL) {



  # forzo
  # pseudo <- pseudo %>%
  #   mutate(CHK = case_when(QUERY_CUP == 2 & QUERY_PO == 0 & QUERY_UE == 0 ~ 0,
  #                          QUERY_CUP == 0 & QUERY_PO == 2 & QUERY_UE == 0 ~ 0,
  #                          QUERY_CUP == 0 & QUERY_PO == 0 & QUERY_UE == 2 ~ 0,
  #                          TRUE ~ 1))

  pseudo <- pseudo %>%
    # MEMO: isola casi con solo un valore 2 non confermato da nessun altro criterio
    mutate(r_sum = rowSums(select(., starts_with("QUERY"))),
           r_max = do.call(pmax, select(., starts_with("QUERY")))) %>%
    mutate(CHK = case_when(r_sum == 2 & r_max == 2 ~ 0,
                           r_max == 9 ~ 0, # MEMO: serve per escludere PATT o altri aggregati
                           TRUE ~ 1)) %>%
    select(-r_sum, -r_max)

  # loads
  if (missing(stoplist)) {
    stoplist <- read_csv2(file.path(INPUT, "stoplist.csv")) %>%
      filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
      # select(COD_LOCALE_PROGETTO, COD_LOCALE_PROGETTO) %>%
      .$COD_LOCALE_PROGETTO
  }

  if (missing(safelist)) {
    safelist <- read_csv2(file.path(INPUT, "safelist.csv")) %>%
      filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
      # select(COD_LOCALE_PROGETTO)
      .$COD_LOCALE_PROGETTO
  }

  # definisce perimetro
  pseudo <- pseudo  %>%
    mutate(PERI = case_when(CHK == 1 ~ 1,
                            # CHK == 2 ~ 0, # DEV: QUESTO NON SI APPLICA PIU...
                            CHK == 0 ~ 0)) %>%
    mutate(PERI = case_when(COD_LOCALE_PROGETTO %in% stoplist ~ 0,
                            COD_LOCALE_PROGETTO %in% safelist ~ 1,
                            TRUE ~ PERI))
  # gestione scarti
  if (debug == TRUE) {
    # defaults
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = TRUE)
    }
    if (is.null(var_ls)) {
      var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
                  # "OC_COD_CICLO", "OC_COD_FONTE",
                  # "FONDO_COMUNITARIO",
                  "x_CICLO", "x_AMBITO", "x_PROGRAMMA",
                  "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
                  # "OC_DESCRIZIONE_PROGRAMMA",
                  "OC_CODICE_PROGRAMMA",
                  "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
                  "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
                  "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
    }
    # filter
    scarti <- pseudo %>%
      filter(PERI == 0) %>%
      select(-PERI) %>%
      left_join(progetti %>%
                  select(var_ls),
                by = "COD_LOCALE_PROGETTO")
    # if ("QUERY_UE" %in% names(pseudo)) {
    #   # aggiunge categorie UE
    #   scarti <- get_categorie_UE(scarti)
    #   # DEV: serve solo se query_ue è nell'elenco delle query
    # }
    # export
    write.csv2(scarti, file.path(TEMP, "scarti_perim.csv"), na = "", row.names = FALSE)
    print(paste0("Il dataset 'scarti' contiene ", dim(scarti)[1], " progetti esclusi dal perimetro"))
  }

  if (export == TRUE) {
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(pseudo)
}

# ----------------------------------------------------------------------------------- #
# Preparazione

# MEMO:
# first: se "chk_tipo_query.csv" non è presente in temp tiene tutto e va oltre
# next: se "chk_tipo_query.csv" è presente in temp allora esegue il filtro
# HAND: compilare a mano il csv!!!

# dominio di CHK:
# 0: elimina
# 1: perimetro
# 2: verifica

# verifica e copia "chk"
# if (file.exists(file.path(tmp_path, "chk_tipo_query.csv"))) {
#
#   # copia "chk"
#   file.copy(from = file.path(tmp_path, "chk_tipo_query.csv"),
#             to = file.path(src_path, "chk_tipo_query.csv"),
#             overwrite = TRUE)
#   # DEV: QUI VA QUALCOSA PER GESTIRE CASO DI SECONDO GIRO... INTANTO TENERE OLD
#
#   # load filtro
#   flt <- read_csv2(file.path(src_path, "chk_tipo_query.csv")) %>%
#     filter(!(is.na(CHK))) %>%
#     select(QUERY_CUP, QUERY_PO, QUERY_UE, CHK)
#
#   # definisce pseudo-perimetro
#   pseudo <- pseudo %>%
#     # MEMO: rimuove precedente versione
#     select(-CHK, -PERI) %>%
#     left_join(flt)
#   rm(flt)
#   # MEMO: Joining, by = c("QUERY_CUP", "QUERY_PO", "QUERY_UE")
#
#   # uniforma
#   pseudo <- pseudo %>%
#     mutate(CHK = ifelse(is.na(CHK), 1, CHK))
#   # MEMO: mette 1 se CHK è NA (per file presente in temp ma non compilato)
#
# } else {
#
#   print("nessun chk presente... primo giro!")
#   # uniforma pseudo-perimetro
#   pseudo <- pseudo %>%
#     mutate(CHK = 1)
#   # MEMO: mette 1 sempre (solo per uniformare columns)
#
# }

# forzo
# ...


# chk
# pseudo %>% count(QUERY_CUP, QUERY_PO, QUERY_UE, CHK) %>% filter(CHK == 0)


# ----------------------------------------------------------------------------------- #
# Stoplist

# load stoplist e safelist
# ...


# ----------------------------------------------------------------------------------- #
# Pseudo-perimetro

# definisce perimetro
# ...

# chk
# pseudo %>% count(QUERY_CUP, QUERY_PO, QUERY_UE, CHK, PERI) %>% filter(CHK == 0)
# CHK: verificare quanta parte di safelist si perde... 45 items ma ne recupero solo 38!

# elenca scarti
# ...

# aggiunge categorie UE
# ...

# DEV: qui va messo ordine nell'elenco delle variabili

# count
# temp <- pseudo %>%
#   filter(PERI == 1) %>%
#   count() %>%
#   .$n
# print(paste0("obs nel perimetro: ", temp))


# definisce perimetro
# pseudo <- pseudo %>%
#   filter(TEMP == 1) %>%
#   select(-TEMP)

# sum(pseudo$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)
# dim(pseudo)[1]
# sum(pseudo$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)


# ----------------------------------------------------------------------------------- #
# Export

# MEMO:
# pseudo: contiene tutto, anche scarti - solo chiavi
# scarti: contiene solo scarti - tutti i dati (compresa CHK per isolare dubbi)

# # export
# write.csv2(pseudo, file.path(tmp_path, "pseudo.csv"), na = "", row.names = FALSE)
# write.csv2(scarti, file.path(tmp_path, "scarti.csv"), na = "", row.names = FALSE)
# # write.table(scarti, file.path(tmp_path, "scarti.csv"), sep = ";", dec = ",", na = "", row.names = FALSE, append = FALSE)
# # MEMO: con "append" ad ogni giro accoda nuovi scarti
# # CHK: forse ho perso primo giro... (praticamente solo stop_list)
# # MEMO: le variabili sono aggiunte a scarti per controlli manuali in "analisi_scarti.R"
#
# rm(safelist, stoplist)



