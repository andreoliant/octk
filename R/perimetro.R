# OC > Explorer > Perimetri
# Crea perimetro
# Focus: turismo

make_perimetro <- function(pseudo, export=TRUE, debug=FALSE, progetti=NULL, var_ls=NULL) {

  # forzo
  pseudo <- pseudo %>%
    mutate(CHK = case_when(QUERY_CUP == 2 & QUERY_PO == 0 & QUERY_UE == 0 ~ 0,
                           QUERY_CUP == 0 & QUERY_PO == 2 & QUERY_UE == 0 ~ 0,
                           QUERY_CUP == 0 & QUERY_PO == 0 & QUERY_UE == 2 ~ 0,
                           TRUE ~ 1))

  # loads
  stoplist <- read_csv2(file.path(INPUT, "stoplist.csv")) %>%
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
    # select(COD_LOCALE_PROGETTO, COD_LOCALE_PROGETTO) %>%
    .$COD_LOCALE_PROGETTO

  safelist <- read_csv2(file.path(INPUT, "safelist.csv")) %>%
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
    # select(COD_LOCALE_PROGETTO)
    .$COD_LOCALE_PROGETTO


  # definisce perimetro
  pseudo <- pseudo  %>%
    mutate(PERI = case_when(CHK == 1 ~ 1,
                            # CHK == 2 ~ 0, # DEV: QUESTO NON SI APPLICA PIU...
                            CHK == 0 ~ 0)) %>%
    mutate(PERI = case_when(COD_LOCALE_PROGETTO %in% stoplist ~ 0,
                            COD_LOCALE_PROGETTO %in% safelist ~ 1,
                            TRUE ~ PERI))

  if (debug == TRUE) {

    # DEV: progetti e var_ls entrano solo qui

    scarti <- pseudo %>%
      filter(PERI == 0) %>%
      select(-PERI) %>%
      left_join(progetti %>%
                  select(var_ls),
                by = "COD_LOCALE_PROGETTO")

    # aggiunge categorie UE
    scarti <- get_categorie_UE(scarti)


    write.csv2(scarti, file.path(TEMP, "scarti_perim.csv"), na = "", row.names = FALSE)
  }

  if (export == TRUE) {
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }

  # count
  # temp <- pseudo %>%
  #   filter(PERI == 1) %>%
  #   count() %>%
  #   .$n
  # print(paste0("obs nel perimetro: ", temp))
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



