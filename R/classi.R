# OC > Explorer > Perimetri
# Classificazione per tematismi
# Focus: turismo


make_classi <- function(pseudo, classe_jolly="Altro", livelli_classe, export=TRUE, debug=FALSE) {

  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro

  # filtra e intgera pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    # select(-CLASSE) %>%
    left_join(progetti %>%
                select(var_ls)) %>%
    filter(PERI == 1) # isola scarti

  # aggiunge categorie UE
  # appo <- get_categorie_UE(appo)

  # recupera natura per modifica aiuti con categoria UE
  appo <- appo %>%
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, CUP_COD_NATURA, CUP_DESCR_NATURA,
                       OC_COD_CATEGORIA_SPESA),
              by = "COD_LOCALE_PROGETTO")

  # ----------------------------------------------------------------------------------- #
  # Classificazione

  # merge
  appo <- appo %>%
    # merge lato CUP
    left_join(read_csv2(file.path(INPUT, "classi_cup.csv")) %>%
                select(CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA, CLASSE),
              by = c("CUP_COD_SETTORE", "CUP_COD_SOTTOSETTORE","CUP_COD_CATEGORIA")) %>%
    # merge lato UE
    left_join(read_csv2(file.path(INPUT, "classi_ue.csv")) %>%
                select(COD_TEMA_CAMPO, CLASSE) %>%
                rename(OC_COD_CATEGORIA_SPESA = COD_TEMA_CAMPO),
              by = "OC_COD_CATEGORIA_SPESA",
              suffix = c("_CUP", "_UE")) %>%
    # MEMO: risolve NA per nuovi progetti con categoria CUP anomala e mai censita >>> CHK!!!
    # MEMO: risolve NA per progetti 1420 senza tema UE
    mutate(CLASSE_CUP = ifelse(is.na(CLASSE_CUP), "Altro", CLASSE_CUP),
           CLASSE_UE = ifelse(is.na(CLASSE_UE), "Altro", CLASSE_UE))

  # MEMO: se vengono NA sono lato CUP e andrebbe rifatto classi_cup.csv
  # appo %>% count(CLASSE_CUP)
  # appo %>% count(CLASSE_UE)

  # crea campo CLASSE e CLASSE_CHK
  appo <- appo %>%
    mutate(CLASSE_CUP = factor(CLASSE_CUP, levels = c(livelli_classe, "Altro")),
           CLASSE_UE = factor(CLASSE_UE, levels = c(livelli_classe, "Altro"))) %>%
    mutate(CHK_CLASSE = CLASSE_CUP == CLASSE_UE)




  # analisi
  # chk <- appo %>%
  #   filter(CLASSE_CUP == "Cultura", CLASSE_UE == "Altro") %>%
  #   arrange(desc(OC_FINANZ_TOT_PUB_NETTO)) %>%
  #   select(TIPO_QUERY, COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO, CUP_DESCR_NATURA, CUP_DESCR_CATEGORIA, DESCR_TEMA_CAMPO)
  # View(chk)

  # MEMO:
  # "Altro" + "Altro" vengono per definizione da "OLD" oppure da "PO" >>> come li gestisco?

  # consolida CLASSE
  appo <- appo %>%
    mutate(CLASSE = case_when((CLASSE_CUP == "Altro") & (CLASSE_UE == "Altro") ~ classe_jolly,
                              CLASSE_CUP == CLASSE_UE ~ as.character(CLASSE_CUP),
                              CLASSE_CUP == "Altro" ~ as.character(CLASSE_UE),
                              CLASSE_UE == "Altro" ~ as.character(CLASSE_CUP),
                              # MEMO: preferenza a CUP per opere e UE per altre nature
                              CUP_COD_NATURA == "03" ~ as.character(CLASSE_CUP),
                              TRUE ~ as.character(CLASSE_UE))) %>%
    # MEMO: la categoria "Altro" qui non esiste più
    mutate(CLASSE = factor(CLASSE, levels = livelli_classe))


  # appo %>%
  #   count(CLASSE)


  # ----------------------------------------------------------------------------------- #
  # Fix manuale

  # load fix
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>%
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    select(COD_LOCALE_PROGETTO, CLASSE)

  # fixlist %>%
  #   count(COD_LOCALE_PROGETTO) %>%
  #   filter(n > 1)
  # WARNING: se questo contiene duplicati poi ritrovo i duplicati in pseudo

  # fix
  appo <- appo %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y)


  # ----------------------------------------------------------------------------------- #
  # Integra pseudo

  # integra appo in pseudo
  pseudo <- pseudo %>%
    left_join(appo %>%
                select("COD_LOCALE_PROGETTO", "CLASSE"),
              by = "COD_LOCALE_PROGETTO")
  # MEMO: restano NA in CLASSE per gli scarti...

  if (debug == TRUE) {

    # matrice cup x ue
    appo %>%
      count(CLASSE_CUP, CLASSE_UE) %>%
      spread(CLASSE_UE, n) %>%
      rename("CUP/UE" = CLASSE_CUP) %>%
      write.csv2(file.path(tmp_path, "matrix_classi.csv"), na = "", row.names = FALSE)

  }

  if (export == TRUE) {

    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }


  return(pseudo)

}




# ----------------------------------------------------------------------------------- #
# Aggiungi vecchia classificazione

# DEV: BLOCCO VALIDO SOLO PER TURISMO

#
# perim_old <- read_csv2(file.path(INPUT, "old_perim.csv")) %>%
#   filter(OC_FLAG_VISUALIZZAZIONE == 0) %>%
#   select(COD_LOCALE_PROGETTO, CLASSE_OLD = OC_FLAG_MACRO_CATEGORIA)
#
# appo1 <- appo %>%
#   left_join(perim_old,
#             by = "COD_LOCALE_PROGETTO") %>%
#   # MEMO: attenzione a codica diversa (1=Natura e NON Cultura!)
#   mutate(CLASSE_OLD = case_when(CLASSE_OLD == 1 ~ "Natura",
#                                 CLASSE_OLD == 2 ~ "Cultura",
#                                 CLASSE_OLD == 3 ~ "Turismo")) %>%
#   mutate(CLASSE_OLD = factor(CLASSE_OLD,
#                              # levels = c(1, 2, 3, 4),
#                              labels = c("Cultura", "Natura", "Turismo"))) %>%
#   mutate(CHK_CLASSE_OLD = CLASSE == CLASSE_OLD)
#
# # appo1 %>%
# #   count(CHK_CLASSE)
#
# # matrice cup x ue
# appo1 %>%
#   count(CLASSE, CLASSE_OLD) %>%
#   spread(CLASSE_OLD, n) %>%
#   rename("NEW/OLD" = CLASSE) %>%
#   write.csv2(file.path(tmp_path, "matrix_classi_old.csv"), na = "", row.names = FALSE)
#
#
#
