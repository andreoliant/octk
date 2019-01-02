# OC > Toolkit
# Query

#' Ricerca progetti per categoria CUP
#'
#' Ricerca progetti per settore, sotto-settore e categoria CUP a partire da input in "categorie_cup.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_CUP.
query_cup <- function(progetti) {

  # load matrix
  matrix_cup <- read_csv2(file.path(INPUT, "categorie_cup.csv"))  %>%
    rename(QUERY_CUP = QUERY) %>%
    mutate(CUP_COD_SETTORE = str_pad(CUP_COD_SETTORE, 2, pad = "0"),
           CUP_COD_SOTTOSETTORE = str_pad(CUP_COD_SOTTOSETTORE, 2, pad = "0"),
           CUP_COD_CATEGORIA = str_pad(CUP_COD_CATEGORIA, 3, pad = "0"))
  # CHK: capire perché sono spariti i padding (Mara?)

  # merge
  peri_cup <- progetti %>%
    select(COD_LOCALE_PROGETTO, CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA) %>%
    inner_join(matrix_cup %>%
                 filter(QUERY_CUP != 0),
               by = c("CUP_COD_SETTORE", "CUP_COD_SOTTOSETTORE","CUP_COD_CATEGORIA")) %>%
    select(COD_LOCALE_PROGETTO, QUERY_CUP)
  # MEMO: uso inner_join per tenere QUERY_CUP

  return(peri_cup)

}


#' Ricerca progetti per articolazione
#'
#' Ricerca progetti per programma, linea (articolazione) e azione (sub-articolazione) a partire da input in "po_linee_azioni.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_PO.
query_po <- function(progetti) {

  # load matrix
  matrix_po <- read_csv2(file.path(INPUT, "po_linee_azioni.csv")) %>%
    rename(QUERY_PO = QUERY)

  # merge
  peri_po <- progetti %>%
    select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, OC_COD_ARTICOLAZ_PROGRAMMA, OC_COD_SUBARTICOLAZ_PROGRAMMA) %>%
    inner_join(matrix_po %>%
                 filter(QUERY_PO != 0),
               by = c("OC_CODICE_PROGRAMMA", "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA")) %>%
    # select(COD_LOCALE_PROGETTO, QUERY_PO) %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_PO)
  # MEMO: uso distinct perché si generano dupli per casi tipo "FS0713:::PAC"

  return(peri_po)

}


#' Ricerca progetti per categoria UE
#'
#' Ricerca progetti per campo d'intervento UE 2014-2020 e tema prioritario UE 2007-2013 a partire da input in "categorie_ue.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_UE.
#' @section Warning:
#' Le variabili di riferimento non sono presenti in progetti e vanno ricercate in "clp_tema_campointervento.csv".
query_ue <- function(progetti) {

  # load matrix
  matrix_ue <- read_csv2(file.path(INPUT, "categorie_ue.csv")) %>%
    rename(QUERY_UE = QUERY)

  # load categorie UE
  message("Caricamento di clp_tema_campointervento.csv in corso...")
  appo_tema <- read_csv2(file.path(DATA, "clp_tema_campointervento.csv")) %>%
    mutate(OC_COD_CICLO = case_when(TIPO == "CAMPO" ~ 2,
                                    TIPO == "TEMA" ~ 1)) %>%
    select(-TIPO)

  # merge
  peri_ue <- progetti %>%
    select(COD_LOCALE_PROGETTO) %>%
    inner_join(appo_tema,
               by = "COD_LOCALE_PROGETTO") %>%
    select(COD_LOCALE_PROGETTO, OC_COD_CICLO, COD_TEMA_CAMPO) %>%
    inner_join(matrix_ue %>%
                 filter(QUERY_UE != 0),
               by = c("OC_COD_CICLO", "COD_TEMA_CAMPO")) %>%
    # select(COD_LOCALE_PROGETTO, QUERY_UE)
    distinct(COD_LOCALE_PROGETTO, QUERY_UE)
  # WARNING: uso distinct rimuovere duplicati di CLP con temi molteplici

  return(peri_ue)

}


# TODO: integrare query da RA e temi FSC
# aree_temi_fsc.csv
# ra.csv



# ----------------------------------------------------------------------------------- #
# Intersezioni

#' Wrapper per query standard su CUP, UE e PO
#'
#' Wrapper per query standard su CUP, UE e PO.
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_CUP, QUERY_PO, QUERY_UE e TIPO_QUERY.
#' @section Warning:
#' I dati valori NA sono convertiti in 0.
make_pseudo_std <- function(progetti, export=TRUE) {

  # query
  peri_cup <- query_cup(progetti)
  peri_po <- query_po(progetti)
  peri_ue <- query_ue(progetti)

  # merge
  pseudo <- peri_cup %>%
    select(COD_LOCALE_PROGETTO, QUERY_CUP) %>%
    full_join(peri_po %>%
                select(COD_LOCALE_PROGETTO, QUERY_PO)) %>%
    full_join(peri_ue %>%
                select(COD_LOCALE_PROGETTO, QUERY_UE)) %>%
    mutate(TIPO_QUERY = case_when(is.na(QUERY_PO) & is.na(QUERY_UE) ~ "cup",
                                  is.na(QUERY_CUP) & is.na(QUERY_UE) ~ "po",
                                  is.na(QUERY_PO) & is.na(QUERY_CUP) ~ "ue",
                                  is.na(QUERY_PO) ~ "cup-ue",
                                  is.na(QUERY_CUP) ~ "po-ue",
                                  is.na(QUERY_UE) ~ "cup-po",
                                  TRUE ~ "match")) %>%
    mutate(QUERY_CUP = ifelse(is.na(QUERY_CUP), 0, QUERY_CUP),
           QUERY_PO = ifelse(is.na(QUERY_PO), 0, QUERY_PO),
           QUERY_UE = ifelse(is.na(QUERY_UE), 0, QUERY_UE))

  if (export == TRUE) {
    write.csv2(pseudo,
               file.path(TEMP, "pseudo.csv"),
               na = "", row.names = FALSE)
  }
  return(pseudo)
}



# pseudo %>%
#   group_by(TIPO_QUERY) %>%
#   summarise_if(is.numeric, sum)



#' Wrapper per query editabile
#'
#' Wrapper per esecuzione di una lista di query a composizione libera.
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_[1], QUERY_[2], QUERY_[N] e TIPO_QUERY.
#' @section Warning:
#' I dati valori NA sono convertiti in 0.
#' La valutazione dinamica di TIPO_QUERY è ancora da implementare.
make_pseudo_edit <- function(progetti, query_ls=c("query_cup"), export=TRUE) {

  # chk <- make_pseudo_2(progetti, query_ls=c("query_cup", "query_po"), export=TRUE)

  # query
  for (q in query_ls) {
    print(q)
    # temp <- match.fun(q)
    temp <- do.call(q, list(progetti))
    if (!exists("pseudo", inherits = FALSE)) {
      # print("primo giro..")
      pseudo <- temp
    } else {
      pseudo <- pseudo %>% full_join(temp)
    }
  }

  # clean NA
  pseudo <- pseudo %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))


  # query
  # peri_cup <- query_cup(progetti)
  # peri_po <- query_po(progetti)
  # peri_ue <- query_ue(progetti)

  # merge
  # pseudo <- peri_cup %>%
  #   select(COD_LOCALE_PROGETTO, QUERY_CUP) %>%
  #   full_join(peri_po %>%
  #               select(COD_LOCALE_PROGETTO, QUERY_PO)) %>%
  #   full_join(peri_ue %>%
  #               select(COD_LOCALE_PROGETTO, QUERY_UE)) %>%
  #   mutate(TIPO_QUERY = case_when(is.na(QUERY_PO) & is.na(QUERY_UE) ~ "cup",
  #                                 is.na(QUERY_CUP) & is.na(QUERY_UE) ~ "po",
  #                                 is.na(QUERY_PO) & is.na(QUERY_CUP) ~ "ue",
  #                                 is.na(QUERY_PO) ~ "cup-ue",
  #                                 is.na(QUERY_CUP) ~ "po-ue",
  #                                 is.na(QUERY_UE) ~ "cup-po",
  #                                 TRUE ~ "match")) %>%
  #   mutate(QUERY_CUP = ifelse(is.na(QUERY_CUP), 0, QUERY_CUP),
  #          QUERY_PO = ifelse(is.na(QUERY_PO), 0, QUERY_PO),
  #          QUERY_UE = ifelse(is.na(QUERY_UE), 0, QUERY_UE))

  # https://stackoverflow.com/questions/51644516/dplyr-mutate-new-dynamic-variables-with-case-when

  if (export == TRUE) {
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(pseudo)
}




# ----------------------------------------------------------------------------------- #
# Confronta con vecchio perimetro e integra righe

add_old_turismo <- function(pseudo, export=TRUE, debug=FALSE) {

  perim_old <- read_csv2(file.path(INPUT, "old_perim.csv")) %>%
    # MEMO: flag vecchio riferito ad aprile
    filter(OC_FLAG_VISUALIZZAZIONE == 0) %>%
    # MEMO: semi_join per eliminare nuovi non visualizzati
    semi_join(progetti, by = "COD_LOCALE_PROGETTO")

  pseudo <- pseudo %>%
    bind_rows(perim_old %>%
                anti_join(pseudo, by = "COD_LOCALE_PROGETTO") %>%
                select(COD_LOCALE_PROGETTO) %>%
                mutate(QUERY_CUP = 0,
                       QUERY_PO = 0,
                       QUERY_UE = 0,
                       TIPO_QUERY = "old"))

  if (debug == TRUE) {
    scarti <- perim_old %>%
      anti_join(pseudo, by = "COD_LOCALE_PROGETTO")  %>%
      left_join(progetti %>%
                  select(var_ls),
                by = "COD_LOCALE_PROGETTO")
    # DEV: qui andrebbe ripreso progetti per integrare

    write.csv2(scarti, file.path(TEMP, "scarti_old_perim.csv"), na = "", row.names = FALSE)
  }

  if (export == TRUE) {
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(pseudo)
}





# ----------------------------------------------------------------------------------- #
# export temporaneo
# WARNING_ se cambia "var_ls" vanno rifatti i puntamenti via indice sotto (es. var_ls[2:18])
# DEV: forse questo blocco va trasformato in funzione (con var_ls come variabile?)

# # semplifciazione categorie UE
# temp <- pseudo %>%
#   select(COD_LOCALE_PROGETTO) %>%
#   left_join(appo_tema,
#             by = "COD_LOCALE_PROGETTO") %>%
#   # group_by(COD_LOCALE_PROGETTO, OC_COD_CICLO) %>%
#   group_by(COD_LOCALE_PROGETTO) %>%
#   summarise(COD_TEMA_CAMPO = paste(COD_TEMA_CAMPO, collapse=":::"),
#             DESCR_TEMA_CAMPO = paste(DESCR_TEMA_CAMPO, collapse=":::"))
# dim(pseudo)[1] == dim(temp)[1]
#
# # merge con progetti
# export <- pseudo %>%
#   # merge variabili anagrafiche
#   left_join(progetti %>%
#               select("COD_LOCALE_PROGETTO", var_ls[2:18]),
#             by = "COD_LOCALE_PROGETTO") %>%
#   # merge con categorie UE
#   left_join(temp,
#             by = "COD_LOCALE_PROGETTO") %>%
#   # merge variabili avanzamento
#   left_join(progetti %>%
#               select("COD_LOCALE_PROGETTO", var_ls[19:21]),
#             by = "COD_LOCALE_PROGETTO")
# dim(pseudo)[1] == dim(export)[1]


# ----------------------------------------------------------------------------------- #
# Export

# write.csv2(pseudo,
#            file.path(tmp_path, "pseudo.csv"),
#            na = "", row.names = FALSE)

# write.csv2(export,
#            file.path(tmp_path, paste0(paste(this_path,"base", oc_ver, sep = "_"), ".csv")),
#            na = "", row.names = FALSE)

# rm(progetti)
# rm(peri_cup, peri_po, peri_ue, appo_tema)
# rm(matrix_cup, matrix_po, matrix_ue)
