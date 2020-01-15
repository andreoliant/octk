# OC > Toolkit
# Query

#' Setup query e stoplist
#'
#' Salva i file csv del blocco "query" in INPUT insieme a stoplist.csv e safelist.csv
#'
#' @return...
setup_query <- function() {

  write.csv2(categorie_cup, file.path(INPUT, "categorie_cup.csv"), row.names = FALSE)
  write.csv2(categorie_ue, file.path(INPUT, "categorie_ue.csv"), row.names = FALSE)
  write.csv2(po_linee_azioni, file.path(INPUT, "po_linee_azioni.csv"), row.names = FALSE)
  write.csv2(delib_cipe, file.path(INPUT, "delib_cipe.csv"), row.names = FALSE)
  write.csv2(strum_att, file.path(INPUT, "strum_att.csv"), row.names = FALSE)
  write.csv2(prog_comp, file.path(INPUT, "prog_comp.csv"), row.names = FALSE)
  write.csv2(aree_temi_fsc, file.path(INPUT, "aree_temi_fsc.csv"), row.names = FALSE)
  write.csv2(ra, file.path(INPUT, "ra.csv"), row.names = FALSE)
  write.csv2(patt, file.path(INPUT, "patt.csv"), row.names = FALSE)
  # DEV: inserire meccanismo per selezione

  write.csv2(stoplist, file.path(INPUT, "stoplist.csv"), row.names = FALSE)
  write.csv2(safelist, file.path(INPUT, "safelist.csv"), row.names = FALSE)
  write.csv2(fixlist, file.path(INPUT, "fixlist.csv"), row.names = FALSE)

}


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
query_ue_old <- function(progetti) {

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

query_ue <- function(progetti) {

  # debug
  # chk <- progetti %>%
  #   count(OC_COD_CICLO, OC_COD_CATEGORIA_SPESA, OC_DESCR_CATEGORIA_SPESA) %>%
  #   separate_rows(OC_COD_CATEGORIA_SPESA, sep = ":::")

  # progetti %>%
  #   filter(is.na(OC_COD_CATEGORIA_SPESA)) %>%
  #   count(x_CICLO, x_AMBITO, x_PROGRAMMA)

  # load matrix
  matrix_ue <- read_csv2(file.path(INPUT, "categorie_ue.csv")) %>%
    rename(OC_COD_CATEGORIA_SPESA = COD_TEMA_CAMPO,
           OC_DESCR_CATEGORIA_SPESA = DESCR_TEMA_CAMPO,
           QUERY_UE = QUERY)
  # load categorie UE
  # message("Caricamento di clp_tema_campointervento.csv in corso...")
  # appo_tema <- read_csv2(file.path(DATA, "clp_tema_campointervento.csv")) %>%
  #   mutate(OC_COD_CICLO = case_when(TIPO == "CAMPO" ~ 2,
  #                                   TIPO == "TEMA" ~ 1)) %>%
  #   select(-TIPO)

  # merge
  peri_ue <- progetti %>%
    # select(COD_LOCALE_PROGETTO, OC_COD_CICLO, OC_COD_CATEGORIA_SPESA) %>%
    # MEMO: elimino OC_COD_CICLO perché assente in progetti_light
    select(COD_LOCALE_PROGETTO, OC_COD_CATEGORIA_SPESA) %>%
    separate_rows(OC_COD_CATEGORIA_SPESA, sep = ":::") %>%
    # MEMO: fix temporaneo
    mutate(OC_COD_CATEGORIA_SPESA = gsub(":$", "", OC_COD_CATEGORIA_SPESA)) %>%
    inner_join(matrix_ue %>%
                 filter(QUERY_UE != 0),
               # by = c("OC_COD_CICLO", "OC_COD_CATEGORIA_SPESA")) %>%
               by = "OC_COD_CATEGORIA_SPESA") %>%
    # select(COD_LOCALE_PROGETTO, QUERY_UE)
    distinct(COD_LOCALE_PROGETTO, QUERY_UE)
  # WARNING: uso distinct rimuovere duplicati di CLP con temi molteplici

  return(peri_ue)

}


# TODO: integrare query da RA e temi FSC
# aree_temi_fsc.csv
# ra.csv


#' Ricerca progetti per strumento attuativo
#'
#' Ricerca progetti per strumento attuativo a partire da input in "strum_att.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_STRUM.
query_strum <- function(progetti) {

  # "COD_STRUMENTO"
  # "DESCR_STRUMENTO"
  # "DESCR_TIPO_STRUMENTO"

  # load matrix
  matrix_strum <- read_csv2(file.path(INPUT, "strum_att.csv")) %>%
    rename(QUERY_STRUM = QUERY)

  # merge
  peri_strum <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_STRUMENTO) %>%
    inner_join(matrix_strum %>%
                 filter(QUERY_STRUM != 0),
               by = "COD_STRUMENTO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_STRUM)
  # MEMO: uso inner_join per tenere QUERY_STRUM

  return(peri_strum)

}




#' Ricerca progetti per progetto complesso
#'
#' Ricerca progetti per progetto complesso a partire da input in "prog_comp.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_PROGCOMP.
query_progcomp <- function(progetti) {

  # load matrix
  matrix_progcomp <- read_csv2(file.path(INPUT, "prog_comp.csv"))  %>%
    rename(QUERY_PROGCOMP = QUERY)

  # merge
  peri_progcomp <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_PROGETTO_COMPLESSO) %>%
    inner_join(matrix_progcomp %>%
                 filter(QUERY_PROGCOMP != 0),
               by = "COD_PROGETTO_COMPLESSO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_PROGCOMP)
  # MEMO: uso inner_join per tenere QUERY_PROGCOMP

  return(peri_progcomp)

}

#' Ricerca progetti per procedura di attivazione
#'
#' Ricerca progetti per procedura di attivazione a partire da input in "patt.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_PATT.
query_patt <- function(progetti) {

  # load matrix
  matrix_patt <- read_csv2(file.path(INPUT, "patt.csv"))  %>%
    rename(QUERY_PATT = QUERY)

  # merge
  peri_patt <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_PROCED_ATTIVAZIONE) %>%
    inner_join(matrix_patt %>%
                 filter(QUERY_PATT != 0),
               by = "COD_PROCED_ATTIVAZIONE") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_PATT)
  # MEMO: uso inner_join per tenere QUERY_PATT

  return(peri_patt)

}


#' Ricerca progetti per delibera CIPE
#'
#' Ricerca progetti per delibera CIPE a partire da input in "delib_cipe.csv".
#' Al momento comprende tutte le delibere (non solo quelle FSC).
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_CIPE.
query_cipe <- function(progetti) {

  # load finanziamenti
  temp <- paste0("finanziamenti_esteso_", bimestre, ".csv")
  if (file.exists(file.path(DATA, temp))) {
    delibere <- read_csv2(file.path(DATA, temp), guess_max = 5000)
    # MEMO: versione per retrocompatibilità
  } else {
    delibere <- read_sas(file.path(DATA, "finanziamenti_preesteso.sas7bdat")) %>%
      rename(NUMERO_DEL_CIPE = numero_del_cipe,
             ANNO_DEL_CIPE = anno_del_cipe,
             IMPORTO = importo)
  }

  # load matrix
  matrix_cipe <- read_csv2(file.path(INPUT, "delib_cipe.csv"))  %>%
    rename(QUERY_CIPE = QUERY)

  # merge
  appo <- progetti %>%
    select(COD_LOCALE_PROGETTO) %>%
    inner_join(delibere %>%
                 filter(IMPORTO > 0) %>%
                 select(COD_LOCALE_PROGETTO, NUMERO_DEL_CIPE, ANNO_DEL_CIPE) %>%
                 inner_join(matrix_cipe %>%
                             filter(QUERY_CIPE != 0),
                           by = c("NUMERO_DEL_CIPE", "ANNO_DEL_CIPE")),
               by = "COD_LOCALE_PROGETTO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_CIPE)
  # MEMO: uso inner_join per tenere QUERY_CIPE

  # appo %>% count(COD_LOCALE_PROGETTO) %>% filter(n>1)

  # isola min per casi con più delibere (altirmenti duplica in pseudo)
  peri_cipe <- appo %>%
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_CIPE = min(QUERY_CIPE))
  # peri_cipe %>% count(COD_LOCALE_PROGETTO) %>% filter(n>1)

  return(peri_cipe)

}


#' Ricerca progetti per Risultato atteso
#'
#' Ricerca progetti per Risultato atteso (RA) a partire da input in "ra.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_RA.
query_ra <- function(progetti) {

  # load matrix
  matrix_ra <- read_csv2(file.path(INPUT, "ra.csv"))  %>%
    rename(QUERY_RA = QUERY,
           COD_RISULTATO_ATTESO = COD_RIS_ATTESO)

  # merge
  peri_ra <- progetti %>%
    # mutate(COD_RIS_ATTESO = COD_RISULTATO_ATTESO) %>%
    select(COD_LOCALE_PROGETTO, COD_RISULTATO_ATTESO) %>%
    separate_rows(COD_RISULTATO_ATTESO, sep = ":::") %>%
    mutate(COD_RISULTATO_ATTESO = case_when(COD_RISULTATO_ATTESO == "06.5.A" ~ "06.5",
                                            COD_RISULTATO_ATTESO == "06.5.B" ~ "06.5",
                                            COD_RISULTATO_ATTESO == "06.5.C" ~ "06.5",
                                            COD_RISULTATO_ATTESO == "06.5.D" ~ "06.5",
                                            COD_RISULTATO_ATTESO == "06.5.E" ~ "06.5",
                                            TRUE ~ COD_RISULTATO_ATTESO)) %>%
    inner_join(matrix_ra %>%
                 filter(QUERY_RA != 0),
               by = "COD_RISULTATO_ATTESO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_RA)
  # MEMO: uso inner_join per tenere QUERY_RA

  return(peri_ra)

}


#' Ricerca progetti per tema prioritario FSC
#'
#' Ricerca progetti per area e tema prioritario FSC (ATP) a partire da input in "aree_temi_fsc.csv".
#' Al momento comprende tutte le delibere (non solo quelle FSC).
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_ATP.
query_atp <- function(progetti) {

  # load ambito FSC
  if (file.exists(file.path(DATA, "ambito_FSC1420.csv"))) {
    operazioni <- read_csv2(file.path(DATA, "ambito_FSC1420.csv"), guess_max = 5000) %>%
      distinct(COD_LOCALE_PROGETTO,
               COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC,
               COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC)

  } else {
    operazioni <- read_sas(file.path(DATA, "operazioni_pucok.sas7bdat")) %>%
      rename(COD_LOCALE_PROGETTO = cod_locale_progetto,
             COD_SETTORE_STRATEGICO_FSC = fsc_settore_strategico,
             DESCR_SETTORE_STRATEGICO_FSC = fsc_descr_settore_strategico,
             COD_ASSE_TEMATICO_FSC = fsc_asse_tematico,
             DESCR_ASSE_TEMATICO_FSC = fsc_descr_asse_tematico) %>%
      distinct(COD_LOCALE_PROGETTO,
               COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC,
               COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC) %>%
      # fix per matera
      mutate(COD_SETTORE_STRATEGICO_FSC = case_when(COD_SETTORE_STRATEGICO_FSC == "4.a" ~ "4",
                                                    COD_SETTORE_STRATEGICO_FSC == "4.b" ~ "4",
                                                    TRUE ~ COD_SETTORE_STRATEGICO_FSC)) %>%
      # fix per porto di pozzuoli
      mutate(COD_ASSE_TEMATICO_FSC = case_when(COD_ASSE_TEMATICO_FSC == "01" ~ "1",
                                               TRUE ~ COD_ASSE_TEMATICO_FSC))
  }


  # load matrix
  matrix_atp <- read_csv2(file.path(INPUT, "aree_temi_fsc.csv"))  %>%
    rename(QUERY_ATP = QUERY) %>%
    mutate(COD_ASSE_TEMATICO_FSC = as.character(COD_ASSE_TEMATICO_FSC))

  # merge
  peri_atp <- progetti %>%
    select(COD_LOCALE_PROGETTO) %>%
    inner_join(operazioni %>%
                 filter(COD_ASSE_TEMATICO_FSC != "") %>%
                 select(COD_LOCALE_PROGETTO, COD_SETTORE_STRATEGICO_FSC, COD_ASSE_TEMATICO_FSC) %>%
                 separate_rows(COD_ASSE_TEMATICO_FSC, sep = ":::") %>%
                 inner_join(matrix_atp %>%
                              filter(QUERY_ATP != 0),
                            by = c("COD_SETTORE_STRATEGICO_FSC", "COD_ASSE_TEMATICO_FSC")),
               by = "COD_LOCALE_PROGETTO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_ATP)
  # MEMO: uso inner_join per tenere QUERY_ATP

  return(peri_atp)

}



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
#' I valori NA sono convertiti in 0.
#' Usare 1 per casi certi, 2 per casi dubbi (è scartato solo 2 unico) e 9 per casi da eliminare (ad es. per PATT)
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
      pseudo <- pseudo %>%
        full_join(temp,
                  by = "COD_LOCALE_PROGETTO")
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

#' Addendum Turismo
#'
#' ....
#'
#' @param pseudo Dataset "pseudo.csv"
#' @return Un dataframe pseudo.
#' @section Warning:
#' Punta a "old_perim.csv".
#' Forse non serve nemmeno più...
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



