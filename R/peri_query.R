# OC > Toolkit
# Query

#' Setup query e stoplist
#'
#' Salva i file csv del blocco "query" in INPUT insieme a stoplist.csv, safelist.csv e fixlist.csv
#'
#' @return File csv in INPUT
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

  write.csv2(stoplist, file.path(INPUT, "stoplist.csv"), row.names = FALSE)
  write.csv2(safelist, file.path(INPUT, "safelist.csv"), row.names = FALSE)
  write.csv2(fixlist, file.path(INPUT, "fixlist.csv"), row.names = FALSE)

}

#' Setup query e stoplist in excel
#'
#' Salva in INPUT un file xls con i file csv del blocco "query", insieme a stoplist.csv, safelist.csv e fixlist.csv
#'
#' @return File xls e csv in INPUt
setup_query_xls <- function() {
  
  # filename per export
  temp_file <- file.path(INPUT, paste0("input_query.xlsx"))
  
  # create
  wb <- createWorkbook()
  addWorksheet(wb, "categorie_cup")
  addWorksheet(wb, "categorie_ue")
  addWorksheet(wb, "po_linee_azioni")
  addWorksheet(wb, "delib_cipe")
  addWorksheet(wb, "strum_att")
  addWorksheet(wb, "prog_comp")
  addWorksheet(wb, "aree_temi_fsc")
  addWorksheet(wb, "ra")
  addWorksheet(wb, "patt")

  # write
  writeData(wb, sheet = "categorie_cup", x = octk::categorie_cup, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "categorie_ue", x = octk::categorie_ue, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "po_linee_azioni", x = octk::po_linee_azioni, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "delib_cipe", x = octk::delib_cipe, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "strum_att", x = octk::strum_att, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "prog_comp", x = octk::prog_comp, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "aree_temi_fsc", x = octk::aree_temi_fsc, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "ra", x = octk::ra, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "patt", x = octk::patt, startCol = 1, startRow = 1, colNames = TRUE)

  # salva
  saveWorkbook(wb, file = temp_file, overwrite = FALSE)
  
  
  # stoplist
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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "categorie_cup")
  } else {
    appo <- read_csv2(file.path(INPUT, "categorie_cup.csv")) 
  }
  matrix_cup <- appo  %>%
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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "po_linee_azioni")
  } else {
    appo <- read_csv2(file.path(INPUT, "po_linee_azioni.csv")) 
  }
  matrix_po <- appo %>%
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


#' Ricerca progetti per campo di intervento UE
#'
#' Ricerca progetti per campo di intervento (2007-2013) o categoria di spesa UE (2014-2020).
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_UE.
query_ue <- function(progetti) {

  # debug
  # chk <- progetti %>%
  #   count(OC_COD_CICLO, OC_COD_CATEGORIA_SPESA, OC_DESCR_CATEGORIA_SPESA) %>%
  #   separate_rows(OC_COD_CATEGORIA_SPESA, sep = ":::")

  # progetti %>%
  #   filter(is.na(OC_COD_CATEGORIA_SPESA)) %>%
  #   count(x_CICLO, x_AMBITO, x_PROGRAMMA)

  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "categorie_ue")
  } else {
    appo <- read_csv2(file.path(INPUT, "categorie_ue.csv")) 
  }
  matrix_ue <- appo %>%
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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "strum_att")
  } else {
    appo <- read_csv2(file.path(INPUT, "strum_att.csv")) 
  }
  matrix_strum <- appo %>%
    rename(QUERY_STRUM = QUERY) %>%
    select(-OC_CODICE_PROGRAMMA, -x_CICLO, -x_AMBITO, -x_PROGRAMMA)

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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "prog_comp")
  } else {
    appo <- read_csv2(file.path(INPUT, "prog_comp.csv")) 
  }
  matrix_progcomp <- appo  %>%
    rename(QUERY_PROGCOMP = QUERY) %>%
    select(-OC_CODICE_PROGRAMMA, -x_CICLO, -x_AMBITO, -x_PROGRAMMA)

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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "patt")
  } else {
    appo <- read_csv2(file.path(INPUT, "patt.csv")) 
  }
  matrix_patt <- appo  %>%
    rename(QUERY_PATT = QUERY) %>%
    select(-OC_CODICE_PROGRAMMA, -x_CICLO, -x_AMBITO, -x_PROGRAMMA)

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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "delib_cipe")
  } else {
    appo <- read_csv2(file.path(INPUT, "delib_cipe.csv")) 
  }
  matrix_cipe <- appo  %>%
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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "ra")
  } else {
    appo <- read_csv2(file.path(INPUT, "ra.csv")) 
  }
  matrix_ra <- appo %>%
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
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "aree_temi_fsc")
  } else {
    appo <- read_csv2(file.path(INPUT, "aree_temi_fsc.csv")) 
  }
  matrix_atp <- appo  %>%
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





#' Aggiunge progetti a pseudo da una lista di CLP
#'
#' Integra pseudo csv con righe provenienti da selezione proveniente da fonte esterna alle funzioni del package e crea colonna "query_add".
#'
#' @param pseudo Dataset in formato "pseudo"
#' @param addendum Dataset in memory oppure nome del file da caricare in INPUT, con elenco di COD_LOCALE_PROGETTO
#' @param export Logic. Vuoi salvare pseudo.csv? 
#' @return Un dataframe pseudo integrato con la colonna QUERY_ADD, che si applica anche a righe esistenti (la funzione usa full_join e non bind_rows!).
#' @section Warning:
#' La funzione è fuori dal blocco "query" solo per maggiore trasparenza, si poteva usare anche quella logica.
add_to_pseudo <- function(pseudo, addendum, export=TRUE) {
  
  # TODO: inserire controllo su OC_FLAG_VISUALIZZAZIONE?
  
  if (is_tibble(addendum)) {
    appo <- addendum %>%
      select(COD_LOCALE_PROGETTO) %>%
      mutate(QUERY_ADD = 1)
  } else if (is.character(addendum)) {
    appo <- read_csv2(file.path(INPUT, addendum)) %>%
      select(COD_LOCALE_PROGETTO) %>%
      mutate(QUERY_ADD = 1)
  } else {
    message("L'addendum non è in un formato corretto")
  }
  
  output <- pseudo %>%
    full_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  if (export == TRUE) {
    write.csv2(output, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(pseudo)
}




