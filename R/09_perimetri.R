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
  write.csv2(keyword, file.path(INPUT, "keyword.csv"), row.names = FALSE)
  write.csv2(aree_temi_psc, file.path(INPUT, "aree_temi_psc.csv"), row.names = FALSE)
  

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
  addWorksheet(wb, "META")
  addWorksheet(wb, "categorie_cup")
  addWorksheet(wb, "categorie_ue")
  addWorksheet(wb, "po_linee_azioni")
  addWorksheet(wb, "delib_cipe")
  addWorksheet(wb, "strum_att")
  addWorksheet(wb, "prog_comp")
  addWorksheet(wb, "aree_temi_fsc")
  addWorksheet(wb, "ra")
  addWorksheet(wb, "patt")
  addWorksheet(wb, "qsn")
  addWorksheet(wb, "tipologie_cup")
  addWorksheet(wb, "comuni")
  addWorksheet(wb, "flag_beniconf")
  addWorksheet(wb, "keyword")
  addWorksheet(wb, "aree_temi_psc")
  
  
  

  # write
  temp0 <- as.character(packageVersion("octk"))
  temp <- data.frame(value = c(bimestre, temp0), var = c("bimestre usato per input", "versione di octk"))
  writeData(wb, sheet = "META", x = temp, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "categorie_cup", x = octk::categorie_cup, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "categorie_ue", x = octk::categorie_ue, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "po_linee_azioni", x = octk::po_linee_azioni, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "delib_cipe", x = octk::delib_cipe, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "strum_att", x = octk::strum_att, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "prog_comp", x = octk::prog_comp, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "aree_temi_fsc", x = octk::aree_temi_fsc, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "ra", x = octk::ra, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "patt", x = octk::patt, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "qsn", x = octk::qsn , startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "tipologie_cup", x = octk::tipologie_cup , startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "comuni", x = octk::comuni, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "flag_beniconf", x = octk::flag_beniconf, startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "keyword", x = octk::keyword , startCol = 1, startRow = 1, colNames = TRUE)
  writeData(wb, sheet = "aree_temi_psc", x = octk::aree_temi_psc, startCol = 1, startRow = 1, colNames = TRUE)
  

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
    appo <- read_csv2(file.path(INPUT, "categorie_cup.csv")) %>%
      # fix padding su csv (in xls sono sanati a monte)
      mutate(CUP_COD_SETTORE = str_pad(CUP_COD_SETTORE, 2, pad = "0"),
             CUP_COD_SOTTOSETTORE = str_pad(CUP_COD_SOTTOSETTORE, 2, pad = "0"),
             CUP_COD_CATEGORIA = str_pad(CUP_COD_CATEGORIA, 3, pad = "0"))
  }
  # matrix_cup <- appo  %>%
  #   rename(QUERY_CUP = QUERY) %>%
  #   mutate(CUP_COD_SETTORE = str_pad(CUP_COD_SETTORE, 2, pad = "0"),
  #          CUP_COD_SOTTOSETTORE = str_pad(CUP_COD_SOTTOSETTORE, 2, pad = "0"),
  #          CUP_COD_CATEGORIA = str_pad(CUP_COD_CATEGORIA, 3, pad = "0"))
  # CHK: capire perché sono spariti i padding (Mara?)
  matrix_cup <- appo  %>%
    rename(QUERY_CUP = QUERY)

  # merge
  peri_cup <- progetti %>%
    select(COD_LOCALE_PROGETTO, CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA) %>%
    inner_join(matrix_cup %>%
                 filter(QUERY_CUP != 0),
               by = c("CUP_COD_SETTORE", "CUP_COD_SOTTOSETTORE","CUP_COD_CATEGORIA")) %>%
    select(COD_LOCALE_PROGETTO, QUERY_CUP)
  # MEMO: uso inner_join per tenere QUERY_CUP
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_cup <- peri_cup %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_CUP = min(QUERY_CUP))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_po <- peri_po %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_PO = min(QUERY_PO))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_ue <- peri_ue %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_UE = min(QUERY_UE))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_strum <- peri_strum %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_STRUM = min(QUERY_STRUM))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_progcomp <- peri_progcomp %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_PROGCOMP = min(QUERY_PROGCOMP))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_patt <- peri_patt %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_PATT = min(QUERY_PATT))

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
    delibere <- read_csv2(file.path(DATA, "finanziamenti_preesteso.csv")) %>%
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
    rename(QUERY_CIPE = QUERY) %>%
    # fix per input in excel
    mutate(NUMERO_DEL_CIPE = as.numeric(NUMERO_DEL_CIPE),
           ANNO_DEL_CIPE = as.numeric(ANNO_DEL_CIPE))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_cipe <- peri_cipe %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_CIPE = min(QUERY_CIPE))

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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_ra <- peri_ra %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_RA = min(QUERY_RA))

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
    operazioni <- read_csv2(file.path(DATA, "oper_pucok_preesteso.csv")) %>%
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
      mutate(COD_ASSE_TEMATICO_FSC = as.character(case_when(COD_ASSE_TEMATICO_FSC == "01" ~ 1,
                                                            TRUE ~ COD_ASSE_TEMATICO_FSC)))
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
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_atp <- peri_atp %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_ATP = min(QUERY_ATP))

  return(peri_atp)

}


#' Ricerca progetti per aree tematiche e settori di intervento PSC
#' 
#' Ricerca progetti per aree tematiche e settori di intervento PSC (ATSI) a partire da input in "aree_temi_psc.csv".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_ATSI_PSC.
query_atsi_psc <- function(progetti) {
  
  # load ambito FSC
  
    operazioni <- read_csv2(file.path(DATA, "oper_pucok_preesteso.csv")) %>%
      rename(COD_LOCALE_PROGETTO = cod_locale_progetto) %>% 
      #        COD_SETTORE_STRATEGICO_FSC = fsc_settore_strategico,
      #        DESCR_SETTORE_STRATEGICO_FSC = fsc_descr_settore_strategico,
      #        COD_ASSE_TEMATICO_FSC = fsc_asse_tematico,
      #        DESCR_ASSE_TEMATICO_FSC = fsc_descr_asse_tematico) %>%
      distinct(COD_LOCALE_PROGETTO,
               psc_area_tematica,
               psc_descr_area_tematica,
               psc_sett_interv,
               psc_descr_sett_interv) %>%
      # # fix per matera
      # mutate(psc_area_tematica = case_when(psc_area_tematica == "4.a" ~ "4",
      #                                               COD_SETTORE_STRATEGICO_FSC == "4.b" ~ "4",
      #                                               TRUE ~ COD_SETTORE_STRATEGICO_FSC)) %>%
      mutate(psc_sett_interv = case_when(psc_area_tematica == "05" & psc_sett_interv == "1" ~ "01",
                                               TRUE ~ psc_sett_interv))
  
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "aree_temi_psc")
  } else {
    appo <- read_csv2(file.path(INPUT, "aree_temi_psc")) 
  }
  matrix_atp <- appo  %>%
    rename(QUERY_ATSI_PSC = QUERY) %>%
    mutate(psc_area_tematica = as.character(psc_area_tematica),
           psc_sett_interv = as.character(psc_sett_interv))
  
  # merge
  peri_atsi <- progetti %>%
    select(COD_LOCALE_PROGETTO) %>%
    inner_join(operazioni %>%
                 filter(psc_area_tematica != "") %>%
                 select(COD_LOCALE_PROGETTO, psc_area_tematica, psc_sett_interv) %>%
                 separate_rows(psc_sett_interv, sep = ":::") %>%
                 inner_join(matrix_atp %>%
                              filter(QUERY_ATSI_PSC != 0),
                            by = c("psc_area_tematica", "psc_sett_interv")),
               by = "COD_LOCALE_PROGETTO") %>%
    distinct(COD_LOCALE_PROGETTO, QUERY_ATSI_PSC)
  # MEMO: uso inner_join per tenere QUERY_ATP
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_atsi <- peri_atsi %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_ATSI_PSC = min(QUERY_ATSI_PSC))
  
  return(peri_atsi)
  
}



#' Ricerca progetti per tipologia CUP
#'
#' Ricerca progetti per natura e tipologia CUP a partire da input in "tipologie_cup".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_TIPO_CUP.
query_tipo_cup <- function(progetti) {
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "tipologie_cup")
  } else {
    appo <- read_csv2(file.path(INPUT, "tipologie_cup.csv")) %>%
      # fix padding su csv (in xls sono sanati a monte)
      mutate(CUP_COD_NATURA = str_pad(CUP_COD_NATURA, 2, pad = "0"),
             CUP_COD_TIPOLOGIA = str_pad(CUP_COD_TIPOLOGIA, 2, pad = "0"))
  }
  # matrix_cup <- appo  %>%
  #   rename(QUERY_CUP = QUERY) %>%
  #   mutate(CUP_COD_SETTORE = str_pad(CUP_COD_SETTORE, 2, pad = "0"),
  #          CUP_COD_SOTTOSETTORE = str_pad(CUP_COD_SOTTOSETTORE, 2, pad = "0"),
  #          CUP_COD_CATEGORIA = str_pad(CUP_COD_CATEGORIA, 3, pad = "0"))
  # CHK: capire perché sono spariti i padding (Mara?)
  matrix_cup <- appo  %>%
    rename(QUERY_TIPO_CUP = QUERY)
  
  # merge
  peri_cup <- progetti %>%
    select(COD_LOCALE_PROGETTO, CUP_COD_NATURA, CUP_COD_TIPOLOGIA) %>%
    inner_join(matrix_cup %>%
                 filter(QUERY_TIPO_CUP != 0),
               by = c("CUP_COD_NATURA", "CUP_COD_TIPOLOGIA")) %>%
    select(COD_LOCALE_PROGETTO, QUERY_TIPO_CUP)
  # MEMO: uso inner_join per tenere QUERY_CUP
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_cup <- peri_cup %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_TIPO_CUP = min(QUERY_TIPO_CUP))
  
  return(peri_cup)
  
}


#' Ricerca progetti per obiettivi QSN
#'
#' Ricerca progetti per obiettivi generali e specifici QSN 2007-2013 a partire da input in "qsn".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_QSN.
query_qsn <- function(progetti) {
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "qsn")
  } else {
    appo <- read_csv2(file.path(INPUT, "qsn.csv"))
  }
  
  matrix <- appo  %>%
    rename(QUERY_QSN = QUERY) %>% 
    # patch per valori numeric
    mutate(QSN_CODICE_OBIETTIVO_SPECIFICO = as.numeric(gsub("\\.", "", QSN_CODICE_OBIETTIVO_SPECIFICO)))
  
  
  # merge
  peri_qsn <- progetti %>%
    select(COD_LOCALE_PROGETTO, QSN_CODICE_OBIETTIVO_SPECIFICO) %>%
    # separate_rows(QSN_CODICE_OBIETTIVO_SPECIFICO, sep = ":::") %>% # MEMO: non ha senso per 713
    inner_join(matrix %>%
                 filter(QUERY_QSN != 0),
               by = "QSN_CODICE_OBIETTIVO_SPECIFICO") %>%
    select(COD_LOCALE_PROGETTO, QUERY_QSN)
  # MEMO: uso inner_join per tenere QUERY_CUP
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  # peri_qsn <- peri_qsn %>% 
  #   group_by(COD_LOCALE_PROGETTO) %>%
  #   summarise(QUERY_QSN = min(QUERY_QSN))
  
  return(peri_qsn)
  
}


#' Ricerca progetti per flag beni confiscati
#'
#' Ricerca progetti per flag beni confiscati da CUP a partire da input in "qsn".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_BENICONF.
query_beniconf <- function(progetti) {
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "flag_beniconf")
  } else {
    appo <- read_csv2(file.path(INPUT, "flag_beniconf.csv"))
  }
  
  matrix <- appo  %>%
    rename(QUERY_BENICONF = QUERY) %>% 
    # fix 
    mutate(OC_FLAG_BENICONF = as.character(OC_FLAG_BENICONF))
  
  # merge
  peri_beniconf <- progetti %>%
    select(COD_LOCALE_PROGETTO, OC_FLAG_BENICONF) %>%
    separate_rows(OC_FLAG_BENICONF, sep = ":::") %>%
    inner_join(matrix %>%
                 filter(QUERY_BENICONF != 0),
               by = "OC_FLAG_BENICONF") %>%
    select(COD_LOCALE_PROGETTO, QUERY_BENICONF)
  # MEMO: uso inner_join per tenere QUERY_CUP
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_beniconf <- peri_beniconf %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_BENICONF = min(QUERY_BENICONF))
  
  return(peri_beniconf)
  
}



#' Ricerca progetti per comune
#'
#' Ricerca progetti per comune di localizzazione a partire da input in "comuni".
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_COMUNI.
query_comuni <- function(progetti) {
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), 
                      sheet = "comuni", 
                      col_types = c("numeric", "text", "numeric", 
                                    "text", "text", "text", "numeric", "text", "text", "text"))
  } else {
    appo <- read_csv2(file.path(INPUT, "comuni.csv"))
  }
  
  matrix <- appo  %>%
    rename(QUERY_COMUNI = QUERY)
  
  # merge
  peri_comuni <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_COMUNE) %>%
    separate_rows(COD_COMUNE, sep = ":::") %>%
    # allinea codici oc a matrix (senza regione)
    mutate(chk = nchar(COD_COMUNE)) %>% 
    filter(chk == 9) %>% 
    mutate(COD_COMUNE = substr(COD_COMUNE, 4, 9)) %>% 
    inner_join(matrix %>%
                 filter(QUERY_COMUNI != 0),
               by = "COD_COMUNE") %>%
    # select(COD_LOCALE_PROGETTO, QUERY_COMUNI, AMBITO, AMBITO_SUB)
    distinct(COD_LOCALE_PROGETTO, QUERY_COMUNI, AMBITO, AMBITO_SUB)
  # MEMO: uso inner_join per tenere QUERY_COMUNI
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_comuni <- peri_comuni %>%
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_COMUNI = min(QUERY_COMUNI),
              AMBITO = paste0(AMBITO, collapse = ":::"),
              AMBITO_SUB = paste0(AMBITO_SUB, collapse = ":::"))
  
  return(peri_comuni)
  
}









#' Ricerca progetti per parole chiave
#'
#' Ricerca progetti per parole chiave, a partire da specifici lemmi.
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_KEY.
#' @note Da implementare la versione che aggrega i lemmi per per keyword ma prode una colonna per ogni keyword (e relativo debug)
query_keyword <- function(progetti) {
  
  # TODO: implementare versione che aggrega per keyword
  
  # load matrix
  if (file.exists(file.path(INPUT, paste0("input_query.xlsx")))) {
    appo <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "keyword")
  } else {
    appo <- read_csv2(file.path(INPUT, "keyword.csv"))
  }
  
  matrix_key <- appo  %>%
    rename(QUERY_KEY = QUERY) %>%
    mutate(LEMMA = str_to_upper(LEMMA)) %>%
    filter(QUERY_KEY != 0)
  
  base_df <- progetti %>%
    select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO) %>%
    # mutate(BASE_VAR = str_to_upper(str_c(OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO))) # MEMO: con un NA salta riga intera
    mutate(BASE_VAR = str_to_upper(paste0(OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO)))
  
  
  # base_df <- tibble(
  #   COD_LOCALE_PROGETTO = c("a1", "b2", "c3"),
  #   BASE_VAR = c("aaa aaa aaa", "bbbbbbbb", "ca cc cb")
  # )
  # matrix_key <- tibble(
  #   LEMMA = c("aaa", "bbb", "ccc", "cc")
  # )
  # matrix_key <- tibble(
  #   LEMMA = c("aaa", "zzz", "xxxx", "cc")
  # )
  
  # loop
  if (exists("peri_key")){
    rm(peri_key)
  }
  for (KEY in matrix_key$LEMMA) {
    # KEY <- matrix_key$LEMMA[4]
    
    test_key <- paste0("\\b", KEY, "\\b")
    
    temp <- base_df %>%
      mutate(QUERY_TEMP = str_detect(BASE_VAR, test_key)) %>%
      select(COD_LOCALE_PROGETTO, QUERY_TEMP) %>%
      filter(QUERY_TEMP)

    if (!exists("peri_key", inherits = FALSE)) {
      # print("primo giro..")
      peri_key <- temp  %>%
        rename(QUERY_KEY = QUERY_TEMP)
    } else {
      peri_key <- peri_key %>%
        full_join(temp,
                  by = "COD_LOCALE_PROGETTO") %>%
        mutate_if(is.logical, replace_na, replace = FALSE) %>%
        mutate(QUERY_KEY = QUERY_KEY + QUERY_TEMP >= 1) %>%
        select(-QUERY_TEMP)
    }
  }
  
  peri_key <- peri_key %>%
    mutate(QUERY_KEY = if_else(TRUE, 1, 0))
  
  # fix per duplicati da ":::" che quadruplicano in make_classi
  peri_key <- peri_key %>% 
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(QUERY_KEY = min(QUERY_KEY))
    
  return(peri_key)
  
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
    # fix per input excel che trasforma in character
    mutate_at(vars(contains('QUERY')), list(as.numeric)) %>%
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
# add_old_turismo <- function(pseudo, export=TRUE, debug=FALSE) {
# 
#   perim_old <- read_csv2(file.path(INPUT, "old_perim.csv")) %>%
#     # MEMO: flag vecchio riferito ad aprile
#     filter(OC_FLAG_VISUALIZZAZIONE == 0) %>%
#     # MEMO: semi_join per eliminare nuovi non visualizzati
#     semi_join(progetti, by = "COD_LOCALE_PROGETTO")
# 
#   pseudo <- pseudo %>%
#     bind_rows(perim_old %>%
#                 anti_join(pseudo, by = "COD_LOCALE_PROGETTO") %>%
#                 select(COD_LOCALE_PROGETTO) %>%
#                 mutate(QUERY_CUP = 0,
#                        QUERY_PO = 0,
#                        QUERY_UE = 0,
#                        TIPO_QUERY = "old"))
# 
#   if (debug == TRUE) {
#     scarti <- perim_old %>%
#       anti_join(pseudo, by = "COD_LOCALE_PROGETTO")  %>%
#       left_join(progetti %>%
#                   select(var_ls),
#                 by = "COD_LOCALE_PROGETTO")
#     # DEV: qui andrebbe ripreso progetti per integrare
# 
#     write.csv2(scarti, file.path(TEMP, "scarti_old_perim.csv"), na = "", row.names = FALSE)
#   }
# 
#   if (export == TRUE) {
#     write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
#   }
#   return(pseudo)
# }
# 




#' Aggiunge progetti a pseudo da una lista di CLP
#'
#' Integra pseudo csv con righe provenienti da selezione proveniente da fonte esterna alle funzioni del package e crea colonna "query_add".
#'
#' @param pseudo Dataset in formato "pseudo"
#' @param addendum Dataset in memory oppure nome del file da caricare in INPUT, con elenco di COD_LOCALE_PROGETTO
#' @param add_name Nome variabile di tipo QUERY_XXX
#' @param export Logic. Vuoi salvare pseudo.csv? 
#' @return Un dataframe pseudo integrato con la colonna QUERY_ADD, che si applica anche a righe esistenti (la funzione usa full_join e non bind_rows!).
#' @section Warning:
#' La funzione è fuori dal blocco "query" solo per maggiore trasparenza, si poteva usare anche quella logica.
add_to_pseudo <- function(pseudo, addendum, add_name="QUERY_ADD", export=TRUE) {
  
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
  
  # rename
  temp <- which(names(appo) == "QUERY_ADD")
  names(appo)[temp] <- add_name
  # MEMO di default è QUERY_ADD ma si può cambiare se servono diversi input
  
  output <- pseudo %>%
    full_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  if (export == TRUE) {
    write.csv2(output, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(output)
}


#' Aggiunge progetti a pseudo da un perimetro
#'
#' Integra pseudo csv con righe provenienti da selezione proveniente da un perimetro realizzato con il package, e in caso di uso di CLASSI le trasfroma in diversi QUERY_XXX.
#'
#' @param pseudo Dataset in formato "pseudo"
#' @param addendum Dataset in memory oppure nome del file in _OUTPUT_SAS da caricare in INPUT, con elenco di COD_LOCALE_PROGETTO
#' @param usa_classi Logico. Se il perimetro è diviso in classi vuoi tracciarle separatamente in diverse colonne di tipo QUERY_XXX? 
#' @param add_name Nome variabile di tipo QUERY_XXX
#' @param export Logico. Vuoi salvare pseudo.csv? 
#' @return Un dataframe pseudo integrato con la colonna QUERY_ADD, che si applica anche a righe esistenti (la funzione usa full_join e non bind_rows!).
#' @section Warning:
#' La funzione è fuori dal blocco "query" solo per maggiore trasparenza, si poteva usare anche quella logica.
add_perimetro_to_pseudo <- function(pseudo, addendum, usa_classi=FALSE, add_name="QUERY_ADD", export=TRUE) {
  
  # TODO: inserire controllo su OC_FLAG_VISUALIZZAZIONE?
  
  if (is_tibble(addendum)) {
    appo <- addendum
  } else if (is.character(addendum)) {
    appo <- read_csv2(file.path(DRIVE, "ELAB", bimestre, "PERIMETRI", "_OUTPUT_SAS", addendum))
  } else {
    message("L'addendum non è in un formato corretto")
  }
  
  if (usa_classi == TRUE) {
    appo <- appo  %>%
      select(COD_LOCALE_PROGETTO, CLASSE) %>%
      mutate(QUERY_ADD = 1) %>% 
      pivot_wider(id_cols = COD_LOCALE_PROGETTO, names_from = CLASSE, values_from = QUERY_ADD,
                  names_prefix = "QUERY_", values_fill = list(QUERY_ADD = 0))
    
    # rename
    temp <- which(names(appo) == "QUERY_ADD")
    names(appo)[temp] <- add_name
    # MEMO di default è QUERY_ADD ma si può cambiare se servono diversi input
    
  } else {
    appo <- appo  %>%
      select(COD_LOCALE_PROGETTO) %>%
      mutate(QUERY_ADD = 1)
    
    # rename
    temp <- which(names(appo) == "QUERY_ADD")
    names(appo)[temp] <- add_name
    # MEMO di default è QUERY_ADD ma si può cambiare se servono diversi input
  }
  
  output <- pseudo %>%
    full_join(appo, by = "COD_LOCALE_PROGETTO") %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  if (export == TRUE) {
    write.csv2(output, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  return(output)
}



#' Converte input da csv a excel
#'
#' Converte gli input in formato csv, ora deprecati, nel nuovo format su file excel.
#'
#' @param OLD Percorso alla cartella di livello WORK di una specifica elaborazione, anche diversa dal bimestre di riferimento.
#' @return Prende tutti i csv nel folder indicato e li inserisce in un file excel in format query_input.xlsx
convert_input_csv_to_xls <- function(OLD) {

  # nomi file di input
  appo0 <- list.files(path = file.path(OLD, "input"))
  temp <- c("fixlist.csv", "safelist.csv", "stoplist.csv", 
            "old_perim.csv", 
            "classi_cup.csv", "classi_ue.csv",
            "input_query.xlsx")
  appo <- appo0[which(!(appo0 %in% temp))]
  
  
  # filename per export in OLD
  temp_file <- file.path(OLD, "input", paste0("input_query.xlsx"))
  
  # create
  wb <- createWorkbook()
  addWorksheet(wb, "META")
  temp <- data.frame(value = c("", ""), var = c("bimestre usato per input", "versione di octk"))
  writeData(wb, sheet = "META", x = temp, startCol = 1, startRow = 1, colNames = TRUE)
  # HAND: completare META a mano
  
  for (input_file in appo) {
    
    # read csv data from input in OLD
    tab <- read_csv2(file.path(OLD, "input", input_file))
    
    # create label
    input_tab <- gsub(".csv", "", input_file)
    print(input_tab)
    
    # fix padding
    if (input_tab == "categorie_cup") {
      tab <- tab  %>%
        mutate(CUP_COD_SETTORE = str_pad(CUP_COD_SETTORE, 2, pad = "0"),
               CUP_COD_SOTTOSETTORE = str_pad(CUP_COD_SOTTOSETTORE, 2, pad = "0"),
               CUP_COD_CATEGORIA = str_pad(CUP_COD_CATEGORIA, 3, pad = "0"))
    }
    
    # set data to excel
    addWorksheet(wb, input_tab)
    writeData(wb, sheet = input_tab, x = tab, startCol = 1, startRow = 1, colNames = TRUE)
    
  }
  
  # write xls
  saveWorkbook(wb, file = temp_file, overwrite = TRUE)
  
}




#' Calcola delta da precedente input
#'
#' Calcola delta da precedente input in format su file excel.
#'
#' @param OLD Percorso alla cartella di livello WORK della precedente elaborazione presa a riferimento.
#' @return Calcola la differenza tra il file indicato in WORK rispetto ai template nel package, salvando un file "input_query_delta.xlsx" da editare manualmente.
make_input_delta  <- function(OLD) {
  # filename per input in OLD
  temp_file <- file.path(OLD, "input", paste0("input_query.xlsx"))
  
  # filename per export
  temp_file_delta <- file.path(INPUT, paste0("input_query_delta.xlsx"))
  
  # nomi fogli di input
  temp <- getSheetNames(temp_file)
  appo <- temp[which(temp != "META")]
  
  # create
  wb <- createWorkbook()
  
  # funzione robusta di pulizia
  safe_utf8_df <- function(df) {
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        x <- stringi::stri_enc_toutf8(x)
        x <- stringi::stri_replace_all_regex(x, "[\\p{C}]", "")
        x
      } else x
    })
    df
  }
  
  for (input_tab in appo) {
    #  input_tab <- appo[5]
    print(input_tab)
    
    # read data from input xls in OLD
    tab <- read_xlsx(temp_file, sheet = input_tab, col_types = "text", )
    
    # read new input data from octk
    tab_new <- eval(as.name(input_tab))%>%
      mutate(NOTE = as.character(NOTE))%>%
      mutate(QUERY = as.character(QUERY))
      
    
    if (input_tab == "delib_cipe") {
      tab_delta <- tab_new %>%
        mutate_if(is.numeric, as.character) %>%
        mutate(NOTE = as.character(NOTE))%>%
        anti_join(tab %>%
                    select(-QUERY, -NOTE) %>%
                    select(NUMERO_DEL_CIPE, ANNO_DEL_CIPE))
      
      chk <- tab %>%
        select(-QUERY, -NOTE) %>%
        select(NUMERO_DEL_CIPE, ANNO_DEL_CIPE)%>%
        anti_join(tab_new %>%
                    mutate_if(is.numeric, as.character))
    
    } else if (input_tab == "patt") {
      tab_delta <- tab_new %>%
        mutate_if(is.numeric, as.character) %>%
        mutate(NOTE = as.character(NOTE))%>%
        anti_join(tab %>%
                    select(-QUERY, -NOTE) %>%
                    select(contains("COD"))) %>% 
        # patch per eliminare caratteri speciali che fanno saltare righe durante la pubblicazione in excel
        mutate(DESCR_PROCED_ATTIVAZIONE = str_remove(DESCR_PROCED_ATTIVAZIONE, "\\u001a"))
        
      
      chk <- tab %>%
        select(-QUERY, -NOTE) %>%
        select(contains("COD")) %>%
        anti_join(tab_new %>%
                    mutate_if(is.numeric, as.character))
    
    } else if (input_tab == "flag_beniconf") {
      tab_delta <- tab_new %>%
        mutate_if(is.numeric, as.character) %>%
        mutate(NOTE = as.character(NOTE))%>%
        anti_join(tab %>%
                    select(-QUERY, -NOTE) %>%
                    select(OC_FLAG_BENICONF), 
                  by = "OC_FLAG_BENICONF")
      
      
      chk <- tab %>%
        select(-QUERY, -NOTE) %>%
        select(OC_FLAG_BENICONF) %>%
        anti_join(tab_new %>%
                    mutate_if(is.numeric, as.character))
    
    } else if (input_tab == "keyword") {
      tab_delta <- tab_new %>%
        anti_join(tab %>%
                    select(-QUERY, -NOTE) %>%
                    select(LEMMA, KEYWORD), 
                  by = c("LEMMA", "KEYWORD"))
      
      
      chk <- tab %>%
        select(-QUERY, -NOTE) %>%
        select(LEMMA, KEYWORD) %>%
        anti_join(tab_new %>%
                    mutate_if(is.numeric, as.character))
      
          
    } else {
      tab_delta <- tab_new %>%
        mutate_if(is.numeric, as.character) %>%
        mutate(NOTE = as.character(NOTE))%>%
        anti_join(tab %>%
                    select(-QUERY, -NOTE) %>%
                    select(contains("COD")))
      
      chk <- tab %>%
        select(-QUERY, -NOTE) %>%
        select(contains("COD")) %>%
        anti_join(tab_new %>%
                    mutate_if(is.numeric, as.character))
    }
    
    
    tab_delta <- safe_utf8_df(tab_delta)
    
    
    # set data to excel
    addWorksheet(wb, input_tab)
    writeData(wb, sheet = input_tab, x = tab_delta, startCol = 1, startRow = 1, colNames = TRUE)
    
  }
  
  # write xls
  saveWorkbook(wb, file = temp_file_delta, overwrite = TRUE)
}


#' Crea file di input integrato con delta da precedente elaborazione
#'
#' Crea file di input integrato con input di precedente elaborazione e delta rispetto all'attuale template nel package, da calcolare con \link[octk]{make_input_delta}.
#'
#' @param OLD Percorso alla cartella di livello WORK della precedente elaborazione presa a riferimento.
#' @return Esegue bind del precedente input e della versione delta, salvando un file "input_query.xlsx".
update_input_with_delta <- function(OLD) {
  
  # filenament per input from OLD
  temp_file <- file.path(OLD, "input", paste0("input_query.xlsx"))
  
  # filename per input delta
  temp_file_delta <- file.path(INPUT, paste0("input_query_delta.xlsx"))
  
  # filename per export in input
  temp_file_export <- file.path(INPUT, paste0("input_query.xlsx"))
  
  
  # nomi fogli di input
  temp <- getSheetNames(temp_file)
  appo <- temp[which(temp != "META")]
  
  # create
  wb <- createWorkbook()
  
  # update metadati
  temp0 <- as.character(packageVersion("octk"))
  temp <- data.frame(value = c(bimestre, temp0), var = c("bimestre usato per input", "versione di octk"))
  addWorksheet(wb, "META")
  writeData(wb, sheet = "META", x = temp, startCol = 1, startRow = 1, colNames = TRUE)
  
  for (input_tab in appo) {
    
    
    # read new input data from octk
    tab_new <- eval(as.name(input_tab))
    
    # read data from input xls in OLD
    tab <- read_xlsx(temp_file, sheet = input_tab, col_types = "text")
    
    
    
    
    
    
    
    
    # DEV: tab semi_join tab_new
    if (input_tab == "delib_cipe") {
      tab <- tab %>%
        mutate_if(is.numeric, as.character) %>%
        semi_join(tab_new %>%
                    mutate_if(is.numeric, as.character) %>%
                    select(-QUERY, -NOTE) %>%
                    select(NUMERO_DEL_CIPE, ANNO_DEL_CIPE))
      
      # chk <- tab %>%
      #   select(-QUERY, -NOTE) %>%
      #   select(NUMERO_DEL_CIPE, ANNO_DEL_CIPE)%>%
      #   anti_join(tab_new %>%
      #               mutate_if(is.numeric, as.character))
      
    } else if (input_tab == "patt") {
      tab <- tab %>%
        mutate_if(is.numeric, as.character) %>%
        semi_join(tab_new %>%
                    mutate_if(is.numeric, as.character) %>%
                    select(-QUERY, -NOTE) %>%
                    select(contains("COD"))) %>% 
        # patch per eliminare caratteri speciali che fanno saltare righe durante la pubblicazione in excel
        mutate(DESCR_PROCED_ATTIVAZIONE = str_remove(DESCR_PROCED_ATTIVAZIONE, "\\u001a"))
      
      
      # chk <- tab %>%
      #   select(-QUERY, -NOTE) %>%
      #   select(contains("COD")) %>%
      #   anti_join(tab_new %>%
      #               mutate_if(is.numeric, as.character))
      
    } else if (input_tab == "flag_beniconf") {
      tab <- tab %>%
        mutate_if(is.numeric, as.character) %>%
        semi_join(tab_new %>%
                    mutate_if(is.numeric, as.character) %>%
                    select(-QUERY, -NOTE) %>%
                    select(OC_FLAG_BENICONF), 
                  by = "OC_FLAG_BENICONF")
      
      
      # chk <- tab %>%
      #   select(-QUERY, -NOTE) %>%
      #   select(OC_FLAG_BENICONF) %>%
      #   anti_join(tab_new %>%
      #               mutate_if(is.numeric, as.character))
      
    } else if (input_tab == "keyword") {
      tab <- tab %>%
        mutate_if(is.numeric, as.character) %>%
        semi_join(tab_new %>%
                    mutate_if(is.numeric, as.character) %>%
                    select(-QUERY, -NOTE) %>%
                    select(LEMMA, KEYWORD), 
                  by = c("LEMMA", "KEYWORD"))
      
      
      # chk <- tab %>%
      #   select(-QUERY, -NOTE) %>%
      #   select(LEMMA, KEYWORD) %>%
      #   anti_join(tab_new %>%
      #               mutate_if(is.numeric, as.character))
      
      
    } else {
      tab <- tab %>%
        mutate_if(is.numeric, as.character) %>%
        semi_join(tab_new %>%
                    mutate_if(is.numeric, as.character) %>%
                    select(-QUERY, -NOTE) %>%
                    select(contains("COD")))
      
      # chk <- tab %>%
      #   select(-QUERY, -NOTE) %>%
      #   select(contains("COD")) %>%
      #   anti_join(tab_new %>%
      #               mutate_if(is.numeric, as.character))
    }
    
    
    
    
    
    
    
    
    
    
    
    
    # read data from delta
    tab_delta <- read_xlsx(temp_file_delta, sheet = input_tab, col_types = "text")
    
    tab_new <- tab %>%
      mutate_if(is.numeric, as.character) %>%
      bind_rows(tab_delta)
    
    # set data to excel
    addWorksheet(wb, input_tab)
    writeData(wb, sheet = input_tab, x = tab_new, startCol = 1, startRow = 1, colNames = TRUE)
    
  }
  
  # write xls
  saveWorkbook(wb, file = temp_file_export, overwrite = TRUE)
}



#' Assestare lista finale perimetro
#'
#' Assestare lista finale perimetro per consentire il caricamento con CUP aggregati
#'
#' @param progetti Dataset "progetti_esteso_<BIMESTRE>.csv".
#' @param perimetro definire il dataset del perimetro finale che contiene solo i CLP
#' @param export settare a TRUE per scaricare CSV finale in OUTPUT
#' @return Un dataframe con COD_LOCALE_PROGETTO.

clp_perimetro <- function(progetti, perimetro, export=FALSE) {
  
    # query
    clp_duplicati <- load_progetti(bimestre, visualizzati = FALSE, light = FALSE)%>%
      select(COD_LOCALE_PROGETTO, OC_PROGETTO_AGGREGATO)

    
    # individuo i CLP finali
    pseudo <- perimetro %>%
      select(COD_LOCALE_PROGETTO)%>%
      left_join(clp_duplicati, by = c("COD_LOCALE_PROGETTO"))%>%
                  mutate(CLP_PERIMETRO = ifelse(is.na(OC_PROGETTO_AGGREGATO), COD_LOCALE_PROGETTO, OC_PROGETTO_AGGREGATO))%>%
                  distinct(CLP_PERIMETRO)
    
    if (export == TRUE) {
      write.csv2(pseudo,
                 file.path(OUTPUT, "perimetro_finale.csv"),
                 na = "", row.names = FALSE)
    }
    return(pseudo)
}



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
      # progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE)
      message("Va in errore perché bisogna caricare progetti con load_progetti e poi passarlo come parametro")
    }
    if (is.null(var_ls)) {
      var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
                  "OC_COD_CICLO", "OC_COD_FONTE", "FONDO_COMUNITARIO",
                  "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
                  "OC_DESCRIZIONE_PROGRAMMA", "OC_CODICE_PROGRAMMA",
                  "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_SUBARTICOLAZ_PROGRAMMA",
                  "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
                  "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE",
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
    # mutate(PERI = case_when(COD_LOCALE_PROGETTO %in% stoplist ~ 0,
    #                         COD_LOCALE_PROGETTO %in% safelist ~ 1,
    #                         TRUE ~ PERI))
    mutate(STOP = case_when(COD_LOCALE_PROGETTO %in% stoplist ~ 1,
                            TRUE ~ 0),
           SAFE = case_when(COD_LOCALE_PROGETTO %in% safelist ~ 1,
                            TRUE ~ 0),
           PERI = case_when(STOP == 1 ~ 0,
                            SAFE == 1 ~ 1,
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
                  "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_SUBARTICOLAZ_PROGRAMMA",
                  "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
                  "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE",
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


# inspect delta

make_delta <- function(perimetro, path_to_old, debug=FALSE) {
  
  # DEV: automation for path_to_old
  # DEV: partire da pseudo e aggiungere var_ls (???)
  
  # path_to_old <- "/Users/aa/coding/oc_explorer/turismo/dat/turismo_20180430.csv"
  perim_old <- read_csv2(path_to_old)
  
  delta <- perimetro %>%
    anti_join(perim_old, by = "COD_LOCALE_PROGETTO")
  
  if (debug == TRUE) {
    delta %>%
      write.csv2(file.path(TEMP, "delta.csv"), na = "", row.names = FALSE)
  }
  return(delta)
  
}


chk_delta <- function(perimetro, path_to_old, debug=FALSE) {
  
  perim_old <- read_csv2(path_to_old)
  
  chk <- chk_match(perimetro, perim_old, id="COD_LOCALE_PROGETTO")
  
  if (debug == TRUE) {
    chk %>%
      write.csv2(file.path(TEMP, "chk_delta.csv"), na = "", row.names = FALSE)
  }
  return(chk)
  
}


make_delta_scarti <- function(pseudo, perimetro, path_to_old, debug=FALSE,
                              var_ls, min_cp=2000000, stoplist=NULL) {
  
  # DEV: se lo faccio girare su pseduo vecchio?
  # ATTENZIONE: ORA LO HO SOVRASCRITTO!
  
  perim_old <- read_csv2(path_to_old)
  
  # stoplist
  if (missing(stoplist)) {
    stoplist <- read_csv2(file.path(INPUT, "stoplist.csv")) %>%
      filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
      .$COD_LOCALE_PROGETTO
  }
  
  delta_scarti <- progetti %>%
    select(var_ls) %>%
    inner_join(pseudo, by = "COD_LOCALE_PROGETTO") %>%
    filter(PERI != 1) %>% # NEWLINE
    filter(!(COD_LOCALE_PROGETTO %in% stoplist)) %>%
    # MEMO: evito di ricontrollare record appena messi in scarti tramite stoplist
    anti_join(perim_old, by = "COD_LOCALE_PROGETTO") %>%
    # MEMO: nel vecchio perimetro la stoplist era già stata scartata
    filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)
  
  
  if (debug == TRUE) {
    delta_scarti %>%
      write.csv2(file.path(TEMP, "delta_scarti.csv"), na = "", row.names = FALSE)
  }
  
  return(delta_scarti)
  
}


#' Crea nuova base per riclassificazione lato categorie CUP
#'
#' ...
#'
#' @param pseudo Dataset "pseudo".
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_cup.csv".
#' #' @section Warning:
#' Da rinominare in "classi_cup.csv" e modificare.
setup_classi_cup <- function(pseudo, progetti, file_name="classi_cup_NEW.csv") {
  
  # intgera pseudo
  out <- pseudo %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "CUP_COD_SETTORE", "CUP_DESCR_SETTORE",
                       "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE",
                       "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA"),
              by = "COD_LOCALE_PROGETTO") %>%
    filter(PERI == 1) %>% # isola scarti
    count(CUP_COD_SETTORE, CUP_DESCR_SETTORE, CUP_COD_SOTTOSETTORE, CUP_DESCR_SOTTOSETTORE,
          CUP_COD_CATEGORIA, CUP_DESCR_CATEGORIA) %>%
    arrange(desc(n)) %>%
    mutate(CLASSE = NA)
  
  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE)
  
  if (file_name == "classi_cup_NEW.csv") {
    message("Integra e rinomina classi_cup_NEW.csv")
  }
  
}


#' Crea nuova base per riclassificazione lato temi/campi UE
#'
#' ...
#'
#' @param pseudo Dataset "pseudo".
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_ue.csv".
#' #' @section Warning:
#' Da rinominare in "classi_ue.csv" e modificare.
setup_classi_ue <- function(pseudo, progetti, file_name="classi_ue_NEW.csv") {
  
  # intgera pseudo
  out <- pseudo %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA"),
              by = "COD_LOCALE_PROGETTO") %>%
    filter(PERI == 1) %>% # isola scarti
    count(OC_COD_CATEGORIA_SPESA, OC_DESCR_CATEGORIA_SPESA) %>%
    arrange(desc(n)) %>%
    mutate(CLASSE = NA)
  
  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE)
  
  if (file_name == "classi_ue_NEW.csv") {
    message("Integra e rinomina classi_ue_NEW.csv")
  }
  
}


#' Setup fixlist per classificazione
#'
#' Salva fixlist.csv
#'
#' @return...
setup_fixlist <- function() {
  
  # DEV: mettere qui dentro anche setup_classi_cup() e setup_classi_ue()
  
  write.csv2(fixlist, file.path(INPUT, "fixlist.csv"), row.names = FALSE)
  
}


#' Classificazione per categoria CUP e campo d'intervento UE
#'
#' Classificazione per categoria CUP e campo d'intervento UE
#'
#' @param pseudo Dataset "pseudo".
#' @param classe_jolly Nome per classe da assegnare ai casi non mappati in classi_cup e classi_ue
#' @param livelli_classe Nomi per factor di classe
#' @param export Logico. Vuoi salvare?
#' @param debug Logico. Vuoi una matrice con freq cup x ue
#' @return Un file "pseudo".
make_classi <- function(pseudo, classe_jolly="Altro", livelli_classe=NULL, export=TRUE, debug=FALSE) {
  
  # if (is.null(var_ls)) {
  #   var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
  #               "OC_COD_CICLO", "OC_COD_FONTE", "FONDO_COMUNITARIO",
  #               "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
  #               "OC_DESCRIZIONE_PROGRAMMA", "OC_CODICE_PROGRAMMA",
  #               "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
  #               "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
  # }
  # MEMO: forse va tolto anche sopra
  
  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro
  
  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    # select(-CLASSE) %>%
    # left_join(progetti %>%
    #             select(var_ls)) %>%
    filter(PERI == 1) # isola scarti
  
  # aggiunge categorie UE
  # appo <- get_categorie_UE(appo)
  
  # intgera pseudo
  appo <- appo %>%
    left_join(progetti %>%
                select(CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA,
                       COD_LOCALE_PROGETTO, CUP_COD_NATURA, CUP_DESCR_NATURA,
                       OC_COD_CATEGORIA_SPESA),
              by = "COD_LOCALE_PROGETTO")
  # MEMO: recupera natura per modifica aiuti con categoria UE
  
  
  # ----------------------------------------------------------------------------------- #
  # Classificazione
  
  # load
  classi_cup <- read_csv2(file.path(INPUT, "classi_cup.csv")) %>%
    select(CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA, CLASSE_CUP = CLASSE)
  
  classi_ue <- read_csv2(file.path(INPUT, "classi_ue.csv")) %>%
    select(OC_COD_CATEGORIA_SPESA, CLASSE_UE = CLASSE)
  
  # switch per livelli_classe e classe_jolly
  if (is.null(livelli_classe)) {
    livelli_classe <- unique(c(unique(classi_cup$CLASSE_CUP), c(unique(classi_ue$CLASSE_UE))))
    classe_jolly <- "NA"
  }
  
  # merge
  appo <- appo %>%
    # merge lato CUP
    left_join(classi_cup,
              by = c("CUP_COD_SETTORE", "CUP_COD_SOTTOSETTORE","CUP_COD_CATEGORIA")) %>%
    # merge lato UE
    left_join(classi_ue,
              by = "OC_COD_CATEGORIA_SPESA") %>%
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
    # mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>%
    count(COD_LOCALE_PROGETTO) %>%
    filter(n > 1)
  print(paste0("verifica se ci sono duplicati in fixlist: ", dim(chk)[1]))
  # WARNING: se questo contiene duplicati poi ritrovo i duplicati in pseudo
  
  # fix
  appo <- appo %>%
    mutate(CLASSE = as.character(CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
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
      write.csv2(file.path(TEMP, "matrix_classi.csv"), na = "", row.names = FALSE)
    
  }
  
  if (export == TRUE) {
    
    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
    
  }
  
  
  return(pseudo)
  
}


#' Classificazione Hard-Soft
#'
#' Classificazione Hard-Soft da nautra CUP
#'
#' @param pseudo Dataset "pseudo".
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_hard_soft <- function (pseudo, export = TRUE) {
  
  out <- pseudo %>% 
    filter(PERI == 1) %>% # isola scarti
    mutate(CLASSE = case_when(CUP_COD_NATURA == "03" ~ "hard",
                              TRUE ~ "soft"))
  
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  out <- out %>% 
    mutate(CLASSE = as.character(CLASSE)) %>% 
    left_join(fixlist, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>% 
    mutate(CLASSE = factor(CLASSE, levels = c("hard", "soft"))) %>% 
    select(-CLASSE.x, -CLASSE.y) %>%
    select(-CUP_DESCR_NATURA, -CUP_COD_NATURA)
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}




#' Crea nuova base per riclassificazione per forma giuridica soggetti
#'
#' Crea nuova base per riclassificazione per forma giuridica soggetti
#'
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_soggetti.csv".
setup_classi_soggetti <- function(file_name="classi_soggetti_NEW.csv") {
  
  out <- octk::forma_giuridica_soggetti
  
  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE, quote = c(1))
  
  if (file_name == "classi_soggetti_NEW.csv") {
    message("Integra e rinomina classi_soggetti_NEW.csv")
    message("Attenzione ad aprire il csv con la colonna 1 come testo")
  }
}


#' Classificazione per soggetti
#'
#' Classificazione per tipologie di soggetti
#'
#' @param pseudo Dataset "pseudo".
#' @param livelli_classe Nomi per factor di classe
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_soggetti <- function (pseudo, livelli_classe=NULL, progetti, export = TRUE) {
  
  if (!("OC_COD_FORMA_GIU_BENEFICIARIO" %in% names(progetti))) {
    progetti <- load_progetti(bimestre, light = FALSE)
  }
  
  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro
  
  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    filter(PERI == 1) # isola scarti
  
  # intgera pseudo
  appo <- appo %>%
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, OC_COD_FORMA_GIU_BENEFICIARIO),
              by = "COD_LOCALE_PROGETTO") %>% 
    # isola primo soggetto
    mutate(OC_COD_FORMA_GIU_BENEFICIARIO = substr(OC_COD_FORMA_GIU_BENEFICIARIO, 1, 6))
  # MEMO: ci sono casi spuri diversi da 9.9.99  (es. 1.2)
  
  
  # ----------------------------------------------------------------------------------- #
  # Classificazione
  
  # load
  classi_soggetti <- read_csv2(file.path(INPUT, "classi_soggetti.csv"), col_types = "ccccc") %>%
    select(OC_COD_FORMA_GIU_BENEFICIARIO, CLASSE)
  
  
  # switch per livelli_classe e classe_jolly
  if (is.null(livelli_classe)) {
    livelli_classe <- unique(classi_soggetti$CLASSE)
    livelli_classe <- livelli_classe[which(!is.na(livelli_classe))]
    if (!("Altro" %in% livelli_classe)){
      livelli_classe <- c(livelli_classe, "Altro")
    }
  }
  
  # merge
  appo <- appo %>%
    left_join(classi_soggetti,
              by = "OC_COD_FORMA_GIU_BENEFICIARIO")
  
  
  # ----------------------------------------------------------------------------------- #
  # Fix manuale
  
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  # fix
  appo <- appo %>%
    # gestione missing
    mutate(CLASSE = ifelse(is.na(CLASSE), "Altro", CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    mutate(CLASSE = factor(CLASSE, levels = livelli_classe)) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y) 
  
  out <- appo
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}


#' Classificazione per ambiti comunali
#'
#' Classificazione per tipologie di soggetti
#'
#' @param pseudo Dataset "pseudo".
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_comuni <- function (pseudo, progetti, export = TRUE) {
  
  # load matrix
  comuni <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "comuni") %>% 
    filter(QUERY == 1) %>% 
    select(COD_COMUNE, DEN_COMUNE, AMBITO, AMBITO_SUB)
  
  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro
  
  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    filter(PERI == 1) # isola scarti
  
  # corregge comuni
  temp <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_COMUNE)%>%
    semi_join(appo,
              by = "COD_LOCALE_PROGETTO") %>% 
    separate_rows(COD_COMUNE, sep = ":::") %>%
    # allinea codici oc a matrix (senza regione)
    mutate(chk = nchar(COD_COMUNE)) %>% 
    filter(chk == 9) %>% 
    mutate(COD_COMUNE = substr(COD_COMUNE, 4, 9)) %>%
    select(-chk) %>% 
    left_join(comuni, by = "COD_COMUNE")
  
  # aggrega comuni
  temp1 <- temp %>%
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(COD_COMUNE = paste(COD_COMUNE, collapse=':::'),
              DEN_COMUNE = paste(DEN_COMUNE, collapse=':::')) %>% 
    left_join(temp %>%
                # elimina ":::NA" da localizzazioni senza ambito
                filter(!is.na(AMBITO)) %>% 
                # elimina dupli da localizzazioni multiple nello stesso ambito
                distinct(COD_LOCALE_PROGETTO, AMBITO, AMBITO_SUB) %>% 
                group_by(COD_LOCALE_PROGETTO) %>%
                summarise(AMBITO = paste(AMBITO, collapse=':::'),
                          AMBITO_SUB = paste(AMBITO_SUB, collapse=':::')),
              by = "COD_LOCALE_PROGETTO")
  
  
  
  # intgera pseudo
  appo <- appo  %>% 
    left_join(temp1, by = "COD_LOCALE_PROGETTO")
  
  
  # ----------------------------------------------------------------------------------- #
  # Classificazione
  
  # merge
  appo <- appo %>%
    mutate(CLASSE = AMBITO)
  
  # TODO: gestire anvhe ambito_sub
  
  # ----------------------------------------------------------------------------------- #
  # Fix manuale
  
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  # fix
  appo <- appo %>%
    mutate(CLASSE = as.character(CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y)
  
  
  out <- appo
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}


#' Export per il dataset finale (DEPRECATA)
#'
#' Popola pseudo con una lista di variabili.
#'
#' @param pseudo Dataset "pseudo".
#' @param focus Nome file da salvare in OUTPUT.
#' @param bimestre Bimestre di riferimento (utilizzato per la composizone del nome file in OUTPUT.
#' @param var_ls Elenco delle variabili di base da esportare. Se nullo usa get_default_vars.
#' @param var_add Elenco delle ulteriori variabili da esportare oltre a quelle di var_ls (se si usa get_default_vars)).
#' @param export Vuoi salvare?
#' @return Un file "[focus]_[bimestre].csv".
export_data <- function(pseudo, focus, bimestre, var_ls=NULL, var_add=NULL, export=TRUE) {
  # DEV: aggiungere progetti in scope
  
  # merge con progetti
  perimetro <- pseudo %>%
    # isola scarti
    filter(PERI == 1) %>%
    select(-CHK, -PERI) # %>%
  # merge variabili anagrafiche (da progetti)
  # left_join(progetti %>%
  #             select("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO", "OC_CODICE_PROGRAMMA"),
  #           by = "COD_LOCALE_PROGETTO")
  
  # x_vars
  # perimetro <- get_x_vars(perimetro, progetti = progetti)
  
  # macroarea
  # perimetro <- get_macroarea(perimetro)
  
  # regione
  # perimetro <- get_regione_simply(perimetro)
  
  # var_ls
  if (is.null(var_ls)) {
    # var_ls <- c("CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
    #             "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
    #             "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA",
    #             "COD_PROCED_ATTIVAZIONE", "DESCR_PROCED_ATTIVAZIONE",
    #             "CUP_COD_NATURA", "CUP_DESCR_NATURA",
    #             "COD_REGIONE",
    #             # "DEN_REGIONE", "COD_PROVINCIA", "DEN_PROVINCIA", "COD_COMUNE", "DEN_COMUNE",
    #             # "OC_COD_SLL", "OC_DENOMINAZIONE_SLL",
    #             "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
    var_ls <- get_default_vars()
  }
  perimetro <- perimetro %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", var_ls),
              by = "COD_LOCALE_PROGETTO") # %>%
  # fix per case in natura CUP
  # mutate(CUP_DESCR_NATURA = ifelse(is.na(CUP_DESCR_NATURA), "NON CLASSIFICATO", toupper(CUP_DESCR_NATURA)))
  
  # var_add
  # aggiunge ulteriori spefiche varibili
  if (!is.null(var_add)) {
    perimetro <- perimetro %>%
      left_join(progetti %>%
                  select("COD_LOCALE_PROGETTO", var_add),
                by = "COD_LOCALE_PROGETTO")
  }
  
  # Dimensione finanziaria
  perimetro <- get_dimensione_fin(perimetro)
  # MEMO: versione più fine rispetto a quella di OC
  
  # Stato di attuazione
  # perimetro <- get_stato_attuazione(df = perimetro, chk_today = "20180531")
  # perimetro <- perimetro %>%
  #   left_join(progetti %>%
  #               select("COD_LOCALE_PROGETTO", "OC_STATO_FASI"),
  #             by = "COD_LOCALE_PROGETTO")
  
  # export
  if (export == TRUE) {
    temp <- paste0(paste(focus, bimestre, sep = "_"), ".csv")
    write.csv2(perimetro, file.path(TEMP, temp), na = "", row.names = FALSE)
  }
  
  return(perimetro)
}



#' Export per il dataset finale in formato excel
#'
#' Esporta perimetro da export_data() in excel.
#'
#' @param perimetro Dataset "perimetro" creato con export_data().
#' @param focus Nome file da salvare in OUTPUT.
#' @param bimestre Bimestre di riferimento (utilizzato per la composizone del nome file in OUTPUT.
#' @param use_template Vuoi usare un template?
#' @return Un file "[focus]_[bimestre].csv".
export_data_xls <- function(perimetro, focus, bimestre, use_template=FALSE) {
  
  # library("openxlsx")
  temp <- paste0(paste(focus, bimestre, sep = "_"), ".xlsx")
  
  if (use_template == TRUE) {
    message("DA IMPLEMENTARE")
    # wb <- loadWorkbook(system.file("extdata", "template.xlsx", package = "oc", mustWork = TRUE))
    # removeTable(wb = wb, sheet = "dati", table = getTables(wb, sheet = "dati"))
    # writeDataTable(wb, sheet = "dati", x = perimetro, stack = TRUE)
    # saveWorkbook(wb, file = file.path(OUTPUT, temp), overwrite = TRUE)
    
    # OLD: FORSE DA BUTTARE
    # for (i in seq_along(tab_ls)) {
    #   print(names(tab_ls)[i])
    #   removeTable(wb = wb, sheet = names(tab_ls)[i], table = getTables(wb, sheet = names(tab_ls)[i]))
    #   writeDataTable(wb, sheet = names(tab_ls)[i], x = tab_ls[[i]], stack = TRUE)
    # }
    #
    
  } else {
    tab_ls <- list(perimetro = perimetro)
    write.xlsx(tab_ls, file = file.path(OUTPUT, temp), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
  }
  
  
  
  # wb <- loadWorkbook(file.path(src_path, "template.xlsx"))
  # removeTable(wb = wb, sheet = "dati", table = getTables(wb, sheet = "dati"))
  # writeDataTable(wb, sheet = "dati", x = perimetro, stack = TRUE)
  # saveWorkbook(wb, file = file.path(dat_path, paste0(paste(this_path, oc_ver, sep = "_"), ".xlsx")), overwrite = TRUE)
  
}





# ----------------------------------------------------------------------------------- #
# reload
reload_perimetro <- function(focus=NULL, bimestre=NULL, livelli_classe) {
  
  # load
  # perimetro <- read_csv2(file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")))
  temp <- paste0(paste(focus, bimestre, sep = "_"), ".csv")
  if (file.exists(temp)) {
    perimetro <- read_csv2(file.path(TEMP, temp))
  } else {
    perimetro <- read_csv2(file.path(TEMP, "dati.csv"))
  }
  
  
  # etc
  # reg_cn <- c("001", "002", "003", "004", "005", "006",
  #             "007", "008", "009", "010", "011", "012")
  # names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
  #                    "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  #
  # reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  # names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  #
  # temp <- c(names(reg_cn[1:3]), "PA TRENTO", "PA BOLZANO", names(reg_cn[5:12]), names(reg_sud), "ALTRO TERRITORIO")
  
  # refactor
  out <- perimetro %>%
    refactor_progetti(.) %>%
    mutate(# CLASSE_FIN = factor(CLASSE_FIN, levels=c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty")),
      # MACROAREA = factor(MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
      # STATO_PROCED = factor(STATO_PROCED, levels = c("Programmazione", "Avvio", "Progettazione", "Affidamento", "Esecuzione", "Esercizio")),
      # CUP_DESCR_NATURA = factor(CUP_DESCR_NATURA,
      #                           levels=c("REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)",
      #                                    "ACQUISTO DI BENI",
      #                                    "ACQUISTO O REALIZZAZIONE DI SERVIZI",
      #                                    "CONCESSIONE DI INCENTIVI AD UNITA' PRODUTTIVE",
      #                                    "CONCESSIONE DI CONTRIBUTI AD ALTRI SOGGETTI (DIVERSI DA UNITA' PRODUTTIVE)",
      #                                    "NON CLASSIFICATO")),
      # DEN_REGIONE = factor(DEN_REGIONE, levels = temp),
      CLASSE = factor(CLASSE, levels = livelli_classe))
  
  return(out)
  
}



#' Export perimetro to SAS
#'
#' Crea file da passare a SAS per il popolmaneto della variabile FOCUS.
#'
#' @param perimetro Dataset "perimetro" da \link[octk]{make_perimetro_edit}o \link[octk]{make_perimetro_std.} 
#' @param focus Nome del file da salvare.
#' @param use_drive Logico. Stai salvano in Drive (TRUE) o in locale (FALSE)?
#' @param keep_classe Logico. Vuoi tenere la variabile CLASSE?
#' @param split_classe Logico. Vuoi separare N file in base alla variabile CLASSE?
#' @return Un file di tipo "[focus]_clp.csv". Viene salvato in PERIMETRI/OUTPUT_SAS se use_drive == TRUE.
export_sas <- function(perimetro, focus="perimetro", use_drive=TRUE, keep_classe=FALSE, split_classe=FALSE) {
  # funzione di esportazione
  if (use_drive == TRUE) {
    export_fun <- function(df, focus) {
      # salva in WORK
      temp_filename <- paste0(focus, "_clp.csv")
      write.csv2(df, file.path(OUTPUT, temp_filename), na = "", row.names = FALSE)
      
      # salva copia ridondante per SAS
      OUTPUT_SAS <- file.path(dirname(dirname(WORK)), "_OUTPUT_SAS")
      write.csv2(df, file.path(OUTPUT_SAS, temp_filename), na = "", row.names = FALSE)
      message("Copia salvata anche in OUTPUT_SAS")
    }
  } else {
    export_fun <- function(df, focus) {
      # salva in WORK locale
      temp_filename <- paste0(focus, "_clp.csv")
      write.csv2(df, file.path(OUTPUT, temp_filename), na = "", row.names = FALSE)
      message("Ricordati di aggiornare anche OUTPUT_SAS!")
    }
  }
  
  # gestione dei casi
  if (split_classe == TRUE) {
    temp <- unique(perimetro$CLASSE)
    for (x in temp) {
      appo <- perimetro %>%
        filter(CLASSE == x) %>%
        select(COD_LOCALE_PROGETTO)
      export_fun(df = appo, focus = x)
    }
  } else {
    if (keep_classe == TRUE) {
      appo <- perimetro %>%
        select(COD_LOCALE_PROGETTO, CLASSE)
      export_fun(df = appo, focus = focus)
    } else {
      appo <- perimetro %>%
        select(COD_LOCALE_PROGETTO)
      export_fun(df = appo, focus = focus)
    }
  }
} 


# internal utility to list exported variables
get_default_vars <- function() {
  out <- c(
    'COD_LOCALE_PROGETTO',
    'CUP',
    'OC_TITOLO_PROGETTO',
    'OC_SINTESI_PROGETTO',
    'x_CICLO',
    'x_AMBITO',
    'OC_CODICE_PROGRAMMA',
    'x_PROGRAMMA',
    'COD_RISULTATO_ATTESO',
    'DESCR_RISULTATO_ATTESO',
    'OC_COD_CATEGORIA_SPESA',
    'OC_DESCR_CATEGORIA_SPESA',
    'OC_COD_ARTICOLAZ_PROGRAMMA',
    'OC_DESCR_ARTICOLAZ_PROGRAMMA',
    'OC_COD_SUBARTICOLAZ_PROGRAMMA',
    'OC_DESCR_SUBARTICOLAZ_PROGRAMMA',
    'COD_STRUMENTO',
    'DESCR_STRUMENTO',
    'DESCR_TIPO_STRUMENTO',
    'COD_PROGETTO_COMPLESSO',
    'DESCRIZIONE_PROGETTO_COMPLESSO',
    'COD_TIPO_COMPLESSITA',
    'DESCR_TIPO_COMPLESSITA',
    'CUP_COD_NATURA',
    'CUP_DESCR_NATURA',
    'CUP_COD_TIPOLOGIA',
    'CUP_DESCR_TIPOLOGIA',
    'CUP_COD_SETTORE',
    'CUP_DESCR_SETTORE',
    'CUP_COD_SOTTOSETTORE',
    'CUP_DESCR_SOTTOSETTORE',
    'CUP_COD_CATEGORIA',
    'CUP_DESCR_CATEGORIA',
    'x_REGIONE',
    'x_MACROAREA',
    'COD_PROVINCIA',
    'DEN_PROVINCIA',
    'COD_COMUNE',
    'DEN_COMUNE',
    'OC_FINANZ_UE_NETTO',
    'OC_FINANZ_TOT_PUB_NETTO',
    'IMPEGNI',
    'TOT_PAGAMENTI',
    'OC_COSTO_COESIONE',
    'OC_IMPEGNI_COESIONE',
    'OC_PAGAMENTI_COESIONE',
    'OC_STATO_PROGETTO',
    'OC_STATO_PROCEDURALE',
    'OC_COD_FASE_CORRENTE',
    'OC_DESCR_FASE_CORRENTE',
    'COD_PROCED_ATTIVAZIONE',
    'DESCR_PROCED_ATTIVAZIONE',
    'OC_CODFISC_BENEFICIARIO',
    'OC_DENOM_BENEFICIARIO'
  )
  return(out)
}


# REPORT PERIMETRI------



# ----------------------------------------------------------------------------------- #
# cicli_temi

report_cicli_temi <- function(perimetro, debug=FALSE) {
  
  cicli_temi <- perimetro %>%
    group_by(x_CICLO, x_TEMA) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  # %>%
  # refactor_ambito(.) %>%
  # refactor_ciclo(.)
  
  if (debug == TRUE) {
    cicli_temi %>%
      write.csv2(file.path(TEMP, "cicli_temi.csv"), na = "", row.names = FALSE)
  }
  
  return(cicli_temi)
  
}



# ----------------------------------------------------------------------------------- #
# cicli_ambiti

report_cicli_ambiti <- function(perimetro, debug=FALSE) {
  
  cicli_ambiti <- perimetro %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  # %>%
  #   refactor_ambito(.) %>%
  #   refactor_ciclo(.)
  
  if (debug == TRUE) {
    cicli_ambiti %>%
      write.csv2(file.path(TEMP, "cicli_ambiti.csv"), na = "", row.names = FALSE)
  }
  
  return(cicli_ambiti)
  
}



# ----------------------------------------------------------------------------------- #
# regioni

report_regioni <- function(perimetro, debug=FALSE) {
  
  
  
  # # OLD:
  # # # defactor
  # # perimetro <- perimetro %>%
  # #   mutate(x_TEMA = as.character(x_TEMA))
  # # # MEMO: evita warning quando aggiungo "Totale"
  # #
  # # # semplifica non-regioni
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")
  # 
  # # NEW BLOCK
  # if (!any(names(perimetro) == "COD_REGIONE")) {
  #   perimetro <- perimetro %>%
  #     left_join(progetti %>%
  #                 select(COD_LOCALE_PROGETTO, COD_REGIONE, DEN_REGIONE),
  #               by = "COD_LOCALE_PROGETTO")
  # }
  # 
  # # regioni
  # appo <- perimetro %>%
  #   mutate(DEN_REGIONE = ifelse(COD_REGIONE %in% c(reg_cn, reg_sud), DEN_REGIONE, "ALTRO TERRITORIO"))
  # # MEMO: semplifica DEN_REGIONE diversi da vera Regione in "ALTRO TERRITORIO"
  
  appo <- perimetro
  
  regioni <- appo %>%
    group_by(x_MACROAREA, x_REGIONE) %>%
    # group_by(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_MACROAREA, x_REGIONE) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/macroarea/regione
    # bind_rows(appo %>%
    #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
    #                       PAG = sum(PAG, na.rm = TRUE))) %>%
    # totali per ciclo + classe/macroarea/regione
    # bind_rows(appo %>%
    #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/macroarea/regione
  # bind_rows(appo %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_MACROAREA, x_REGIONE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per macroarea/regione
  # bind_rows(appo %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", x_MACROAREA, x_REGIONE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  as.data.frame() %>%
    # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
    # mutate(x_MACROAREA = factor(x_MACROAREA, levels=c("Sud", "Centro-Nord", "Nazionale", "Trasversale", "Estero"))) %>%
    refactor_macroarea(.) %>%
    mutate(x_REGIONE = factor(x_REGIONE, levels = c(names(reg_cn), names(reg_sud), "ALTRO TERRITORIO"))) %>%
    # arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_MACROAREA, x_REGIONE)
    arrange(x_MACROAREA, x_REGIONE)
  
  
  if (debug == TRUE) {
    regioni %>%
      write.csv2(file.path(TEMP, "regioni.csv"), na = "", row.names = FALSE)
  }
  
  return(regioni)
  
}



# ----------------------------------------------------------------------------------- #
# dimensioni

report_dimensioni <- function(perimetro, debug=FALSE) {
  
  # dimensioni
  dimensioni <- perimetro %>%
    group_by(x_DIM_FIN) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) # %>%
  # totali per ciclo/fondo + classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per ciclo + classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per dimensione
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", x_TEMA_FIN) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # as.data.frame() %>%
  # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
  # mutate(x_DIM_FIN = factor(x_TEMA_FIN, levels = c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty"))) %>%
  # arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_TEMA, x_TEMA_FIN)
  
  if (debug == TRUE) {
    dimensioni %>%
      write.csv2(file.path(TEMP, "dimensioni.csv"), na = "", row.names = FALSE)
  }
  
  return(dimensioni)
  
  
}


# ----------------------------------------------------------------------------------- #
# stati procedurali
# MEMO: nuova variabile usata per IDRICO e DISSESTO


report_stati <- function(perimetro, debug=FALSE) {
  
  # stato per CP
  stati <- perimetro %>%
    group_by(OC_STATO_PROCEDURALE) %>%
    summarise(N = n(),
              CP = sum(CP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/stato
    # bind_rows(perimetro %>%
    #             group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
    #                       PAG = sum(PAG, na.rm = TRUE))) %>%
    # totali per ciclo + classe/stato
    # bind_rows(perimetro %>%
    #             group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
    #             summarise(N = n(),
    #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per classe/stato
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA, OC_STATO_PROCEDURALE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # totali per stato
  # bind_rows(perimetro %>%
  #             group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", x_TEMA = "Totale", OC_STATO_PROCEDURALE) %>%
  #             summarise(N = n(),
  #                       CP = sum(CP, na.rm = TRUE),
  #                       PAG = sum(PAG, na.rm = TRUE))) %>%
  # as.data.frame() %>%
  # mutate(x_TEMA = factor(x_TEMA, levels = c(livelli_classe, "Totale"))) %>%
  mutate(OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE,
                                       levels =  c("Non avviato",
                                                   "In avvio di progettazione",
                                                   "In corso di progettazione",
                                                   "In affidamento",
                                                   "In esecuzione",
                                                   "Eseguito",
                                                   "Non determinabile"))) %>%
    arrange(OC_STATO_PROCEDURALE)
  
  
  if (debug == TRUE) {
    stati %>%
      write.csv2(file.path(TEMP, "stati.csv"), na = "", row.names = FALSE)
  }
  
  return(stati)
  
  
  
}


# ----------------------------------------------------------------------------------------- #
#workflow


workflow_report <- function(clp_csv, report_ls=NULL, progetti=NULL, use_coe=TRUE, operazioni=NULL, 
                            tema=NULL, livelli_tema=NULL, nome_file=NULL, use_template=FALSE, debug=FALSE) {
  
  # DEBUG:
  # livelli_tema <- c("Cultura", "Natura", "Turismo")
  
  if (is.null(report_ls)) {
    report_ls <- c("report_cicli_temi", "report_cicli_ambiti", "report_regioni", "report_dimensioni", "report_stati")
  }
  
  # switch per variabili finanziarie
  if (use_coe == TRUE) {
    if (is.null(operazioni)) {
      operazioni <- load_operazioni(bimestre) 
    }
    
    appo <- clp_csv %>%
      left_join(operazioni %>%
                  select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
                         x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ, x_MACROAREA, x_REGIONE,
                         COE, COE_IMP, COE_PAG,
                         OC_STATO_PROCEDURALE,
                         OC_COD_TEMA_SINTETICO),
                by = "COD_LOCALE_PROGETTO") %>%
      rename(CP = COE, 
             IMP = COE_IMP, 
             PAG = COE_PAG)
  } else {
    
    
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, light=TRUE)
    }
    
    appo <- clp_csv %>%
      left_join(progetti %>%
                  select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
                         x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_REGNAZ, x_MACROAREA, x_REGIONE,
                         OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI,
                         OC_STATO_PROCEDURALE,
                         # DEV: aggiungere beneficiario
                         OC_COD_TEMA_SINTETICO),
                by = "COD_LOCALE_PROGETTO") %>%
      rename(CP = OC_FINANZ_TOT_PUB_NETTO, 
             IMP = IMPEGNI, 
             PAG = TOT_PAGAMENTI)
  }
  
  # DEV: qui potrebbe essere necessario aggiungere campi da "progetti" a "operazioni", quindi "progetti" andrebbe importato prima di if
  
  # switch per tema
  if (is.null(tema)) {
    appo1 <- appo %>%
      rename(x_TEMA = OC_COD_TEMA_SINTETICO)
    
    if (is.null(livelli_tema)) {
      livelli_tema <- unique(appo$OC_COD_TEMA_SINTETICO)
    }
    
    # TODO: qui va ricodificato codice con descrizione
    # TODO: qui va iserito factor
  } else if (tema == "CLASSE") {
    
    if (is.null(livelli_tema)) {
      livelli_tema <- unique(appo$CLASSE)
    }
    
    appo1 <- appo %>%
      rename(x_TEMA = CLASSE) %>%
      mutate(x_TEMA = factor(x_TEMA, levels = livelli_tema))
  } 
  
  # dimensione finanziaria
  appo2 <- get_dim_fin(df = appo1, debug_mode=FALSE) 
  
  
  # export
  export_report_edit(perimetro = appo2, report_ls = report_ls, nome_file, debug, use_template) 
  
}












#' Wrapper per report con lista di contenuti editabile
#'
#' Wrapper per report con lista di contenuti editabile.
#'
#' @param perimetro Dataset da workflow_report
#' @param report_ls Elenco dei report da generare
#' @return Un dataframe con COD_LOCALE_PROGETTO, QUERY_[1], QUERY_[2], QUERY_[N] e TIPO_QUERY.
export_report_edit <- function(perimetro, report_ls, nome_file=NULL, debug=FALSE, use_template=FALSE) {
  
  # report_ls <- c("report_cicli_temi", "report_cicli_ambiti", "report_regioni", "report_dimensioni", "report_stati")
  tab_list <- list()
  
  appo <- perimetro
  # appo <- appo2
  
  # query
  for (q in report_ls) {
    print(q)
    tab_list[[q]] <- do.call(q, list(appo, debug))
  }
  
  # libs
  library("openxlsx")
  
  if (is.null(nome_file)) {
    nome_file <- "elaborazione.xlsx"
  }
  
  if (use_template == FALSE) {
    # write all tables
    # MEMO: usato al primo giro per creare template (poi integrato a mano)
    write.xlsx(tab_list, file = file.path(OUTPUT, nome_file), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
    # CHK: verificare numero righe con formato...
  } else {
    
    # edit template
    # wb <- loadWorkbook(file.path(src_path, "elab_template.xlsx"))
    # wb <- loadWorkbook(system.file("extdata", "elab_template.xlsx", package = "oc", mustWork = TRUE))
    wb <- loadWorkbook(system.file("extdata", "elab_template.xlsx", package = "octk", mustWork = TRUE))
    for (i in seq_along(tab_list)) {
      print(names(tab_list)[i])
      removeTable(wb = wb, sheet = names(tab_list)[i], table = getTables(wb, sheet = names(tab_list)[i]))
      writeDataTable(wb, sheet = names(tab_list)[i], x = tab_list[[i]], stack = TRUE)
    }
    saveWorkbook(wb, file = file.path(OUTPUT, nome_file), overwrite = TRUE)
    
    # DEV: inserire formati
    
  }
}

