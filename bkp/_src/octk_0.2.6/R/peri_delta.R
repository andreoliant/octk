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
    # DEV: siccome non uso il vecchio pseudo ma quello nuovo... ricontrollo tutto! qui andrebbe messo filtro su pseudo vecchio...
    anti_join(perim_old, by = "COD_LOCALE_PROGETTO") %>%
    filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)


  if (debug == TRUE) {
    delta_scarti %>%
      write.csv2(file.path(TEMP, "delta_scarti.csv"), na = "", row.names = FALSE)
  }

  return(delta_scarti)

}


