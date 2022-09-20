# Report regionali
# Liste progetti per programmi FSC

#' Lista di progetti dei Patti
#'
#' Esporta la lista di progetti di uno o più Patto, in formato Report Ministro.
#'
#' @param patti_list Elenco dei Patti da esportare, in formato c("2016PATTIABR").
#' @param progetti File in formato "progetti" OCTK.
#' @param perimetro Perimetro di riferimento per ulteriore approfondimento (es. regione). Opzionale.
#' @param min_cp Dimensione minima dei progetti da considerare (in euro).
#' @param export Vuoi salvare in csv in TEMP?
#' @param bimestre Quale bimestre di riferimento?
#' @param focus Suffisso per file in export
#' @return L'intero datbase dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
make_lista_patti <- function(patti_list=NULL, progetti, perimetro=NULL,
                             min_cp=0,
                             export=TRUE, bimestre=bimestre, focus=focus,
                             DATA=DATA, TEMP=TEMP)
{

  # load operazioni
  operazioni <- read_sas(file.path(DATA, "operazioni_pucok.sas7bdat")) %>%
    select("COD_LOCALE_PROGETTO" = "cod_locale_progetto",
           "COD_SETTORE_STRATEGICO_FSC" = "fsc_settore_strategico",
           "DESCR_SETTORE_STRATEGICO_FSC" = "fsc_descr_settore_strategico",
           "COD_ASSE_TEMATICO_FSC" = "fsc_asse_tematico",
           "DESCR_ASSE_TEMATICO_FSC" = "fsc_descr_asse_tematico")

  # load ambito_FSC1420.csv
  # ambito_FSC1420 <- read_csv2(file.path(DATA, "ambito_FSC1420.csv"))

  if (missing(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  }

  if (!any(names(progetti) == "OC_SINTESI_PROGETTO")) {
    appo <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = FALSE) %>%
      select(COD_LOCALE_PROGETTO, OC_SINTESI_PROGETTO)

    progetti <- progetti %>%
      left_join(appo, by = "COD_LOCALE_PROGETTO")
  }

  if (is.null(patti_list)) {
    # load programmazione FSC
    patti_list <- load_db(ciclo = "2014-2020", ambito = "FSC", simplify_loc=FALSE, use_temi=FALSE) %>%
      # filter(OC_TIPOLOGIA_PROGRAMMA == "Patto Territoriale") %>%
      filter(OC_TIPOLOGIA_PROGRAMMA == "Patti per lo sviluppo") %>%
      distinct(OC_CODICE_PROGRAMMA) %>%
      .$OC_CODICE_PROGRAMMA
  }

  # switch
  if (is.null(perimetro)) {
    patti <- progetti %>%
      filter(OC_CODICE_PROGRAMMA %in% patti_list) %>%
      filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)

  } else {
    patti <- progetti %>%
      filter(OC_CODICE_PROGRAMMA %in% patti_list) %>%
      filter(COD_LOCALE_PROGETTO %in% perimetro$COD_LOCALE_PROGETTO) %>%
      filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)
  }

  # lista patti
  patti <- patti %>%
    left_join(operazioni %>%
                # CHK: verificare se qui sotto è sufficiente il distinct
                distinct(COD_LOCALE_PROGETTO,
                         COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC,
                         COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC) %>%
                mutate(AREA_TEAMATICA = paste(COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC, sep = ". "),
                       TEMA_PRIORITARIO = paste(COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC, sep = ". ")) %>%
                select(COD_LOCALE_PROGETTO, AREA_TEAMATICA, TEMA_PRIORITARIO),
              by = "COD_LOCALE_PROGETTO") %>%
    arrange(desc(OC_FINANZ_TOT_PUB_NETTO)) %>%
    select(x_PROGRAMMA, CUP, OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO, CUP_DESCR_SETTORE,
           AREA_TEAMATICA, TEMA_PRIORITARIO,
           OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI, OC_STATO_PROCEDURALE,
           OC_DENOM_BENEFICIARIO, DEN_PROVINCIA, DEN_COMUNE)

  if (export == TRUE) {
    write.csv2(patti, file.path(TEMP, "lista_patti.csv"), row.names = FALSE, na = "0")
  }
  # DEV: manca return
}


#' Lista di progetti dei Piani nazionali FSC
#'
#' Esporta la lista di progetti di uno o più iani nazionali FSC, in formato Report Ministro.
#'
#' @param piani_list Elenco dei Piani da esportare, in formato c("2016PATTIABR").
#' @param progetti File in formato "progetti" OCTK.
#' @param perimetro Perimetro di riferimento per ulteriore approfondimento (es. regione). Opzionale.
#' @param use_all Logico. Vuoi considerate anche le altre assegnazioni CIPE? Altrimenti considera solo Piani nazionali in senso stretto.
#' @param min_cp Dimensione minima dei progetti da considerare (in euro).
#' @param export Vuoi salvare in csv in TEMP?
#' @param bimestre Quale bimestre di riferimento?
#' @param focus Suffisso per file in export
#' @return L'intero datbase dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
make_lista_piani <- function(piani_list=NULL, progetti, perimetro=NULL,
                             min_cp=0,
                             use_all=TRUE,
                             export=TRUE, bimestre=bimestre, focus=focus,
                             DATA=DATA, TEMP=TEMP)
{

  # load operazioni
  operazioni <- read_sas(file.path(DATA, "operazioni_pucok.sas7bdat")) %>%
    select("COD_LOCALE_PROGETTO" = "cod_locale_progetto",
           "COD_SETTORE_STRATEGICO_FSC" = "fsc_settore_strategico",
           "DESCR_SETTORE_STRATEGICO_FSC" = "fsc_descr_settore_strategico",
           "COD_ASSE_TEMATICO_FSC" = "fsc_asse_tematico",
           "DESCR_ASSE_TEMATICO_FSC" = "fsc_descr_asse_tematico")

  if (is.null(piani_list)) {
    if (use_all == TRUE) {
      # test <- c("Piano Operativo", "Altro")
      test <- c("Piani nazionali", "Altre assegnazioni CIPE")

    } else {
      # test <- c("Piano Operativo")
      test <- c("Piani nazionali")
    }

    # load programmazione FSC
    piani_list <- load_db(ciclo = "2014-2020", ambito = "FSC", simplify_loc=FALSE, use_temi=FALSE) %>%
      # count(OC_TIPOLOGIA_PROGRAMMA)
      filter(OC_TIPOLOGIA_PROGRAMMA %in% test) %>%
      distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>%
      # fix
      filter(OC_CODICE_PROGRAMMA != "Vuoto2", # Azioni di Sistema
             OC_CODICE_PROGRAMMA != "2019AGCOEFSC001") %>% # CPT
      select(OC_CODICE_PROGRAMMA) %>%
      bind_rows(tibble(OC_CODICE_PROGRAMMA = c("2016XXAMPSAP00", "2015XXXXXPSO000"))) %>% # PS DISSESTO + FONDO PROGETTAZIONE
      .$OC_CODICE_PROGRAMMA

    # patti_list <- load_db(ciclo = "2014-2020", ambito = "FSC", simplify_loc=FALSE, use_temi=FALSE) %>%
    #   filter(OC_TIPOLOGIA_PROGRAMMA == "Patto Territoriale") %>%
    #   distinct(OC_CODICE_PROGRAMMA) %>%
    #   .$OC_CODICE_PROGRAMMA
  }

  if (missing(progetti)) {
    progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  }

  if (!any(names(progetti) == "OC_SINTESI_PROGETTO")) {
    appo <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = FALSE) %>%
      select(COD_LOCALE_PROGETTO, OC_SINTESI_PROGETTO)

    progetti <- progetti %>%
      left_join(appo, by = "COD_LOCALE_PROGETTO")
  }

  # switch
  if (is.null(perimetro)) {
    piani <-  progetti %>%
      filter(OC_CODICE_PROGRAMMA %in% piani_list) %>%
      filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)

  } else {
    piani <- progetti %>%
      filter(OC_CODICE_PROGRAMMA %in% piani_list) %>%
      filter(COD_LOCALE_PROGETTO %in% perimetro$COD_LOCALE_PROGETTO) %>%
      filter(OC_FINANZ_TOT_PUB_NETTO >= min_cp)
  }

  # lista patti
  piani <- piani %>%
    left_join(operazioni %>%
                # CHK: verificare se qui sotto è sufficiente il distinct
                distinct(COD_LOCALE_PROGETTO,
                         COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC,
                         COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC) %>%
                mutate(AREA_TEAMATICA = paste(COD_SETTORE_STRATEGICO_FSC, DESCR_SETTORE_STRATEGICO_FSC, sep = ". "),
                       TEMA_PRIORITARIO = paste(COD_ASSE_TEMATICO_FSC, DESCR_ASSE_TEMATICO_FSC, sep = ". ")) %>%
                select(COD_LOCALE_PROGETTO, AREA_TEAMATICA, TEMA_PRIORITARIO),
              by = "COD_LOCALE_PROGETTO") %>%
    arrange(desc(OC_FINANZ_TOT_PUB_NETTO)) %>%
    select(x_PROGRAMMA, CUP, OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO, CUP_DESCR_SETTORE,
           AREA_TEAMATICA, TEMA_PRIORITARIO,
           OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI, OC_STATO_PROCEDURALE,
           OC_DENOM_BENEFICIARIO, DEN_PROVINCIA, DEN_COMUNE)

  if (export == TRUE) {
    write.csv2(piani, file.path(TEMP, "lista_piani.csv"), row.names = FALSE, na = "0")
  }
  return(piani)
}

