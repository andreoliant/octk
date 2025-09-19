# funzioni del blocco "accordi"
#da aggiornare

#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_ordinarie <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_ordinarie.xlsx"), guess_max=100000)
  return(interventi)
}

#' Carica dati interventi accordi per anticipazioni
#'
#' Carica dati interventi accordi per anticipazioni
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_anticipazioni <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_anticipazioni.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_complementari <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_complementari.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per completamenti Campania
#'
#' Carica dati interventi accordi per completamenti Campania
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_completamenti <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_completamenti.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_cofinanziamenti <- function(DB) {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_cofinanziamenti_por.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi 
#'
#' Carica dati interventi accordi per tutte le assegnazioni
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi <- function(DB) {
  # Verifica se esiste il file unico degli accordi
  file_accordi <- file.path(DB, "Interventi_DBCOE_accordi.xlsx")
  
  if (file.exists(file_accordi)) {
    # Se il file esiste, caricalo direttamente
    interventi <- read_xlsx(file_accordi, guess_max=100000)
  } else {
    # Altrimenti, procedi con il caricamento dei singoli file
    appo1 <- load_db_accordi_ordinarie(DB)
    appo2 <- load_db_accordi_anticipazioni(DB)
    appo3 <- load_db_accordi_complementari(DB)
    appo4 <- load_db_accordi_completamenti(DB)
    appo5 <- load_db_accordi_cofinanziamenti(DB)
    
    interventi <- appo1 %>% 
      bind_rows(appo2) %>% 
      bind_rows(appo3) %>% 
      bind_rows(appo4) %>% 
      bind_rows(appo5)
  }
  
  return(interventi)
}

#' Workflow per template monitoraggio extra-sistema accordi
#' 
#' Workflow per template monitoraggio extra-sistema accordi
#'
#' @param interventi Elenco interventi da load_db_accordi()
#' @param template Nome del file xlsx in input
#' @return File in output nella cartella "monitoraggio"
workflow_accordi_monitoraggio <- function(interventi, template) {
  
  # DEBUG:
  # template <- "template_monitoraggio_accordi.xlsx"
  
  regioni <- interventi %>% 
    distinct(AMMINISTRAZIONE_TITOLARE) %>% 
    .$AMMINISTRAZIONE_TITOLARE
  
  dir.create(file.path(OUTPUT, "monitoraggio"))
  
  for (regione in regioni) {
    print(regione)
    crea_report_accordi_monitoraggio(regione, interventi, template) 
  }
  
}

#' Crea template monitoraggio extra-sistema accordi per una regione
#' 
#' Crea template monitoraggio extra-sistema accordi per una regione
#'
#' @param regione valore per selezionare AMMINISTRAZIONE_TITOLARE
#' @param interventi Elenco interventi da load_db_accordi()
#' @param template Nome del file xlsx in input
#' @return File in output nella cartella "monitoraggio"
crea_report_accordi_monitoraggio <- function(regione, interventi, template) {
  
  # DEBUG:
  # regione <- "MARCHE"
  # template <- "template_monitoraggio_accordi.xlsx"
  wb <- loadWorkbook(file.path(INPUT, template))
  
  # setup_stili()
  
  appo1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    # filter(AMBITO == "FSC") %>% 
    filter(SEZIONE %in% c("Ordinaria", "Stralcio 2 (delibera CIPESS n. 57/2024)", "Complementare")) %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(TIPOLOGIA == "Intervento") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR) %>% 
    mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR + FINANZ_REG) %>% #NEW: modifica introdotta per FdR Puglia con risorse regionali
    select(ID, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, 
           AREA_TEMATICA, SETTORE_INTERVENTO, 
           CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_COE)
  
  appo2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    # filter(AMBITO == "FSC") %>% 
    filter(SEZIONE %in% c("Ordinaria", "Stralcio 2 (delibera CIPESS n. 57/2024)", "Complementare")) %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(TIPOLOGIA == "Linea") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    # mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR) %>% 
    mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR + FINANZ_REG) %>% #NEW: modifica introdotta per FdR Puglia con risorse regionali
    select(ID, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, 
           AREA_TEMATICA, SETTORE_INTERVENTO, 
           TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_COE)

  writeData(wb, sheet = "progetti", x = appo1, startCol = 1, startRow = 3, colNames = FALSE)
  
  start_row <- 3
  n_max <- dim(appo1)[1] + start_row -1
  
  addStyle(wb, sheet = "progetti", style_border, rows = 2, cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = "progetti", style_border, rows = seq(start_row, n_max), cols = c(1:8, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "progetti", style_number2, rows = seq(start_row, n_max), cols = c(9:15), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "progetti", style_date, rows = seq(start_row, n_max), cols = c(16:21), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet = "progetti", cols = c(1:8, 22), widths = 24)
  setColWidths(wb, sheet = "progetti", cols = c(9:15), widths = 16)
  setColWidths(wb, sheet = "progetti", cols = c(16:21), widths = 12)
  
  ungroupColumns(wb, "progetti", cols = 1:22)
  
  writeData(wb, sheet = "linee", x = appo2, startCol = 1, startRow = 3, colNames = FALSE)
  
  start_row <- 3
  
  if (dim(appo2)[1]>0) {
    n_max <- dim(appo2)[1] + start_row -1
  } else {
    n_max <- start_row + 1
  }

  addStyle(wb, sheet = "linee", style_border, rows = 2, cols = c(1:9), gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = "linee", style_border, rows = seq(start_row, n_max), cols = c(1:7), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "linee", style_number2, rows = seq(start_row, n_max), cols = c(8:9), gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet = "linee", cols = c(1:7), widths = 24)
  setColWidths(wb, sheet = "linee", cols = c(8:9), widths = 16)

  ungroupColumns(wb, "linee", cols = 1:9)
  
  temp_file <- paste0(regione, "_monitoraggio_accordi.xlsx")
  saveWorkbook(wb, file = file.path(OUTPUT, "monitoraggio", temp_file), overwrite = TRUE)
  
}

#' Workflow per template pubblicazione accordi
#' 
#' Workflow per template pubblicazione accordi
#'
#' @param interventi Elenco interventi da load_db_accordi()
#' @return File in output nella cartella "pubblicazione"
workflow_accordi_pubblicazione <- function(interventi) {
  
  # DEBUG:
  # template <- "template_pubblicazione_accordi.xlsx"
  
  regioni <- interventi %>% 
    distinct(AMMINISTRAZIONE_TITOLARE) %>% 
    .$AMMINISTRAZIONE_TITOLARE
  
  # scarta interventi annuallati presenti per memoria
  interventi <- interventi %>% 
    filter(FINANZ_TOT > 0)
  
  dir.create(file.path(OUTPUT, "pubblicazione"))
  
  for (regione in regioni) {
    print(regione)
    
    if (regione %in% c("MARCHE", "PUGLIA", "SARDEGNA")) {
      crea_accordi_pubblicazione_base(regione, interventi)
    } else if (regione == "CAMPANIA") {
      crea_accordi_pubblicazione_campania(regione, interventi)
    } else {
      crea_accordi_pubblicazione_nofdr(regione, interventi)
    }
  }
}

#' Crea template pubblicazione accordi per una regione (base)
#' 
#' Crea template pubblicazione accordi per una regione - Versione base con FdR
#'
#' @param regione valore per selezionare AMMINISTRAZIONE_TITOLARE
#' @param interventi Elenco interventi da load_db_accordi()
#' @return File in output nella cartella "pubblicazione"
crea_accordi_pubblicazione_base <- function(regione, interventi) {
  
  # DEBUG:
  # regione <- "MARCHE"
  
  template <- "template_pubblicazione_accordi_base.xlsx"
  wb <- loadWorkbook(file.path(INPUT, template))
  
  titolo <- tibble(TITOLO = c(paste0("Accordo per la Coesione Governo - Regione ", regione)))
  
  start_row <- 6
  
  # setup_stili()
  
  temi <- tibble(AREA_TEMATICA = c("01-RICERCA E INNOVAZIONE",
                                   "02-DIGITALIZZAZIONE",
                                   "03-COMPETITIVITÀ IMPRESE",
                                   "04-ENERGIA",
                                   "05-AMBIENTE E RISORSE NATURALI",
                                   "06-CULTURA",
                                   "07-TRASPORTI E MOBILITÀ",
                                   "08-RIQUALIFICAZIONE URBANA",
                                   "09-LAVORO E OCCUPABILITÀ",
                                   "10-SOCIALE E SALUTE",
                                   "11-ISTRUZIONE E FORMAZIONE",
                                   "12-CAPACITÀ AMMINISTRATIVA"))
  
  art3_fsc <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Anticipazioni")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
    
  art3_fdr <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Complementare")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),
                          FINANZ_REG = sum(FINANZ_REG, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_cofin <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria", "Complementare")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_ALTRO_PSC = sum(FINANZ_ALTRO_PSC, na.rm = TRUE),
                          FINANZ_ALTRO_POC = sum(FINANZ_ALTRO_POC, na.rm = TRUE),
                          FINANZ_ALTRO_UE = sum(FINANZ_ALTRO_UE, na.rm = TRUE),
                          FINANZ_ALTRO_PNRR = sum(FINANZ_ALTRO_PNRR, na.rm = TRUE),
                          FINANZ_ALTRO_LOC = sum(FINANZ_ALTRO_LOC, na.rm = TRUE),
                          FINANZ_ALTRO_NAZ = sum(FINANZ_ALTRO_NAZ, na.rm = TRUE),
                          FINANZ_ALTRO_PRIV = sum(FINANZ_ALTRO_PRIV, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_n <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(N = n()),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_cofin_pr <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Cofinanziamento PR")) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE))
  
  writeData(wb, sheet = "ART3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_fsc, startCol = 2, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_fdr, startCol = 5, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_cofin, startCol = 7, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_n, startCol = 16, startRow = 6, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_cofin_pr, startCol = 2, startRow = 19, colNames = FALSE)
  
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(4), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(5), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(6:18), cols = c(1, 16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(6:18), cols = c(2:15), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(19:20), cols = c(1), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(19:20), cols = c(2:4), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "ART3", cols = 1:16)
  
  
  a1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A1", x = a1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a1)[1] + start_row -1
  addStyle(wb, sheet = "A1", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)

  ungroupColumns(wb, "A1", cols = 1:30)
  
  
  a2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Anticipazioni")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC,
           NOTE)
  
  writeData(wb, sheet = "A2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A2", x = a2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a2)[1] + start_row -1
  addStyle(wb, sheet = "A2", style_border_blue, rows = c(4), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = c(5), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_number2, rows = seq(start_row, n_max), cols = c(8:9), gridExpand = TRUE, stack = TRUE)

  ungroupColumns(wb, "A2", cols = 1:10)
  

  a3 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Complementare")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A3", x = a3, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a3)[1] + start_row -1
  addStyle(wb, sheet = "A3", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A3", cols = 1:30)
  
  
  b1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    group_by(SEZIONE) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
              SPESA_2023 = sum(SPESA_2023, na.rm = TRUE),
              SPESA_2024 = sum(SPESA_2024, na.rm = TRUE),
              SPESA_2025 = sum(SPESA_2025, na.rm = TRUE),
              SPESA_2026 = sum(SPESA_2026, na.rm = TRUE),
              SPESA_2027 = sum(SPESA_2027, na.rm = TRUE),
              SPESA_2028 = sum(SPESA_2028, na.rm = TRUE),
              SPESA_2029 = sum(SPESA_2029, na.rm = TRUE),
              SPESA_2030 = sum(SPESA_2030, na.rm = TRUE),
              SPESA_2031 = sum(SPESA_2031, na.rm = TRUE),
              SPESA_2032 = sum(SPESA_2032, na.rm = TRUE),
              SPESA_2033 = sum(SPESA_2033, na.rm = TRUE),
              SPESA_2034 = sum(SPESA_2034, na.rm = TRUE),
              SPESA_2035 = sum(SPESA_2035, na.rm = TRUE))
  
  writeData(wb, sheet = "B1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B1", x = b1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b1)[1] + start_row -1
  addStyle(wb, sheet = "B1", style_border_blue, rows = c(4), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = c(5), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = seq(start_row, n_max), cols = c(1, 16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_number2, rows = seq(start_row, n_max), cols = c(2:15), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B1", cols = 1:16)
  
  
  b2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FSC,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B2", x = b2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b2)[1] + start_row -1
  addStyle(wb, sheet = "B2", style_border_blue, rows = c(4), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = c(5), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_number2, rows = seq(start_row, n_max), cols = c(8:21), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B2", cols = 1:22)
  
  
  b3 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Complementare")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FDR, FINANZ_REG,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B3", x = b3, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b3)[1] + start_row -1
  addStyle(wb, sheet = "B3", style_border_blue, rows = c(4), cols = c(1:23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_border, rows = c(5), cols = c(1:23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_border, rows = seq(start_row, n_max), cols = c(1:7, 23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_number2, rows = seq(start_row, n_max), cols = c(8:22), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B3", cols = 1:23)
  
  temp_file <- paste0("Allegati_Accordo_Coesione_Regione_", regione, ".xlsx")
  saveWorkbook(wb, file = file.path(OUTPUT, "pubblicazione", temp_file), overwrite = TRUE)
  
}


#' Crea template pubblicazione accordi per una regione (no FdR)
#' 
#' Crea template pubblicazione accordi per una regione - Versione senza FdR
#'
#' @param regione valore per selezionare AMMINISTRAZIONE_TITOLARE
#' @param interventi Elenco interventi da load_db_accordi()
#' @return File in output nella cartella "pubblicazione"
crea_accordi_pubblicazione_nofdr <- function(regione, interventi) {
  
  # DEBUG:
  # regione <- "MARCHE"
  
  template <- "template_pubblicazione_accordi_nofdr.xlsx"
  wb <- loadWorkbook(file.path(INPUT, template))
  
  titolo <- tibble(TITOLO = c(paste0("Accordo per la Coesione Governo - Regione ", regione)))
  
  start_row <- 6
  
  # setup_stili()
  
  temi <- tibble(AREA_TEMATICA = c("01-RICERCA E INNOVAZIONE",
                                   "02-DIGITALIZZAZIONE",
                                   "03-COMPETITIVITÀ IMPRESE",
                                   "04-ENERGIA",
                                   "05-AMBIENTE E RISORSE NATURALI",
                                   "06-CULTURA",
                                   "07-TRASPORTI E MOBILITÀ",
                                   "08-RIQUALIFICAZIONE URBANA",
                                   "09-LAVORO E OCCUPABILITÀ",
                                   "10-SOCIALE E SALUTE",
                                   "11-ISTRUZIONE E FORMAZIONE",
                                   "12-CAPACITÀ AMMINISTRATIVA"))
  
  art3_fsc <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Anticipazioni")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_fdr <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Complementare")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),
                          FINANZ_REG = sum(FINANZ_REG, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_cofin <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria", "Complementare")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_ALTRO_PSC = sum(FINANZ_ALTRO_PSC, na.rm = TRUE),
                          FINANZ_ALTRO_POC = sum(FINANZ_ALTRO_POC, na.rm = TRUE),
                          FINANZ_ALTRO_UE = sum(FINANZ_ALTRO_UE, na.rm = TRUE),
                          FINANZ_ALTRO_PNRR = sum(FINANZ_ALTRO_PNRR, na.rm = TRUE),
                          FINANZ_ALTRO_LOC = sum(FINANZ_ALTRO_LOC, na.rm = TRUE),
                          FINANZ_ALTRO_NAZ = sum(FINANZ_ALTRO_NAZ, na.rm = TRUE),
                          FINANZ_ALTRO_PRIV = sum(FINANZ_ALTRO_PRIV, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_n <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(N = n()),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_cofin_pr <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Cofinanziamento PR")) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE))
  
  writeData(wb, sheet = "ART3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_fsc, startCol = 2, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_fdr, startCol = 5, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_cofin, startCol = 7, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_n, startCol = 16, startRow = 6, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_cofin_pr, startCol = 2, startRow = 19, colNames = FALSE)
  
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(4), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(5), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(6:18), cols = c(1, 16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(6:18), cols = c(2:15), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(19:20), cols = c(1), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(19:20), cols = c(2:4), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "ART3", cols = 1:16)
  
  
  a1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A1", x = a1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a1)[1] + start_row -1
  addStyle(wb, sheet = "A1", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A1", cols = 1:30)
  
  
  a2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Anticipazioni")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC,
           NOTE)
  
  writeData(wb, sheet = "A2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A2", x = a2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a2)[1] + start_row -1
  addStyle(wb, sheet = "A2", style_border_blue, rows = c(4), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = c(5), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_number2, rows = seq(start_row, n_max), cols = c(8:9), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A2", cols = 1:10)

  
  b1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    group_by(SEZIONE) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
              SPESA_2023 = sum(SPESA_2023, na.rm = TRUE),
              SPESA_2024 = sum(SPESA_2024, na.rm = TRUE),
              SPESA_2025 = sum(SPESA_2025, na.rm = TRUE),
              SPESA_2026 = sum(SPESA_2026, na.rm = TRUE),
              SPESA_2027 = sum(SPESA_2027, na.rm = TRUE),
              SPESA_2028 = sum(SPESA_2028, na.rm = TRUE),
              SPESA_2029 = sum(SPESA_2029, na.rm = TRUE),
              SPESA_2030 = sum(SPESA_2030, na.rm = TRUE),
              SPESA_2031 = sum(SPESA_2031, na.rm = TRUE),
              SPESA_2032 = sum(SPESA_2032, na.rm = TRUE),
              SPESA_2033 = sum(SPESA_2033, na.rm = TRUE),
              SPESA_2034 = sum(SPESA_2034, na.rm = TRUE),
              SPESA_2035 = sum(SPESA_2035, na.rm = TRUE))
  
  writeData(wb, sheet = "B1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B1", x = b1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b1)[1] + start_row -1
  addStyle(wb, sheet = "B1", style_border_blue, rows = c(4), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = c(5), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = seq(start_row, n_max), cols = c(1, 16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_number2, rows = seq(start_row, n_max), cols = c(2:15), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B1", cols = 1:16)
  
  
  b2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FSC,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B2", x = b2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b2)[1] + start_row -1
  addStyle(wb, sheet = "B2", style_border_blue, rows = c(4), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = c(5), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_number2, rows = seq(start_row, n_max), cols = c(8:21), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B2", cols = 1:22)
  
  temp_file <- paste0("Allegati_Accordo_Coesione_Regione_", regione, ".xlsx")
  saveWorkbook(wb, file = file.path(OUTPUT, "pubblicazione", temp_file), overwrite = TRUE)
  
}



#' Crea template pubblicazione accordi per regione Campania
#' 
#' Crea template pubblicazione accordi per una regione - Versione speciale per Campania
#'
#' @param regione valore per selezionare AMMINISTRAZIONE_TITOLARE
#' @param interventi Elenco interventi da load_db_accordi()
#' @return File in output nella cartella "pubblicazione"
crea_accordi_pubblicazione_campania <- function(regione, interventi) {
  
  # DEBUG:
  # regione <- "MARCHE"
  
  template <- "template_pubblicazione_accordi_campania.xlsx"
  wb <- loadWorkbook(file.path(INPUT, template))
  
  titolo <- tibble(TITOLO = c(paste0("Accordo per la Coesione Governo - Regione ", regione)))
  
  start_row <- 6
  
  # setup_stili()
  
  temi <- tibble(AREA_TEMATICA = c("01-RICERCA E INNOVAZIONE",
                                   "02-DIGITALIZZAZIONE",
                                   "03-COMPETITIVITÀ IMPRESE",
                                   "04-ENERGIA",
                                   "05-AMBIENTE E RISORSE NATURALI",
                                   "06-CULTURA",
                                   "07-TRASPORTI E MOBILITÀ",
                                   "08-RIQUALIFICAZIONE URBANA",
                                   "09-LAVORO E OCCUPABILITÀ",
                                   "10-SOCIALE E SALUTE",
                                   "11-ISTRUZIONE E FORMAZIONE",
                                   "12-CAPACITÀ AMMINISTRATIVA"))
  
  art3_fsc <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Stralcio 2 (delibera CIPESS n. 57/2024)")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Anticipazioni")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)

  art3_fdr <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Complementare")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),
                          FINANZ_REG = sum(FINANZ_REG, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_cofin <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria", "Complementare",
                                      "Stralcio 2 (delibera CIPESS n. 57/2024)")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(FINANZ_ALTRO_PSC = sum(FINANZ_ALTRO_PSC, na.rm = TRUE),
                          FINANZ_ALTRO_POC = sum(FINANZ_ALTRO_POC, na.rm = TRUE),
                          FINANZ_ALTRO_UE = sum(FINANZ_ALTRO_UE, na.rm = TRUE),
                          FINANZ_ALTRO_PNRR = sum(FINANZ_ALTRO_PNRR, na.rm = TRUE),
                          FINANZ_ALTRO_LOC = sum(FINANZ_ALTRO_LOC, na.rm = TRUE),
                          FINANZ_ALTRO_NAZ = sum(FINANZ_ALTRO_NAZ, na.rm = TRUE),
                          FINANZ_ALTRO_PRIV = sum(FINANZ_ALTRO_PRIV, na.rm = TRUE)),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  art3_n <- temi  %>% 
    left_join(interventi %>% 
                filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
                filter(SEZIONE %in% c("Ordinaria", "Complementare", "Anticipazioni",
                                      "Stralcio 2 (delibera CIPESS n. 57/2024)")) %>% 
                group_by(AREA_TEMATICA) %>% 
                summarise(N = n()),
              by = "AREA_TEMATICA") %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(-AREA_TEMATICA)
  
  
  art3_cofin_pr <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Cofinanziamento PR")) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE))
  
  art3_bagnoli <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Stralcio 3 (delibera CIPESS n. 55/2024)")) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE))
  
  art3_completamenti <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Completamenti (delibera CIPESS n. 42/2024)")) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE))
  
  
  writeData(wb, sheet = "ART3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_fsc, startCol = 2, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_fdr, startCol = 6, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_cofin, startCol = 8, startRow = 6, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_n, startCol = 17, startRow = 6, colNames = FALSE)
  
  writeData(wb, sheet = "ART3", x = art3_bagnoli, startCol = 5, startRow = 19, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_completamenti, startCol = 5, startRow = 20, colNames = FALSE)
  writeData(wb, sheet = "ART3", x = art3_cofin_pr, startCol = 2, startRow = 21, colNames = FALSE)
  
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(4), cols = c(1:17), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border_blue, rows = c(5), cols = c(1:17), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(6:18), cols = c(1, 17), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(6:18), cols = c(2:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_border, rows = c(19:22), cols = c(1), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "ART3", style_number2, rows = c(19:22), cols = c(2:5), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "ART3", cols = 1:17)
  
  
  a1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A1", x = a1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a1)[1] + start_row -1
  addStyle(wb, sheet = "A1", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A1", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A1", cols = 1:30)
  
  
  
  a2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Stralcio 2 (delibera CIPESS n. 57/2024)")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A2", x = a2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a2)[1] + start_row -1
  addStyle(wb, sheet = "A2", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A2", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A2", cols = 1:30)
  
  
  
  a3 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Completamenti (delibera CIPESS n. 42/2024)")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A3", x = a3, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a3)[1] + start_row -1
  addStyle(wb, sheet = "A3", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A3", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A3", cols = 1:30)
  
  a4 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Anticipazioni")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC,
           NOTE)
  
  writeData(wb, sheet = "A4", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A4", x = a4, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a4)[1] + start_row -1
  addStyle(wb, sheet = "A4", style_border, rows = c(4), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A4", style_border, rows = c(5), cols = c(1:10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A4", style_border, rows = seq(start_row, n_max), cols = c(1:7, 10), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A4", style_number2, rows = seq(start_row, n_max), cols = c(8:9), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A4", cols = 1:10)
  

  
  a5 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Complementare")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC, FINANZ_FDR, FINANZ_REG, FINANZ_ALTRO,
           FINANZ_ALTRO_PSC, FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR, FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, NOTE_COFIN,
           PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE, PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE,
           INC_DT_APERTURA_AV, INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS,
           NOTE)
  
  writeData(wb, sheet = "A5", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "A5", x = a5, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(a5)[1] + start_row -1
  addStyle(wb, sheet = "A5", style_border_blue, rows = c(4), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A5", style_border, rows = c(5), cols = c(1:30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A5", style_border, rows = seq(start_row, n_max), cols = c(1:7, 20, 30), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A5", style_number2, rows = seq(start_row, n_max), cols = c(8:19), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "A5", style_date, rows = seq(start_row, n_max), cols = c(21:29), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "A5", cols = 1:30)
  
  
  b0 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria", 
                          "Stralcio 2 (delibera CIPESS n. 57/2024)",
                          "Completamenti (delibera CIPESS n. 42/2024)")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    group_by(SEZIONE) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
              SPESA_2023 = sum(SPESA_2023, na.rm = TRUE),
              SPESA_2024 = sum(SPESA_2024, na.rm = TRUE),
              SPESA_2025 = sum(SPESA_2025, na.rm = TRUE),
              SPESA_2026 = sum(SPESA_2026, na.rm = TRUE),
              SPESA_2027 = sum(SPESA_2027, na.rm = TRUE),
              SPESA_2028 = sum(SPESA_2028, na.rm = TRUE),
              SPESA_2029 = sum(SPESA_2029, na.rm = TRUE),
              SPESA_2030 = sum(SPESA_2030, na.rm = TRUE),
              SPESA_2031 = sum(SPESA_2031, na.rm = TRUE),
              SPESA_2032 = sum(SPESA_2032, na.rm = TRUE),
              SPESA_2033 = sum(SPESA_2033, na.rm = TRUE),
              SPESA_2034 = sum(SPESA_2034, na.rm = TRUE),
              SPESA_2035 = sum(SPESA_2035, na.rm = TRUE))
  
  b0_tot <- b0 %>% 
    mutate(SEZIONE = "Totale") %>% 
    group_by(SEZIONE) %>% 
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
              SPESA_2023 = sum(SPESA_2023, na.rm = TRUE),
              SPESA_2024 = sum(SPESA_2024, na.rm = TRUE),
              SPESA_2025 = sum(SPESA_2025, na.rm = TRUE),
              SPESA_2026 = sum(SPESA_2026, na.rm = TRUE),
              SPESA_2027 = sum(SPESA_2027, na.rm = TRUE),
              SPESA_2028 = sum(SPESA_2028, na.rm = TRUE),
              SPESA_2029 = sum(SPESA_2029, na.rm = TRUE),
              SPESA_2030 = sum(SPESA_2030, na.rm = TRUE),
              SPESA_2031 = sum(SPESA_2031, na.rm = TRUE),
              SPESA_2032 = sum(SPESA_2032, na.rm = TRUE),
              SPESA_2033 = sum(SPESA_2033, na.rm = TRUE),
              SPESA_2034 = sum(SPESA_2034, na.rm = TRUE),
              SPESA_2035 = sum(SPESA_2035, na.rm = TRUE))
  
  b0 <- b0 %>% 
    bind_rows(b0_tot) %>% 
    mutate(SEZIONE = factor(SEZIONE, levels = c("Ordinaria",
                                                "Stralcio 2 (delibera CIPESS n. 57/2024)",
                                                "Completamenti (delibera CIPESS n. 42/2024)",
                                                "Totale"))) %>%
    arrange(SEZIONE)

  writeData(wb, sheet = "B", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B", x = b0, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b0)[1] + start_row -1
  addStyle(wb, sheet = "B", style_border_blue, rows = c(4), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B", style_border, rows = c(5), cols = c(1:16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B", style_border, rows = seq(start_row, n_max), cols = c(1, 16), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B", style_number2, rows = seq(start_row, n_max), cols = c(2:15), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B", cols = 1:16)
  
  
  b1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Ordinaria")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FSC,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B1", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B1", x = b1, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b1)[1] + start_row -1
  addStyle(wb, sheet = "B1", style_border_blue, rows = c(4), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = c(5), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_border, rows = seq(start_row, n_max), cols = c(1:7, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B1", style_number2, rows = seq(start_row, n_max), cols = c(8:21), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B1", cols = 1:22)
  
  
  b2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Stralcio 2 (delibera CIPESS n. 57/2024)")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FSC,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B2", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B2", x = b2, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b2)[1] + start_row -1
  addStyle(wb, sheet = "B2", style_border_blue, rows = c(4), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = c(5), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_border, rows = seq(start_row, n_max), cols = c(1:7, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B2", style_number2, rows = seq(start_row, n_max), cols = c(8:21), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B2", cols = 1:22)
  
  
  b3 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Completamenti (delibera CIPESS n. 42/2024)")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FSC,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B3", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B3", x = b3, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b3)[1] + start_row -1
  addStyle(wb, sheet = "B3", style_border_blue, rows = c(4), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_border, rows = c(5), cols = c(1:22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_border, rows = seq(start_row, n_max), cols = c(1:7, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B3", style_number2, rows = seq(start_row, n_max), cols = c(8:21), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B3", cols = 1:22)
  
  
  b4 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(SEZIONE %in% c("Complementare")) %>% 
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    select(ID, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, AREA_TEMATICA, SETTORE_INTERVENTO, CUP, TITOLO_PROGETTO,
           FINANZ_FDR, FINANZ_REG,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, 
           SPESA_2030, SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035)
  
  writeData(wb, sheet = "B4", x = titolo, startCol = 1, startRow = 1, colNames = FALSE)
  writeData(wb, sheet = "B4", x = b4, startCol = 1, startRow = start_row, colNames = FALSE)
  
  n_max <- dim(b4)[1] + start_row -1
  addStyle(wb, sheet = "B4", style_border_blue, rows = c(4), cols = c(1:23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B4", style_border, rows = c(5), cols = c(1:23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B4", style_border, rows = seq(start_row, n_max), cols = c(1:7, 23), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "B4", style_number2, rows = seq(start_row, n_max), cols = c(8:22), gridExpand = TRUE, stack = TRUE)
  
  ungroupColumns(wb, "B4", cols = 1:23)
  
  temp_file <- paste0("Allegati_Accordo_Coesione_Regione_", regione, ".xlsx")
  saveWorkbook(wb, file = file.path(OUTPUT, "pubblicazione", temp_file), overwrite = TRUE)
  
}


#' Verifica variazioni interventi accordi
#'
#' Verifica variazioni tra due versioni del database interventi degli accordi
#'
#' @param db_ver_new Nuova versione del DBCOE da confrontare.
#' @param db_ver_old Precedente versione del DBCOE da confrontare.
#' @return Salva in TEMP 3 report in ofrmalto long (analisi variazioni, sintesi per regione e sintesi totali) 
chk_variazioni_accordi <- function(db_ver_new, db_ver_old) {
  
  # DEBUG:
  # db_ver_new <- "20250630.00"
  # db_ver_old <- "20250430.00"
  
  DB_NEW <- file.path(DRIVE, "PROGRAMMAZIONE", db_ver_new)
  DB_OLD <- file.path(DRIVE, "PROGRAMMAZIONE", db_ver_old)
  
  interventi_new <- load_db_accordi(DB_NEW)
  interventi_old <- load_db_accordi(DB_OLD)
  
  # testo
  appo_text <- interventi_old %>% 
    mutate(OC_FLAG_MONITORAGGIO = as.character(OC_FLAG_MONITORAGGIO)) %>% 
    select(ID, AMBITO, CUP, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_TITOLARE, OC_CODICE_PROGRAMMA,
           DESCRIZIONE_PROGRAMMA, COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO, MACROAREA, REGIONE,
           AMMINISTRAZIONE_BENEFICIARIA, TITOLO_PROGETTO, CICLO_PROGRAMMAZIONE, COD_LOCALE_PROGETTO,
           COD_PROC_ATTIVAZIONE, OC_FLAG_MONITORAGGIO, NOTE, ULTIMA_VERSIONE, NOTE_COFIN,
           AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
    pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE") %>% 
    full_join(interventi_new %>% 
                mutate(OC_FLAG_MONITORAGGIO = as.character(OC_FLAG_MONITORAGGIO)) %>% 
                select(ID, AMBITO, CUP, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_TITOLARE, OC_CODICE_PROGRAMMA,
                       DESCRIZIONE_PROGRAMMA, COD_AREA_TEMATICA, COD_SETTORE_INTERVENTO, MACROAREA, REGIONE,
                       AMMINISTRAZIONE_BENEFICIARIA, TITOLO_PROGETTO, CICLO_PROGRAMMAZIONE, COD_LOCALE_PROGETTO,
                       COD_PROC_ATTIVAZIONE, OC_FLAG_MONITORAGGIO, NOTE, ULTIMA_VERSIONE, NOTE_COFIN,
                       AREA_TEMATICA, SETTORE_INTERVENTO) %>% 
                pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE"),
              by = c("ID", "AMMINISTRAZIONE_TITOLARE", "SEZIONE", "VARIABILE"), suffix = c("_old", "_new")) %>% 
    mutate(TIPO_VARIABILE = "TEXT") %>% 
    filter(!(is.na(VALORE_old) & is.na(VALORE_new))) %>% 
    mutate(CHK = case_when(VALORE_old == VALORE_new ~ 0, 
                           VALORE_old != VALORE_new ~ 1,
                           is.na(VALORE_old) ~ 1,
                           is.na(VALORE_new) ~ 1)) %>% 
    filter(CHK > 0)
  
  # euro
  appo_euro <- interventi_old %>% 
    select(ID, AMMINISTRAZIONE_TITOLARE, SEZIONE, FINANZ_TOT,  FINANZ_FSC, FINANZ_FDR, FINANZ_ALTRO,
           SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, SPESA_2030,
           SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035, FINANZ_REG,
           FINANZ_ALTRO_PSC,  FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR,
           FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, FINANZ_POR) %>% 
    pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE") %>% 
    full_join(interventi_new %>% 
                select(ID, AMMINISTRAZIONE_TITOLARE, SEZIONE, FINANZ_TOT,  FINANZ_FSC, FINANZ_FDR, FINANZ_ALTRO,
                       SPESA_2023, SPESA_2024, SPESA_2025, SPESA_2026, SPESA_2027, SPESA_2028, SPESA_2029, SPESA_2030,
                       SPESA_2031, SPESA_2032, SPESA_2033, SPESA_2034, SPESA_2035, FINANZ_REG,
                       FINANZ_ALTRO_PSC,  FINANZ_ALTRO_POC, FINANZ_ALTRO_UE, FINANZ_ALTRO_PNRR,
                       FINANZ_ALTRO_LOC, FINANZ_ALTRO_NAZ, FINANZ_ALTRO_PRIV, FINANZ_POR) %>% 
                pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE"),
              by = c("ID", "AMMINISTRAZIONE_TITOLARE", "SEZIONE", "VARIABILE"), suffix = c("_old", "_new")) %>% 
    mutate(TIPO_VARIABILE = "EURO") %>% 
    filter(!(is.na(VALORE_old) & is.na(VALORE_new))) %>% 
    mutate(CHK = case_when(VALORE_old == VALORE_new ~ 0, 
                           VALORE_old != VALORE_new ~ 1,
                           is.na(VALORE_old) ~ 1,
                           is.na(VALORE_new) ~ 1),
           DELTA_EURO = VALORE_old -VALORE_new) %>% 
    filter(CHK > 0)
  
  # date
  appo_date <- interventi_old %>% 
    select(ID, AMMINISTRAZIONE_TITOLARE, SEZIONE, PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE,
           PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE, INC_DT_APERTURA_AV,
           INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS) %>% 
    pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE") %>% 
    full_join(interventi_new %>% 
                select(ID, AMMINISTRAZIONE_TITOLARE, SEZIONE, PROGRAM_INI_PRE, PROGRAM_FIN_PRE, PROGET_INI_PRE,
                       PROGET_FIN_PRE, ESEC_INI_PRE, ESEC_FIN_PRE, INC_DT_APERTURA_AV,
                       INC_DT_CHIUSURA_AV, INC_DT_ATTIV_MIS) %>% 
                pivot_longer(cols = c(-ID, -AMMINISTRAZIONE_TITOLARE, -SEZIONE), names_to = "VARIABILE", values_to = "VALORE"),
              by = c("ID", "AMMINISTRAZIONE_TITOLARE", "SEZIONE", "VARIABILE"), suffix = c("_old", "_new")) %>% 
    mutate(TIPO_VARIABILE = "DATE") %>% 
    filter(!(is.na(VALORE_old) & is.na(VALORE_new))) %>% 
    mutate(CHK = case_when(VALORE_old == VALORE_new ~ 0, 
                           VALORE_old != VALORE_new ~ 1,
                           is.na(VALORE_old) ~ 1,
                           is.na(VALORE_new) ~ 1),
           DELTA_DAYS = difftime(VALORE_old, VALORE_new, units="days")) %>% 
    filter(CHK > 0)
  
  write.xlsx(x = list(text = appo_text, euro = appo_euro, date = appo_date),
             file.path(TEMP, paste0("chk_delta_", db_ver_new, "_", db_ver_old, ".xlsx")))
  
  # report by amministrazione
  chk_amministrazioni <- appo_text %>% 
    select(AMMINISTRAZIONE_TITOLARE, SEZIONE, TIPO_VARIABILE, VARIABILE, CHK) %>% 
    bind_rows(appo_euro %>% 
                select(AMMINISTRAZIONE_TITOLARE, SEZIONE, TIPO_VARIABILE, VARIABILE, CHK, DELTA_EURO)) %>% 
    bind_rows(appo_date %>% 
                select(AMMINISTRAZIONE_TITOLARE, SEZIONE, TIPO_VARIABILE, VARIABILE, CHK, DELTA_DAYS)) %>% 
    group_by(AMMINISTRAZIONE_TITOLARE, SEZIONE, TIPO_VARIABILE, VARIABILE) %>% 
    summarise(CHK = sum(CHK, na.rm = TRUE),
              DELTA_EURO = sum(DELTA_EURO, na.rm = TRUE),
              DELTA_DAYS = sum(DELTA_DAYS, na.rm = TRUE)) %>% 
    filter(CHK > 0)
  
  write.xlsx(chk_amministrazioni, file.path(TEMP, paste0("chk_delta_", db_ver_new, "_", db_ver_old, "_regioni.xlsx")))
  
  # report sintesi
  chk <- appo_text %>% 
    select(TIPO_VARIABILE, VARIABILE, CHK) %>% 
    bind_rows(appo_euro %>% 
                select(TIPO_VARIABILE, VARIABILE, CHK, DELTA_EURO)) %>% 
    bind_rows(appo_date %>% 
                select(TIPO_VARIABILE, VARIABILE, CHK, DELTA_DAYS)) %>% 
    group_by(TIPO_VARIABILE, VARIABILE) %>% 
    summarise(CHK = sum(CHK, na.rm = TRUE),
              DELTA_EURO = sum(DELTA_EURO, na.rm = TRUE),
              DELTA_DAYS = sum(DELTA_DAYS, na.rm = TRUE)) %>% 
    filter(CHK > 0)
  
  write.xlsx(chk, file.path(TEMP, paste0("chk_delta_", db_ver_new, "_", db_ver_old, "_sintesi.xlsx")))
  
  
  message(paste0("Il report chk_delta_", db_ver_new, "_", db_ver_old, ".xlsx e le relative sintesi sono salvati in TEMP"))
  
}
