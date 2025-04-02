# funzioni del blocco "accordi"

#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' @return Dataframe
load_db_accordi_ordinarie <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_ordinarie.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per anticipazioni
#'
#' Carica dati interventi accordi per anticipazioni
#'
#' @return Dataframe
load_db_accordi_anticipazioni <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_anticipazioni.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' @return Dataframe
load_db_accordi_complementari <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_complementari.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per completamenti Campania
#'
#' Carica dati interventi accordi per completamenti Campania
#'
#' @return Dataframe
load_db_accordi_completamenti <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_completamenti.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' @return Dataframe
load_db_accordi_cofinanziamenti <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_cofinanziamenti_por.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi 
#'
#' Carica dati interventi accordi per tutte le assegnazioni
#'
#' @return Dataframe
load_db_accordi <- function() {
 appo1 <- load_db_accordi_ordinarie()
 appo2 <- load_db_accordi_anticipazioni()
 appo3 <- load_db_accordi_complementari()
 appo4 <- load_db_accordi_completamenti()
 appo5 <- load_db_accordi_cofinanziamenti()
  
  interventi <- appo1 %>% 
    bind_rows(appo2) %>% 
    bind_rows(appo3) %>% 
    bind_rows(appo4) %>% 
    bind_rows(appo5)
  
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
    mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR) %>% 
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
    mutate(FINANZ_COE = FINANZ_FSC + FINANZ_FDR) %>% 
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

