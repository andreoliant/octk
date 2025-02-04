# funzioni del blocco "accordi"


load_db_accordi_ordinarie <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_ordinarie.xlsx"))
  return(interventi)
}

load_db_accordi_anticipazioni <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_anticipazioni.xlsx"))
  return(interventi)
}

load_db_accordi_complementari <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_complementari.xlsx"))
  return(interventi)
}

load_db_accordi_completamenti <- function() {
  interventi <- read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_completamenti.xlsx"))
  return(interventi)
}

load_db_accordi <- function() {
 appo1 <- load_db_accordi_ordinarie()
 appo2 <- load_db_accordi_anticipazioni()
 appo3 <- load_db_accordi_complementari()
 appo4 <- load_db_accordi_completamenti()
  
  interventi <- appo1 %>% 
    bind_rows(appo2) %>% 
    bind_rows(appo3) %>% 
    bind_rows(appo4)
  
  return(interventi)
}

workflow_accordi_monitoraggio <- function(interventi, template) {
  
  # DEBUG:
  # template <- "template_monitoraggio_accordi.xlsx"
  
  regioni <- interventi %>% 
    distinct(AMMINISTRAZIONE_TITOLARE) %>% 
    .$AMMINISTRAZIONE_TITOLARE
  
  for (regione in regioni) {
    print(regione)
    crea_report_accordi_monitoraggio(regione, interventi, template) 
  }
  
}

crea_report_accordi_monitoraggio <- function(regione, interventi, template) {
  
  # DEBUG:
  # regione <- "MOLISE"
  # template <- "template_monitoraggio_accordi.xlsx"
  wb <- loadWorkbook(file.path(INPUT, template))
  
  # setup_stili()
  
  appo1 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(AMBITO == "FSC") %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(TIPOLOGIA == "Intervento") %>% 
    select(ID, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, 
           AREA_TEMATICA, SETTORE_INTERVENTO, 
           CUP, TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC)
  
  appo2 <- interventi %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(AMBITO == "FSC") %>% 
    filter(AMMINISTRAZIONE_TITOLARE == regione) %>% 
    filter(TIPOLOGIA == "Linea") %>% 
    select(ID, SEZIONE, TIPOLOGIA, AMMINISTRAZIONE_BENEFICIARIA, 
           AREA_TEMATICA, SETTORE_INTERVENTO, 
           TITOLO_PROGETTO,
           FINANZ_TOT, FINANZ_FSC)

  writeData(wb, sheet = "progetti", x = appo1, startCol = 1, startRow = 3, colNames = FALSE)
  
  start_row <- 3
  n_max <- dim(appo1)[1] + start_row -1
  
  addStyle(wb, sheet = "progetti", style_border, rows = seq(start_row, n_max), cols = c(1:8, 11, 22), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "progetti", style_number2, rows = seq(start_row, n_max), cols = c(9:10, 12:15), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "progetti", style_date, rows = seq(start_row, n_max), cols = c(16:21), gridExpand = TRUE, stack = TRUE)
  
  setColWidths(wb, sheet = "progetti", cols = c(1:8, 11, 22), widths = 24)
  setColWidths(wb, sheet = "progetti", cols = c(9:10, 12:15), widths = 16)
  setColWidths(wb, sheet = "progetti", cols = c(16:21), widths = 12)
  
  ungroupColumns(wb, "progetti", cols = 1:22)
  
  
  writeData(wb, sheet = "linee", x = appo2, startCol = 1, startRow = 3, colNames = FALSE)
  
  start_row <- 3
  n_max <- dim(appo2)[1] + start_row -1
  
  addStyle(wb, sheet = "linee", style_border, rows = seq(start_row, n_max), cols = c(1:7), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = "linee", style_number2, rows = seq(start_row, n_max), cols = c(8:9), gridExpand = TRUE, stack = TRUE)

  setColWidths(wb, sheet = "linee", cols = c(1:7), widths = 24)
  setColWidths(wb, sheet = "linee", cols = c(8:9), widths = 16)

  ungroupColumns(wb, "linee", cols = 1:9)
  
  temp_file <- paste0(regione, "_monitoraggio_accordi.xlsx")
  saveWorkbook(wb, file = file.path(OUTPUT, "monitoraggio", temp_file), overwrite = TRUE)
  
}

