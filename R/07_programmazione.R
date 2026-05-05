# Programmazione


#' Carica un dataset "dati" dal database della programmazione
#'
#' Carica il dataset "dati" richiesto dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @param ciclo Ciclo di programmazione.
#' @param ambito Ambito di programmazione.
#' @return Il dataset di programmazione per l'ambito e ciclo richiesto.
load_db_dati <- function(DB, ciclo, ambito) {
  
  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FESR"
  
  # crea nome file da importare
  if (ciclo == "2014-2020") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "FEASR" ~ "FEASR",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "YEI" ~ "SIE",
                      ambito == "CTE" ~ "CTE",
                      ambito == "POC" ~ "POC",
                      ambito == "SNAI" ~ "SNAI", 
                      ambito == "FSC" ~ "FSC", 
                      # CHK: decidere se vive
                      TRUE ~ ambito)
    filename <- paste0("Dati_DBCOE_", temp, "1420.xlsx")
    
  } else if (ciclo == "2021-2027") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "JTF" ~ "SIE",
                      ambito == "CTE" ~ "CTE",
                      ambito == "POC" ~ "POC",
                      ambito == "SNAI" ~ "SNAI", 
                      ambito == "FSC" ~ "FSC",
                      TRUE ~ ambito)
    filename <- paste0("Dati_DBCOE_", temp, "2127.xlsx")
    
  } else {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "PAC" ~ "PAC",
                      ambito == "FSC" ~ "FSC",
                      TRUE ~ ambito)
    filename <- paste0("Dati_DBCOE_", temp, "0713.xlsx")
  }
  
  # importa file excel
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)

  # ricodifica da "FSE+" a "FSE" standard con filtro
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI"  | ambito == "JTF") {
    appo <- appo %>%
      mutate(AMBITO = if_else(AMBITO == "FSE+", "FSE", AMBITO)) %>%
      # separa FESR, FSE e YEI che sono nel file SIE insieme
      filter(AMBITO == ambito)
  }
  
  # aggiungo ciclo e ambito
  appo <- appo %>%
    mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
           x_AMBITO = ambito) 
  
  # fix character
  appo <- appo %>% 
    mutate(COD_AREA_TEMATICA_PSC = as.character(COD_AREA_TEMATICA_PSC),
           DESCR_AREA_TEMATICA_PSC = as.character(DESCR_AREA_TEMATICA_PSC),
           COD_SETTORE_INTERVENTO_PSC = as.character(COD_SETTORE_INTERVENTO_PSC),
           DESCR_SETTORE_INTERVENTO_PSC = as.character(DESCR_SETTORE_INTERVENTO_PSC),
           COD_RISULTATO_ATTESO = as.character(COD_RISULTATO_ATTESO),
           NOTE = as.character(NOTE))
  
  # ricodifica macroarea
  appo <- ricodifica_macroaree(appo)

  # recaftor
  appo <- refactor_ambito(appo)
  appo <- refactor_ciclo(appo)
  
  return(appo)
  
}

#' Carica un dataset "info" dal database della programmazione
#'
#' Carica il dataset "info" richiesto dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @param ciclo Ciclo di programmazione.
#' @param ambito Ambito di programmazione.
#' @return Il dataset di programmazione per l'ambito e ciclo richiesto.
load_db_info <- function(DB, ciclo, ambito) {
  
  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FESR"
  
  # crea nome file da importare
  if (ciclo == "2014-2020") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "FEASR" ~ "FEASR",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "YEI" ~ "SIE",
                      ambito == "CTE" ~ "CTE",
                      ambito == "POC" ~ "POC",
                      ambito == "SNAI" ~ "SNAI", 
                      ambito == "FSC" ~ "FSC", 
                      # CHK: decidere se vive
                      TRUE ~ ambito)
    filename <- paste0("Info_DBCOE_", temp, "1420.xlsx")
    
  } else if (ciclo == "2021-2027") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "JTF" ~ "SIE",
                      ambito == "CTE" ~ "CTE",
                      ambito == "POC" ~ "POC",
                      ambito == "SNAI" ~ "SNAI", 
                      ambito == "FSC" ~ "FSC",
                      TRUE ~ ambito)
    filename <- paste0("Info_DBCOE_", temp, "2127.xlsx")
    
  } else {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "PAC" ~ "PAC",
                      ambito == "FSC" ~ "FSC",
                      TRUE ~ ambito)
    filename <- paste0("Info_DBCOE_", temp, "0713.xlsx")
  }
  
  # importa file excel
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)
  
  # ricodifica da "FSE+" a "FSE" standard con filtro
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI"  | ambito == "JTF") {
    appo <- appo %>%
      mutate(AMBITO = if_else(AMBITO == "FSE+", "FSE", AMBITO)) %>%
      # separa FESR, FSE e YEI che sono nel file SIE insieme
      filter(AMBITO == ambito)
  }
  
  # aggiungo ciclo e ambito
  appo <- appo %>%
    mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
           x_AMBITO = ambito) 

  # fix
  appo <- appo %>% 
    mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
           DATA_DECISIONE = as.Date(DATA_DECISIONE),
           FLAG_ULTIMA_DECISIONE = toupper(FLAG_ULTIMA_DECISIONE),
           FLAG_PRIMA_DECISIONE = toupper(FLAG_PRIMA_DECISIONE),
           FLAG_VAR_RIS_ULTIMA = toupper(FLAG_VAR_RIS_ULTIMA),
           NOTE_DECISIONE = as.character(NOTE_DECISIONE),
           VERSIONE = as.character(VERSIONE),
           NOTE = as.character(NOTE))
  
  return(appo)
  
}

#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' Carica dati interventi accordi per assegnazioni ordinarie
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_ordinarie <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_ordinarie.xlsx"), guess_max=100000)
  return(interventi)
}

#' Carica dati interventi accordi per anticipazioni
#'
#' Carica dati interventi accordi per anticipazioni
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_anticipazioni <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_anticipazioni.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' Carica dati interventi accordi per assegnazioni complementari FdR
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_complementari <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_complementari.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per completamenti Campania
#'
#' Carica dati interventi accordi per completamenti Campania
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_completamenti <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_completamenti.xlsx"))
  return(interventi)
}

#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' Carica dati interventi accordi per cofinanziamenti PR
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_cofinanziamenti <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_cofinanziamenti_por.xlsx"))
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
    interventi <- readxl::read_xlsx(file_accordi, guess_max=100000)
  } else {
    # Altrimenti, procedi con il caricamento dei singoli file
    appo1 <- load_db_accordi_ordinarie(DB=DB) #%>% mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
    appo2 <- load_db_accordi_anticipazioni(DB=DB) #%>% mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
    appo3 <- load_db_accordi_complementari(DB=DB) #%>% mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
    appo4 <- load_db_accordi_completamenti(DB=DB) #%>% mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
    appo5 <- load_db_accordi_cofinanziamenti(DB=DB) #%>% mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
    
    interventi <- appo1 %>% 
      bind_rows(appo2) %>% 
      bind_rows(appo3) %>% 
      bind_rows(appo4) %>% 
      bind_rows(appo5)
  }
  
  return(interventi)
}


#' Carica dati interventi accordi AACC
#'
#' Carica dati interventi accordi delle AACC
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_accordi_aacc <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_accordi_aacc.xlsx"), guess_max=100000)
  return(interventi)
}


#' Carica dati altri interventi FSC
#'
#' Carica dati altri interventi FSC
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_altro_fsc <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_altro_fsc_2127.xlsx"), guess_max=100000)
  return(interventi)
}

#' Carica dati interventi CIS
#'
#' Carica dati interventi CIS
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_cis <- function(DB) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_CIS.xlsx"), guess_max=100000)
  return(interventi)
}

#' Carica lista interventi PSC
#'
#' Carica la lista di interventi delle sezioni ordinarie di PSC dal DBCOE in base alla variabile DB da oc_init().
#'
#' @param use_flt Vuoi caricare solo gli interventi monitorabili (con FLAG_MONITORAGGIO == 1)?
#' @details I progetti privi di OGV rientrano tra gli interventi monitorabili se la delibera di definanziamento non è ancora intervenuta a fronte di istruttoria OGV chiusa.
#' @return Dataframe
load_db_psc <- function(DB, use_flt=FALSE) {
  interventi <- readxl::read_xlsx(file.path(DB, "Interventi_DBCOE_PSC.xlsx"), 
                          col_types = c("text", "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text", "text", "text",
                                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                        "numeric",
                                        "text", "text", "text", 
                                        "text", "text", "text", "text"))
  if (use_flt == TRUE) {
    interventi <- interventi %>% 
      filter(FLAG_MONITORAGGIO == 1)
  }
  
  return(interventi)
}



#' Carica un dataset "correzioni" dal database della programmazione
#'
#' Carica il dataset "correzioni" per SIE e POC 2014-2020 richiesto dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset "correzioni".
load_correzioni_siepoc1420 <- function(DB) {
  out <- readxl::read_xlsx(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx")) %>%
    filter(FLAG_FONTE_FORMALE == "SI") %>% 
    mutate(CODICE_MORONI = NA_character_)
  return(out)
}

#' Carica un dataset "stime" dal database della programmazione
#'
#' Carica il dataset "stime" per SIE e POC 2014-2020 richiesto dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset "stime".
load_stime_siepoc1420 <- function(DB) {
  out <- readxl::read_xlsx(file.path(DB, "Stime_DBCOE_SIEPOC.xlsx")) %>% 
    mutate(CODICE_MORONI = NA_character_)
  return(out)
}

#' Carica l'elenco ufficiale dei nomi dal database della programmazione
#'
#' Carica l'elenco ufficiale dei nomi dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset con i nomi ufficiali.
load_nomi_ufficiali <- function(DB) {
  out <- readxl::read_xlsx(file.path(DB, "Elenco_ufficiale_nomi.xlsx"))
  return(out)
}

#' Carica i totali di riferimento dal database della programmazione
#'
#' Carica i totali di riferimento dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset con i totali di riferimento.
load_totali_dbcoe <- function(DB) {
  out <- readxl::read_xlsx(file.path(DB, "Totali.xlsx"))
  return(out)
}

#' Update lista denominazione programmi in inglese
#'
#' Update lista denominazione programmi in inglese.
#'
#' @param db_old Versione precedente del DBCOE, nel format standard tipo 20221231.01
#' @param progetti Dataset progetti importato con load_progetti
#' @return Il file "label_programmi_en.xlsx" viene copiato dalla versione precedente a quella corrente e integrato con i nuovi programmi
update_lista_programmi_en <- function(db_old, progetti=NULL) {
  
  # DEBUG:
  # db_new="20230630.00"
  # db_old="20230430.00"
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
  }
  
  appo <- readxl::read_xlsx(file.path(dirname(DB), db_old, "label_programmi_en.xlsx")) %>% 
    mutate(NUOVI = 0) # %>% 
  # filter(!(x_AMBITO %in% c("FEAMP", "FEASR")))
  # mutate(toupper(LABEL_PROGRAMMA_IT)) # DEV: primo giro
  
  temp <- init_programmazione_dati(DB)
  
  # # primo giro (elimino programmi con flag != 1)
  # appo2 <- appo %>% 
  #   left_join(temp %>% 
  #               distinct(OC_CODICE_PROGRAMMA, FLAG_MONITORAGGIO) %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>% 
  #               summarise(FLAG_MONITORAGGIO = paste(FLAG_MONITORAGGIO, collapse=":::")),
  #             by = "OC_CODICE_PROGRAMMA")
  # appo3 <- appo2 %>% 
  #   filter(FLAG_MONITORAGGIO == 1) %>% 
  #   select(-FLAG_MONITORAGGIO)
  # write.xlsx(appo3, file.path(dirname(DB), db_old, "label_programmi_en.xlsx"))
  
  temp1 <- temp %>%
    filter(FLAG_MONITORAGGIO == 1) %>% 
    distinct(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO) %>% 
    anti_join(appo, by = "OC_CODICE_PROGRAMMA") %>% 
    rename(LABEL_PROGRAMMA_IT = DESCRIZIONE_PROGRAMMA) %>% 
    mutate(LABEL_PROGRAMMA_EN = NA_character_,
           NOTE = NA_character_) %>% 
    mutate(NUOVI = 1) %>% 
    mutate(LABEL_PROGRAMMA_IT = toupper(LABEL_PROGRAMMA_IT))
  
  appo1 <- appo %>% 
    bind_rows(temp1)
  dim(appo)[1]+dim(temp1)[1]==dim(appo1)[1]
  
  # label da progetti pubblicati per allineamento a sito
  label_programmi <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>% 
    separate_rows(OC_DESCRIZIONE_PROGRAMMA, OC_CODICE_PROGRAMMA, sep = ":::")%>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
  
  # rewrite x_PROGRAMMA su label sito
  appo2 <- appo1 %>%
    left_join(label_programmi) %>%
    mutate(LABEL_PROGRAMMA_IT = if_else(is.na(OC_DESCRIZIONE_PROGRAMMA), LABEL_PROGRAMMA_IT, OC_DESCRIZIONE_PROGRAMMA)) %>%
    select(-OC_DESCRIZIONE_PROGRAMMA)
  
  write.xlsx(appo2, file.path(DB, "label_programmi_en.xlsx"))
  
}


#' Update lista siti web programmi
#'
#' Update lista siti web programmie.
#'
#' @param db_old Versione precedente del DBCOE, nel format standard tipo 20221231.01
#' @return Il file "label_programmi_en.xlsx" viene copiato dalla versione precedente a quella corrente e integrato con i nuovi programmi
update_lista_programmi_sitiweb <- function(db_old) {
  
  # DEBUG:
  # db_new="20230630.00"
  # db_old="20230430.00"
  
  appo <- readxl::read_xlsx(file.path(dirname(DB), db_old, "link_sito_programmi.xlsx")) %>% 
    mutate(NUOVI = 0) # %>%
  # filter(!(x_AMBITO %in% c("FEAMP", "FEASR")))
  # mutate(toupper(DENOM_PROGRAMMA)) # DEV: primo giro
  
  temp <- init_programmazione_dati(DB)
  
  # # primo giro (elimino programmi con flag != 1)
  # appo2 <- appo %>%
  #   left_join(temp %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, FLAG_MONITORAGGIO) %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
  #               summarise(FLAG_MONITORAGGIO = paste(FLAG_MONITORAGGIO, collapse=":::")),
  #             by = "OC_CODICE_PROGRAMMA")
  # appo3 <- appo2 %>%
  #   filter(FLAG_MONITORAGGIO == 1) %>%
  #   select(-FLAG_MONITORAGGIO)
  # write.xlsx(appo3, file.path(dirname(DB), db_old, "link_sito_programmi.xlsx"))
  
  temp1 <- temp %>%
    filter(FLAG_MONITORAGGIO == 1) %>% 
    distinct(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, x_CICLO, x_AMBITO) %>% 
    anti_join(appo, by = "OC_CODICE_PROGRAMMA") %>% 
    rename(DENOM_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>% 
    mutate(LINK_SITO = NA_character_,
           NOTE = NA_character_) %>% 
    mutate(NUOVI = 1) %>% 
    mutate(DENOM_PROGRAMMA = toupper(DENOM_PROGRAMMA))
  
  appo1 <- appo %>% 
    bind_rows(temp1)
  dim(appo)[1]+dim(temp1)[1]==dim(appo1)[1]
  
  # label da elenco ufficiale
  label_programmi <- load_nomi_ufficiali(DB) %>%
    distinct(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA)
  
  # rewrite x_PROGRAMMA su label sito
  appo2 <- appo1 %>%
    left_join(label_programmi, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(DENOM_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA), DENOM_PROGRAMMA, DESCRIZIONE_PROGRAMMA)) %>%
    select(-DESCRIZIONE_PROGRAMMA)
  
  write.xlsx(appo2, file.path(DB, "link_sito_programmi.xlsx"))
  
}

#' Ricodifica la voce macroarea lato programmazione come x_MACROAREA
#'
#' Ricodifica la voce macroarea lato programmazione come x_MACROAREA
#'
#' @param programmi Dataframe da init_programmazione_dati()
#' @return Un dataframe con x_MACROAREA
ricodifica_macroaree <- function(programmi) {
  
  if ("MACROAREA" %in% names(programmi)) {
    programmi <- programmi %>% 
      rename(x_MACROAREA = MACROAREA)
  }
  
  # DEBUG:
  # programmi <- out
  # programmi %>% count(x_MACROAREA)
  
  out <- programmi %>% 
    # rename(x_MACROAREA = MACROAREA) %>%
    mutate(x_MACROAREA = case_when(x_MACROAREA == "CN" ~ "Centro-Nord",
                                   x_MACROAREA == "SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "MZ" ~ "Mezzogiorno",
                                   x_MACROAREA == "ND" ~ "Ambito nazionale",
                                   x_MACROAREA == "NC" ~ "Ambito nazionale",
                                   x_MACROAREA == "VOID" ~ "Ambito nazionale",
                                   is.na(x_MACROAREA) ~ "Ambito nazionale",
                                   TRUE ~ x_MACROAREA))
  return(out)
}






#' Inizializza il database della programmazione
#'
#' Carica il databse della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @return L'intero database dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
init_programmazione_dati <- function(DB, use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE) {
  
  # DEBUG:
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  
  # print(paste0("dentro init: ", DB))
  
  # loads
  po_fsc2127 <- load_db_dati(DB, "2021-2027", "FSC") 
  po_fesr2127 <- load_db_dati(DB, "2021-2027", "FESR")
  po_fse2127 <- load_db_dati(DB, "2021-2027", "FSE")
  po_jtf2127 <- load_db_dati(DB, "2021-2027", "JTF")
  po_snai2127 <- load_db_dati(DB, "2021-2027", "SNAI")
  po_cte2127 <- load_db_dati(DB, "2021-2027", "CTE")
  po_poc2127 <- load_db_dati(DB, "2021-2027", "POC") 
  
  po_fsc1420 <- load_db_dati(DB, "2014-2020", "FSC")
  po_fesr1420 <- load_db_dati(DB, "2014-2020", "FESR")
  po_fse1420 <- load_db_dati(DB, "2014-2020", "FSE")
  po_poc1420 <- load_db_dati(DB, "2014-2020", "POC")
  po_yei1420 <- load_db_dati(DB, "2014-2020", "YEI")
  po_feamp1420 <- load_db_dati(DB, "2014-2020", "FEAMP")
  po_snai1420 <- load_db_dati(DB, "2014-2020", "SNAI")
  po_cte1420 <- load_db_dati(DB, "2014-2020", "CTE")
  po_feasr1420 <- load_db_dati(DB, "2014-2020", "FEASR")
  
  po_fsc713 <- load_db_dati(DB, "2007-2013", "FSC")
  po_fesr713 <- load_db_dati(DB, "2007-2013", "FESR")
  po_fse713 <- load_db_dati(DB, "2007-2013", "FSE")
  po_pac713 <- load_db_dati(DB, "2007-2013", "PAC")
  
  programmi <- po_fsc2127 %>%
    # 2127
    bind_rows(po_fesr2127) %>%
    bind_rows(po_fse2127) %>%
    bind_rows(po_jtf2127) %>%
    bind_rows(po_snai2127) %>%
    bind_rows(po_cte2127) %>%
    bind_rows(po_poc2127) %>% 
    # 1420
    bind_rows(po_fsc1420) %>%
    bind_rows(po_poc1420) %>%
    bind_rows(po_fesr1420) %>%
    bind_rows(po_fse1420) %>%
    bind_rows(po_yei1420) %>%
    bind_rows(po_feamp1420) %>%
    bind_rows(po_snai1420) %>%
    bind_rows(po_cte1420) %>%
    bind_rows(po_feasr1420) %>%
    # 713
    bind_rows(po_fsc713) %>%
    bind_rows(po_pac713) %>%
    bind_rows(po_fesr713) %>%
    bind_rows(po_fse713) %>%
    as.data.frame(.)
  
  # correzioni SIE-POC 1420
  if (use_fix_siepoc == TRUE) {
    if (file.exists(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx")) &
        file.exists(file.path(DB, "Stime_DBCOE_SIEPOC.xlsx"))) {
      # swtich correzioni vs stime
      if (stime_fix_siepoc == TRUE) {
        ant_siepoc <- load_stime_siepoc1420(DB)  %>% 
          mutate(FINANZ_FSC = 0,
                 FINANZ_FDR = 0)
      } else {
        ant_siepoc <- load_correzioni_siepoc1420(DB) %>% 
          mutate(COD_OBIETTIVO_TEMATICO = NA_character_,)
      }
    } else {
      message("errore, le correzioni SIE-POC 1420 non sono implementate")
    }
    
    # fix 
    ant_siepoc <- ant_siepoc %>% 
      mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
             x_AMBITO = AMBITO) %>%
      ricodifica_macroaree() %>% 
      refactor_ambito() %>% 
      refactor_ciclo() %>% 
      select(names(programmi))
    
    # summarise
    # ant_siepoc <- ant_siepoc %>%
    #   select(names(programmi)) %>%
    #   group_by(across(c(-FINANZ_UE, -FINANZ_FSC, -FINANZ_FDR, -FINANZ_ALTRO, -FINANZ_TOTALE))) %>%
    #   summarise(FINANZ_UE = sum(FINANZ_UE, na.rm = TRUE),
    #             FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
    #             FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),
    #             FINANZ_ALTRO = sum(FINANZ_ALTRO, na.rm = TRUE),
    #             FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # DEV: non serve perché non ho schiacciato a monte programmi

    # bind
    programmi <- programmi %>% 
      anti_join(ant_siepoc, by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>% 
      bind_rows(ant_siepoc)
  }
  
  # sovrascrive dati da file dati DBCOE per sezione ordinaria PSC con dati per cicli da file interventi PSC
  if (use_cicli_psc == TRUE){

    programmazione <- load_db_psc(DB, use_flt=TRUE)
    
    # converte macroaree in formato long
    programmazione2 <- programmazione %>% 
      group_by(ID_PSC, AMBITO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA,
               CICLO_PROGRAMMAZIONE, TIPO_AR, SEZIONE, FLAG_MONITORAGGIO) %>% 
      summarise(RISORSE_SUD = sum(RISORSE_SUD, na.rm = TRUE),
                RISORSE_CN = sum(RISORSE_CN, na.rm = TRUE)) %>% 
      pivot_longer(cols = c("RISORSE_SUD", "RISORSE_CN"), 
                   names_to = "MACROAREA", values_to = "RISORSE") %>% 
      mutate(MACROAREA = case_when(MACROAREA == "RISORSE_SUD" ~ "Mezzogiorno",
                                   MACROAREA == "RISORSE_CN" ~ "Centro-Nord",
                                   TRUE ~ "CHK")) %>% 
      # elimina righe vuote
      filter(!(RISORSE == 0)) 
    
    # chk
    sum(programmazione$RISORSE_CN, na.rm = TRUE) + sum(programmazione$RISORSE_SUD, na.rm = TRUE) - sum(programmazione2$RISORSE, na.rm = TRUE)
    
    cicli_psc <- programmazione2 %>% 
      # integra variabili mancanti da standard
      mutate(COD_LIVELLO_1 = SEZIONE,
             DESCR_LIVELLO_1 = SEZIONE,
             SEZIONE = "ORD+CIS",
             TIPOLOGIA_PROGRAMMA = "PSC",
             x_AMBITO = AMBITO,
             x_MACROAREA = MACROAREA,
             AMMINISTRAZIONE = NA_character_,
             DEN_REGIONE = NA_character_,
             FINANZ_FSC = RISORSE, 
             FINANZ_FDR = 0,
             FINANZ_UE = 0, 
             FINANZ_ALTRO = 0, 
             CAT_REGIONE = NA_character_, 
             COD_OBIETTIVO_TEMATICO = NA_character_,
             DESCR_OBIETTIVO_TEMATICO = NA_character_, 
             COD_RISULTATO_ATTESO = NA_character_,
             DESCR_RISULTATO_ATTESO = NA_character_, 
             COD_AREA_TEMATICA_PSC = NA_character_, 
             DESCR_AREA_TEMATICA_PSC = NA_character_, 
             COD_SETTORE_INTERVENTO_PSC = NA_character_, 
             DESCR_SETTORE_INTERVENTO_PSC = NA_character_,
             NOTE = NA_character_,
             x_CICLO = CICLO_PROGRAMMAZIONE,
             CICLO_RISORSE = CICLO_PROGRAMMAZIONE, 
             CODICE_MORONI = NA_character_
      ) %>% 
      # adatta nomi a file dati dbcoe
      rename(FINANZ_TOTALE = RISORSE, 
             TIPOLOGIA_AMMINISTRAZIONE = TIPO_AR) %>%
      ungroup(.) %>% 
      select(c(names(programmi), "ID_PSC")) %>%
      group_by(across(c(-FINANZ_UE, -FINANZ_FSC, -FINANZ_FDR, -FINANZ_ALTRO, -FINANZ_TOTALE))) %>% 
      summarise(FINANZ_UE = sum(FINANZ_UE, na.rm = TRUE),
                FINANZ_FSC = sum(FINANZ_FSC, na.rm = TRUE),
                FINANZ_FDR = sum(FINANZ_FDR, na.rm = TRUE),
                FINANZ_ALTRO = sum(FINANZ_ALTRO, na.rm = TRUE),
                FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # DEV: qui  serve perché non ho schiacciato a monte programmi
    
    sezspec <- load_db_dati(DB, "2014-2020", "FSC") %>% 
      # MEMO: qui forza valori di use_location e use_articolaz a TRUE perché servono le variabili
      filter(TIPOLOGIA_PROGRAMMA == "PSC" & 
               COD_LIVELLO_1 %in% c("SEZ_SPEC_1_COVID", "SEZ_SPEC_2_FS")) %>% 
      # fix per nome programma che duplica righe
      select(-DESCRIZIONE_PROGRAMMA) %>% 
      # recupera variabili mancanti
      ricodifica_macroaree(.) %>% 
      mutate(SEZIONE = COD_LIVELLO_1) %>% 
      # fix perché importato dopo
      select(-TIPOLOGIA_AMMINISTRAZIONE) %>% 
      left_join(octk::info_psc %>% 
                  select(OC_CODICE_PROGRAMMA, ID_PSC, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_AMMINISTRAZIONE=TIPO_AR),
                by = "OC_CODICE_PROGRAMMA") %>% 
      ungroup(.) %>% 
      select(c(names(programmi), "ID_PSC"))
    
    programmi2 <- programmi %>% 
      filter(TIPOLOGIA_PROGRAMMA != "PSC" | is.na(TIPOLOGIA_PROGRAMMA)) %>% 
      # recupera sezioni speciali
      bind_rows(sezspec) %>% 
      # integra nuova sezione ordinaria
      bind_rows(cicli_psc)
    
    # chk
    sum(programmi$FINANZ_TOTALE, na.rm = TRUE) - sum(programmi2$FINANZ_TOTALE, na.rm = TRUE)
    
    programmi <- programmi2
    
  }
  
  # print(paste0("dentro init - righe in programmi: ", dim(programmi)[1]))
  
  return(programmi)
  
}




#' Inizializza informazioni aggiuntive del database della programmazione
#'
#' Inizializza informazioni aggiuntive del database della programmazione, con possibilità di collassare i dati per programma o tenere in evidenza singole decisioni e versioni dei programmi.
#'
#' @return Tutti i dati di supporto.
init_programmazione_info <- function() {
  
  # loads
  po_fsc2127 <- load_db_info(DB, "2021-2027", "FSC") 
  po_fesr2127 <- load_db_info(DB, "2021-2027", "FESR")
  po_fse2127 <- load_db_info(DB, "2021-2027", "FSE")
  po_jtf2127 <- load_db_info(DB, "2021-2027", "JTF")
  po_snai2127 <- load_db_info(DB, "2021-2027", "SNAI")
  po_cte2127 <- load_db_info(DB, "2021-2027", "CTE")
  po_poc2127 <- load_db_info(DB, "2021-2027", "POC") 
  
  po_fsc1420 <- load_db_info(DB, "2014-2020", "FSC")
  po_fesr1420 <- load_db_info(DB, "2014-2020", "FESR")
  po_fse1420 <- load_db_info(DB, "2014-2020", "FSE")
  po_poc1420 <- load_db_info(DB, "2014-2020", "POC")
  po_yei1420 <- load_db_info(DB, "2014-2020", "YEI")
  po_feamp1420 <- load_db_info(DB, "2014-2020", "FEAMP")
  po_snai1420 <- load_db_info(DB, "2014-2020", "SNAI")
  po_cte1420 <- load_db_info(DB, "2014-2020", "CTE")
  po_feasr1420 <- load_db_info(DB, "2014-2020", "FEASR")
  
  po_fsc713 <- load_db_info(DB, "2007-2013", "FSC")
  po_fesr713 <- load_db_info(DB, "2007-2013", "FESR")
  po_fse713 <- load_db_info(DB, "2007-2013", "FSE")
  po_pac713 <- load_db_info(DB, "2007-2013", "PAC")
  
  info <- po_fsc2127 %>%
    # 2127
    bind_rows(po_fesr2127) %>%
    bind_rows(po_fse2127) %>%
    bind_rows(po_jtf2127) %>%
    bind_rows(po_snai2127) %>%
    bind_rows(po_cte2127) %>%
    bind_rows(po_poc2127) %>% 
    # 1420
    bind_rows(po_fsc1420) %>%
    bind_rows(po_poc1420) %>%
    bind_rows(po_fesr1420) %>%
    bind_rows(po_fse1420) %>%
    bind_rows(po_yei1420) %>%
    bind_rows(po_feamp1420) %>%
    bind_rows(po_snai1420) %>%
    bind_rows(po_cte1420) %>%
    bind_rows(po_feasr1420) %>%
    # 713
    bind_rows(po_fsc713) %>%
    bind_rows(po_pac713) %>%
    bind_rows(po_fesr713) %>%
    bind_rows(po_fse713) %>%
    as.data.frame(.) 

  # add LINK_SITO
  link_sito <- readxl::read_xlsx(file.path(DB, "link_sito_programmi.xlsx"))
  
  info <- info %>%
    left_join(link_sito %>% 
                select(OC_CODICE_PROGRAMMA, AMBITO = x_AMBITO, LINK_SITO), 
              by = c("OC_CODICE_PROGRAMMA", "AMBITO"))

  return(info)
}

#' Workflow di preparazione della programmazione per la pubblicazione.
#'
#' Applica convenzioni per pubblicazione su OpenCoesione, che non possono essere direttamente riportate nel DBCOE.
#'
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_fix_siepoc Logico. Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param use_location Logico. Vuoi mostreare le macroaree territoriali? Serve per opendata su dotazioni.
#' @param progetti Dataset progetti importato con load_progetti
#' @return Il dataset dei programmi con risorse e evewntualmente infomrmazioni di supporto
workflow_programmazione <- function(use_flt=TRUE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_location=FALSE, progetti) {
  
  # DEBUG:
  # use_location=TRUE
  # use_fix_siepoc=FALSE
  
  #load
  interventi <- init_programmazione_dati(DB, use_fix_siepoc = use_fix_siepoc, stime_fix_siepoc = stime_fix_siepoc) %>%
    rename(x_PROGRAMMA = DESCRIZIONE_PROGRAMMA,
           x_GRUPPO = TIPOLOGIA_PROGRAMMA)
  
  # OLD:
  # # label da progetti pubblicati per allineamento a sito
  # label_programmi <- progetti %>%
  #   distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>% 
  #   separate_rows(OC_DESCRIZIONE_PROGRAMMA, OC_CODICE_PROGRAMMA, sep = ":::")%>%
  #   distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
  # 
  # # filtro pubblicati
  # interventi <- interventi %>%
  #   mutate(PUB = if_else(OC_CODICE_PROGRAMMA %in% label_programmi$OC_CODICE_PROGRAMMA, TRUE, FALSE))
  
  # NEW:
  # label da progetti pubblicati per allineamento a sito
  chk_programmi_pubblicati <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA)
  
  # filtro pubblicati
  interventi <- interventi %>%
    mutate(PUB = if_else(OC_CODICE_PROGRAMMA %in% chk_programmi_pubblicati$OC_CODICE_PROGRAMMA, TRUE, FALSE))
  
  # kill YEI (fonde tutto FSE del programma IOG in ambito YEI)
  interventi <- interventi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    refactor_ambito(.)
  
  # applica FLAG_MONITORAGGIO
  if (use_flt == TRUE) {
    interventi <- interventi %>%
      filter(FLAG_MONITORAGGIO == 1)
  }
  
  # summary (opzione 2: il programma pluri-fondo è duplicato nei due ambiti e il valore esposto è sempre il totale) 
  if (use_location == TRUE) {
      programmi <- interventi %>%
      group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_GRUPPO, x_CICLO, AMMINISTRAZIONE, PUB, x_MACROAREA, CAT_REGIONE) %>%
      summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
                RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  } else {
  programmi <- interventi %>%
    group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_GRUPPO, x_CICLO, AMMINISTRAZIONE, PUB) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))

  }
  
  # label da elenco ufficiale
  label_programmi <- load_nomi_ufficiali(DB) %>%
    distinct(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA)

  # rewrite x_PROGRAMMA su label sito
  programmi <- programmi %>%
    left_join(label_programmi, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(x_PROGRAMMA = if_else(is.na(DESCRIZIONE_PROGRAMMA), x_PROGRAMMA, DESCRIZIONE_PROGRAMMA)) %>%
    select(-DESCRIZIONE_PROGRAMMA)
  

  # revisione label (da riportare il più possibile nel DBCOE)
  out <- programmi %>% 
    mutate(x_PROGRAMMA = toupper(x_PROGRAMMA)) %>% 
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(x_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
                                x_AMBITO == "SNAI" ~ "ALTRO",
                                TRUE ~ x_AMBITO)) %>% 
    mutate(x_GRUPPO = case_when(x_AMBITO == "ALTRO" & grepl("SNAI", x_GRUPPO) ~ "SNAI-SERVIZI",
                                x_AMBITO == "ALTRO" ~ "VARI",
                                x_AMBITO == "POC" & x_GRUPPO == "POC Nazionale" ~ "NAZIONALI",
                                x_AMBITO == "POC" & x_GRUPPO == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
                                x_AMBITO == "POC" & x_GRUPPO == "POC Regionale" ~ "REGIONALI",
                                x_AMBITO == "POC" & x_GRUPPO == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
                                x_AMBITO == "FSC" & x_GRUPPO != "PSC" & x_GRUPPO != "ACCORDI" ~ "VARI",
                                x_AMBITO == "PAC" & x_GRUPPO == "PAC Nazionale" ~ "NAZIONALI",
                                x_AMBITO == "PAC" & x_GRUPPO == "PAC Regionale" ~ "REGIONALI",
                                TRUE ~ x_GRUPPO))

  return(out)
  
}


#' Esporta report con risorse coesione per ciclo, ambito e macroarea
#'
#' Esporta report con risorse coesione per ciclo, ambito e macroarea.
#'
#' @param use_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param force_yei Logico. Vuoi forzare FSE in YEI?
#' @param export Vuoi salvare il file?
#' @param export_name Con quale filename vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e macroarea.
make_report_risorse <- function(use_meuro=FALSE, use_flt=FALSE, 
                                use_cicli_psc=FALSE,
                                use_fix_siepoc=TRUE, stime_fix_siepoc=FALSE, 
                                force_yei=FALSE, export=FALSE, export_name=NULL) {
  
  # DEBUG:
  # use_meuro=TRUE
  # use_flt=TRUE
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=FALSE
  # force_yei=TRUE

  programmi <- init_programmazione_dati(DB, use_cicli_psc=use_cicli_psc,
                                        use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) 
  
  #split REACT-EU per allineamento a struttura tavole
  programmi <- programmi %>% 
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(x_AMBITO = case_when(x_AMBITO == "FESR" & CAT_REGIONE == "REACT" ~ "FESR_REACT",
                                x_AMBITO == "FSE" & CAT_REGIONE == "REACT" ~ "FSE_REACT",
                                TRUE ~ x_AMBITO)) %>% 
    refactor_ambito()
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2 | FLAG_MONITORAGGIO == 3)
  }
  
  if (force_yei == TRUE) {
    programmi <- programmi %>%
      mutate(x_AMBITO = if_else(OC_CODICE_PROGRAMMA == "2014IT05M9OP001", "YEI", as.character(x_AMBITO))) %>%
      refactor_ambito(.)
  }

  programmi <- ricodifica_macroaree(programmi)
  
  out <- programmi %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)) %>%
    refactor_macroarea(.)
  
  if (use_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1))
  }

  out_2 <- out %>% 
    refactor_ciclo() %>% 
    refactor_ambito() %>% 
    refactor_macroarea() %>% 
    pivot_wider(id_cols = c("x_CICLO", "x_AMBITO"), names_from = "x_MACROAREA", 
                values_from = c("RISORSE", "RISORSE_UE"), values_fill = 0) %>% 
    arrange(desc(x_CICLO), x_AMBITO) %>% 
    mutate(RISORSE = `RISORSE_Centro-Nord` + `RISORSE_Mezzogiorno` + `RISORSE_Ambito nazionale`,
           RISORSE_UE = `RISORSE_UE_Centro-Nord` + `RISORSE_UE_Mezzogiorno` + `RISORSE_UE_Ambito nazionale`)
  
  
  if (export == TRUE) {
    if (!is.null(export_name)) {
      fname <- export_name
    } else {
      if (use_meuro==TRUE) {
        fname <- "risorse_coesione_meuro.xlsx"
      } else {
        fname <- "risorse_coesione.xlsx"
      }
    }
    
    write.xlsx(out_2, file.path(TEMP, fname), rowNames = FALSE)
  }
  
  return(out_2)
  
}



#' Lista programmi per pagina dedicata
#'
#' Crea la lista dei programmi da pubblicare nella "pagina programmi" del sito di OC
#' 
#' @param programmi Dati di base da workflow_programmazione().
#' @param progetti Dataset di tipo 'operazioni' (serve per verificare i programmi pubblicati)
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export Vuoi salvare il file?
#' @return Lista dei programmi 2007-2013 e 2014-2020 applicando le convenzioni per la pubblicazione nella pagina "programmi" del sito.
make_pagina_programmi <- function(programmi=NULL, progetti=NULL, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                  export=TRUE){
  
  # DEBUG:
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=FALSE
  # progetti=operazioni
  
  if (is.null(programmi)) {
    # if (is.null(progetti)) {
    #   progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    # }
    programmi <- workflow_programmazione(use_flt=TRUE, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc, progetti=progetti)
  }
  
  # chk <- programmi %>% 
  #   ungroup() %>% 
  #   count(x_CICLO, x_AMBITO, x_GRUPPO)
  
  # duplica pluri-fondo (SIE)
  appo <- programmi %>%
    # # filter(x_GRUPPO != "PSC") %>% # MEMO: qui perde casi 2127
    # filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>%
    ungroup() %>% 
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, x_GRUPPO, PUB) %>% # MEMO: qui non va messo AMMINISTRAZIONE che non si vede e crea duplicazioni
    left_join(programmi %>%
                # filter(x_GRUPPO != "PSC") %>%
                # filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE),
                          RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA")
  # DEV: spostato in widget pagina programmi
  
  programmi <- appo
  
  # duplica psc
  psc <- programmi %>% 
    # filter(x_GRUPPO == "PSC") %>% # MEMO: solo alcuni PSC hanno 713
    filter(OC_CODICE_PROGRAMMA %in% c("PSCABRUZZO", "PSCBASILICATA", "PSCBOLZANO", "PSCCALABRIA",
                                      "PSCCAMPANIA", "PSCCULTURA", "PSCEMILROMAGNA", "PSCFRIULI", 
                                      "PSCISTRUZIONE", "PSCLIGURIA", "PSCLOMBARDIA", "PSCMARCHE",
                                      "PSCMOLISE", "PSCPIEMONTE", "PSCPUGLIA", "PSCSARDEGNA",
                                      "PSCSICILIA", "PSCTOSCANA", "PSCTRENTO", 
                                      "PSCUMBRIA", "PSCVALLEAOSTA", "PSCVENETO")) %>% 
  mutate(x_CICLO = "2007-2013")
  # DEV: questa cosa non si può fare direttamente sopra perché tutti i record PSC sono sul ciclo 1420
  
  programmi <- programmi %>%
    bind_rows(psc)

  # carica info
  info <- init_programmazione_info()
  
  # fix di info
  info <- info %>%
    mutate(TIPO_DECISIONE = case_when(TIPO_DECISIONE == "Delibera CIPE" ~ "Delibera",
                                      TIPO_DECISIONE == "Delibera" ~ "Delibera",
                                      TRUE ~ TIPO_DECISIONE)) %>% 
    mutate(TIPO_DECISIONE_EN = TIPO_DECISIONE) %>%
    mutate(TIPO_DECISIONE_EN = case_when(TIPO_DECISIONE == "Delibera CIPE" ~ "Resolution CIPE",
                                         TIPO_DECISIONE == "Delibera" ~ "Resolution",
                                         TIPO_DECISIONE == "Decisione CE" ~ "Decision EC",
                                         TIPO_DECISIONE == "Legge" ~ "Law",
                                         TIPO_DECISIONE == "Altra norma" ~ "Other",
                                         TIPO_DECISIONE == "Decreto Legge" ~ "Decree",
                                         TIPO_DECISIONE == "DM MEF" ~ "Decree",
                                         TIPO_DECISIONE == "Ordinanza" ~ "Order")) %>%
    mutate(LABEL_DECISIONE_IT = ifelse(is.na(NUMERO_DECISIONE),
                                       "",
                                       paste0(TIPO_DECISIONE, " n. ", NUMERO_DECISIONE, " del ",format(DATA_DECISIONE, "%d/%m/%Y"))),
           LABEL_DECISIONE_EN = ifelse(is.na(NUMERO_DECISIONE),
                                       "",
                                       paste0(TIPO_DECISIONE_EN, " n. ", NUMERO_DECISIONE, " of ",format(DATA_DECISIONE, "%d/%m/%Y"))))
  
  info_last <- info %>%
    distinct(OC_CODICE_PROGRAMMA) %>%
    left_join(info %>%
                filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>%
                distinct(OC_CODICE_PROGRAMMA, LINK_SITO) %>%
                filter(!is.na(LINK_SITO)),
              by = "OC_CODICE_PROGRAMMA") %>%
    left_join(info %>%
                arrange(SEQ_DECISIONE) %>%
                filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>%
                distinct(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_IT, LABEL_DECISIONE_EN, LINK_DECISIONE) %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(LABEL_DECISIONE_IT = paste(LABEL_DECISIONE_IT, collapse = ":::"),
                          LABEL_DECISIONE_EN = paste(LABEL_DECISIONE_EN, collapse = ":::"),
                          LINK_DECISIONE = paste(LINK_DECISIONE, collapse = ":::")),
              by = "OC_CODICE_PROGRAMMA") %>%
    left_join(info %>%
                filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>%   
                distinct(OC_CODICE_PROGRAMMA, LINK_DOC = LINK_DOCUMENTO) %>%
                # unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(LINK_DOC = paste(LINK_DOC, collapse = ":::")),
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
           LABEL_DECISIONE_EN = gsub(":::NA", "", LABEL_DECISIONE_EN),
           LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
           # LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
           LINK_DOC = gsub(":::NA", "", LINK_DOC))
  

  # integra info
  appo <- programmi %>% 
    left_join(info_last, by = "OC_CODICE_PROGRAMMA")
  
  # chk
  dim(appo)[1] == dim(programmi)[1]
  
  # label programmi in inglese
  # programmi_en <- readxl::read_xlsx(file.path(DB, "label_programmi_en.xlsx")) %>% 
  #   distinct(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA_EN)
  programmi_en <- readxl::read_xlsx(file.path(DB, "Elenco_ufficiale_nomi.xlsx")) %>% 
    distinct(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA_EN)

  # integrazioni
  appo1 <- appo %>%
    # integra label inglese
    left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
           LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN)) %>%
    # make URL_PROGRAMMA
    mutate(LINK_DOC_IT = paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"),
           LINK_DOC_EN = paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/documenti/")) %>%
    # label sito
    mutate(LABEL_SITO_IT = if_else(is.na(LINK_SITO), "", "Sito web"),
           LABEL_SITO_EN = if_else(is.na(LINK_SITO), "", "Website"),
           LABEL_DOC_IT  = if_else(is.na(LINK_DOC_IT), "", "Documenti"),
           LABEL_DOC_EN  = if_else(is.na(LINK_DOC_EN), "", "Documents")) %>% 
    # link decisione
    mutate(LINK_DECISIONE = case_when(x_AMBITO == "FSC" ~ LINK_DECISIONE,
                                      x_AMBITO == "POC" ~ LINK_DECISIONE,
                                      # x_AMBITO == "PAC" ~ LINK_DECISIONE,
                                      TRUE ~ "")) %>% 
    # label ambito e tipo
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_AMBITO_IT = x_AMBITO,
           LABEL_AMBITO_EN = case_when(x_AMBITO == "FESR" ~ "ERDF",
                                       x_AMBITO == "FSE" ~ "ESF",
                                       x_AMBITO == "FEASR" ~ "EAFRD",
                                       x_AMBITO == "FEAMP" ~ "MFF",
                                       x_AMBITO == "CTE" ~ "ETC",
                                       x_AMBITO == "FSC" ~ "DCF",
                                       x_AMBITO == "POC" ~ "COP",
                                       x_AMBITO == "PAC" ~ "CAP",
                                       x_AMBITO == "ALTRO" ~ "OTHER",
                                       x_AMBITO == "JTF" ~ "JTF",
                                       x_AMBITO == "IOG" ~ "YEI"),
           LABEL_TIPO_IT = x_GRUPPO,
           LABEL_TIPO_EN = case_when(x_GRUPPO == "PON" ~ "NOP",
                                     x_GRUPPO == "POR" ~ "ROP",
                                     x_GRUPPO == "PN" ~ "NP",
                                     x_GRUPPO == "PR" ~ "RP",
                                     x_GRUPPO == "PATTI" ~ "DEVELOPMENT PACT",
                                     x_GRUPPO == "PIANI STRALCIO" ~ "EXCERPT PLAN",
                                     x_GRUPPO == "PIANI OPERATIVI" ~ "NATIONAL PLAN",
                                     x_GRUPPO == "POC REGIONALI" ~ "REGIONAL COP",
                                     x_GRUPPO == "POC NAZIONALI" ~ "NATIONAL COP",
                                     x_GRUPPO == "Transfrontaliero" ~ "Transnational",
                                     x_GRUPPO == "Transnazionale" ~ "Crossborder",
                                     x_AMBITO == "JTF" ~ "JTF",
                                     x_AMBITO == "IOG" ~ "YEI",
                                     x_AMBITO == "ALTRO" & grepl("SNAI", x_GRUPPO) ~ "IANS-SERVICES",
                                     x_AMBITO == "ALTRO" ~ "OTHERS",
                                     TRUE ~ x_GRUPPO)) %>% 
    # maiusc
    mutate(LABEL_PROGRAMMA_EN = toupper(LABEL_PROGRAMMA_EN)) %>% 
    # scarta FEASR e FEAMP
    filter(LABEL_AMBITO_IT != "FEASR", LABEL_AMBITO_IT != "FEAMP") %>% 
    # ricodifica label
    mutate(LABEL_TIPO_EN = case_when(LABEL_AMBITO_EN == "DCF" & LABEL_TIPO_EN == "PACTS" ~ "PACTS",
                                    LABEL_AMBITO_EN == "DCF" & LABEL_TIPO_EN == "PSC" ~ "PSC",
                                    LABEL_AMBITO_EN == "DCF" ~ "OTHERS",
                                    LABEL_AMBITO_EN == "COP" & LABEL_TIPO_IT == "NAZIONALI" ~ "NAZIONAL",
                                    LABEL_AMBITO_EN == "COP" & LABEL_TIPO_IT == "REGIONALI" ~ "REGIONAL",
                                    LABEL_AMBITO_EN == "COP" & LABEL_TIPO_IT == "COMPLETAMENTI" ~ "COMPLETAMENTI",
                                    LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "NAZIONALI" ~ "NAZIONAL",
                                    LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "REGIONALI" ~ "REGIONAL",
                                    LABEL_TIPO_IT == "POIN" ~ "INOP",
                                    TRUE ~ LABEL_TIPO_EN)) %>% 
    # link documenti
    mutate(LINK_DOC = paste0("../programmi/", OC_CODICE_PROGRAMMA, "/documenti/")) %>% # TEST
    mutate(LINK_DOC = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #serve per non generare link su sito
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
                                           OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
                                           TRUE ~ LINK_DOC)) %>% 
    # round
    mutate(RISORSE = round(RISORSE, 0),
           RISORSE_UE = round(RISORSE_UE, 0)) %>% 
    # ricodifica completamenti
    mutate(LABEL_DOC_EN = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #serve per non generare link su sito
                                OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
                                OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
                                TRUE ~ LABEL_DOC_EN)) %>% 
    mutate(LABEL_DOC_IT = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #serve per non generare link su sito
                                OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
                                OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
                                OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
                                TRUE ~ LABEL_DOC_IT))
  
  # fix per pubblicazione
  appo2 <- appo1 %>% 
    mutate(OC_CODICE_PROGRAMMA = case_when(PUB == FALSE ~ NA_character_,
                                           TRUE ~ OC_CODICE_PROGRAMMA))

  out <- appo2 %>% 
    ungroup() %>% 
    select(OC_CODICE_PROGRAMMA,
           LABEL_PROGRAMMA_IT,
           LABEL_PROGRAMMA_EN,
           LABEL_CICLO = x_CICLO,
           LABEL_AMBITO_IT,
           LABEL_AMBITO_EN,
           LABEL_TIPO_IT,
           LABEL_TIPO_EN,
           RISORSE,
           RISORSE_UE,
           LABEL_DECISIONE_IT,
           LABEL_DECISIONE_EN,
           LINK_DECISIONE,
           LABEL_DOC_IT,
           LABEL_DOC_EN,
           LINK_DOC,
           LABEL_SITO_IT,
           LABEL_SITO_EN,
           LINK_SITO,
           PUB) %>% 
    arrange(LABEL_TIPO_IT, desc(PUB)) %>% 
    select(-PUB)
  
  # split cicli
  out_1420 <- out %>% 
    filter(LABEL_CICLO == "2014-2020") %>% 
    mutate(LABEL_AMBITO_IT = factor(LABEL_AMBITO_IT, levels = c("FESR", "FSE", "IOG", "CTE",
                                                                "FSC", "POC", "ALTRO"))) %>% 
    arrange(LABEL_AMBITO_IT)
  
  out_713 <- out %>% 
    filter(LABEL_CICLO == "2007-2013") %>% 
    mutate(LABEL_AMBITO_IT = factor(LABEL_AMBITO_IT, levels = c("FESR", "FSE", "FSC", "PAC"))) %>% 
    arrange(LABEL_AMBITO_IT)
  
  out_2127 <- out %>% 
    filter(LABEL_CICLO == "2021-2027") %>% 
    mutate(LABEL_AMBITO_IT = if_else(LABEL_AMBITO_IT == "FSE", "FSE+", LABEL_AMBITO_IT)) %>% 
    mutate(LABEL_AMBITO_IT = if_else(LABEL_AMBITO_IT == "POC", "FDR", LABEL_AMBITO_IT)) %>% 
    mutate(LABEL_AMBITO_IT = factor(LABEL_AMBITO_IT, levels = c("FESR", "FSE+", "JTF", "CTE",
                                                                "FSC", "FDR", "ALTRO"))) %>% 
    arrange(LABEL_AMBITO_IT)
  
  if (export == TRUE) {
    require(withr)
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_1420, file.path(OUTPUT, "programmi_1420.csv"), row.names = FALSE, na = "", fileEncoding = "utf-8")
      )
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_713, file.path(OUTPUT, "programmi_0713.csv"), row.names = FALSE, na = "", fileEncoding = "utf-8")
    )
    # NEW 2127
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_2127, file.path(OUTPUT, "programmi_2127.csv"), row.names = FALSE, na = "", fileEncoding = "utf-8")
    )
  }
  
  return(out)
}


#' Opendata per dotazioni
#'
#' Crea il file opendata con le dotazioni dei programmi da pubblicare sul sito di OC
#' 
#' @param programmi Dati di base da workflow_programmazione().
#' @param progetti Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return File opendata con le dotazioni per ambito e per i cicli 2007-2013 e 2014-2020. 
#' @note ...
make_opendata_dotazioni <- function(programmi=NULL, progetti=NULL, use_fix_siepoc=TRUE, stime_fix_siepoc=FALSE, export=TRUE, export_xls=TRUE) {
  
  # DEBUG:
  # use_fix_siepoc=FALSE
  
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_flt=TRUE, use_location=TRUE,
                                         use_fix_siepoc=use_fix_siepoc, progetti=progetti)
    # DEV: aggiunte macroaree a workflow, non serve più passaggio per interventi
  }

  # interventi <- init_programmazione_dati(use_temi = FALSE, use_sog = TRUE, use_eu = TRUE, use_location = TRUE, 
  #                                        use_flt = TRUE, use_ciclo = TRUE, use_713 = TRUE, 
  #                                        use_fix_siepoc=use_fix_siepoc)
  # DEV: mancavano macroaree, ora ono riprotate in workflow
  
  # DEV: eliminato perché:
  # 1) non è mai stato publicato (non funzionava)
  # 2) non ha senso duplicare i valori per cicli  come avviene su sito (già non lo facciamo per ambiti)
  # fix per psc in programmi
  # psc <- programmi %>% 
  #   filter(x_GRUPPO == "PSC") %>% 
  #   bind_rows(programmi %>% 
  #               filter(x_GRUPPO == "PSC") %>% 
  #               mutate(x_CICLO = "2007-2013")) %>% 
  #   bind_rows(programmi %>% 
  #               filter(x_GRUPPO == "PSC") %>% 
  #               mutate(x_CICLO = "2000-2006"))
  # 
  # programmi <- programmi %>% 
  #   # filter(x_GRUPPO != "PSC") %>% 
  #   filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
  #   bind_rows(psc)

  # OLD:
  # applica convenzione workflow a interventi
  # appo1 <- interventi %>%
  #   # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2) %>% 
  #   filter(FLAG_MONITORAGGIO == 1) %>% 
  #   # MEMO: programmi è già filtrato da workflow
  #   select(-DESCRIZIONE_PROGRAMMA, -TIPOLOGIA_PROGRAMMA, -AMBITO,
  #          -CICLO_PROGRAMMAZIONE) %>%
  #   left_join(programmi %>%
  #               select(-RISORSE), 
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  # MEMO: 
  # usa dati finanziari da "interventi" 
  # ma sovrascrivo le convenzioni da "workflow" presenti in "programmi"
  # (interventi sono N:1 su programmi)
  
  # NEW: ora macroaree nel workflow
  appo1 <- programmi %>% 
    mutate(LABEL_PROGRAMMA = x_PROGRAMMA,
           LABEL_AMBITO = x_AMBITO)

  out <- appo1 %>%
    ungroup() %>% 
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_LIVELLO = NA,
           # LABEL_PROGRAMMA = toupper(x_PROGRAMMA),
           # LABEL_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
           #                          x_AMBITO == "SNAI" ~ "SNAI-Servizi",
           #                          TRUE ~ x_AMBITO),
           # DEV: riportato a livello di workflow
           CAT_REGIONE = case_when(x_AMBITO == "FESR" ~ CAT_REGIONE,
                                   x_AMBITO == "FSE" ~ CAT_REGIONE,
                                   x_AMBITO == "YEI" ~ CAT_REGIONE,
                                   TRUE ~ "")) %>% 
    select(OC_CODICE_PROGRAMMA,
           LABEL_PROGRAMMA,
           LABEL_AMBITO,
           LABEL_CICLO = x_CICLO,
           OC_TIPOLOGIA_PROGRAMMA = x_GRUPPO,
           CATEGORIA_REGIONI = CAT_REGIONE, # = OC_AREA_OBIETTIVO_UE,
           x_MACROAREA,
           AMMINISTRAZIONE,
           RISORSE, 
           RISORSE_UE) %>% 
    # aggrego perché interventi è più dettagliato
    group_by(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA, LABEL_AMBITO, LABEL_CICLO,
             OC_TIPOLOGIA_PROGRAMMA, CATEGORIA_REGIONI, x_MACROAREA, AMMINISTRAZIONE) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE),
              RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE))
  
  # FIX nomi psc ministeri creativi
  # out <- out %>% 
  #   mutate(LABEL_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
  #                                      OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
  #                                      TRUE ~ LABEL_PROGRAMMA))
  
  # fix per export
  # out <- out %>% 
  #   mutate(OC_TIPOLOGIA_PROGRAMMA = case_when(LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PATTI" ~ "PATTI",
  #                                             LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "PSC",
  #                                             # LABEL_AMBITO == "FSC" & grepl("PSC_", OC_CODICE_PROGRAMMA) ~ "PSC", # fix per NA
  #                                    LABEL_AMBITO == "FSC" ~ "VARI",
  #                                    LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "NAZIONALI",
  #                                    LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
  #                                    LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "REGIONALI",
  #                                    LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
  #                                    LABEL_AMBITO == "SNAI-Servizi" ~ "SNAI-SERVIZI",
  #                                    TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>% 
  #   filter(RISORSE != 0)
  # DEV: riprotato in workflow
  
  out <- out %>% 
    filter(RISORSE != 0)
  
  # clean
  out <- out %>% 
    mutate(#RISORSE = format(RISORSE, nsmall=2, big.mark=".", decimal.mark=","),
           CATEGORIA_REGIONI = ifelse(is.na(CATEGORIA_REGIONI), "-", as.character(CATEGORIA_REGIONI)),
           AMMINISTRAZIONE = ifelse(is.na(AMMINISTRAZIONE), "Amministrazioni varie", as.character(AMMINISTRAZIONE)))
  
  # filter (serve per automazione controlli)
  out <- out %>% 
    filter(LABEL_AMBITO != "FEASR",
           LABEL_AMBITO != "FEAMP",
           LABEL_AMBITO != "CTE")
  
  # fix FDR
  out <- out %>% 
    mutate(LABEL_AMBITO = if_else(LABEL_CICLO == "2021-2027" & LABEL_AMBITO == "POC", "FDR", LABEL_AMBITO))
    
  
  # export
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "dotazioni.csv"), row.names = FALSE)
  }
  
  # export xls
  if (export_xls == TRUE) {
    # TODO: rivedere allineamento tra header template e variabili in export (teniamo un solo template)
    
    # looper
    # looper <- out %>%
    #   as_tibble() %>% 
    #   distinct(LABEL_CICLO, LABEL_AMBITO) %>%
    #   mutate(LABEL_CICLO = as.character(LABEL_CICLO),
    #          LABEL_AMBITO = as.character(LABEL_AMBITO)) %>%
    #   mutate(LABEL_AMBITO = case_when(LABEL_AMBITO == "FESR" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   LABEL_AMBITO == "FSE" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   LABEL_AMBITO == "IOG" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   TRUE ~ LABEL_AMBITO)) %>%
    #   mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-Servizi", "CTE", "PAC")),
    #          LABEL_CICLO = factor(LABEL_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    #   distinct(LABEL_CICLO, LABEL_AMBITO)
    
    # TODO: aggrego FESR e FSE in SIE solo per 1420, forse va fatto anche per 713 oppure nemmeno per 1420?
    
    
    # versione solo per 1420
    # looper <- tibble(
    #   LABEL_AMBITO = c("SIE", "POC", "FSC", "FSC"),
    #   LABEL_CICLO = c("2014-2020", "2014-2020", "2014-2020", "2007-2013"),
    # )
    
    # looper <- tibble(
    #   LABEL_AMBITO = c("SIE", "POC", "FSC", "FSC", "FS", "PAC"),
    #   LABEL_CICLO = c("2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    # )
    
    # NEW 2127
    looper <- tibble(
      LABEL_AMBITO = c("SIE", "FSC", "FDR", "SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2021-2027", "2021-2027", "2021-2027", "2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    )
    
    # # A tibble: 9 x 2
    # LABEL_AMBITO LABEL_CICLO
    # <fct>        <fct>      
    #   1 SIE          2014-2020  
    # 2 FEASR        2014-2020  
    # 3 FEAMP        2014-2020  
    # 4 CTE          2014-2020  
    # 5 FSC          2014-2020  
    # 6 POC          2014-2020  
    # 7 SNAI-Servizi 2014-2020  
    # 8 FSC          2000-2006  
    # 9 FSC          2007-2013 
  
    
    # fix vari
    out_2 <- out %>%
      # mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-SERVIZI", "CTE", "PAC"))) %>% 
      # NEW 2127
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "JTF", "SNAI-SERVIZI", "CTE", "PAC", "FDR"))) %>% 
      # appo per spostare psc
      # mutate(LABEL_CICLO_2 = case_when(OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "2014-2020",
      #                                  TRUE ~ LABEL_CICLO)) %>% 
      # DEV: non serve più
      mutate(LABEL_CICLO_2 = LABEL_CICLO) %>% 
      mutate(AMMINISTRAZIONE = case_when(AMMINISTRAZIONE == "???" ~ "",
                                         TRUE ~ AMMINISTRAZIONE))
    
    
    # loop
    for (i in seq_along(rownames(looper))) {
      x_ambito <- looper[[i, "LABEL_AMBITO"]]
      x_ciclo <- looper[[i, "LABEL_CICLO"]]
      print(paste0("elaboro ", x_ciclo, "-", x_ambito))
      
      # filter
      if (x_ambito == "SIE" | x_ambito == "FS") {
        # out_3 <- out_2 %>%  
        #   filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG", LABEL_CICLO_2 == x_ciclo)%>% 
        #   select(-LABEL_CICLO_2)
        # NEW 2127
        out_3 <- out_2 %>%  
          filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG" | LABEL_AMBITO == "JTF", LABEL_CICLO_2 == x_ciclo)%>% 
          select(-LABEL_CICLO_2)
        
      } else {
        out_3 <- out_2 %>%  
          filter(LABEL_AMBITO == x_ambito, LABEL_CICLO_2 == x_ciclo) %>% 
          select(-LABEL_CICLO_2)
      }
      
      # xls
      require("openxlsx") 
      # wb <- loadWorkbook(file.path(INPUT, "TemplateDotazioni.xlsx"))
      wb <- loadWorkbook(system.file("extdata", "template_dotazioni.xlsx", package="octk"))
      writeData(wb, x = out_3, sheet = "Dotazioni", startCol = 1, startRow = 2, colNames = FALSE)
      fname <- paste0("Dotazioni", "_", x_ciclo, "_", x_ambito, ".xlsx")
      saveWorkbook(wb, file = file.path(OUTPUT, fname), overwrite = TRUE)
    }
  }
  
  return(out)
}



#' Opendata per decisioni
#'
#' Crea il file opendata con le decisioni dei programmi da pubblicare sul sito di OC
#' 
#' @param programmi Dati di base da workflow_programmazione().
#' @param progetti Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return File opendata con le decisioni per ambito e per i cicli 2007-2013 e 2014-2020. 
#' @note ...
make_opendata_decisioni <- function(programmi=NULL, progetti=NULL, export=TRUE, export_xls=TRUE) {
 
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_flt=TRUE, use_fix_siepoc=FALSE, progetti=progetti)
    # MEMO: use_info porta solo alcune variabili perché richiede sum_po = TRUE
  }
   
  info <- init_programmazione_info()
  # DEV: perché non lo prendo da workflow?
  
  
  info <- info %>% 
    mutate(LABEL_DECISIONE_IT = ifelse(is.na(NUMERO_DECISIONE),
                                       "",
                                       paste0(TIPO_DECISIONE, " n. ", NUMERO_DECISIONE, " del ",format(DATA_DECISIONE, "%d/%m/%Y"))))
  # OLD:
  # appo1 <- programmi %>%
  #   mutate(LABEL_PROGRAMMA = x_PROGRAMMA,
  #          LABEL_AMBITO = x_AMBITO) %>% 
  #   left_join(info %>% 
  #               select(-x_AMBITO, -x_CICLO), 
  #             by = "OC_CODICE_PROGRAMMA")
  
  appo1 <- programmi %>%
    mutate(LABEL_PROGRAMMA = x_PROGRAMMA,
           LABEL_AMBITO = x_AMBITO) %>% 
    left_join(info %>% 
                select(-x_CICLO), 
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))
  # MEMO: uso convenzioni da "workflow" in "programmi" e aggiungo dati per singole delibere (che sono N:1 su programmi)
  # 2348
  
  # inglese
  appo2 <- appo1 
  
  out <- appo2 %>%
    ungroup() %>% 
    select(OC_CODICE_PROGRAMMA,
           LABEL_PROGRAMMA,
           LABEL_AMBITO,
           LABEL_CICLO = x_CICLO,
           OC_TIPOLOGIA_PROGRAMMA = x_GRUPPO,
           VERSIONE_PROGRAMMA= VERSIONE,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           SEQ_DECISIONE,
           FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE,
           LABEL_DECISIONE_IT, 
    )
  

  out <- out %>% 
    mutate(LINK_DECISIONE = case_when(LABEL_AMBITO == "FSC" ~ LINK_DECISIONE,
                                      LABEL_AMBITO == "POC" ~ LINK_DECISIONE,
                                      LABEL_AMBITO == "PAC" ~ LINK_DECISIONE,
                                      TRUE ~ ""))
  
  out <- out %>% 
    select(-LINK_DECISIONE, -LABEL_DECISIONE_IT) %>% 
    arrange(desc(OC_TIPOLOGIA_PROGRAMMA))
  
  # clean
  out <- out %>% 
    mutate(DATA_DECISIONE = format(DATA_DECISIONE, "%d/%m/%Y"),
           TIPO_DECISIONE	= ifelse(is.na(TIPO_DECISIONE), "n.d.", as.character(TIPO_DECISIONE)),
           NUMERO_DECISIONE	= ifelse(is.na(NUMERO_DECISIONE), "n.d.", as.character(NUMERO_DECISIONE)),
           DATA_DECISIONE	= ifelse(is.na(DATA_DECISIONE), "n.d.", as.character(DATA_DECISIONE)),
           VERSIONE_PROGRAMMA	= ifelse(is.na(VERSIONE_PROGRAMMA), "-", as.character(VERSIONE_PROGRAMMA)),
           SEQ_DECISIONE	= ifelse(is.na(SEQ_DECISIONE), "-", as.character(SEQ_DECISIONE)))

  # fix FDR
  out <- out %>% 
    mutate(LABEL_AMBITO = if_else(LABEL_CICLO == "2021-2027" & LABEL_AMBITO == "POC", "FDR", LABEL_AMBITO))
  
  # export
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "decisioni.csv"), row.names = FALSE, na = "")
  }
  
  # export xls
  if (export_xls == TRUE) {
    # TODO: rivedere allineamento tra header template e variabili in export (teniamo un solo template)
    
    
    # looper
    # looper <- out %>%
    #   distinct(LABEL_CICLO, LABEL_AMBITO) %>%
    #   mutate(LABEL_CICLO = as.character(LABEL_CICLO),
    #          LABEL_AMBITO = as.character(LABEL_AMBITO)) %>%
    #   mutate(LABEL_AMBITO = case_when(LABEL_AMBITO == "FESR" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   LABEL_AMBITO == "FSE" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   LABEL_AMBITO == "IOG" &  LABEL_CICLO == "2014-2020" ~ "SIE",
    #                                   TRUE ~ LABEL_AMBITO)) %>%
    #   mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-Servizi", "CTE", "PAC")),
    #          LABEL_CICLO = factor(LABEL_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    #   distinct(LABEL_CICLO, LABEL_AMBITO)
    
    # looper <- tibble(
    #   LABEL_AMBITO = c("SIE", "POC", "FSC", "FSC", "FS", "PAC"),
    #   LABEL_CICLO = c("2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    # )
    
    # NEW 2127
    looper <- tibble(
      LABEL_AMBITO = c("SIE", "FSC", "FDR", "SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2021-2027", "2021-2027", "2021-2027", "2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    )

    # TODO: aggrego FESR e FSE in SIE solo per 1420, forse va fatto anche per 713 oppure nemmeno per 1420?

    # clean
    out_2 <- out %>%
      # mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-Servizi", "CTE", "PAC")))
      # NEW 2127
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "JTF", "SNAI-Servizi", "CTE", "PAC", "FDR")))


    # loop
    for (i in seq_along(rownames(looper))) {
      x_ambito <- looper[[i, "LABEL_AMBITO"]]
      x_ciclo <- looper[[i, "LABEL_CICLO"]]
      print(paste0("elaboro ", x_ciclo, "-", x_ambito))

      # filter
      if (x_ambito == "SIE" | x_ambito == "FS") {
        # out_3 <- out_2 %>%
        #   filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG", LABEL_CICLO == x_ciclo)
        # NEW 2127
        out_3 <- out_2 %>%
          filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG" | LABEL_AMBITO == "JTF", LABEL_CICLO == x_ciclo)

      } else {
        out_3 <- out_2 %>%
          filter(LABEL_AMBITO == x_ambito, LABEL_CICLO == x_ciclo)
      }
      
      # xls
      require("openxlsx")
      # wb <- loadWorkbook(file.path(INPUT, "TemplateDecisioni.xlsx"))
      wb <- loadWorkbook(system.file("extdata", "template_decisioni.xlsx", package="octk"))
      writeData(wb, x = out_3, sheet = "Decisioni", startCol = 1, startRow = 2, colNames = FALSE)
      fname <- paste0("Decisioni", "_", x_ciclo, "_", x_ambito, ".xlsx")
      saveWorkbook(wb, file = file.path(OUTPUT, fname), overwrite = TRUE)
    }
  }
  return(out)
}


#' Verifica variazione risorse per ciclo e ambito nei report per la pubblicazione
#'
#' Verifica variazione risorse per ciclo e ambito nei report per la pubblicazione. Confronta due dataframe risultanti da make_report_risorse() o i file csv da questa esportati.
#'
#' @param risorse_new Dataframe da make_report_risorse()
#' @param risorse_old Dataframe da make_report_risorse()
#' @param path_to_new Percorso ad attuale folder in cui si trovano i file "risorse_coesione_2014-2020.csv" e "risorse_coesione_2007-2013.csv" generati con make_report_risorse().
#' @param path_to_old Percorso a precedente folder in cui si trova il file "risorse_coesione.csv" oppure i file "risorse_coesione_2014-2020.csv" e "risorse_coesione_2007-2013.csv" generati con make_report_risorse().
#' @param export vuoi salvare il file?
#' @return Un dataframe per ciclo e ambito.
chk_variazione_risorse_ciclo_ambito <- function(risorse_new=NULL, risorse_old=NULL, path_to_new=NULL, path_to_old=NULL, export=FALSE){
  
  if (is.null(risorse_new)) {
    if (is.null(path_to_new)) {
      message("Indica un file da confrontare")
    } else {
      risorse_new <- read_csv2(path_to_new)
    }
  }
  
  if (is.null(risorse_old)) {
    if (is.null(path_to_old)) {
      message("Indica un file da confrontare")
    } else {
      # risorse_old <- read_csv2(path_to_old)
      risorse_old <- readxl::read_xlsx(path_to_old)
    }
  }
  
  out <- risorse_new %>%
    as_tibble(.) %>%
    mutate(RISORSE = `RISORSE_Centro-Nord` + `RISORSE_Mezzogiorno` + `RISORSE_Ambito nazionale`) %>% 
    select(x_CICLO, x_AMBITO, RISORSE) %>%
    left_join(risorse_old %>%
                # fix per "PAC" per edizioni antiche
                as_tibble(.) %>%
                mutate(RISORSE = `RISORSE_Centro-Nord` + `RISORSE_Mezzogiorno` + `RISORSE_Ambito nazionale`) %>% 
                mutate(x_AMBITO = case_when(x_CICLO == "2007-2013" & x_AMBITO == "POC" ~ "PAC",
                                            TRUE ~ x_AMBITO)) %>%
                refactor_ambito(.) %>%
                select(x_CICLO, x_AMBITO, RISORSE),
              by = c("x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK = RISORSE.new - RISORSE.old)
  
  if (export==TRUE) {
    # write.csv2(out, file.path(TEMP, "delta_risorse_ciclo_ambito.csv"), row.names = FALSE)
    write.xlsx(out, file.path(TEMP, "delta_risorse_ciclo_ambito.xlsx"))
  }
  
  return(out)
  
}


#' Verifica variazione risorse per programma nei report per la pubblicazione
#'
#' Verifica variazione risorse per programma nei report per la pubblicazione. Confronta due dataframe risultanti da make_pagina_programmi() o i file csv da questa esportati.
#'
#' @param programmi_new Dataframe attuale da make_pagina_programmi()
#' @param programmi_old Dataframe precedente da make_pagina_programmi()
#' @param path_to_new Percorso ad attuale folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().
#' @param path_to_old Percorso a precedente folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().
#' @param encoding_old Cambia encoding del file old se è stato modificato da excel. Default su "UTF-8", diventa "latin3".
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_risorse_programmi <- function(programmi_new=NULL, programmi_old=NULL, path_to_new=NULL, path_to_old=NULL, encoding_old="UTF-8", export=FALSE){
  
  # DEBUG:
  # programmi_new = programmi
  # path_to_old = OLD <- file.path(DRIVE, "ELAB", "20230630", "PROGRAMMAZIONE", "sito", "V.01", "output")
  # encoding_old = "latin3" # dopo fix manuali da excel
  
  if (is.null(programmi_new)) {
    if (is.null(path_to_new)) {
      message("Indica un file da confrontare")
    } else {
      programmi_new <- read_csv2(file.path(path_to_new, "programmi_0713.csv")) %>% 
        bind_rows(read_csv2(file.path(path_to_new, "programmi_1420.csv"))) %>% 
        bind_rows(read_csv2(file.path(path_to_new, "programmi_2127.csv")))
    }
  }
  
  if (is.null(programmi_old)) {
    if (is.null(path_to_old)) {
      message("Indica il folder con i file da confrontare")
    } else {
      programmi_old <- read_csv2(file.path(path_to_old, "programmi_0713.csv"), locale=locale(encoding=encoding_old)) %>% 
        bind_rows(read_csv2(file.path(path_to_old, "programmi_1420.csv"), locale=locale(encoding=encoding_old))) %>% 
        bind_rows(read_csv2(file.path(path_to_old, "programmi_2127.csv"), locale=locale(encoding=encoding_old)))
    }
  }
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    rename(x_PROGRAMMA = LABEL_PROGRAMMA_IT,
           x_CICLO = LABEL_CICLO,
           x_AMBITO = LABEL_AMBITO_IT,
           x_GRUPPO = LABEL_TIPO_IT) %>% 
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA, x_GRUPPO, RISORSE) %>%
    full_join(programmi_old %>%
                # fix per "PAC"
                as_tibble(.) %>%
                rename(x_PROGRAMMA = LABEL_PROGRAMMA_IT,
                       x_CICLO = LABEL_CICLO,
                       x_AMBITO = LABEL_AMBITO_IT,
                       x_GRUPPO = LABEL_TIPO_IT) %>%
                mutate(x_AMBITO = case_when(x_CICLO == "2007-2013" & x_AMBITO == "POC" ~ "PAC",
                                            TRUE ~ x_AMBITO)) %>%
                # refactor_ambito(.) %>% # DEV: questo annulla SNAI-Servizi
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, RISORSE),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK = RISORSE.new - RISORSE.old) %>% 
    filter(abs(CHK) > 0)
  
  # 2017AREAINTABRU
  
  if (export==TRUE) {
    # write.csv2(out, file.path(TEMP, "delta_risorse_programmi.csv"), row.names = FALSE)
    write.xlsx(out, file.path(TEMP, "delta_risorse_programmi.xlsx"))
  }
  
  return(out)
  
}


#' Verifica variazione risorse per programma nel DBCOE
#'
#' Verifica variazione risorse per programma nel DBCOE. Confronta due versioni del database.
#'
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_dbcoe <- function(dbcoe_new=NULL, dbcoe_old=NULL, use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, export=FALSE){
  
  # DEBUG:
  # dbcoe_new="20250228.00"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)

  if (is.null(dbcoe_new)) {
    # default su DB cofigurato in oc_init()
    programmi_new <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc)
  } else {
    print("ok new")
    # modifica parametro DB
    DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
    programmi_new <- init_programmazione_dati(DB=DB1, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc)
  }
  # print(dbcoe_new)
  # print(DB1)
  
  if (is.null(dbcoe_old)) {
    message("Indica il folder con i file da confrontare")
  } else {
    print("ok old")
    # modifica parametro DB
    DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
    programmi_old <- init_programmazione_dati(DB=DB2, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc)
  }
  # print(dbcoe_old)
  # print(DB2)
  
  # out <- programmi_new %>%
  #   as_tibble(.) %>%
  #   ungroup(.) %>% 
  #   group_by(FLAG_MONITORAGGIO, OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, CICLO_RISORSE, x_AMBITO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_PROGRAMMA) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)) %>% 
  #   full_join(programmi_old %>%
  #               ungroup(.) %>% 
  #               group_by(FLAG_MONITORAGGIO, OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, CICLO_RISORSE, x_AMBITO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_PROGRAMMA)%>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE", "CICLO_RISORSE", "x_AMBITO"),
  #             suffix = c(".new", ".old")) %>%
  #   mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
  #          RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
  #   mutate(CHK_IMPORTO = RISORSE.new - RISORSE.old,
  #          CHK_FLAG = FLAG_MONITORAGGIO.new == FLAG_MONITORAGGIO.old) %>% 
  #   filter(abs(CHK_IMPORTO) > 0 | CHK_FLAG == FALSE)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, CICLO_RISORSE, x_AMBITO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_PROGRAMMA, FLAG_MONITORAGGIO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, CICLO_RISORSE, x_AMBITO, DESCRIZIONE_PROGRAMMA, TIPOLOGIA_PROGRAMMA, FLAG_MONITORAGGIO)%>%
                summarise(RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE", "CICLO_RISORSE", "x_AMBITO", "FLAG_MONITORAGGIO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK_IMPORTO = RISORSE.new - RISORSE.old) %>% 
    filter(abs(round(CHK_IMPORTO,2)) > 0)
  
  # write.xlsx(programmi_new, file.path(TEMP, "prova.xlsx"))
  
  # print(paste0("righe in programmi new: ", dim(programmi_new)[1]))
  # print(paste0("righe in programmi old: ", dim(programmi_old)[1]))
  # print(paste0("righe in out: ", dim(out)[1]))
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_dbcoe_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}


#' Verifica allineamento PSC nel DBCOE
#'
#' Verifica allineamento della sezione ordinaria dei PSC nel DBCOE confrontando file "Dati" e file "Interventi".
#'
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_allineamento_interventi_psc <- function(export=FALSE){
  
  # DEBUG:
  # export=TRUE
  
  print(DB)
  
  dati_psc <- load_db_dati(DB, "2014-2020", "FSC") %>% 
    filter(TIPOLOGIA_PROGRAMMA == "PSC",
           COD_LIVELLO_1 %in% c("SEZ_ORD", "SEZ_CIS"))
  
  interventi_psc <- load_db_psc(DB, use_flt=FALSE)
  
  out <- dati_psc %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    full_join(interventi_psc %>%
                filter(FLAG_MONITORAGGIO == 1) %>% 
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE)),
              by = c("OC_CODICE_PROGRAMMA"),
              suffix = c(".dati", ".int")) %>%
    mutate(RISORSE.dati = if_else(is.na(RISORSE.dati), 0, round(RISORSE.dati, 2)),
           RISORSE.int = if_else(is.na(RISORSE.int), 0, round(RISORSE.int, 2))) %>%
    mutate(CHK = RISORSE.int - RISORSE.dati) %>% 
    filter(abs(CHK) > 0)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, "chk_allineamento_interventi_psc.xlsx"))
  }
  
  return(out)
  
}


#' Verifica allineamento Accordi nel DBCOE
#'
#' Verifica allineamento Accordi nel DBCOE confrontando file "Dati" e file "Interventi".
#'
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_allineamento_interventi_accordi <- function(export=FALSE){
  
  # DEBUG:
  # export=TRUE
  
  print(DB)
  
  dati_accordi<- load_db_dati(DB, "2021-2027", "FSC") %>% 
    filter(TIPOLOGIA_PROGRAMMA == "ACCORDI") %>% 
    bind_rows(load_db_dati(DB, "2021-2027", "POC") %>% 
                filter(TIPOLOGIA_PROGRAMMA == "ACCORDI")) %>% 
    filter(TIPOLOGIA_AMMINISTRAZIONE == "REGIONALE")
  
  interventi_accordi <- load_db_accordi(DB)
  
  out <- dati_accordi %>%
    # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, FLAG_MONITORAGGIO) %>%
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm=TRUE),
              FINANZ_FDR = sum(FINANZ_FDR, na.rm=TRUE)) %>% 
    mutate(FINANZ_FSC = if_else(is.na(FINANZ_FSC), 0, FINANZ_FSC),
           FINANZ_FDR = if_else(is.na(FINANZ_FDR), 0, FINANZ_FDR)) %>%
    mutate(RISORSE = FINANZ_FSC + FINANZ_FDR) %>% 
    full_join(interventi_accordi %>%
                # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2) %>% 
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, FLAG_MONITORAGGIO) %>%
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm=TRUE),
                          FINANZ_FDR = sum(FINANZ_FDR, na.rm=TRUE)) %>% 
                mutate(FINANZ_FSC = if_else(is.na(FINANZ_FSC), 0, FINANZ_FSC),
                       FINANZ_FDR = if_else(is.na(FINANZ_FDR), 0, FINANZ_FDR)) %>%
                mutate(RISORSE = FINANZ_FSC + FINANZ_FDR),
              by = c("OC_CODICE_PROGRAMMA", "FLAG_MONITORAGGIO"),
              suffix = c(".dati", ".int")) %>%
    mutate(RISORSE.dati = if_else(is.na(RISORSE.dati), 0, round(RISORSE.dati, 2)),
           RISORSE.int = if_else(is.na(RISORSE.int), 0, round(RISORSE.int, 2))) %>%
    mutate(CHK_RISORSE = RISORSE.int - RISORSE.dati) %>% 
    filter(abs(CHK_RISORSE) > 0)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, "chk_allineamento_interventi_accordi.xlsx"))
  }
  
  return(out)
  
}


#' Verifica allineamento risorse UE 1420 nel DBCOE
#'
#' Verifica allineamento della risorse UE 1420 nel DBCOE confrontando file "Dati", "Correzioni" e "Stime".
#'
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_allineamento_risorse_ue_siepoc1420 <- function(export=FALSE){
  
  # DEBUG:
  # export=TRUE
  
  print(DB)
  
  dati_sie <- load_db_dati(DB, "2014-2020", "SIE") %>% 
    filter(OC_CODICE_PROGRAMMA != "FEADREACT")
  
  correzioni <- load_correzioni_siepoc1420(DB) %>% 
    filter(AMBITO != "POC")
  
  stime <- load_stime_siepoc1420(DB)
  
  out <- dati_sie %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_UE, na.rm=TRUE)) %>% 
    full_join(correzioni %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA)%>%
                summarise(RISORSE = sum(FINANZ_UE, na.rm=TRUE)),
              by = c("OC_CODICE_PROGRAMMA"),
              suffix = c(".dati", ".corr"))  %>% 
    full_join(stime %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA)%>%
                summarise(RISORSE.stim = sum(FINANZ_UE, na.rm=TRUE)),
              by = c("OC_CODICE_PROGRAMMA")) %>%
    mutate(CHK.dati.corr = round(RISORSE.dati - RISORSE.corr, 0),
           CHK.dati.stim = round(RISORSE.dati - RISORSE.stim, 0),
           CHK.stim.corr = round(RISORSE.stim - RISORSE.corr, 0)) %>% 
    filter(abs(CHK.dati.corr)!= 0 | abs(CHK.dati.stim) != 0 | abs(CHK.stim.corr) != 0)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, "chk_allineamento_risorse_ue_siepoc1420.xlsx"))
  }
  
  return(out)
  
}


#' Verifica allineamento dei dati finanziari storicizzati nel DBCOE
#'
#' Verifica allineamento nel DBCOE confrontando file "Dati" e file "Info".
#'
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_allineamento_dati_info <- function(export=FALSE){
  
  # DEBUG:
  # export=TRUE
  
  print(DB)
  
  dati <- init_programmazione_dati(DB)
  
  info <- init_programmazione_info()
  info %>% count(FLAG_ULTIMA_DECISIONE)

  out <- dati %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, CICLO_PROGRAMMAZIONE, x_AMBITO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)) %>% 
    full_join(info %>%
                filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>% 
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, x_AMBITO)%>%
                summarise(N = n(),
                          RISORSE = sum(FINANZ_TOTALE, na.rm=TRUE)), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE", "x_AMBITO"),
              suffix = c(".dati", ".info")) %>%
    mutate(RISORSE.dati = if_else(is.na(RISORSE.dati), 0, round(RISORSE.dati, 2)),
           RISORSE.info = if_else(is.na(RISORSE.info), 0, round(RISORSE.info, 2))) %>%
    mutate(CHK = RISORSE.info - RISORSE.dati) %>% 
    filter(abs(CHK) > 0)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, "chk_allineamento_dati_info.xlsx"))
  }
  
  return(out)
  
}


#' Verifica allineamento elenco nomi ufficiali DBCOE
#'
#' Verifica allineamento elenco nomi ufficiali del DBCOE confrontando file "Dati" e file "Nomi ufficiali".
#' Controlla sia nomi assenti che nomi presenti ma diversi.
#'
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_nomi_ufficiali <- function(export=TRUE) {

  # DEBUG:
  # export=TRUE
  
  print(DB)
  
  dati <- init_programmazione_dati(DB)
  
  nomi <- load_nomi_ufficiali(DB)
  
  out <- dati %>%
    distinct(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, AMBITO, DESCRIZIONE_PROGRAMMA) %>% 
    # fix per righe di "nomi" poste a "SIE" 
    mutate(AMBITO = case_when(AMBITO == "FESR" ~ "SIE",
                              AMBITO == "FSE" ~ "SIE",
                              AMBITO == "YEI" ~ "SIE",
                              TRUE ~ AMBITO)) %>% 
    full_join(nomi %>%
                distinct(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, AMBITO, DESCRIZIONE_PROGRAMMA), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE", "AMBITO"),
              suffix = c(".dati", ".ufficiale")) %>%
    mutate(CHK = case_when(DESCRIZIONE_PROGRAMMA.dati == DESCRIZIONE_PROGRAMMA.ufficiale ~ "ok",
                           is.na(DESCRIZIONE_PROGRAMMA.dati) ~ "assente in dati",
                           is.na(DESCRIZIONE_PROGRAMMA.ufficiale) ~ "assente in nomi ufficiali",
                           TRUE ~ "denominazione diversa"))
  
  temp <- out %>% count(CHK)
  print(temp)

  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, "chk_nomi_ufficiali.xlsx"))
  }
  
  return(out)
}


#' Verifica allineamento totali finanziari FSC
#'
#' Verifica allineamento del file "dati" con i totali finanziari FSC
#'
#' @return Un messaggio di log.
chk_totali_fsc <- function() {
  
  totali <- load_totali_dbcoe(DB)
  
  # chk totale risorse 2127 (FSC_01)
  chk <- totali %>% filter(ID == "FSC_01") %>% .$RISORSE
  
  temp <- load_db_dati(DB, "2021-2027", "FSC") %>% 
    filter(CICLO_RISORSE == "2021-2027") %>% 
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    .$RISORSE
  
  delta <- round(temp - chk,2)
  
  message("FSC 2021-2027: il totale di riferimento è ", chk, ", il valore nel DBCOE è ", temp, " (la differenza è ", delta, ")")

  # chk totale risorse 1420 (FSC_02)
  # MEMO: si calcola scontando i PSC censiti e sostituendo con PSC contabili del 1420
  chk <- totali %>% filter(ID == "FSC_02") %>% .$RISORSE
  
  temp <- load_db_dati(DB, "2014-2020", "FSC") %>% 
    filter(CICLO_RISORSE == "2014-2020") %>% 
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    .$RISORSE
  
  psc_new <- totali %>% filter(ID == "PSC_05") %>% .$RISORSE 
  
  psc_old <- load_db_dati(DB, "2014-2020", "FSC") %>% 
    filter(CICLO_RISORSE == "2014-2020") %>% 
    filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    .$RISORSE
  
  temp1 <- temp - psc_old + psc_new
  delta <- round(temp1 - chk,2)
  
  message("FSC 2014-2020: il totale di riferimento è ", chk, ", il valore nel DBCOE è ", temp1, " (la differenza è ", delta, ")")
  
  
}


#' Verifica allineamento totali finanziari FSC per accordi coesione
#'
#' Verifica allineamento del file "dati" con i totali finanziari FSC per gli accordi per la coesione
#'
#' @return Un messaggio di log.
chk_totali_accordi_fsc <- function() {
  
  totali <- load_totali_dbcoe(DB)
  
  # chk totale risorse 2127 (FSC_01)
  chk <- totali %>% filter(ID == "ACCORDI_07") %>% .$RISORSE
  
  appo0 <- load_db_dati(DB, "2021-2027", "FSC") 
  
  # dati accordi
  appo1 <- appo0 %>% 
    filter(CICLO_RISORSE == "2021-2027",
           TIPOLOGIA_PROGRAMMA == "ACCORDI",
           TIPOLOGIA_AMMINISTRAZIONE == "REGIONALE") %>% 
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    .$RISORSE
  
  # dati riduzioni contate in accordi
  appo2 <- appo0 %>% 
    filter(CICLO_RISORSE == "2021-2027") %>%
    filter(OC_CODICE_PROGRAMMA == "RISIDROCAL" | # forestali calabria (440 Meuro)
           OC_CODICE_PROGRAMMA == "RID_CAMPI_REG" | # campi flegrei campania (206 Meuro)
           OC_CODICE_PROGRAMMA == "RID_STRETTO_REG") %>% # ponte stretto messina calabria e sicilia (1.600 Meuro)
    summarise(RISORSE = sum(FINANZ_FSC, na.rm=TRUE)) %>% 
    .$RISORSE

  temp <- appo1 + appo2
  delta <- round(temp - chk,2)
  
  message("Imputazioni programmatiche accordi FSC 2021-2027: il totale di riferimento è ", chk, ", il valore nel DBCOE è ", temp, " (la differenza è ", delta, ")")
   
}

fix_dbcoe_sie <- function(DB, backup=FALSE, debug=FALSE) {
  
  # load
  db_sie <- read_excel(file.path(DB, "Dati_DBCOE_SIE2127_CI.xlsx"))
  db_sie_dati <- read_excel(file.path(DB, "Dati_DBCOE_SIE2127.xlsx"))
  # MEMO: usa dbsie normale per domini perché é  più completo
  
  if (isTRUE(backup)) {
    write.xlsx(appo, file.path(DB, "Dati_DBCOE_SIE2127_CI_bkp.xlsx"))
  }
  
  # domini
  amministrazioni <- db_sie_dati %>% 
    distinct(OC_CODICE_PROGRAMMA, AMMINISTRAZIONE, TIPOLOGIA_AMMINISTRAZIONE, TIPOLOGIA_PROGRAMMA) %>% 
    filter(!is.na(AMMINISTRAZIONE), !is.na(TIPOLOGIA_AMMINISTRAZIONE), !is.na(TIPOLOGIA_PROGRAMMA))
  amministrazioni %>% count(OC_CODICE_PROGRAMMA) %>% filter(n>1)
  
  regioni <- db_sie_dati %>% 
    distinct(OC_CODICE_PROGRAMMA, DEN_REGIONE) %>% 
    filter(!is.na(DEN_REGIONE))
  regioni %>% count(OC_CODICE_PROGRAMMA) %>% filter(n>1)
  
  assi <- db_sie_dati %>% 
    distinct(OC_CODICE_PROGRAMMA, COD_LIVELLO_1,	DESCR_LIVELLO_1) %>% 
    filter(!is.na(COD_LIVELLO_1), !is.na(DESCR_LIVELLO_1))
  assi %>% count(OC_CODICE_PROGRAMMA, COD_LIVELLO_1) %>% filter(n>1)
  
  op <- db_sie %>% 
    distinct(COD_OBIETTIVO_TEMATICO, DESCR_OBIETTIVO_TEMATICO) %>% 
    filter(!is.na(COD_OBIETTIVO_TEMATICO), !is.na(DESCR_OBIETTIVO_TEMATICO))
  op %>% count(COD_OBIETTIVO_TEMATICO) %>% filter(n>1)
  
  os <- db_sie %>% 
    distinct(COD_RISULTATO_ATTESO, DESCR_RISULTATO_ATTESO) %>% 
    filter(!is.na(COD_RISULTATO_ATTESO), !is.na(DESCR_RISULTATO_ATTESO))
  os %>% count(COD_RISULTATO_ATTESO) %>% filter(n>1)
  
  ci <- db_sie %>% 
    distinct(COD_SETTORE_INTERVENTO, DESCR_SETTORE_INTERVENTO) %>% 
    filter(!is.na(COD_SETTORE_INTERVENTO), !is.na(DESCR_SETTORE_INTERVENTO))
  ci %>% count(COD_SETTORE_INTERVENTO) %>% filter(n>1)
  
  temi <- octk::info_psc_matrix_temi %>% 
    select(COD_AREA_TEMATICA_PSC=COD_AREA_TEMATICA, 
           DESCR_AREA_TEMATICA_PSC=DES_AREA_TEMATICA, 
           COD_SETTORE_INTERVENTO_PSC=COD_SETTORE_INTERVENTO, 
           DESCR_SETTORE_INTERVENTO_PSC=DES_SETTORE_INTERVENTO) %>% 
    mutate(COD_SETTORE_INTERVENTO_PSC = paste0(COD_AREA_TEMATICA_PSC, ".", COD_SETTORE_INTERVENTO_PSC))
  
  # elab
  appo <- db_sie %>% 
    # elimina variabili da domini
    select(-AMMINISTRAZIONE, -TIPOLOGIA_AMMINISTRAZIONE, -TIPOLOGIA_PROGRAMMA,
           -DEN_REGIONE, 
           -DESCR_LIVELLO_1,
           -DESCR_OBIETTIVO_TEMATICO,
           -DESCR_RISULTATO_ATTESO,
           -DESCR_SETTORE_INTERVENTO) %>% 
    # integra variabili da domini
    left_join(amministrazioni, by = "OC_CODICE_PROGRAMMA") %>% 
    left_join(regioni, by = "OC_CODICE_PROGRAMMA") %>% 
    left_join(assi, by = c("OC_CODICE_PROGRAMMA", "COD_LIVELLO_1")) %>% 
    left_join(op, by = "COD_OBIETTIVO_TEMATICO") %>% 
    left_join(os, by = "COD_RISULTATO_ATTESO") %>% 
    left_join(ci, by = "COD_SETTORE_INTERVENTO") %>% 
    # elimina temi
    select(-COD_AREA_TEMATICA_PSC, -DESCR_AREA_TEMATICA_PSC, -COD_SETTORE_INTERVENTO_PSC, -DESCR_SETTORE_INTERVENTO_PSC) %>% 
    # padding ci
    mutate(COD_SETTORE_INTERVENTO = str_pad(COD_SETTORE_INTERVENTO, width = 3, pad = "0")) %>% 
    # integra temi temp da ci per casi dove non basta os
    mutate(
      TEMP = case_when(
        COD_SETTORE_INTERVENTO == "001"   ~ "01.02", # Investment in fixed assets, including research infrastructure, in micro enterprises directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "002"   ~ "01.02", # Investment in fixed assets, including research infrastructure, in small and medium-sized enterprises (including private research centres) directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "003"   ~ "01.02", # Investment in fixed assets, including research infrastructure, in large enterprises directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "004"   ~ "01.02", # Investment in fixed assets, including research infrastructure, in public research centres and higher education directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "005"   ~ "01.01", # Investment in intangible assets in micro enterprises directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "006"   ~ "01.01", # Investment in intangible assets in SMEs (including private research centres) directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "007"   ~ "01.01", # Investment in intangible assets in large enterprises directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "008"   ~ "01.01", # Investment in intangible assets in public research centres and higher education directly linked to research and innovation activities
        COD_SETTORE_INTERVENTO == "009"   ~ "01.01", # Research and innovation activities in micro enterprises including networking (industrial research, experimental development, feasibility studies)
        COD_SETTORE_INTERVENTO == "010"  ~ "01.01", # Research and innovation activities in SMEs, including networking
        COD_SETTORE_INTERVENTO == "011"  ~ "01.01", # Research and innovation activities in large enterprises, including networking
        COD_SETTORE_INTERVENTO == "012"  ~ "01.01", # Research and innovation activities in public research centres, higher education and centres of competence including networking (industrial research, experimental development, feasibility studies)
        COD_SETTORE_INTERVENTO == "013"  ~ "02.01", # Digitising SMEs (including e-Commerce, e-Business and networked business processes, digital innovation hubs, living labs, web entrepreneurs and ICT start-ups, B2B)
        COD_SETTORE_INTERVENTO == "014"  ~ "02.01", # Digitising large enterprises (including e-Commerce, e-Business and networked business processes, digital innovation hubs, living labs, web entrepreneurs and ICT start-ups, B2B)
        COD_SETTORE_INTERVENTO == "015"  ~ "02.01", # Digitising SMEs or large enterprises (including e-Commerce, e-Business and networked business processes, digital innovation hubs, living labs, web entrepreneurs and ICT ...) compliant with greenhouse gas emission reduction or energy efficiency criteria
        COD_SETTORE_INTERVENTO == "016"  ~ "02.01", # Government ICT solutions, e-services, applications
        COD_SETTORE_INTERVENTO == "017"  ~ "02.01", # Government ICT solutions, eservices, applications compliant with greenhouse gas emission reduction or energy efficiency criteria
        COD_SETTORE_INTERVENTO == "018"  ~ "02.01", # IT services and applications for digital skills and digital inclusion
        COD_SETTORE_INTERVENTO == "019"  ~ "02.01", # e-Health services and applications (including e-Care, Internet of Things for physical activity and ambient assisted living)
        COD_SETTORE_INTERVENTO == "020"  ~ "03.01", # Business infrastructure for SMEs (including industrial parks and sites)
        COD_SETTORE_INTERVENTO == "021"  ~ "03.01", # SME business development and internationalisation, including productive investments
        COD_SETTORE_INTERVENTO == "022"  ~ "03.01", # Support for large enterprises through financial instruments, including productive investments
        COD_SETTORE_INTERVENTO == "023"  ~ "03.04", # Skills development for smart specialisation, industrial transition, entrepreneurship and adaptability of enterprises to change
        COD_SETTORE_INTERVENTO == "024"  ~ "03.01", # Advanced support services for SMEs and groups of SMEs (including management, marketing and design services)
        COD_SETTORE_INTERVENTO == "025"  ~ "01.01", # Incubation, support to spin offs and spin outs and start ups
        COD_SETTORE_INTERVENTO == "026"  ~ "01.01", # Support for innovation clusters including between businesses, research organisations and public authorities and business networks primarily benefiting SMEs
        COD_SETTORE_INTERVENTO == "027"  ~ "03.01", # Innovation processes in SMEs (process, organisational, marketing, co-creation, user and demand driven innovation)
        COD_SETTORE_INTERVENTO == "028"  ~ "01.01", # Technology transfer and cooperation between enterprises, research centres and higher education sector
        COD_SETTORE_INTERVENTO == "029"  ~ "01.01", # Research and innovation processes, technology transfer and cooperation between enterprises, research centres and universities, focusing on the low carbon economy, resilience and adaptation to climate change
        COD_SETTORE_INTERVENTO == "030"  ~ "01.01", # Research and innovation processes, technology transfer and cooperation between enterprises, focusing on circular economy
        COD_SETTORE_INTERVENTO == "032"  ~ "02.02", # ICT: Very High-Capacity broadband network (backbone/backhaul network)
        COD_SETTORE_INTERVENTO == "033"  ~ "02.02", # ICT: Very High-Capacity broadband network (access/local loop with a performance equivalent to an optical fibre installation up to the distribution point at the serving location for multi-dwelling premises)
        COD_SETTORE_INTERVENTO == "034"  ~ "02.02", # ICT: Very High-Capacity broadband network (access/local loop with a performance equivalent to an optical fibre installation up to the distribution point at the serving location for homes and business premises)
        COD_SETTORE_INTERVENTO == "035"  ~ "02.02", # -> NEW: ICT: Very High-Capacity broadband network (access/local loop with a performance equivalent to an optical fibre installation up to the base station for advanced wireless communication)
        COD_SETTORE_INTERVENTO == "036"  ~ "02.01", # ICT: Other types of ICT infrastructure (including large-scale computer resources/equipment, data centres, sensors and other wireless equipment)
        COD_SETTORE_INTERVENTO == "037"  ~ "02.01", # ICT: Other types of ICT infrastructure (including large-scale computer resources/equipment, data centres, sensors and other wireless equipment) compliant with the carbon emission reduction and energy efficiency criteria
        COD_SETTORE_INTERVENTO == "038"  ~ "04.01", # Energy efficiency and demonstration projects in SMEs and supporting measures
        COD_SETTORE_INTERVENTO == "039"  ~ "04.01", # Energy efficiency and demonstration projects in large enterprises and supporting measures
        COD_SETTORE_INTERVENTO == "040"  ~ "04.01", # Energy efficiency and demonstration projects in SMEs or large enterprises and supporting measures compliant with energy efficiency criteria
        COD_SETTORE_INTERVENTO == "041"  ~ "04.01", # Energy efficiency renovation of existing housing stock, demonstration projects and supporting measures
        COD_SETTORE_INTERVENTO == "042"  ~ "04.01", # Energy efficiency renovation of existing housing stock, demonstration projects and supporting measures compliant with energy efficiency criteria
        COD_SETTORE_INTERVENTO == "043"  ~ "04.01", # -> NEW: Construction of new energy efficient buildings
        COD_SETTORE_INTERVENTO == "044"  ~ "04.01", # Energy efficiency renovation or energy efficiency measures regarding public infrastructure, demonstration projects and supporting measures
        COD_SETTORE_INTERVENTO == "045"  ~ "04.01", # Energy efficiency renovation or energy efficiency measures regarding public infrastructure, demonstration projects and supporting measures compliant with energy efficiency criteria
        COD_SETTORE_INTERVENTO == "046"  ~ "04.02", # Support to entities that provide services contributing to the low carbon economy and to resilience to climate change, including awareness-raising measures
        COD_SETTORE_INTERVENTO == "047"  ~ "04.02", # Renewable energy: wind
        COD_SETTORE_INTERVENTO == "048"  ~ "04.02", # Renewable energy: solar
        COD_SETTORE_INTERVENTO == "049"  ~ "04.02", # Renewable energy: biomass
        COD_SETTORE_INTERVENTO == "050"  ~ "04.02", # Renewable energy: biomass with high greenhouse gas savings
        COD_SETTORE_INTERVENTO == "051"  ~ "04.02", # Renewable energy: marine
        COD_SETTORE_INTERVENTO == "052"  ~ "04.02", # Other renewable energy (including geothermal energy)
        COD_SETTORE_INTERVENTO == "053"  ~ "04.03", # Smart Energy Systems (including smart grids and ICT systems) and related storage
        COD_SETTORE_INTERVENTO == "054"  ~ "04.02", # High efficiency co-generation, district heating and cooling
        COD_SETTORE_INTERVENTO == "055"  ~ "04.01", # High efficiency co generation, efficient district heating and cooling with low lifecycle emissions
        COD_SETTORE_INTERVENTO == "058"  ~ "05.01", # Adaptation to climate change measures and prevention and management of climate related risks: floods and landslides (including awareness raising, civil protection and disaster management systems, infrastructures and ecosystem based approaches)
        COD_SETTORE_INTERVENTO == "059"  ~ "05.01", # Adaptation to climate change measures and prevention and management of climate related risks: fires (including awareness raising, civil protection and disaster management systems, infrastructures and ecosystem based approaches)
        COD_SETTORE_INTERVENTO == "060"  ~ "05.01", # Adaptation to climate change measures and prevention and management of climate related risks: others, e.g. storms and drought (including awareness raising, civil protection and disaster management systems, infrastructures and ecosystem based approaches)
        COD_SETTORE_INTERVENTO == "061"  ~ "05.01", # Risk prevention and management of non climate related natural risks and risks linked to human activities, including awareness raising, civil protection and disaster management systems, infrastructures and ecosystem based approaches
        COD_SETTORE_INTERVENTO == "062"  ~ "05.02", # Provision of water for human consumption (extraction, treatment, storage and distribution infrastructure, efficiency measures, drinking water supply)
        COD_SETTORE_INTERVENTO == "063"  ~ "05.02", # Provision of water for human consumption (extraction, treatment, storage and distribution infrastructure, efficiency measures, drinking water supply) compliant with efficiency criteria
        COD_SETTORE_INTERVENTO == "064"  ~ "05.02", # Water management and water resource conservation (including river basin management, specific climate change adaptation measures, reuse, leakage reduction)
        COD_SETTORE_INTERVENTO == "065"  ~ "05.02", # Waste water collection and treatment
        COD_SETTORE_INTERVENTO == "066"  ~ "05.02", # Waste water collection and treatment compliant with energy efficiency criteria
        COD_SETTORE_INTERVENTO == "067"  ~ "05.03", # Household waste management: prevention, minimisation, sorting, reuse, recycling measures
        COD_SETTORE_INTERVENTO == "068"  ~ "05.03", # Household waste management: residual waste treatment
        COD_SETTORE_INTERVENTO == "069"  ~ "05.03", # Commercial, industrial waste management: prevention, minimisation, sorting, reuse, recycling measures
        COD_SETTORE_INTERVENTO == "070"  ~ "05.03", # Commercial, industrial waste management: residual and hazardous waste
        COD_SETTORE_INTERVENTO == "071"  ~ "05.03", # Promoting the use of recycled materials as raw materials
        COD_SETTORE_INTERVENTO == "072"  ~ "05.03", # Use of recycled materials as raw materials compliant with the efficiency criteria
        COD_SETTORE_INTERVENTO == "073"  ~ "05.05", # Rehabilitation of industrial sites and contaminated land
        COD_SETTORE_INTERVENTO == "074"  ~ "05.05", # Rehabilitation of industrial sites and contaminated land compliant with efficiency criteria
        COD_SETTORE_INTERVENTO == "075"  ~ "05.03", # Support to environmentally-friendly production processes and resource efficiency in SMEs
        COD_SETTORE_INTERVENTO == "076"  ~ "05.03", # Support to environmentally-friendly production processes and resource efficiency in large enterprises
        # COD_SETTORE_INTERVENTO == "077" & OC_CODICE_PROGRAMMA == "2021IT16FFPR001" & COD_RISULTATO_ATTESO == "RSO3.2"~ "07.05", # fix anomalia molise
        COD_SETTORE_INTERVENTO == "077"  ~ "05.05", # Air quality and noise reduction measures
        COD_SETTORE_INTERVENTO == "078"  ~ "05.05", # Protection, restoration and sustainable use of Natura 2000 sites
        COD_SETTORE_INTERVENTO == "079"  ~ "05.05", # Nature and biodiversity protection, natural heritage and resources, green and blue infrastructure
        COD_SETTORE_INTERVENTO == "080"  ~ "05.05", # Other measures to reduce greenhouse gas emissions in the area of preservation and restoration of natural areas with high potential for carbon absorption and storage, e.g. by rewetting of moorlands, the capture of landfill gas
        COD_SETTORE_INTERVENTO == "081"  ~ "07.05", # Clean urban transport infrastructure
        COD_SETTORE_INTERVENTO == "082"  ~ "07.05", # Clean urban transport rolling stock
        COD_SETTORE_INTERVENTO == "083"  ~ "07.05", # Cycling infrastructure
        COD_SETTORE_INTERVENTO == "084"  ~ "07.05", # Digitalisation of urban transport
        COD_SETTORE_INTERVENTO == "085"  ~ "07.05", # Digitalisation of transport when dedicated in part to greenhouse gas emissions reduction: urban transport
        COD_SETTORE_INTERVENTO == "086"  ~ "07.05", # Alternative fuels infrastructure
        COD_SETTORE_INTERVENTO == "089"  ~ "07.01", # Newly built or upgraded secondary road links to TEN-T road network and nodes
        COD_SETTORE_INTERVENTO == "090"  ~ "07.01", # Newly built or upgraded other national, regional and local access roads
        COD_SETTORE_INTERVENTO == "093"  ~ "07.01", # Other reconstructed or modernised roads (motorway, national, regional or local)
        COD_SETTORE_INTERVENTO == "094"  ~ "07.01", # Digitalisation of transport: road
        COD_SETTORE_INTERVENTO == "095"  ~ "07.01", # Digitalisation of transport when dedicated in part to greenhouse gas emissions reduction: road
        COD_SETTORE_INTERVENTO == "098"  ~ "07.02", # Other newly built or upgraded railways
        COD_SETTORE_INTERVENTO == "100" ~ "07.02", # Reconstructed or modernised railways - TEN-T core network
        COD_SETTORE_INTERVENTO == "102" ~ "07.02", # Other reconstructed or modernised railways
        COD_SETTORE_INTERVENTO == "103" ~ "07.02", # Other reconstructed or modernised railways – electric/zero emission
        COD_SETTORE_INTERVENTO == "104" ~ "07.02", # Digitalisation of transport: rail
        COD_SETTORE_INTERVENTO == "105" ~ "07.02", # European Rail Traffic Management System (ERTMS)
        COD_SETTORE_INTERVENTO == "107" ~ "07.02", # Mobile zero emission/electric powered rail assets
        COD_SETTORE_INTERVENTO == "108" ~ "07.02", # Multimodal transport (TEN-T)
        COD_SETTORE_INTERVENTO == "109" ~ "07.02", # Multimodal transport (not urban)
        COD_SETTORE_INTERVENTO == "111" ~ "07.03", # Seaports (TEN-T) excluding facilities dedicated to transport of fossil fuels
        COD_SETTORE_INTERVENTO == "112" ~ "07.03", # Other seaports
        COD_SETTORE_INTERVENTO == "113" ~ "07.03", # Other seaports excluding facilities dedicated to transport of fossil fuels
        COD_SETTORE_INTERVENTO == "118" ~ "07.04", # Security, safety and air traffic management systems, for existing airports
        COD_SETTORE_INTERVENTO == "120" ~ "07.02", # Digitising transport when dedicated in part to greenhouse gas emissions reduction: other transport modes
        COD_SETTORE_INTERVENTO == "121" ~ "11.01", # Infrastructure for early childhood education and care
        COD_SETTORE_INTERVENTO == "122" ~ "11.01", # Infrastructure for primary and secondary education
        COD_SETTORE_INTERVENTO == "123" ~ "11.01", # Infrastructure for tertiary education
        COD_SETTORE_INTERVENTO == "124" ~ "11.01", # Infrastructure for vocational education and training and adult learning
        COD_SETTORE_INTERVENTO == "125" ~ "10.01", # Housing infrastructure for migrants, refugees and persons under or applying for international protection
        COD_SETTORE_INTERVENTO == "126" ~ "10.01", # Housing infrastructure (other than for migrants, refugees and persons under or applying for international protection)
        # COD_SETTORE_INTERVENTO == "127" & OC_CODICE_PROGRAMMA == "2021IT16RFPR008" & COD_RISULTATO_ATTESO == "RSO4.6"~ "06.01", #fix anomalia lazio
        COD_SETTORE_INTERVENTO == "127" ~ "10.01", # Other social infrastructure contributing to social inclusion in the community
        COD_SETTORE_INTERVENTO == "128" ~ "10.02", # Health infrastructure
        COD_SETTORE_INTERVENTO == "129" ~ "10.02", # Health equipment
        COD_SETTORE_INTERVENTO == "130" ~ "10.02", # Health mobile assets
        COD_SETTORE_INTERVENTO == "131" ~ "10.02", # Digitalisation in health care
        COD_SETTORE_INTERVENTO == "132" ~ "05.01", # Critical equipment and supplies necessary to address emergency situation
        COD_SETTORE_INTERVENTO == "134" ~ "09.01", # Measures to improve access to employment
        COD_SETTORE_INTERVENTO == "135" ~ "09.01", # Measures to promote access to employment of long-term unemployed
        COD_SETTORE_INTERVENTO == "136" ~ "09.01", # Specific support for youth employment and socio-economic integration of young people
        COD_SETTORE_INTERVENTO == "137" ~ "09.01", # Support for self-employment and business start-ups
        COD_SETTORE_INTERVENTO == "138" ~ "09.01", # Support for social economy and social enterprises
        COD_SETTORE_INTERVENTO == "139" ~ "09.01", # Measures to modernise and strengthen labour market institutions and services to assess and anticipate skills needs and to ensure timely and tailor-made assistance
        COD_SETTORE_INTERVENTO == "140" ~ "09.01", # Support for labour market matching and transitions
        COD_SETTORE_INTERVENTO == "141" ~ "09.01", # Support for labour mobility
        COD_SETTORE_INTERVENTO == "142" ~ "09.01", # Measures to promote women’s labour market participation and reduce gender-based segregation in the labour market
        COD_SETTORE_INTERVENTO == "143" ~ "09.01", # Measures promoting work-life balance, including access to childcare and care for dependent persons
        COD_SETTORE_INTERVENTO == "144" ~ "09.01", # Measures for a healthy and well–adapted working environment addressing health risks, including promotion of physical activity
        COD_SETTORE_INTERVENTO == "145" ~ "11.02", # Support for the development of digital skills
        COD_SETTORE_INTERVENTO == "146" ~ "09.01", # Support for adaptation of workers, enterprises and entrepreneurs to change
        COD_SETTORE_INTERVENTO == "147" ~ "09.01", # Measures encouraging active and healthy ageing
        COD_SETTORE_INTERVENTO == "148" ~ "11.02", # Support for early childhood education and care (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "149" ~ "11.02", # Support for primary to secondary education (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "150" ~ "11.02", # Support for tertiary education (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "151" ~ "11.02", # Support for adult education (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "152" ~ "09.01", # Measures to promote equal opportunities and active participation in society
        COD_SETTORE_INTERVENTO == "153" ~ "09.01", # Pathways to integration and re-entry into employment for disadvantaged people
        COD_SETTORE_INTERVENTO == "154" ~ "10.03", # Measures to improve access of marginalised groups such as the Roma to education, employment and to promote their social inclusion
        COD_SETTORE_INTERVENTO == "155" ~ "10.03", # Support to the civil society working with marginalised communities such as the Roma
        COD_SETTORE_INTERVENTO == "156" ~ "09.01", # Specific actions to increase participation of third-country nationals in employment
        COD_SETTORE_INTERVENTO == "157" ~ "10.03", # Measures for the social integration of third-country nationals
        COD_SETTORE_INTERVENTO == "158" ~ "10.03", # Measures to enhancing the equal and timely access to quality, sustainable and affordable services
        COD_SETTORE_INTERVENTO == "159" ~ "10.03", # Measures to enhancing the delivery of family and community-based care services
        COD_SETTORE_INTERVENTO == "160" ~ "10.03", # Measures to improve the accessibility, effectiveness and resilience of healthcare systems (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "161" ~ "10.03", # Measures to improve access to long-term care (excluding infrastructure)
        COD_SETTORE_INTERVENTO == "162" ~ "10.03", # Measures to modernise social protection systems, including promoting access to social protection
        COD_SETTORE_INTERVENTO == "163" ~ "10.03", # Promoting social integration of people at risk of poverty or social exclusion, including the most deprived and children
        COD_SETTORE_INTERVENTO == "164" ~ "10.03", # Addressing material deprivation through food and/or material assistance to the most deprived, including accompanying measures
        COD_SETTORE_INTERVENTO == "165" ~ "06.01", # Protection, development and promotion of public tourism assets and tourism services
        COD_SETTORE_INTERVENTO == "166" ~ "06.02", # Protection, development and promotion of cultural heritage and cultural services
        COD_SETTORE_INTERVENTO == "167" ~ "06.01", # Protection, development and promotion of natural heritage and eco-tourism other than Natura 2000 sites
        COD_SETTORE_INTERVENTO == "168" ~ "08.01", # -> NEW: Physical regeneration and security of public spaces
        COD_SETTORE_INTERVENTO == "169" ~ "12.01", # -> NEW: Territorial development initiatives, including preparation of territorial strategies
        COD_SETTORE_INTERVENTO == "170" ~ "12.01", # Improve the capacity of programme authorities and bodies linked to the implementation of the Funds
        COD_SETTORE_INTERVENTO == "171" ~ "03.01", # Enhancing cooperation with partners both within and outside the Member State
        COD_SETTORE_INTERVENTO == "172" ~ "10.01", # Cross-financing under the ERDF (support to ESF+-type actions necessary for the implementation of the ERDF part of the operation and directly linked to it)
        COD_SETTORE_INTERVENTO == "173" ~ "03.01", # Enhancing institutional capacity of public authorities and stakeholders to implement territorial cooperation projects and initiatives in a cross-border, transnational, maritime and inter-regional context
        COD_SETTORE_INTERVENTO == "179" ~ "12.02", # Information and communication
        COD_SETTORE_INTERVENTO == "180" ~ "12.02", # Preparation, implementation, monitoring and control
        COD_SETTORE_INTERVENTO == "181" ~ "12.02", # Evaluation and studies, data collection
        COD_SETTORE_INTERVENTO == "182" ~ "12.01", # Reinforcement of the capacity of Member State authorities, beneficiaries and relevant partners
        COD_SETTORE_INTERVENTO == "188" ~ "01.01", # Productive investments in large enterprises linked primarily to clean and resource-efficient technologies
        COD_SETTORE_INTERVENTO == "189" ~ "01.01", # Productive investments in SMEs linked primarily to clean and resource-efficient technologies
        COD_SETTORE_INTERVENTO == "190" ~ "01.01", # Productive investments in large enterprises linked primarily to biotechnologies
        COD_SETTORE_INTERVENTO == "191" ~ "01.01", # Productive investments in SMEs linked primarily to biotechnologies
        COD_SETTORE_INTERVENTO == "192" ~ "01.01", # Productive investments in large enterprises linked primarily to digital technologies and deep tech innovation
        COD_SETTORE_INTERVENTO == "193" ~ "01.01", # Productive investments in SMEs linked primarily to digital technologies and deep tech innovation
        COD_SETTORE_INTERVENTO == "198" ~ "12.01", # -> NEW: Defence infrastructure and infrastructure construction and upgrades for dual use, including military mobility
        TRUE ~ NA_character_)) %>% 
    # integra 
    mutate(
      COD_SETTORE_INTERVENTO_PSC = case_when(
        # gestisce casi anomali
        COD_RISULTATO_ATTESO == "RSO5.1" ~ TEMP,
        COD_RISULTATO_ATTESO == "RSO5.2" ~ TEMP,
        COD_RISULTATO_ATTESO == "JSO8.1" ~ TEMP,
        COD_RISULTATO_ATTESO == "RSO5.3" ~ TEMP, #MEMO: contiene alloggi e energia 
        
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "001" ~ "01.01", # Investment in fixed assets, including research infrastructure, in micro enterprises directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "002" ~ "01.01", # Investment in fixed assets, including research infrastructure, in small and medium-sized enterprises (including private research centres) directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "003" ~ "01.01", # Investment in fixed assets, including research infrastructure, in large enterprises directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "004" ~ "01.02", # Investment in fixed assets, including research infrastructure, in public research centres and higher education directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "005" ~ "01.01", # Investment in intangible assets in micro enterprises directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "006" ~ "01.01", # Investment in intangible assets in SMEs (including private research centres) directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "007" ~ "01.01", # Investment in intangible assets in large enterprises directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "008" ~ "01.01", # Investment in intangible assets in public research centres and higher education directly linked to research and innovation activities
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "009" ~ "01.01", # Research and innovation activities in micro enterprises including networking (industrial research, experimental development, feasibility studies)
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "010" ~ "01.01", # Research and innovation activities in SMEs, including networking
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "011" ~ "01.01", # Research and innovation activities in large enterprises, including networking
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "012" ~ "01.01", # Research and innovation activities in public research centres, higher education and centres of competence including networking (industrial research, experimental development, feasibility studies)
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "013" ~ "01.01", # Digitising SMEs (including e-Commerce, e-Business and networked business processes, digital innovation hubs, living labs, web entrepreneurs and ICT start-ups, B2B)
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "016" ~ "01.01", # Government ICT solutions, e-services, applications
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "018" ~ "01.01", # IT services and applications for digital skills and digital inclusion
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "021" ~ "01.01", # SME business development and internationalisation, including productive investments
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "022" ~ "01.01", # Support for large enterprises through financial instruments, including productive investments
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "023" ~ "01.01", # Skills development for smart specialisation, industrial transition, entrepreneurship and adaptability of enterprises to change
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "024" ~ "01.02", # Advanced support services for SMEs and groups of SMEs (including management, marketing and design services)
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "025" ~ "01.02", # Incubation, support to spin offs and spin outs and start ups
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "026" ~ "01.01", # Support for innovation clusters including between businesses, research organisations and public authorities and business networks primarily benefiting SMEs
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "027" ~ "01.01", # Innovation processes in SMEs (process, organisational, marketing, co-creation, user and demand driven innovation)
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "028" ~ "01.01", # Technology transfer and cooperation between enterprises, research centres and higher education sector
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "029" ~ "01.01", # Research and innovation processes, technology transfer and cooperation between enterprises, research centres and universities, focusing on the low carbon economy, resilience and adaptation to climate change
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "030" ~ "01.01", # Research and innovation processes, technology transfer and cooperation between enterprises, focusing on circular economy
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "037" ~ "01.02", # ICT: Other types of ICT infrastructure (including large-scale computer resources/equipment, data centres, sensors and other wireless equipment) compliant with the carbon emission reduction and energy efficiency criteria
        COD_RISULTATO_ATTESO == "RSO1.1" & COD_SETTORE_INTERVENTO == "170" ~ "01.01", # Improve the capacity of programme authorities and bodies linked to the implementation of the Funds
        COD_RISULTATO_ATTESO == "RSO1.1" ~ "CHK",
        
        COD_RISULTATO_ATTESO == "RSO3.1" & COD_SETTORE_INTERVENTO == "108" ~ "07.02", # Multimodal transport (TEN-T)
        COD_RISULTATO_ATTESO == "RSO3.1" & COD_SETTORE_INTERVENTO == "100" ~ "07.02", # Reconstructed or modernised railways - TEN-T core network
        COD_RISULTATO_ATTESO == "RSO3.1" & COD_SETTORE_INTERVENTO == "111" ~ "07.03", # Seaports (TEN-T) excluding facilities dedicated to transport of fossil fuels
        COD_RISULTATO_ATTESO == "RSO3.1" ~ "CHK",
        
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "077" ~ "07.01", # Air quality and noise reduction measures
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "086" ~ "07.05", # Alternative fuels infrastructure
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "083" ~ "07.05", # Cycling infrastructure
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "095" ~ "07.01", # Digitalisation of transport when dedicated in part to greenhouse gas emissions reduction: road
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "104" ~ "07.02", # Digitalisation of transport: rail
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "094" ~ "07.01", # Digitalisation of transport: road
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "105" ~ "07.02", # European Rail Traffic Management System (ERTMS)
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "107" ~ "07.02", # Mobile zero emission/electric powered rail assets
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "109" ~ "07.02", # Multimodal transport (not urban)
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "090" ~ "07.01", # Newly built or upgraded other national, regional and local access roads
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "089" ~ "07.01", # Newly built or upgraded secondary road links to TEN-T road network and nodes
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "098" ~ "07.02", # Other newly built or upgraded railways
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "102" ~ "07.02", # Other reconstructed or modernised railways
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "103" ~ "07.02", # Other reconstructed or modernised railways – electric/zero emission
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "093" ~ "07.01", # Other reconstructed or modernised roads (motorway, national, regional or local)
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "112" ~ "07.03", # Other seaports
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "113" ~ "07.03", # Other seaports excluding facilities dedicated to transport of fossil fuels
        COD_RISULTATO_ATTESO == "RSO3.2" & COD_SETTORE_INTERVENTO == "118" ~ "07.04", # Security, safety and air traffic management systems, for existing airports
        COD_RISULTATO_ATTESO == "RSO3.2" ~ "CHK",
        
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "019" ~ "10.02", # e-Health services and applications (including e-Care, Internet of Things for physical activity and ambient assisted living)
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "042" ~ "10.01", # Energy efficiency renovation of existing housing stock, demonstration projects and supporting measures compliant with energy efficiency criteria
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "125" ~ "10.01", # Housing infrastructure for migrants, refugees and persons under or applying for international protection
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "126" ~ "10.01", # Housing infrastructure (other than for migrants, refugees and persons under or applying for international protection)
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "127" ~ "10.01", # Other social infrastructure contributing to social inclusion in the community
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "138" ~ "10.03", # Support for social economy and social enterprises
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "143" ~ "10.03", # Measures promoting work-life balance, including access to childcare and care for dependent persons
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "170" ~ "10.03", # Improve the capacity of programme authorities and bodies linked to the implementation of the Funds
        COD_RISULTATO_ATTESO == "RSO4.3" & COD_SETTORE_INTERVENTO == "172" ~ "10.03", # Cross-financing under the ERDF (support to ESF+-type actions necessary for the implementation of the ERDF part of the operation and directly linked to it)
        COD_RISULTATO_ATTESO == "RSO4.3" ~ "CHK",
        
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "172" ~ "06.02", # Cross-financing under the ERDF (support to ESF+-type actions necessary for the implementation of the ERDF part of the operation and directly linked to it)
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "170" ~ "06.02", # Improve the capacity of programme authorities and bodies linked to the implementation of the Funds
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "018" ~ "06.02", # IT services and applications for digital skills and digital inclusion
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "127" ~ "06.02", # Other social infrastructure contributing to social inclusion in the community
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "163" ~ "06.02", # Promoting social integration of people at risk of poverty or social exclusion, including the most deprived and children
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "166" ~ "06.01", # Protection, development and promotion of cultural heritage and cultural services
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "167" ~ "05.05", # Protection, development and promotion of natural heritage and eco-tourism other than Natura 2000 sites
        COD_RISULTATO_ATTESO == "RSO4.6" & COD_SETTORE_INTERVENTO == "165" ~ "03.02", # Protection, development and promotion of public tourism assets and tourism services
        COD_RISULTATO_ATTESO == "RSO4.6" ~ "CHK",
        
        COD_RISULTATO_ATTESO == "TA" ~ TEMP,
        
        # gestisce casi normali
        COD_RISULTATO_ATTESO == "ESO4.1"  ~ "09.01",
        COD_RISULTATO_ATTESO == "ESO4.2"  ~ "09.01",
        COD_RISULTATO_ATTESO == "ESO4.3"  ~ "09.01",
        COD_RISULTATO_ATTESO == "ESO4.4"  ~ "09.01",
        COD_RISULTATO_ATTESO == "ESO4.5"  ~ "11.02",
        COD_RISULTATO_ATTESO == "ESO4.6"  ~ "11.02",
        COD_RISULTATO_ATTESO == "ESO4.7"  ~ "11.02",
        COD_RISULTATO_ATTESO == "ESO4.8"  ~ "09.01",
        COD_RISULTATO_ATTESO == "ESO4.9"  ~ "10.03",
        COD_RISULTATO_ATTESO == "ESO4.10" ~ "10.03",
        COD_RISULTATO_ATTESO == "ESO4.11" ~ "10.03",
        COD_RISULTATO_ATTESO == "ESO4.12" ~ "10.03",
        COD_RISULTATO_ATTESO == "ESO4.13" ~ "10.03",
        COD_RISULTATO_ATTESO == "RSO1.2"  ~ "02.01",
        COD_RISULTATO_ATTESO == "RSO1.3"  ~ "03.01",
        COD_RISULTATO_ATTESO == "RSO1.4"  ~ "03.04",
        COD_RISULTATO_ATTESO == "RSO1.5"  ~ "02.02", #NEW: Rafforzare la connettività digitale
        COD_RISULTATO_ATTESO == "RSO1.6"  ~ "01.01",
        COD_RISULTATO_ATTESO == "RSO1.7"  ~ "12.01", #NEW: Potenziamento dell'offerta industriale per la difesa (non letterale)
        COD_RISULTATO_ATTESO == "RSO2.1"  ~ "04.01",
        COD_RISULTATO_ATTESO == "RSO2.2"  ~ "04.02",
        COD_RISULTATO_ATTESO == "RSO2.3"  ~ "04.03",
        COD_RISULTATO_ATTESO == "RSO2.4"  ~ "05.01",
        COD_RISULTATO_ATTESO == "RSO2.5"  ~ "05.02",
        COD_RISULTATO_ATTESO == "RSO2.6"  ~ "05.03",
        COD_RISULTATO_ATTESO == "RSO2.7"  ~ "05.05",
        COD_RISULTATO_ATTESO == "RSO2.8"  ~ "07.05",
        COD_RISULTATO_ATTESO == "RSO2.9"  ~ "01.01",
        COD_RISULTATO_ATTESO == "RSO2.10" ~ "05.01", #NEW: Sostenere gli investimenti volti alla ricostruzione in risposta a una catastrofe naturale verificatasi tra il 1° gennaio 2024 e il 31 dicembre 2025
        COD_RISULTATO_ATTESO == "RSO2.11" ~ "04.01", #NEW: Promuovere l’accesso ad alloggi sostenibili e a prezzi accessibili
        COD_RISULTATO_ATTESO == "RSO2.12" ~ "04.03", #NEW: Promuovere gli interconnettori dell’energia e le relative infrastrutture di trasmissione, di distribuzione, di stoccaggio e di sostegno, nonché la protezione delle infrastrutture energetiche critiche, così come la realizzazione dell’infrastruttura di ricarica
        COD_RISULTATO_ATTESO == "RSO3.3"  ~ "12.01", #NEW: Sviluppare infrastrutture di difesa resilienti o a duplice uso, anche per favorire la mobilità militare nell’Unione, nonché rafforzare la preparazione a conflitti e aggressioni
        COD_RISULTATO_ATTESO == "RSO4.1"  ~ "09.01",
        COD_RISULTATO_ATTESO == "RSO4.2"  ~ "11.01",
        COD_RISULTATO_ATTESO == "RSO4.4"  ~ "10.01",
        COD_RISULTATO_ATTESO == "RSO4.5"  ~ "10.02",
        COD_RISULTATO_ATTESO == "RSO4.7"  ~ "10.01", #NEW: Promuovere l’accesso ad alloggi a prezzi accessibili
        # COD_RISULTATO_ATTESO == "RSO5.3"  ~ "08.01", #NEW: Promuovere lo sviluppo territoriale integrato attraverso l’accesso ad alloggi sostenibili e a prezzi accessibili in tutti i tipi di territori
        # COD_RISULTATO_ATTESO == "TA" ~ "12.02", # at
        TRUE ~ NA_character_)) %>% 
    select(-TEMP) %>% 
    # integra dominio
    left_join(temi, by = "COD_SETTORE_INTERVENTO_PSC") %>% 
    # riordina
    select(names(db_sie))
  
  # chk
  message("Verifica DBCOE:")
  
  chk <- sum(appo$FINANZ_TOTALE, na.rm = TRUE) - sum(db_sie$FINANZ_TOTALE, na.rm = TRUE)
  message(paste0("La differenza di risorse tra prima e dopo elaborazione temi e domini è ", chk))
  
  chk0 <- appo %>% filter(is.na(DESCR_LIVELLO_1)) %>% count() %>% .$n
  message(paste0("Le righe senza nome asse sono ", chk0, " (vedi file in TEMP, poi integra nel DBCOE)"))
  
  chk1 <- appo %>% 
    filter(is.na(DESCR_LIVELLO_1)) %>% 
    count(OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, COD_LIVELLO_1) %>% 
    select(-n)
  write.xlsx(chk1, file.path(TEMP, "chk_na_label_assi_dbsie.xlsx"))
  
  chk2 <- appo %>% filter(is.na(COD_SETTORE_INTERVENTO_PSC) | COD_SETTORE_INTERVENTO_PSC == "CHK") %>% count() %>% .$n
  message(paste0("Le righe senza classificazione tematica sono ", chk2, " (vedi file in TEMP, poi integra mapping nello script)"))
  
  chk3 <- appo %>% 
    filter(is.na(COD_SETTORE_INTERVENTO_PSC)) %>% 
    count(COD_RISULTATO_ATTESO, DESCR_RISULTATO_ATTESO) %>% 
    left_join(appo %>% filter(is.na(COD_SETTORE_INTERVENTO_PSC)) %>% 
                distinct(COD_RISULTATO_ATTESO, COD_SETTORE_INTERVENTO) %>% 
                group_by(COD_RISULTATO_ATTESO) %>% 
                summarise(COD_SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, collapse = ":::")),
              by = "COD_RISULTATO_ATTESO")
  write.xlsx(chk3, file.path(TEMP, "chk_na_temi_dbsie.xlsx"))
  
  chk4 <- appo %>% 
    filter(COD_SETTORE_INTERVENTO_PSC == "CHK") %>% 
    count(COD_RISULTATO_ATTESO, DESCR_RISULTATO_ATTESO) %>% 
    left_join(appo %>% filter(COD_SETTORE_INTERVENTO_PSC == "CHK") %>% 
                distinct(COD_RISULTATO_ATTESO, COD_SETTORE_INTERVENTO) %>% 
                group_by(COD_RISULTATO_ATTESO) %>% 
                summarise(COD_SETTORE_INTERVENTO = paste0(COD_SETTORE_INTERVENTO, collapse = ":::")),
              by = "COD_RISULTATO_ATTESO")
  write.xlsx(chk4, file.path(TEMP, "chk_chk_temi_dbsie.xlsx"))
  
  
  # export
  write.xlsx(appo, file.path(DB, "Dati_DBCOE_SIE2127_CI.xlsx"))
  
  
  # debug
  debug_tables <- NULL
  
  if (isTRUE(debug)) {
    debug_t1 <- appo %>% 
      group_by(
        COD_RISULTATO_ATTESO,
        DESCR_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO,
        DESCR_SETTORE_INTERVENTO,
        COD_SETTORE_INTERVENTO_PSC,
        DESCR_SETTORE_INTERVENTO_PSC
      ) %>% 
      summarise(
        N_RIGHE = dplyr::n(),
        FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      arrange(
        COD_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO,
        COD_SETTORE_INTERVENTO_PSC
      )
    
    debug_t2 <- appo %>% 
      group_by(
        COD_RISULTATO_ATTESO,
        DESCR_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO_PSC,
        DESCR_SETTORE_INTERVENTO_PSC
      ) %>% 
      summarise(
        N_RIGHE = dplyr::n(),
        FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      arrange(
        COD_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO_PSC
      )
    
    
    debug_t3 <- appo %>% 
      group_by(
        COD_RISULTATO_ATTESO,
        DESCR_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO_PSC,
        DESCR_SETTORE_INTERVENTO_PSC,
        COD_SETTORE_INTERVENTO,
        DESCR_SETTORE_INTERVENTO
      ) %>% 
      summarise(
        N_RIGHE = dplyr::n(),
        FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      arrange(
        COD_RISULTATO_ATTESO,
        COD_SETTORE_INTERVENTO_PSC,
        COD_SETTORE_INTERVENTO
      )
    
    debug_tables <- list(
      riepilogo_os_ci_temi = debug_t1,
      riepilogo_os_temi = debug_t2,
      riepilogo_os_temi_ci = debug_t3
    )
    
    write.xlsx(
      debug_tables,
      file.path(TEMP, "debug_fix_dbcoe_sie.xlsx")
    )
  }
  
  return(list(
    riepilogo_os_ci_temi = debug_t1,
    riepilogo_os_temi = debug_t2,
    riepilogo_os_temi_ci = debug_t3
  ))
}