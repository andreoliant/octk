# Programmazione

#' Carica un dataset dal database della programmazione
#'
#' Carica il datase richiesto dal dataset della programmazione.
#'
#' @param ciclo Ciclo di programmazione.
#' @param ambito Ambito di programmazione..
#' @param simplify_loc Logico. Vuoi semplificare le localizzazioni per compatibilit? con lo standard dei Report CD?
#' @param use_temi Logico. Vuoi avere anche i temi prioritari FSC?
#' @param use_sog Logico. Vuoi avere anche il soggetto programmatore?
#' @param use_eu Logico. Vuoi avere anche il finanziamento EU e categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_articolaz Logico. Oltre ai temi, vuoi importare anche le articolazioni? Utile per FESR e FSE
#' @param use_location Logico. Vuoi avere anche la localizzazione dei progetti per Regione e Macroarea?
#' @return Il dataset di programmazione per l'ambito richiesto, con pulizia delle denominazioni territoriali e della codifica di aree tematiche e temi prioritari FSC.
load_db_old <- function(ciclo, ambito, simplify_loc=FALSE, use_temi=FALSE, use_sog=FALSE, use_ue=FALSE, use_flt=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=FALSE){
  
  # DEV: decidere se fare importazione di tutto e poi selezionare variabili a valle....
  
  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FESR"
  
  # crea nome file da importare
  if (ciclo == "2014-2020") {
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      # ambito == "FEASR" ~ "SIE",
                      # ambito == "FEAMP" ~ "SIE",
                      ambito == "FEASR" ~ "FEASR",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "YEI" ~ "SIE",
                      ambito == "CTE" ~ "CTE", #AF
                      ambito == "POC" ~ "POC", #AF
                      ambito == "SNAI" ~ "SNAI", #AF 
                      ambito == "FSC" ~ "FSC", #AF
                      # CHK: decidere se vive
                      TRUE ~ ambito)
    # filename <- paste0(temp, "_1420.xlsx")
    filename <- paste0("Dati_DBCOE_", temp, "1420.xlsx") # DBPROG_FSC1420.xlsx
    
  } else if (ciclo == "2021-2027") {
    # NEW 2127
    temp <- case_when(ambito == "FESR" ~ "SIE",
                      ambito == "FSE" ~ "SIE",
                      ambito == "FEAMP" ~ "FEAMP",
                      ambito == "JTF" ~ "SIE",
                      # ambito == "CTE" ~ "CTE",
                      # ambito == "POC" ~ "POC",
                      ambito == "SNAI" ~ "SNAI", 
                      ambito == "FSC" ~ "FSC",
                      TRUE ~ ambito)
    filename <- paste0("Dati_DBCOE_", temp, "2127.xlsx")

  } else {
    temp <- case_when(ambito == "FESR" ~ "SIE", #AF
                      ambito == "FSE" ~ "SIE", #AF
                      ambito == "PAC" ~ "PAC", #AF
                      ambito == "FSC" ~ "FSC", #AF
                      TRUE ~ ambito)
    #filename <- paste0(temp, "_0713.xlsx") # MODIFICA AF
    filename <- paste0("Dati_DBCOE_", temp, "0713.xlsx") # DBPROG_FSC0713.xlsx
  }
  
  # importa file excel
  # OLD: appo <-  read_excel(file.path(DATA, "db", filename), guess_max = 5000) # MEMO: versione prima di GoogleDrive
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)
  # VERSIONE  CON UNICO FILE - DA SISTEMARE: appo1 <-  read_excel(file.path(DB, "prova2.xlsx"), sheet = filename, guess_max = 5000)
  
  # filtra ambiti da SIE
  # if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI") {
  #   appo <- appo %>%
  #     filter(AMBITO == ambito)
  # }
  
  # NEW 2127
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI"  | ambito == "JTF") {
    appo <- appo %>%
      # MEMO: ricodifica da "FSE+" a "FSE" standard
      mutate(AMBITO = if_else(AMBITO == "FSE+", "FSE", AMBITO)) %>%
      filter(AMBITO == ambito)
  }
  
  # MEMO: ELIMINATO PERCH? INCORPORATO NEL DB SIA IL FLAG ULTIMA DECISIONE CHE MONITORAGGIO. 
  # MEMO2: INFATTI NEI NUOVI DB PROGRAMMAZIONE SONO RICOMPRESE SOLO LE ULTIME DECISIONI, LO STORICO E' SALVATO A PARTE IN ALTRO DB
  # MEMO3: PER QUANTO RIGUARDA IL FLAG MONITORAGGIO, PRESENTE NEL DB PROGRAMMAZIONE, AD OGGI E' UNA VARIABILE NON PIU UTILE. 
  # MEMO4: !ATTENZIONE! IN FLAG MONITORAGGIO SONO UTILIZZATE LE PIU DIVERSE FRASI E CODICI PER IL POPOLAMENTO DELLA VARIABILE A SECONDA DELL'AMBITO PREVISTO
  # if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI" | ambito == "FEASR" | ambito == "FEAMP" ) {
  #   appo <- appo %>%
  #     filter(OC_DESCR_FONTE == ambito) %>%
  #     # MEMO: questo serve per integrare versione con Asse
  #     filter(OC_FLAG_ULTIMA_DECISIONE == "X")
  # }
  
  # if (ambito == "CTE" ) {
  #   appo <- appo %>%
  #     filter(OC_DESCR_FONTE == ambito) %>%
  #     # MEMO: questo serve per integrare versione con Asse
  #     filter(OC_FLAG_ULTIMA_DECISIONE == "X") %>%
  #     filter(OC_FLAG_MONITORAGGIO == 1) 
  # }
  
  # patch per specifiche DB FSC
  # if (ambito == "FSC") {
  #   appo <- appo %>%
  #     mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC)
  # }
  
  # switch varibili da tenere
  var_ls <- c("OC_CODICE_PROGRAMMA", "DESCRIZIONE_PROGRAMMA", "TIPOLOGIA_PROGRAMMA",
              "AMBITO",
              "FINANZ_TOTALE")
  
  # SPOSTARE "OC_MACROAREA", "DEN_REGIONE" IN IF CON "USE_LOCATION" 
  
  if (use_temi == TRUE) {
    var_ls <- c(var_ls,
                "COD_AREA_TEMATICA_PSC", "DESCR_AREA_TEMATICA_PSC", 
                "COD_SETTORE_INTERVENTO_PSC", "DESCR_SETTORE_INTERVENTO_PSC",
                "COD_RISULTATO_ATTESO", "DESCR_RISULTATO_ATTESO")
    
    appo <- appo %>%
      mutate(COD_RISULTATO_ATTESO = as.character(COD_RISULTATO_ATTESO))
    
  }
  
  if (use_articolaz == TRUE) {
    var_ls <- c(var_ls,
                "COD_LIVELLO_1", "DESCR_LIVELLO_1" )
  }
  
  
  if (use_sog == TRUE) {
    var_ls <- c(var_ls,
                "AMMINISTRAZIONE", "TIPOLOGIA_AMMINISTRAZIONE")
  }
  
  if (use_ue == TRUE) {
    var_ls <- c(var_ls,
                "FINANZ_UE", "FINANZ_ALTRO", "CAT_REGIONE")
    
  }
  
  if (use_flt == TRUE) {
    var_ls <- c(var_ls,
                "FLAG_MONITORAGGIO")
    
    # patch per dati da consolidare nel DB
    appo <- appo %>%
      mutate(FLAG_MONITORAGGIO = as.numeric(FLAG_MONITORAGGIO)) %>%
      mutate(FLAG_MONITORAGGIO = case_when(FLAG_MONITORAGGIO == 1 ~ 1,
                                           FLAG_MONITORAGGIO == 0 ~ 0,
                                           FLAG_MONITORAGGIO == 2 ~ 2, # presente per FSC e POC
                                           FLAG_MONITORAGGIO == 3 ~ 3, # presente per FSC e POC
                                           FLAG_MONITORAGGIO == 4 ~ 4, # presente per POC (valori exart. 242 conteggiati anche nei FS)
                                           FLAG_MONITORAGGIO == 9 ~ 9, # presente per FSC
                                           TRUE ~ 0))
    
  }
  
  if (use_location == TRUE) {
    var_ls <- c(var_ls,
                "MACROAREA", "DEN_REGIONE")
  }
  
  if (use_ciclo == TRUE) {
    var_ls <- c(var_ls, 
                "CICLO_PROGRAMMAZIONE", "CICLO_RISORSE")
  }
  
  # aggiungo ciclo e ambito
  appo <- appo %>%
    mutate(x_CICLO = CICLO_PROGRAMMAZIONE, # x_CICLO = ciclo, 
           x_AMBITO = ambito) 
  # MEMO: qui sovrascrive ambito "strategia" ad ambito specificato nel DB (che può essere "contabile - es. completamenti MIUR FSC dentro POC)
  
  # %>%
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  appo <- refactor_ambito(appo)
  appo <- refactor_ciclo(appo)
  
  var_ls <- c(var_ls, 
              "x_CICLO", "x_AMBITO")
  
  # select varibili di interesse
  appo <- appo %>%
    select(all_of(var_ls))
  
  return(appo)
  
}

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

#' Carica un dataset "correzioni" dal database della programmazione
#'
#' Carica il dataset "correzioni" per SIE e POC 2014-2020 richiesto dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset "correzioni".
load_correzioni_siepoc1420 <- function(DB) {
  out <- read_xlsx(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx")) %>%
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
  out <- read_xlsx(file.path(DB, "Stime_DBCOE_SIEPOC.xlsx")) %>% 
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
  out <- read_xlsx(file.path(DB, "Elenco_ufficiale_nomi.xlsx"))
  return(out)
}

#' Carica i totali di riferimento dal database della programmazione
#'
#' Carica i totali di riferimento dal database della programmazione.
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Il dataset con i totali di riferimento.
load_totali_dbcoe <- function(DB) {
  out <- read_xlsx(file.path(DB, "Totali.xlsx"))
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
  
  appo <- read_xlsx(file.path(dirname(DB), db_old, "label_programmi_en.xlsx")) %>% 
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
  
  appo <- read_xlsx(file.path(dirname(DB), db_old, "link_sito_programmi.xlsx")) %>% 
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
#' @param use_temi Vuoi caricare il DB con correzione dei temi prioritari?
#' @param use_sog Vuoi caricare il DB con il soggetto programmatore?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param use_ciclo Voi caricare il ciclo?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_CICLO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_CICLO da DB)?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC? [FUNZIONALITA' DEPRECATA]
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @return L'intero database dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
init_programmazione_dati_old <- function(use_temi=FALSE, use_sog=FALSE, use_eu=FALSE, use_flt=FALSE, 
                                     use_713=FALSE, 
                                     use_articolaz=FALSE, use_location=FALSE, use_ciclo=TRUE, tipo_ciclo="CICLO_STRATEGIA", 
                                     # use_en=FALSE, 
                                     use_po_psc=FALSE,
                                     use_cicli_psc=FALSE,
                                     use_fix_siepoc=FALSE,
                                     stime_fix_siepoc=FALSE)
{
  # use_temi = FALSE
  # use_sog= TRUE
  # use_eu= TRUE
  # use_713 = TRUE
  # use_ciclo = FALSE
  # use_flt = FALSE
  # use_location = TRUE
  # use_articolaz = TRUE
  # use_po_psc=FALSE
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=FALSE
  
  po_fsc <- load_db_old("2014-2020", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF aggiunto use_locatione che prima mancava
  po_fesr <- load_db_old("2014-2020", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_fse <- load_db_old("2014-2020", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_poc <- load_db_old("2014-2020", "POC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_yei <- load_db_old("2014-2020", "YEI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feamp <- load_db_old("2014-2020", "FEAMP", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_snai <- load_db_old("2014-2020", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_cte <- load_db_old("2014-2020", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feasr <- load_db_old("2014-2020", "FEASR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF mancava, aggiunto
  
  # NEW 2127
  po_fsc2127 <- load_db_old("2021-2027", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) 
  po_fesr2127 <- load_db_old("2021-2027", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_fse2127 <- load_db_old("2021-2027", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_jtf2127 <- load_db_old("2021-2027", "JTF", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_snai2127 <- load_db_old("2021-2027", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_cte2127 <- load_db_old("2021-2027", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_poc2127 <- load_db_old("2021-2027", "POC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) 
  
  if (use_temi == TRUE) {
    po_fsc2127 <- po_fsc2127 %>% 
      mutate(COD_AREA_TEMATICA_PSC = as.character(COD_AREA_TEMATICA_PSC))
  }
  
  programmi <- po_fsc %>%
    bind_rows(po_poc) %>%
    bind_rows(po_fesr) %>%
    bind_rows(po_fse) %>%
    bind_rows(po_yei) %>%
    bind_rows(po_feamp) %>%
    bind_rows(po_snai) %>%
    bind_rows(po_cte) %>%
    bind_rows(po_feasr) %>%
    # NEW 2127
    bind_rows(po_fsc2127) %>%
    bind_rows(po_fesr2127) %>%
    bind_rows(po_fse2127) %>%
    bind_rows(po_jtf2127) %>%
    bind_rows(po_snai2127) %>%
    bind_rows(po_cte2127) %>%
    bind_rows(po_poc2127) %>%
    as.data.frame(.)
  
  if (use_713 == TRUE) {
    po_fsc713 <- load_db_old("2007-2013", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fesr713 <- load_db_old("2007-2013", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fse713 <- load_db_old("2007-2013", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_pac713 <- load_db_old("2007-2013", "PAC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    
    programmi <- programmi %>%
      bind_rows(po_fsc713) %>%
      bind_rows(po_pac713) %>%
      bind_rows(po_fesr713) %>%
      bind_rows(po_fse713)

    # patch per programmi su due ambiti o su due cicli nello stesso ambito (con gruppi diversi)
    # programmi <- programmi %>%
    #   mutate(OC_TIPOLOGIA_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "2007IT001FA005" ~ "NAZ-INF",
    #                                             OC_CODICE_PROGRAMMA == "2007IT005FAMG1" ~ "PAC Nazionale",
    #                                             OC_CODICE_PROGRAMMA == "2007SA002FA016" ~ "REG",
    #                                             OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" ~ "Piani nazionali",
    #                                             OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" ~ "Altre assegnazioni CIPE",
    #                                             OC_CODICE_PROGRAMMA == "CIS_TA_PUG" ~ "Altre assegnazioni CIPE",
    #                                             OC_CODICE_PROGRAMMA == "TEMP_0713_020" ~ "Altre assegnazioni CIPE",
    #                                             TRUE ~ OC_TIPOLOGIA_PROGRAMMA))
    
  }
  
  # spostato in workflow pubblicazione
  # if (use_en == TRUE) {
  #   programmi_en <- read_csv2(file.path(DB, "programmi_SIE_EN.csv")) %>%
  #     select(-LABEL_PROGRAMMA_IT) 
  #   
  #   # label LABEL_PROGRAMMA_EN
  #   left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
  #     mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
  #            LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN))
  # }

  if (use_ciclo == TRUE) {
    if (tipo_ciclo == "CICLO_RISORSE") {
      programmi <- programmi %>%
        mutate(x_CICLO = CICLO_RISORSE)
    } else if (tipo_ciclo == "CICLO_STRATEGIA") {
      programmi <- programmi
      # MEMO: x_CICLO di default è gia CICLO_STRATEGIA
    }
  }
  
  if (use_location == TRUE) {
    # ricodifica x_MACROAREA
    programmi %>% count(MACROAREA)
    programmi <- ricodifica_macroaree(programmi)
    programmi %>% count(x_MACROAREA)
  }
  
  
  # NEW ANTICIPAZIONI
  if (use_fix_siepoc == TRUE) {
    if (file.exists(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx"))) {
      ant_siepoc <- load_correzioni_siepoc1420(DB) 
    } else {
      message("errore, le anticipazioni non sono implementate")
    }
    
    # swtich correzioni vs stime
    if (stime_fix_siepoc == TRUE) {
      ant_siepoc <- load_stime_siepoc1420(DB)
    } else {
      ant_siepoc <- ant_siepoc %>%
        filter(FLAG_FONTE_FORMALE == "SI")
    }
    
    # fix label macroarea 
    if ("x_MACROAREA" %in% names(programmi)) {
      ant_siepoc <- ant_siepoc %>%
        rename(x_MACROAREA = MACROAREA) %>% 
        ricodifica_macroaree()
    }
    
    

    ant_siepoc <- ant_siepoc %>% 
      mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
             x_AMBITO = AMBITO,
             # x_PROGRAMMA,
             # x_GRUPPO,
             # RISORSE,
             # RISORSE_UE
             )
    
    if (use_eu == TRUE) {
      ant_siepoc <- ant_siepoc %>%
        select(names(programmi)) %>%
        # group_by(across(-"FINANZ_TOTALE")) %>%
        group_by(across(c(-FINANZ_TOTALE, -FINANZ_UE, -FINANZ_ALTRO))) %>%
        # summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
        summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE),
                  FINANZ_UE = sum(FINANZ_UE, na.rm = TRUE),
                  FINANZ_ALTRO = sum(FINANZ_ALTRO, na.rm = TRUE))
    } else {
      
      ant_siepoc <- ant_siepoc %>%
        select(names(programmi)) %>%
        group_by(across(c(-FINANZ_TOTALE))) %>%
        summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    }
    
    programmi <- programmi %>% 
      anti_join(ant_siepoc, by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>% 
      bind_rows(ant_siepoc)
  }
  
  if (use_po_psc == TRUE){
    # sovrascrive dati da DBCOE per PSC con dati custum per PO
    
    # DEBUG:
    # memo <- programmi
    # programmi <- memo
    # sum(memo$FINANZ_TOTALE, na.rm=T)
    
    # OLD:
    # po_psc <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
    
    
    # NEW:
    if (file.exists(file.path(DB, "Fonti_DBCOE_PSC.xlsx"))) {
      po_psc <- read_xlsx(file.path(DB, "Fonti_DBCOE_PSC.xlsx")) %>% 
        mutate(CICLO_RISORSE = CICLO_PROGRAMMAZIONE)
      
    } else if (file.exists(file.path(DB, "fsc_matrice_po_psc.xlsx"))) {
      po_psc <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
    } else {
      message("errore, non esistevano ancora i psc!!!")
    }
    
    if ("x_MACROAREA" %in% names(programmi)) {
      po_psc <- po_psc %>% 
        # rename(x_MACROAREA = MACROAREA) 
        ricodifica_macroaree()
    }
   
    po_psc <- po_psc %>% 
      mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
             x_AMBITO = "FSC") %>% 
      select(c(names(programmi), "ID_PSC", "PSC")) %>%
      # select(names(programmi), "ID_PSC") %>% # DEBUG: solo per usare chk sotto
      group_by(across(-"FINANZ_TOTALE")) %>% 
      summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # sum(po_psc$FINANZ_TOTALE) # 81.878.600.134
    
    programmi <- programmi %>% 
      # filter(TIPOLOGIA_PROGRAMMA != "PSC") %>% # MEMO: qui perdo anche i casi NA
      filter(TIPOLOGIA_PROGRAMMA != "PSC" | is.na(TIPOLOGIA_PROGRAMMA)) %>% 
      # mutate(ID_PSC = NA_character_,
      #        PSC = NA_character_) %>% 
      # CHK: qui non devo eslcudere tutto, solo quello che è diverso da CSR (e COVID?)
      bind_rows(po_psc)
    # sum(programmi$FINANZ_TOTALE)

  }
  
  if (use_cicli_psc == TRUE){
    # sovrascrive dati da file dati DBCOE per PSC con dati per cicli da file interventi PSC

    programmazione <- load_db_psc(use_flt=TRUE)
    
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
             FINANZ_UE = 0, 
             FINANZ_ALTRO = 0, 
             CAT_REGIONE = NA_character_, 
             x_CICLO = CICLO_PROGRAMMAZIONE,
             CICLO_RISORSE = CICLO_PROGRAMMAZIONE, 
             ) %>% 
      # adatta nomi a file dati dbcoe
      rename(FINANZ_TOTALE = RISORSE, 
             TIPOLOGIA_AMMINISTRAZIONE = TIPO_AR) %>%
      ungroup(.) %>% 
      select(c(names(programmi), "ID_PSC")) %>%
      group_by(across(-"FINANZ_TOTALE")) %>% 
      summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))

    # programmi2 <- programmi %>% 
    #   filter(!(TIPOLOGIA_PROGRAMMA == "PSC" & COD_LIVELLO_1 %in% c("SEZ_ORD", "SEZ_CIS")) | is.na(TIPOLOGIA_PROGRAMMA)) %>% 
    #   bind_rows(cicli_psc)
    
    sezspec <- load_db("2014-2020", "FSC", simplify_loc = TRUE, use_temi = use_temi, 
                       use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, 
                       use_location = TRUE, use_ciclo = use_ciclo, 
                       use_articolaz = TRUE) %>% 
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
      select(c(names(programmi), "ID_PSC")) %>%
      group_by(across(-"FINANZ_TOTALE")) %>% 
      summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    
    programmi2 <- programmi %>% 
      filter(TIPOLOGIA_PROGRAMMA != "PSC" | is.na(TIPOLOGIA_PROGRAMMA)) %>% 
      # recupera sezioni speciali
      bind_rows(sezspec) %>% 
      # integra nuova sezione ordinaria
      bind_rows(bind_rows(cicli_psc))
    
    # programmi %>% 
    #   filter(TIPOLOGIA_PROGRAMMA == "PSC" & 
    #            COD_LIVELLO_1 %in% c("SEZ_SPEC_1_COVID", "SEZ_SPEC_2_FS")) %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # 
    # programmi %>% 
    #   filter(TIPOLOGIA_PROGRAMMA == "PSC" & 
    #            COD_LIVELLO_1 %in% c("SEZ_CIS", "SEZ_ORD")) %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # 
    # programmi %>% 
    #   filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    # 
    # cicli_psc %>%
    #   ungroup() %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    
    # chk
    sum(programmi$FINANZ_TOTALE, na.rm = TRUE) - sum(programmi2$FINANZ_TOTALE, na.rm = TRUE)
    
    # programmi <- programmi2 %>% 
    #   group_by(across(-"FINANZ_TOTALE")) %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    
    programmi <- programmi2 %>% 
      group_by(across(c(-"FINANZ_TOTALE", -"FINANZ_UE"))) %>% 
      summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE),
                FINANZ_UE = sum(FINANZ_UE, na.rm = TRUE))

  }
  
  return(programmi)

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
#' @param usa_en Vuoi caricare le traduzioni in inglese?
#' @param usa_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param sum_po Vuoi dati aggregati come summary per programma, con decisioni collassati?
#' @param sum_po_last Vuoi dati aggregati come summary per programma, con solo l'ultima decisione?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @return Tutti i dati di supporto.
init_programmazione_info_old <- function(use_en = FALSE, use_713 = FALSE, sum_po = FALSE, sum_po_last=FALSE, 
                                     use_po_psc=FALSE, use_fix_siepoc=FALSE) {
  
  if (use_713 == TRUE) {
    info_FSC0713 <- read_xlsx(file.path(DB, "Info_DBCOE_FSC0713.xlsx"))
    # info_FS0713  <- read_xlsx(file.path(DB, "Info_DBCOE_FS0713.xlsx"))
    info_FS0713  <- read_xlsx(file.path(DB, "Info_DBCOE_SIE0713.xlsx"))
    info_PAC0713 <- read_xlsx(file.path(DB, "Info_DBCOE_PAC0713.xlsx"))
    info_FSC     <- read_xlsx(file.path(DB, "Info_DBCOE_FSC1420.xlsx"))
    info_SIE     <- read_xlsx(file.path(DB, "Info_DBCOE_SIE1420.xlsx"))
    info_POC     <- read_xlsx(file.path(DB, "Info_DBCOE_POC1420.xlsx"))
    info_FEASR   <- read_xlsx(file.path(DB, "Info_DBCOE_FEASR1420.xlsx"))
    info_FEAMP   <- read_xlsx(file.path(DB, "Info_DBCOE_FEAMP1420.xlsx"))
    info_CTE     <- read_xlsx(file.path(DB, "Info_DBCOE_CTE1420.xlsx"))
    info_SNAI     <- read_xlsx(file.path(DB, "Info_DBCOE_SNAI1420.xlsx"))
    # NEW 2127
    info_FSC_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_FSC2127.xlsx"))
    info_SIE_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_SIE2127.xlsx"))
    info_SNAI_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_SNAI2127.xlsx"))
    info_CTE_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_CTE2127.xlsx"))
    
    info <- info_FSC0713 %>%
      bind_rows(info_FS0713) %>%
      bind_rows(info_PAC0713 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE))) %>%
      bind_rows(info_FSC) %>%
      # mutate(VERSIONE = as.character(VERSIONE)) %>% #fix per NA
      bind_rows(info_SIE) %>%
      bind_rows(info_FEASR) %>% 
      bind_rows(info_CTE) %>%
      bind_rows(info_FEAMP) %>%
      bind_rows(info_POC) %>% 
      bind_rows(info_SNAI %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE))) %>% 
      # NEW 2127
      bind_rows(info_FSC_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) %>% 
      bind_rows(info_SIE_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) %>% 
      bind_rows(info_SNAI_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) %>% 
      bind_rows(info_CTE_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) 
      
    
  } else {
    info_FSC   <- read_xlsx(file.path(DB, "Info_DBCOE_FSC1420.xlsx"))
    info_SIE   <- read_xlsx(file.path(DB, "Info_DBCOE_SIE1420.xlsx"))
    info_POC   <- read_xlsx(file.path(DB, "Info_DBCOE_POC1420.xlsx"))
    info_FEASR <- read_xlsx(file.path(DB, "Info_DBCOE_FEASR1420.xlsx"))
    info_FEAMP <- read_xlsx(file.path(DB, "Info_DBCOE_FEAMP1420.xlsx"))
    info_CTE   <- read_xlsx(file.path(DB, "Info_DBCOE_CTE1420.xlsx"))
    info_SNAI     <- read_xlsx(file.path(DB, "Info_DBCOE_SNAI1420.xlsx"))
    # NEW 2127
    info_FSC_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_FSC2127.xlsx"))
    info_SIE_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_SIE2127.xlsx"))
    info_SNAI_2127 <- read_xlsx(file.path(DB, "Info_DBCOE_SNAI2127.xlsx"))
    
    info <- info_FSC       %>%
      bind_rows(info_SIE)   %>%
      bind_rows(info_FEASR) %>% 
      bind_rows(info_CTE)   %>%
      bind_rows(info_FEAMP) %>%
      bind_rows(info_POC)%>% 
      # bind_rows(info_SNAI)%>%
      bind_rows(info_SNAI %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE))) %>% 
      # NEW 2127
      bind_rows(info_FSC_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) %>% 
      bind_rows(info_SIE_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE))) %>% 
      bind_rows(info_SNAI_2127 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE),
                         DATA_DECISIONE = as.Date(DATA_DECISIONE)))  
    
  }
  
  
  # sovrascrive psc
  # if (fix_psc == TRUE){
  if (use_po_psc == TRUE) {
    # appo <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx"))
    # appo <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", "fsc_delibere_psc.xlsx"))
    appo <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", "fsc_delibere_psc.xlsx")) %>% 
      mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE))
    
    temp <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
    
    psc <- appo %>% 
      distinct(OC_CODICE_PROGRAMMA) %>% 
      rename(ID_PSC = OC_CODICE_PROGRAMMA)
    
    temp1 <- temp %>% 
      distinct(OC_CODICE_PROGRAMMA, ID_PSC, PSC)
    
    info_2 <- info %>% 
      left_join(temp1, by = "OC_CODICE_PROGRAMMA") %>% 
      anti_join(psc) %>% 
      bind_rows(appo)
    
    info <- info_2
    
    # TODO: verificare qui ci sono dei duplicati
    
  }
  
  # add LINK_SITO
  # link_sito <- read_csv2(file.path(DB, "programmi_link_sito.csv"))
  link_sito <- read_xlsx(file.path(DB, "link_sito_programmi.xlsx"))
  info <- info %>%
    # select(-LINK_SITO) %>% # elimina versione residua nel db
    left_join(link_sito %>% 
                select(OC_CODICE_PROGRAMMA, LINK_SITO), 
              by = "OC_CODICE_PROGRAMMA") # aggiunge versione da file dedicato
  
  
  if (use_en == TRUE) {
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
      # mutate(LABEL_DECISIONE_IT = paste0(TIPO_DECISIONE,
      #                                    " n. ",
      #                                    NUMERO_DECISIONE,
      #                                    " del ",
      #                                    format(DATA_DECISIONE, "%d/%m/%Y")),
      #        LABEL_DECISIONE_EN = paste0(TIPO_DECISIONE_EN,
      #                                    " n. ",
      #                                    NUMERO_DECISIONE,
      #                                    " - ",
      #                                    format(DATA_DECISIONE, "%m/%d/%Y"))) %>% 
      mutate(LABEL_DECISIONE_IT = ifelse(is.na(NUMERO_DECISIONE),
                                         "",
                                         paste0(TIPO_DECISIONE, " n. ", NUMERO_DECISIONE, " del ",format(DATA_DECISIONE, "%d/%m/%Y"))),
             LABEL_DECISIONE_EN = ifelse(is.na(NUMERO_DECISIONE),
                                         "",
                                         paste0(TIPO_DECISIONE_EN, " n. ", NUMERO_DECISIONE, " of ",format(DATA_DECISIONE, "%d/%m/%Y"))))
    
    # Collapse di stringhe multiple 
    if (sum_po == TRUE) {
      # Usa solo ultima decisione
      if (sum_po_last == TRUE) {
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
          # left_join(info %>%
          #             filter(FLAG_ULTIMA_DECISIONE == "X") %>%
          #             distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
          #             unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
          #             unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
          #             group_by(OC_CODICE_PROGRAMMA) %>%
          #             summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
          #                       LINK_DOC = paste(LINK_DOC, collapse = ":::")),
          #           by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>%                      distinct(OC_CODICE_PROGRAMMA, LINK_DOC = LINK_DOCUMENTO) %>%
                      # unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LINK_DOC = paste(LINK_DOC, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
                 LABEL_DECISIONE_EN = gsub(":::NA", "", LABEL_DECISIONE_EN),
                 LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
                 # LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
                 LINK_DOC = gsub(":::NA", "", LINK_DOC))
        
      } else {
        info_last <- info %>%
          distinct(OC_CODICE_PROGRAMMA) %>%
          left_join(info %>%
                      distinct(OC_CODICE_PROGRAMMA, LINK_SITO) %>%
                      filter(!is.na(LINK_SITO)),
                    by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      arrange(SEQ_DECISIONE) %>%
                      distinct(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_IT, LABEL_DECISIONE_EN, LINK_DECISIONE) %>%
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LABEL_DECISIONE_IT = paste(LABEL_DECISIONE_IT, collapse = ":::"),
                                LABEL_DECISIONE_EN = paste(LABEL_DECISIONE_EN, collapse = ":::"),
                                LINK_DECISIONE = paste(LINK_DECISIONE, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          # left_join(info %>%
          #             filter(FLAG_ULTIMA_DECISIONE == "X") %>%
          #             distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
          #             unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
          #             unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
          #             group_by(OC_CODICE_PROGRAMMA) %>%
          #             summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
          #                       LINK_DOC = paste(LINK_DOC, collapse = ":::")),
          #           by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X") %>%
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
      }

    
    # Collapse di stringhe multiple 
    # if (sum_po == TRUE) {
      # info_last <- info %>%
      #   distinct(OC_CODICE_PROGRAMMA) %>%
      #   left_join(info %>%
      #               distinct(OC_CODICE_PROGRAMMA, LINK_SITO) %>%
      #               filter(!is.na(LINK_SITO)),
      #             by = "OC_CODICE_PROGRAMMA") %>%
      #   left_join(info %>%
      #               arrange(SEQ_DECISIONE) %>%
      #               distinct(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_IT, LABEL_DECISIONE_EN, LINK_DECISIONE) %>%
      #               group_by(OC_CODICE_PROGRAMMA) %>%
      #               summarise(LABEL_DECISIONE_IT = paste(LABEL_DECISIONE_IT, collapse = ":::"),
      #                         LABEL_DECISIONE_EN = paste(LABEL_DECISIONE_EN, collapse = ":::"),
      #                         LINK_DECISIONE = paste(LINK_DECISIONE, collapse = ":::")),
      #             by = "OC_CODICE_PROGRAMMA") %>%
      #   # left_join(info %>%
      #   #             filter(FLAG_ULTIMA_DECISIONE == "X") %>%
      #   #             distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
      #   #             unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
      #   #             unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
      #   #             group_by(OC_CODICE_PROGRAMMA) %>%
      #   #             summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
      #   #                       LINK_DOC = paste(LINK_DOC, collapse = ":::")),
      #   #           by = "OC_CODICE_PROGRAMMA") %>%
      #   left_join(info %>%
      #               filter(FLAG_ULTIMA_DECISIONE == "X") %>%
      #               distinct(OC_CODICE_PROGRAMMA, LINK_DOC = LINK_DOCUMENTO) %>%
      #               # unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
      #               group_by(OC_CODICE_PROGRAMMA) %>%
      #               summarise(LINK_DOC = paste(LINK_DOC, collapse = ":::")),
      #             by = "OC_CODICE_PROGRAMMA") %>%
      #   mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
      #          LABEL_DECISIONE_EN = gsub(":::NA", "", LABEL_DECISIONE_EN),
      #          LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
      #          # LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
      #          LINK_DOC = gsub(":::NA", "", LINK_DOC))
    } else {
      info_last <- info
    }
    # DEV: spostare in altro file l'elenco dei siti
    # MEMO: ordina decisioni
    # clean NA
    
    
    
  } else {
    # non inglese
    
    info <- info %>%
      # mutate(LABEL_DECISIONE_IT = paste0(TIPO_DECISIONE,
      #                                    " n. ",
      #                                    NUMERO_DECISIONE,
      #                                    " del ",
      #                                    format(DATA_DECISIONE, "%d/%m/%Y")))
      mutate(TIPO_DECISIONE = case_when(TIPO_DECISIONE == "Delibera CIPE" ~ "Delibera",
                                        TIPO_DECISIONE == "Delibera" ~ "Delibera",
                                        TRUE ~ TIPO_DECISIONE)) %>% 
      mutate(LABEL_DECISIONE_IT = ifelse(is.na(NUMERO_DECISIONE),
                                         "",
                                         paste0(TIPO_DECISIONE, " n. ", NUMERO_DECISIONE, " del ",format(DATA_DECISIONE, "%d/%m/%Y"))))
    
    # Collapse di stringhe multiple 
    if (sum_po == TRUE) {
      # Usa solo ultima decisione
      if (sum_po_last == TRUE) {
        info_last <- info %>%
          distinct(OC_CODICE_PROGRAMMA) %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>% 
                      distinct(OC_CODICE_PROGRAMMA, LINK_SITO) %>%
                      filter(!is.na(LINK_SITO)),
                    by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X" | FLAG_ULTIMA_DECISIONE == "x") %>% 
                      arrange(SEQ_DECISIONE) %>%
                      distinct(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_IT, LINK_DECISIONE) %>%
                      # MEMO: lascio gruop_by per non sbagliare...
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LABEL_DECISIONE_IT = paste(LABEL_DECISIONE_IT, collapse = ":::"),
                                LINK_DECISIONE = paste(LINK_DECISIONE, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          # left_join(info %>%
          #             filter(FLAG_ULTIMA_DECISIONE == "X") %>%
          #             distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
          #             unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
          #             unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
          #             group_by(OC_CODICE_PROGRAMMA) %>%
          #             summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
          #                       LINK_DOC = paste(LINK_DOC, collapse = ":::")),
          #           by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X") %>%
                      distinct(OC_CODICE_PROGRAMMA, LINK_DOC = LINK_DOCUMENTO) %>%
                      # unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LINK_DOC = paste(LINK_DOC, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
                 LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
                 # LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
                 LINK_DOC = gsub(":::NA", "", LINK_DOC))
        
      } else {
        info_last <- info %>%
          distinct(OC_CODICE_PROGRAMMA) %>%
          left_join(info %>%
                      distinct(OC_CODICE_PROGRAMMA, LINK_SITO) %>%
                      filter(!is.na(LINK_SITO)),
                    by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      arrange(SEQ_DECISIONE) %>%
                      distinct(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_IT, LINK_DECISIONE) %>%
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LABEL_DECISIONE_IT = paste(LABEL_DECISIONE_IT, collapse = ":::"),
                                LINK_DECISIONE = paste(LINK_DECISIONE, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          # left_join(info %>%
          #             filter(FLAG_ULTIMA_DECISIONE == "X") %>%
          #             distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
          #             unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
          #             unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
          #             group_by(OC_CODICE_PROGRAMMA) %>%
          #             summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
          #                       LINK_DOC = paste(LINK_DOC, collapse = ":::")),
          #           by = "OC_CODICE_PROGRAMMA") %>%
          left_join(info %>%
                      filter(FLAG_ULTIMA_DECISIONE == "X") %>%
                      distinct(OC_CODICE_PROGRAMMA, LINK_DOC = LINK_DOCUMENTO) %>%
                      # unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                      group_by(OC_CODICE_PROGRAMMA) %>%
                      summarise(LINK_DOC = paste(LINK_DOC, collapse = ":::")),
                    by = "OC_CODICE_PROGRAMMA") %>%
          mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
                 LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
                 # LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
                 LINK_DOC = gsub(":::NA", "", LINK_DOC))
      }
      
    } else {
      info_last <- info
    }
  }
  
  # NEW: integra fix per pagina programmi
  if (use_fix_siepoc == TRUE) {
    if (file.exists(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx"))) {
      ant_siepoc <- load_correzioni_siepoc1420(DB)
      
      info_last <- info_last %>% 
        left_join(ant_siepoc %>% 
                    # MEMO: uso distinct per casi come calabria dove ho due righe FESR e FSE 
                    # e voglio applicare l'etichetta a entrambe (anche se magari la riduzione riguarda solo FESR) 
                    # perché uso logica di widget programmi con duplicazione
                    distinct(OC_CODICE_PROGRAMMA, FONTE_PUBB, FONTE_PUBB_EN), 
                  by = "OC_CODICE_PROGRAMMA") %>% 
        mutate(LABEL_DECISIONE_IT = case_when(!is.na(FONTE_PUBB) ~ paste0(LABEL_DECISIONE_IT, " come modificato da ", FONTE_PUBB),
                                              is.na(FONTE_PUBB) ~ LABEL_DECISIONE_IT,
                                              TRUE ~ "CHK"),
               LABEL_DECISIONE_EN = case_when(!is.na(FONTE_PUBB_EN) ~ paste0(LABEL_DECISIONE_EN, " as modified by ", FONTE_PUBB_EN),
                                              is.na(FONTE_PUBB_EN) ~ LABEL_DECISIONE_EN,
                                              TRUE ~ "CHK")) %>% 
        select(-FONTE_PUBB, -FONTE_PUBB_EN)
      
    } else {
      message("errore, le anticipazioni non sono implementate")
    }
  }
  
  # fix NA su LINK_DECISIONE
  info_last <- info_last %>% 
    mutate(LINK_DECISIONE = gsub("NA", "", LINK_DECISIONE))
  
  return(info_last)
  
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
  link_sito <- read_xlsx(file.path(DB, "link_sito_programmi.xlsx"))
  
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
#' @param ciclo Vuoi un ciclo specifico?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param use_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_eu Logico. vuoi vedere risorse UE ove previste?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC? [FUNZIONALITA' DEPRECATA]
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param force_yei Logico. Vuoi forzare FSE in YEI?
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e macroarea.
make_report_risorse_old <- function(ciclo=NULL, use_meuro=FALSE, use_flt=FALSE, use_eu=FALSE, use_po_psc=FALSE, use_cicli_psc=FALSE,
                                use_fix_siepoc=TRUE, stime_fix_siepoc=FALSE, force_yei=FALSE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE) {
  
  # DEBUG:
  # use_po_psc=FALSE
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=FALSE
  # use_flt=TRUE
  # use_eu=TRUE
  # tipo_ciclo="CICLO_STRATEGIA"
  
  programmi <- init_programmazione_dati(use_cicli_psc=use_cicli_psc,
                                        use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) 
  
  # NEW: split REACT-EU per allineamento a struttura tavole
  programmi <- programmi %>% 
    mutate(x_AMBITO=case_when(x_AMBITO=="FESR" & CAT_REGIONE=="REACT"~ "FESR_REACT",
                               x_AMBITO=="FSE" & CAT_REGIONE=="REACT"~"FSE_REACT",
                               TRUE~x_AMBITO))
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2 | FLAG_MONITORAGGIO == 3)
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2 | FLAG_MONITORAGGIO == 3 | FLAG_MONITORAGGIO == 4)
    # MEMO: FLAG_MONITORAGGIO == 4 contiene valori negativi per scontare da quadro risorse le risorse ex art. 242 che sarebbero altrimenti duplicate tra POC e FS
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }
  
  if (force_yei == TRUE) {
    programmi <- programmi %>%
      mutate(x_AMBITO = if_else(OC_CODICE_PROGRAMMA == "2014IT05M9OP001", "YEI", as.character(x_AMBITO))) %>%
      refactor_ambito(.)
  }
  
  # programmi %>% count(x_MACROAREA)
  
  # programmi <- programmi %>% 
  #   mutate(x_MACROAREA = case_when(x_MACROAREA == "CN" ~ "Centro-Nord",
  #                                  x_MACROAREA == "SUD" ~ "Mezzogiorno",
  #                                  x_MACROAREA == "MZ" ~ "Mezzogiorno",
  #                                  x_MACROAREA == "ND" ~ "Ambito nazionale",
  #                                  x_MACROAREA == "NC" ~ "Ambito nazionale",
  #                                  x_MACROAREA == "VOID" ~ "Ambito nazionale",
  #                                  TRUE ~ x_MACROAREA))
  
  programmi <- ricodifica_macroaree(programmi)
  
  if (!is.null(ciclo)) {
    programmi <- programmi %>%
      filter(x_CICLO == ciclo)
  }
  
  # if (tipo_ciclo == "CICLO_RISORSE") {
  #   programmi <- programmi %>%
  #     mutate(x_CICLO = CICLO_RISORSE)
  # } else if (tipo_ciclo == "CICLO_STRATEGIA") {
  #   programmi <- programmi
  #   # MEMO: x_CICLO di default è gia CICLO_STRATEGIA
  # }
  
  if (use_eu == TRUE) {
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
  } else {
    out <- programmi %>%
      group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
      summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE)) %>%
      refactor_macroarea(.)
    
    if (use_meuro == TRUE) {
      out <- out %>%
        mutate(RISORSE = round(RISORSE / 1000000, 1))
    }
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
      if (!is.null(ciclo)) {
        fname <- paste0("risorse_coesione_", ciclo, ".csv")
        } else {
          if (use_meuro==TRUE) {
            fname <- "risorse_coesione_meuro.csv"
            } else {
              fname <- "risorse_coesione.csv"
            }
          }
    
    write.csv2(out_2, file.path(TEMP, fname), row.names = FALSE)
  }
  
  return(out_2)
  
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
  programmi_en <- read_xlsx(file.path(DB, "label_programmi_en.xlsx")) %>% 
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
    mutate(LABEL_AMBITO_IT = factor(LABEL_AMBITO_IT, levels = c("FESR", "FSE+", "JTF", "CTE",
                                                                "FSC", "ALTRO"))) %>% 
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
      LABEL_AMBITO = c("SIE", "FSC", "SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2021-2027", "2021-2027", "2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
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
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "JTF", "SNAI-SERVIZI", "CTE", "PAC"))) %>% 
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
  
  appo1 <- programmi %>%
    mutate(LABEL_PROGRAMMA = x_PROGRAMMA,
           LABEL_AMBITO = x_AMBITO) %>% 
    left_join(info %>% 
                select(-x_AMBITO, -x_CICLO), 
              by = "OC_CODICE_PROGRAMMA")
  # MEMO: uso convenzioni da "workflow" in "programmi" e aggiungo dati per singole delibere (che sono N:1 su programmi)
  
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
      LABEL_AMBITO = c("SIE", "FSC", "SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2021-2027", "2021-2027", "2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    )

    # TODO: aggrego FESR e FSE in SIE solo per 1420, forse va fatto anche per 713 oppure nemmeno per 1420?

    # clean
    out_2 <- out %>%
      # mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-Servizi", "CTE", "PAC")))
      # NEW 2127
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "JTF", "SNAI-Servizi", "CTE", "PAC")))


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
      risorse_old <- read_xlsx(path_to_old)
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
  
  interventi_accordi <- load_db_accordi()
  
  out <- dati_accordi %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA) %>%
    summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm=TRUE),
              FINANZ_FDR = sum(FINANZ_FDR, na.rm=TRUE)) %>% 
    mutate(FINANZ_FSC = if_else(is.na(FINANZ_FSC), 0, FINANZ_FSC),
           FINANZ_FDR = if_else(is.na(FINANZ_FDR), 0, FINANZ_FDR)) %>%
    mutate(RISORSE = FINANZ_FSC + FINANZ_FDR) %>% 
    full_join(interventi_accordi %>%
                # filter(OC_FLAG_MONITORAGGIO == 1) %>% 
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(FINANZ_FSC = sum(FINANZ_FSC, na.rm=TRUE),
                          FINANZ_FDR = sum(FINANZ_FDR, na.rm=TRUE)) %>% 
                mutate(FINANZ_FSC = if_else(is.na(FINANZ_FSC), 0, FINANZ_FSC),
                       FINANZ_FDR = if_else(is.na(FINANZ_FDR), 0, FINANZ_FDR)) %>%
                mutate(RISORSE = FINANZ_FSC + FINANZ_FDR),
              by = c("OC_CODICE_PROGRAMMA"),
              suffix = c(".dati", ".int")) %>%
    mutate(RISORSE.dati = if_else(is.na(RISORSE.dati), 0, round(RISORSE.dati, 2)),
           RISORSE.int = if_else(is.na(RISORSE.int), 0, round(RISORSE.int, 2))) %>%
    mutate(CHK = RISORSE.int - RISORSE.dati) %>% 
    filter(abs(CHK) > 0)
  
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
