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
load_db <- function(ciclo, ambito, simplify_loc=FALSE, use_temi=FALSE, use_sog=FALSE, use_ue=FALSE, use_flt=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=FALSE){
  
  # DEV: decidere se fare importazione di tutto e poi selezionare variabili a valle....
  
  # DEBUG:
  # ciclo <- "2014-2020"
  # ambito <- "FSC" | "FESR" | "FSE" | "POC" / "PAC" per "0713"
  
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
    filename <- paste0("DBPROG_", temp, "1420.xlsx") # DBPROG_FSC1420.xlsx
    
  } else {
    temp <- case_when(ambito == "FESR" ~ "SIE", #AF
                      ambito == "FSE" ~ "SIE", #AF
                      ambito == "PAC" ~ "PAC", #AF
                      ambito == "FSC" ~ "FSC", #AF
                      TRUE ~ ambito)
    #filename <- paste0(temp, "_0713.xlsx") # MODIFICA AF
    filename <- paste0("DBPROG_", temp, "0713.xlsx") # DBPROG_FSC0713.xlsx
  }
  
  # importa file excel
  # OLD: appo <-  read_excel(file.path(DATA, "db", filename), guess_max = 5000) # MEMO: versione prima di GoogleDrive
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)
  # VERSIONE  CON UNICO FILE - DA SISTEMARE: appo1 <-  read_excel(file.path(DB, "prova2.xlsx"), sheet = filename, guess_max = 5000)
  
  # filtra ambiti da SIE
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI") {
    appo <- appo %>%
      filter(OC_DESCR_FONTE == ambito)
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
  if (ambito == "FSC") {
    appo <- appo %>%
      mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC)
  }
  
  # switch varibili da tenere
  var_ls <- c("OC_CODICE_PROGRAMMA", "OC_DESCRIZIONE_PROGRAMMA", "OC_TIPOLOGIA_PROGRAMMA",
              "OC_DESCR_FONTE",
              "FINANZ_TOTALE_PUBBLICO") #AF OK
  
  # SPOSTARE "OC_MACROAREA", "DEN_REGIONE" IN IF CON "USE_LOCATION" 
  
  if (use_temi == TRUE) {
    if(use_articolaz == TRUE) {
      var_ls <- c(var_ls,
                  "DESCR_SETTORE_STRATEGICO_FSC", "DESCR_ASSE_TEMATICO_FSC",
                  "COD_RA", "DESCR_RA",
                  "OC_COD_ARTICOLAZ_PROGRAMMA")
    } else {
      var_ls <- c(var_ls,
                  "DESCR_SETTORE_STRATEGICO_FSC", "DESCR_ASSE_TEMATICO_FSC",
                  "COD_RA", "DESCR_RA")
    }
  }
  
  
  if (use_sog == TRUE) {
    var_ls <- c(var_ls,
                "AMMINISTRAZIONE_TITOLARE")
  }
  
  if (use_ue == TRUE) {
    var_ls <- c(var_ls,
                "FINANZ_UE", "OC_AREA_OBIETTIVO_UE")#AF OK
    
  }
  
  if (use_flt == TRUE) {
    var_ls <- c(var_ls,
                "OC_FLAG_MONITORAGGIO")#AF OK
    
    # patch per dati da consolidare nel DB
    appo <- appo %>%
      mutate(OC_FLAG_MONITORAGGIO = as.numeric(OC_FLAG_MONITORAGGIO)) %>%
      mutate(OC_FLAG_MONITORAGGIO = case_when(OC_FLAG_MONITORAGGIO == 1 ~ 1,
                                              OC_FLAG_MONITORAGGIO == 0 ~ 0,
                                              OC_FLAG_MONITORAGGIO == 2 ~ 2, # presente per FSC e POC
                                              OC_FLAG_MONITORAGGIO == 9 ~ 9, # presente per FSC
                                              # is.na(OC_FLAG_MONITORAGGIO) ~ 1, # questa è poco logica ma dipende dai dati
                                              # OC_FLAG_MONITORAGGIO == "" ~ 1, # questa dovrebbe corrispondere alla condizione sotto
                                              # is.character(OC_FLAG_MONITORAGGIO) & 
                                              #   nchar(OC_FLAG_MONITORAGGIO) == 1 ~ 1,
                                              # is.character(OC_FLAG_MONITORAGGIO) & 
                                              #   nchar(OC_FLAG_MONITORAGGIO) > 1 ~ 0,
                                              TRUE ~ 0))
    
  }
  
  if (use_location == TRUE) {
    var_ls <- c(var_ls,
                "OC_MACROAREA", "DEN_REGIONE")
  }
  
  if (use_ciclo == TRUE) {
    var_ls <- c(var_ls, 
                "CICLO_PROGRAMMAZIONE", "CICLO_RISORSE")
  }
  
  # aggiungo ciclo e ambito
  appo <- appo %>%
    mutate(x_CICLO = CICLO_PROGRAMMAZIONE, # x_CICLO = ciclo, 
           x_AMBITO = ambito) 
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

#' Inizializza il database della programmazione
#'
#' Carica il databse della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param use_temi Vuoi caricare il DB con correzione dei temi prioritari?
#' @param use_sog Vuoi caricare il DB con il soggetto programmatore?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @return L'intero database dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
init_programmazione <- function(use_temi=FALSE, use_sog=FALSE, use_eu=FALSE, use_flt=FALSE, use_713=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=FALSE, tipo_ciclo="CICLO_STRATEGIA", use_en=FALSE)
{
  # use_temi = FALSE
  # use_sog=TRUE
  # use_eu=TRUE
  # use_713 = TRUE
  # use_ciclo = TRUE
  # use_flt = TRUE
  # use_location = FALSE
  po_fsc <- load_db("2014-2020", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo) #AF aggiunto use_locatione che prima mancava
  po_fesr <- load_db("2014-2020", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_fse <- load_db("2014-2020", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_poc <- load_db("2014-2020", "POC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_yei <- load_db("2014-2020", "YEI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo)
  po_feamp <- load_db("2014-2020", "FEAMP", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_snai <- load_db("2014-2020", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_cte <- load_db("2014-2020", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
  po_feasr <- load_db("2014-2020", "FEASR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo) #AF mancava, aggiunto
  
  programmi <- po_fsc %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "FSC") %>%
    bind_rows(po_poc) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "POC")) %>%
    bind_rows(po_fesr) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "FESR")) %>%
    bind_rows(po_fse) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "FSE")) %>%
    bind_rows(po_yei) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "YEI")) %>%
    bind_rows(po_feamp) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "FEAMP")) %>%
    bind_rows(po_snai) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "SNAI")) %>%
    bind_rows(po_cte) %>%
    # mutate(x_CICLO = "2014-2020",
    #        x_AMBITO = "CTE")) %>%
    bind_rows(po_feasr) %>%   #AF mancava, ho aggiunto
    
    as.data.frame(.)
  
  if (use_713 == TRUE) {
    po_fsc713 <- load_db("2007-2013", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
    po_fesr713 <- load_db("2007-2013", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
    po_fse713 <- load_db("2007-2013", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
    po_pac713 <- load_db("2007-2013", "PAC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo)
    
    programmi <- programmi %>%
      bind_rows(po_fsc713) %>%
      # mutate(x_CICLO = "2007-2013",
      #        x_AMBITO = "FSC")) %>%
      bind_rows(po_pac713) %>%
      # mutate(x_CICLO = "2007-2013",
      #        x_AMBITO = "PAC")) %>%
      bind_rows(po_fesr713) %>%
      # mutate(x_CICLO = "2007-2013",
      #        x_AMBITO = "FESR")) %>%
      bind_rows(po_fse713) # %>%
    #             mutate(x_CICLO = "2007-2013",
    #                    x_AMBITO = "FSE"))
    
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
  
  if (use_en == TRUE) {
    programmi_en <- read_csv2(file.path(DB, "programmi_SIE_EN.csv")) %>%
      select(-LABEL_PROGRAMMA_IT) 
    
    # label LABEL_PROGRAMMA_EN
    left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
      mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
             LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN))
  }

  if (use_ciclo == TRUE) {
    if (tipo_ciclo == "CICLO_RISORSE") {
      programmi <- programmi %>%
        mutate(x_CICLO = CICLO_RISORSE)
    } else if (tipo_ciclo == "CICLO_STRATEGIA") {
      programmi <- programmi
      # MEMO: x_CICLO di default è gia CICLO_STRATEGIA
    }
  }
  
  
  return(programmi)
  
}




#' Inizializza informazioni aggiuntive del database della programmazione
#'
#' Inizializza informazioni aggiuntive del database della programmazione, con possibilità di collassare i dati per programma o tenere in evidenza singole decisioni e versioni dei programmi.
#'
#' @param usa_en Vuoi caricare le traduzioni in inglese?
#' @param usa_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param sum_po Vuoi dati aggregati come summary per programma, con decisioni collassati?
#' @return Tutti i dati di supporto.
init_info <- function(use_en = FALSE, use_713 = FALSE, sum_po = FALSE) {
  
  if (use_713 == TRUE) {
    info_FSC0713 <- read_xlsx(file.path(DB, "info_FSC_0713.xlsx"))
    info_FS0713  <- read_xlsx(file.path(DB, "info_FS_0713.xlsx"))
    info_PAC0713 <- read_xlsx(file.path(DB, "info_PAC_0713.xlsx"))
    info_FSC     <- read_xlsx(file.path(DB, "info_FSC_1420.xlsx"))
    info_SIE     <- read_xlsx(file.path(DB, "info_SIE_1420.xlsx"))
    info_POC     <- read_xlsx(file.path(DB, "info_POC_1420.xlsx"))
    info_FEASR   <- read_xlsx(file.path(DB, "info_FEASR_1420.xlsx"))
    info_FEAMP   <- read_xlsx(file.path(DB, "info_FEAMP_1420.xlsx"))
    info_CTE     <- read_xlsx(file.path(DB, "info_CTE_1420.xlsx"))
    
    info<-info_FSC0713              %>%
      bind_rows(info_FS0713)    %>%
      bind_rows(info_PAC0713)   %>%
      bind_rows (info_FSC)      %>%
      bind_rows(info_SIE)       %>%
      bind_rows(info_FEASR)     %>% 
      bind_rows(info_CTE)       %>%
      bind_rows(info_FEAMP)     %>%
      bind_rows(info_POC)  
    
  } else {
    info_FSC   <- read_xlsx(file.path(DB, "info_FSC_1420.xlsx"))
    info_SIE   <- read_xlsx(file.path(DB, "info_SIE_1420.xlsx"))
    info_POC   <- read_xlsx(file.path(DB, "info_POC_1420.xlsx"))
    info_FEASR <- read_xlsx(file.path(DB, "info_FEASR_1420.xlsx"))
    info_FEAMP <- read_xlsx(file.path(DB, "info_FEAMP_1420.xlsx"))
    info_CTE   <- read_xlsx(file.path(DB, "info_CTE_1420.xlsx"))
    
    info <- info_FSC       %>%
      bind_rows(info_SIE)   %>%
      bind_rows(info_FEASR) %>% 
      bind_rows(info_CTE)   %>%
      bind_rows(info_FEAMP) %>%
      bind_rows(info_POC)  
    
  }
  
  # add LINK_SITO
  link_sito <- read_csv2(file.path(DB, "programmi_link_sito.csv"))
  info <- info %>%
    select(-LINK_SITO) %>% # elimina versione residua nel db
    left_join(link_sito, 
              by = "OC_CODICE_PROGRAMMA") # aggiunge versione da file dedicato
  
  if (use_en == TRUE) {
    info <- info %>%
      mutate(TIPO_DECISIONE_EN = TIPO_DECISIONE) %>%
      mutate(TIPO_DECISIONE_EN = case_when(TIPO_DECISIONE == "Delibera CIPE" ~ "Resolution CIPE",
                                           TIPO_DECISIONE == "Decisione CE" ~ "Decision EC",
                                           TIPO_DECISIONE == "Legge" ~ "Law")) %>%
      mutate(LABEL_DECISIONE_IT = paste0(TIPO_DECISIONE,
                                         " n. ",
                                         NUMERO_DECISIONE,
                                         " del ",
                                         format(DATA_DECISIONE, "%d/%m/%Y")),
             LABEL_DECISIONE_EN = paste0(TIPO_DECISIONE_EN,
                                         " n. ",
                                         NUMERO_DECISIONE,
                                         " - ",
                                         format(DATA_DECISIONE, "%m/%d/%Y")))
    
    # Collapse di stringhe multiple 
    if (sum_po == TRUE) {
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
        left_join(info %>%
                    filter(FLAG_ULTIMA_DECISIONE == "X") %>%
                    distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
                    unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
                    unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                    group_by(OC_CODICE_PROGRAMMA) %>%
                    summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
                              LINK_DOC = paste(LINK_DOC, collapse = ":::")),
                  by = "OC_CODICE_PROGRAMMA") %>%
        mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
               LABEL_DECISIONE_EN = gsub(":::NA", "", LABEL_DECISIONE_EN),
               LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
               LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
               LINK_DOC = gsub(":::NA", "", LINK_DOC))
    } else {
      info_last <- info
    }
    # DEV: spostare in altro file l'elenco dei siti
    # MEMO: ordina decisioni
    # clean NA
    
    
    
  } else {
    info <- info %>%
      mutate(LABEL_DECISIONE_IT = paste0(TIPO_DECISIONE,
                                         " n. ",
                                         NUMERO_DECISIONE,
                                         " del ",
                                         format(DATA_DECISIONE, "%d/%m/%Y")))
    
    # Collapse di stringhe multiple 
    if (sum_po == TRUE) {
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
        left_join(info %>%
                    filter(FLAG_ULTIMA_DECISIONE == "X") %>%
                    distinct(OC_CODICE_PROGRAMMA, LABEL_DOC_1, LABEL_DOC_2, LINK_DOC_1, LINK_DOC_2) %>%
                    unite(LABEL_DOC, LABEL_DOC_1, LABEL_DOC_2, sep = ":::") %>%
                    unite(LINK_DOC, LINK_DOC_1, LINK_DOC_2, sep = ":::") %>%
                    group_by(OC_CODICE_PROGRAMMA) %>%
                    summarise(LABEL_DOC = paste(LABEL_DOC, collapse = ":::"),
                              LINK_DOC = paste(LINK_DOC, collapse = ":::")),
                  by = "OC_CODICE_PROGRAMMA") %>%
        mutate(LABEL_DECISIONE_IT = gsub(":::NA", "", LABEL_DECISIONE_IT),
               LINK_DECISIONE = gsub(":::NA", "", LINK_DECISIONE),
               LABEL_DOC = gsub(":::NA", "", LABEL_DOC),
               LINK_DOC = gsub(":::NA", "", LINK_DOC))
    } else {
      info_last <- info
    }
  }
  
  return(info_last)
  
}

#' Workflow per utilizzo dataset della programmazione.
#'
#' @param use_info Logico. Vuoi aggiungere le informazioni di supporto?
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param progetti Dataset progetti importato con load_progetti
#' @return Il dataset dei programmi con risorse e evewntualmente infomrmazioni di supporto
workflow_programmazione <- function(use_info=FALSE, use_flt=TRUE, progetti=NULL) {
  
  #load
  interventi <- init_programmazione(use_temi = FALSE, use_sog=TRUE, use_eu=TRUE, use_713 = TRUE, use_ciclo = TRUE, use_flt = TRUE) %>%
    rename(x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA,
           x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA)
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
  }
  
  info_last <- init_info(use_en = FALSE, use_713 = TRUE, sum_po = TRUE)
  
  
  # label da progetti pubblicati per allineamento a sito
  label_programmi <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
  
  # filtro pubblicati
  interventi <- interventi %>%
    mutate(PUB = if_else(OC_CODICE_PROGRAMMA %in% label_programmi$OC_CODICE_PROGRAMMA, TRUE, FALSE))
  
  # kill YEI (fonde tutto FSE del programma IOG in ambito YEI)
  interventi <- interventi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI",
                                TRUE ~ x_AMBITO)) %>%
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI", "CTE", "PAC")))
  
  # applica FLAG_MONITORAGGIO
  if (use_flt == TRUE) {
    interventi <- interventi %>%
      filter(OC_FLAG_MONITORAGGIO == 1)
  }
  
  # summary (opzione 1: il programma pluri-fondo mostra il valore specifico di ogni ambito)
  programmi <- interventi %>%
    # MEMO: con x_GRUPPO duplica programmi tipo Piombino e Dissesto perché viene da due cicli diversi lato risorse (ma hanno stesso x_CICLO lato strategia)
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, PUB) %>%
    left_join(interventi %>%
                group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))
  
  # summary (opzione 2: il programma pluri-fondo è duplicato nei due ambiti e il valore esposto è sempre il totale) 
  # programmi <- interventi %>%
  #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, PUB) %>%
  #   left_join(interventi %>%
  #               group_by(OC_CODICE_PROGRAMMA) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA")
  # MEMO: questa versione è coerente con impostazione pagine aggregate sito ma poco informativa...
  
  # ripristina x_GRUPPO (tolto sopra per evitare duplicazione in summarise sopra)
  programmi <- programmi %>%
    left_join(interventi %>%
                # patch per programmi su due ambiti o su due cicli nello stesso ambito (con gruppi diversi)
                mutate(x_GRUPPO =  case_when(OC_CODICE_PROGRAMMA == "2007IT001FA005" ~ "NAZ-INF",
                                             OC_CODICE_PROGRAMMA == "2007IT005FAMG1" ~ "PAC Nazionale",
                                             OC_CODICE_PROGRAMMA == "2007SA002FA016" ~ "REG",
                                             OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" ~ "Piani nazionali",
                                             OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" ~ "Altre assegnazioni CIPE",
                                             OC_CODICE_PROGRAMMA == "CIS_TA_PUG" ~ "Altre assegnazioni CIPE",
                                             OC_CODICE_PROGRAMMA == "TEMP_0713_020" ~ "Altre assegnazioni CIPE",
                                             TRUE ~ x_GRUPPO)) %>%
                distinct(OC_CODICE_PROGRAMMA, x_GRUPPO) %>%
                filter(!is.na(x_GRUPPO)),
              by = "OC_CODICE_PROGRAMMA")
  
  # rewrite x_PROGRAMMA
  programmi <- programmi %>%
    left_join(label_programmi) %>%
    mutate(x_PROGRAMMA = if_else(is.na(OC_DESCRIZIONE_PROGRAMMA), x_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)) %>%
    select(-OC_DESCRIZIONE_PROGRAMMA)
  
  #aggiunge informazioni di supporto
  if (use_info == TRUE) {
    programmi_base <- programmi
    programmi <- programmi %>%
      left_join(info_last,
                by = "OC_CODICE_PROGRAMMA")
    # CHK: duplicato, programmi passa da 405 a 406
    temp <- programmi %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
    chk <- programmi %>% semi_join(temp)
    print("Controlla se numerosità è invariata!!!")
    dim(programmi)[1] == dim(programmi_base)[1]
  }
  
  return(programmi)
  
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
#' @param force_yei Logico. Vuoi forzare FSE in YEI?
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e macroarea.
make_report_risorse <- function(ciclo=NULL, use_meuro=FALSE, use_flt=FALSE, use_eu=FALSE, force_yei=FALSE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE) {
  
  programmi <- init_programmazione(use_temi = FALSE, use_713 = TRUE, use_location = TRUE, use_ciclo = TRUE, use_eu=use_eu, use_flt=use_flt, tipo_ciclo=tipo_ciclo) 
  # %>%
  #   rename(x_MACROAREA = OC_MACROAREA)
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(OC_FLAG_MONITORAGGIO == 1 | OC_FLAG_MONITORAGGIO == 2)
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
    programmi %>%
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
      summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE),
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
      summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
      refactor_macroarea(.)
    
    if (use_meuro == TRUE) {
      out <- out %>%
        mutate(RISORSE = round(RISORSE / 1000000, 1))
    }
  }
  
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "risorse_coesione.csv"), row.names = FALSE)
  }
  
  return(out)
  
}



#' Verifica variazione risorse per programma
#'
#' Verifica variazione risorse per programma. Confronta due dataframe risultanti da make_report_programmi_coesione().
#'
#' @param programmi_new Dataframe da make_report_programmi_coesione()
#' @param programmi_old Dataframe da make_report_programmi_coesione()
#' @param path_to_new Percorso a csv generato con make_report_programmi_coesione().
#' @param path_to_old Percorso a csv generato con make_report_programmi_coesione().
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_risorse_programmi <- function(programmi_new=NULL, programmi_old=NULL, path_to_new=NULL, path_to_old=NULL, export=FALSE){
  
  if (is.null(programmi_new)) {
    if (is.null(path_to_new)) {
      message("Indica un file da confrontare")
    } else {
      programmi_new <- read_csv2(path_to_new)
    }
  }
  
  if (is.null(programmi_old)) {
    if (is.null(path_to_old)) {
      message("Indica un file da confrontare")
    } else {
      programmi_old <- read_csv2(path_to_old)
    }
  }
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE) %>%
    left_join(programmi_old %>%
                # fix per "PAC"
                as_tibble(.) %>%
                mutate(x_AMBITO = case_when(x_CICLO == "2007-2013" & x_AMBITO == "POC" ~ "PAC",
                                            TRUE ~ x_AMBITO)) %>%
                refactor_ambito(.) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, RISORSE),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK = RISORSE.new - RISORSE.old)

  if (export==TRUE) {
    write.csv2(out, file.path(TEMP, "delta_risorse_programme.csv"), row.names = FALSE)
  }
  
  return(out)
  
}



#' Verifica risorse per ciclo contabile e ciclo strategia
#'
#' Verifica risorse per ciclo contabile e ciclo strategia.
#'
#' @param programmi_new Dataframe da make_report_programmi_coesione()
#' @param programmi_old Dataframe da make_report_programmi_coesione()
#' @param path_to_new Percorso a csv generato con make_report_programmi_coesione().
#' @param path_to_old Percorso a csv generato con make_report_programmi_coesione().
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_risorse_ciclo_contabile_strategia <- function(use_flt=TRUE, force_yei=FALSE, use_eu=FALSE, export=TRUE) {
  
  risorse_startegia <- make_report_risorse(use_meuro=TRUE, use_flt=use_flt, force_yei=force_yei, use_eu=use_eu,
                                           tipo_ciclo="CICLO_STRATEGIA", export=TRUE)
  
  risorse_contabili <- make_report_risorse(use_meuro=TRUE, use_flt=use_flt, force_yei=force_yei, use_eu=use_eu,
                                           tipo_ciclo="CICLO_RISORSE", export=TRUE)

  out <- risorse_startegia %>%
    left_join(risorse_contabili,
              by = c("x_CICLO", "x_AMBITO", "x_MACROAREA"),
              suffix = c(".s", ".c")) %>%
    mutate_if(is.numeric, list(~replace_na(., 0))) %>%
    mutate(DELTA_RISORSE = RISORSE.s - RISORSE.c)
  
  if (use_eu == TRUE) {
    out <- out %>%
      mutate(DELTA_UE = RISORSE_UE.s - RISORSE_UE.c)
  }
  
  if (export==TRUE) {
    write.csv2(out, file.path(TEMP, "chk_ciclo_contabile_strategia.csv"), row.names = FALSE)
  }
  
  return(out)
}


#' Ricodifica la voce macroarea lato programmazione come x_MACROAREA
#'
#' Ricodifica la voce macroarea lato programmazione come x_MACROAREA
#'
#' @param programmi Dataframe da init_programmazione()
#' @return Un dataframe con x_MACROAREA
ricodifica_macroaree <- function(programmi) {
  
  out <- programmi %>% 
    rename(x_MACROAREA = OC_MACROAREA) %>%
    mutate(x_MACROAREA = case_when(x_MACROAREA == "CN" ~ "Centro-Nord",
                                   x_MACROAREA == "SUD" ~ "Mezzogiorno",
                                   x_MACROAREA == "MZ" ~ "Mezzogiorno",
                                   x_MACROAREA == "ND" ~ "Ambito nazionale",
                                   x_MACROAREA == "NC" ~ "Ambito nazionale",
                                   x_MACROAREA == "VOID" ~ "Ambito nazionale",
                                   TRUE ~ x_MACROAREA))
  return(out)
}


#' Carica un dataset di tipo "interventi" dal database della programmazione
#'
#' Carica un dataset di tipo "interventi" dal database della programmazione.
#'
#' @param tipo Tipologia di dataset interventi.
#' @param simplify_loc Logico. Vuoi semplificare le localizzazioni per compatibilit? con lo standard dei Report CD?
#' @param use_temi Logico. Vuoi avere anche i temi prioritari FSC?
#' @param use_sog Logico. Vuoi avere anche il soggetto programmatore?
#' @param use_eu Logico. Vuoi avere anche il finanziamento EU e categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_articolaz Logico. Oltre ai temi, vuoi importare anche le articolazioni? Utile per FESR e FSE
#' @param use_location Logico. Vuoi avere anche la localizzazione dei progetti per Regione e Macroarea?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @return Il dataset di programmazione per l'ambito richiesto, con pulizia delle denominazioni territoriali e della codifica di aree tematiche e temi prioritari FSC.
load_db_interventi <- function(tipo, simplify_loc=FALSE, use_temi=FALSE, use_sog=FALSE, use_ue=FALSE, 
                               use_flt=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=FALSE, tipo_ciclo="CICLO_RISORSE"){
  
  # DEV: decidere se fare importazione di tutto e poi selezionare variabili a valle....
  
 # crea nome file da importare
  if (tipo == "CIS") {

    filename <- paste0("DBCOE_interventi_CIS.xlsx") 
    
  } else {
    message("Non è ancora implementato")
    
  }
  
  # importa file excel
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)
  # VERSIONE  CON UNICO FILE - DA SISTEMARE: appo1 <-  read_excel(file.path(DB, "prova2.xlsx"), sheet = filename, guess_max = 5000)
  
  
  # switch varibili da tenere
  var_ls <- c("OC_CODICE_PROGRAMMA", "OC_TITOLO_PROGETTO",
              "OC_DESCR_FONTE",
              "FINANZ_TOTALE_PUBBLICO")
  
  if (use_temi == TRUE) {
    if(use_articolaz == TRUE) {
      var_ls <- c(var_ls,
                  "DESCR_SETTORE_STRATEGICO_FSC", "DESCR_ASSE_TEMATICO_FSC",
                  "COD_RA", "DESCR_RA",
                  "OC_COD_ARTICOLAZ_PROGRAMMA")
    } else {
      var_ls <- c(var_ls,
                  "DESCR_SETTORE_STRATEGICO_FSC", "DESCR_ASSE_TEMATICO_FSC",
                  "COD_RA", "DESCR_RA")
    }
  }
  
  
  if (use_sog == TRUE) {
    var_ls <- c(var_ls,
                "AMMINISTRAZIONE_BENEFICIARIA",
                "TIPOLOGIA_DI_AMMINISTRAZIONE_BENEFICIARIA")
  }
  
  if (use_ue == TRUE) {
    var_ls <- c(var_ls,
                "FINANZ_UE", "OC_AREA_OBIETTIVO_UE")
    
  }
  
  if (use_flt == TRUE) {
    var_ls <- c(var_ls,
                "OC_FLAG_MONITORAGGIO")
    
    # patch per dati da consolidare nel DB
    appo <- appo %>%
      mutate(OC_FLAG_MONITORAGGIO = as.numeric(OC_FLAG_MONITORAGGIO)) %>%
      mutate(OC_FLAG_MONITORAGGIO = case_when(OC_FLAG_MONITORAGGIO == 1 ~ 1,
                                              OC_FLAG_MONITORAGGIO == 0 ~ 0,
                                              OC_FLAG_MONITORAGGIO == 2 ~ 2, # presente per FSC e POC
                                              OC_FLAG_MONITORAGGIO == 9 ~ 9, # presente per FSC
                                              # is.na(OC_FLAG_MONITORAGGIO) ~ 1, # questa è poco logica ma dipende dai dati
                                              # OC_FLAG_MONITORAGGIO == "" ~ 1, # questa dovrebbe corrispondere alla condizione sotto
                                              # is.character(OC_FLAG_MONITORAGGIO) & 
                                              #   nchar(OC_FLAG_MONITORAGGIO) == 1 ~ 1,
                                              # is.character(OC_FLAG_MONITORAGGIO) & 
                                              #   nchar(OC_FLAG_MONITORAGGIO) > 1 ~ 0,
                                              TRUE ~ 0))
    
  }
  
  if (use_location == TRUE) {
    var_ls <- c(var_ls,
                "OC_MACROAREA", "DEN_REGIONE")
  }
  
  if (use_ciclo == TRUE) {
    var_ls <- c(var_ls, 
                "CICLO_PROGRAMMAZIONE", "CICLO_RISORSE")

  }
  
  # variabili custom per tipo
  if (tipo == "CIS") {
    var_ls <- c(var_ls, 
                "CIS", "COD_CIS", "CIS_NOME")
  }
  
  
  # aggiungo ciclo e ambito
  if (use_ciclo == TRUE) {
    if (tipo_ciclo == "CICLO_STRATEGIA") {
      appo <- appo %>%
        mutate(x_CICLO = CICLO_PROGRAMMAZIONE)
    } else if (tipo_ciclo == "CICLO_RISORSE") {
      appo <- appo %>%
        mutate(x_CICLO = CICLO_RISORSE)
    }
  }
  appo <- appo %>%
    mutate(x_AMBITO = OC_DESCR_FONTE) 
  appo <- refactor_ambito(appo)
  appo <- refactor_ciclo(appo)
  
  var_ls <- c(var_ls, 
              "x_CICLO", "x_AMBITO")
  
  
  # aggiungo clp
  var_ls <- c(var_ls, 
              "COD_LOCALE_PROGETTO")
  
  # select varibili di interesse
  appo <- appo %>%
    select(all_of(var_ls))
  
  
  # integra x_PROGRAMMA e x_GRUPPO da programmi
  po_riclass <- init_programmazione(use_temi=FALSE, use_713=TRUE, use_flt=FALSE, 
                                    use_ciclo=TRUE, tipo_ciclo="CICLO_PROGRAMMAZIONE", use_location=FALSE) %>%
    rename(x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA) %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
  
  appo <- appo %>%
    left_join(po_riclass, 
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
  
  
  # integra x_PROGRAMMA per casi con x_AMBITO non convenzionale
  temp <- po_riclass %>% 
    filter(!(OC_CODICE_PROGRAMMA == "2007IT001FA005" & x_AMBITO == "PAC"),
           !(OC_CODICE_PROGRAMMA == "2007SA002FA016" & x_AMBITO == "PAC"),
           !(OC_CODICE_PROGRAMMA == "2007IT005FAMG1" & x_AMBITO == "PAC")) %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
    rename(x_PROGRAMMA_NEW = x_PROGRAMMA)
  
  # temp %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
  
  appo <- appo %>%
    left_join(temp, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_NEW, x_PROGRAMMA))
    
  
  
  return(appo)
}





