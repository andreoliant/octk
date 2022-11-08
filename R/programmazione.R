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
    filename <- paste0("Dati_DBCOE_", temp, "1420.xlsx") # DBPROG_FSC1420.xlsx
    
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
  if (ambito == "FESR" | ambito == "FSE" | ambito == "YEI") {
    appo <- appo %>%
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

#' Inizializza il database della programmazione
#'
#' Carica il databse della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param use_temi Vuoi caricare il DB con correzione dei temi prioritari?
#' @param use_sog Vuoi caricare il DB con il soggetto programmatore?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @return L'intero database dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
init_programmazione_dati <- function(use_temi=FALSE, use_sog=FALSE, use_eu=FALSE, use_flt=FALSE, use_713=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=FALSE, tipo_ciclo="CICLO_STRATEGIA", use_en=FALSE, use_po_psc=FALSE)
{
  # use_temi = FALSE
  # use_sog= FALSE
  # use_eu= FALSE
  # use_713 = TRUE
  # use_ciclo = FALSE
  # use_flt = FALSE
  # use_location = FALSE
  # use_articolaz = TRUE
  po_fsc <- load_db("2014-2020", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF aggiunto use_locatione che prima mancava
  po_fesr <- load_db("2014-2020", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_fse <- load_db("2014-2020", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_poc <- load_db("2014-2020", "POC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_yei <- load_db("2014-2020", "YEI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feamp <- load_db("2014-2020", "FEAMP", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_snai <- load_db("2014-2020", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_cte <- load_db("2014-2020", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feasr <- load_db("2014-2020", "FEASR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF mancava, aggiunto
  
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
    po_fsc713 <- load_db("2007-2013", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fesr713 <- load_db("2007-2013", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fse713 <- load_db("2007-2013", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_pac713 <- load_db("2007-2013", "PAC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    
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
  
  if (use_location == TRUE) {
    # ricodifica x_MACROAREA
    programmi %>% count(MACROAREA)
    programmi <- ricodifica_macroaree(programmi)
    programmi %>% count(x_MACROAREA)
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
      filter(TIPOLOGIA_PROGRAMMA != "PSC") %>% 
      # mutate(ID_PSC = NA_character_,
      #        PSC = NA_character_) %>% 
      # CHK: qui non devo eslcudere tutto, solo quello che è diverso da CSR (e COVID?)
      bind_rows(po_psc)
    # sum(programmi$FINANZ_TOTALE)

    # chk <- memo %>%
    #   filter(TIPOLOGIA_PROGRAMMA == "PSC") %>% 
    #   group_by(ID_PSC = OC_CODICE_PROGRAMMA) %>% 
    #   summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = T)) %>% 
    #   full_join(po_psc %>% 
    #               group_by(ID_PSC) %>% 
    #               summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = T)),
    #             by = "ID_PSC") %>% 
    #   mutate(CHK = FINANZ_TOTALE.x - FINANZ_TOTALE.y)
    
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
#' @param sum_po_last Vuoi dati aggregati come summary per programma, con solo l'ultima decisione?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @return Tutti i dati di supporto.
init_programmazione_info <- function(use_en = FALSE, use_713 = FALSE, sum_po = FALSE, sum_po_last=FALSE, use_po_psc=FALSE) {
  
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
    
    info <- info_FSC0713 %>%
      bind_rows(info_FS0713) %>%
      bind_rows(info_PAC0713 %>% 
                  mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE))) %>%
      bind_rows (info_FSC) %>%
      mutate(VERSIONE = as.character(VERSIONE)) %>% #fix per NA
      bind_rows(info_SIE) %>%
      bind_rows(info_FEASR) %>% 
      bind_rows(info_CTE) %>%
      bind_rows(info_FEAMP) %>%
      bind_rows(info_POC)
    
  } else {
    info_FSC   <- read_xlsx(file.path(DB, "Info_DBCOE_FSC1420.xlsx"))
    info_SIE   <- read_xlsx(file.path(DB, "Info_DBCOE_SIE1420.xlsx"))
    info_POC   <- read_xlsx(file.path(DB, "Info_DBCOE_POC1420.xlsx"))
    info_FEASR <- read_xlsx(file.path(DB, "Info_DBCOE_FEASR1420.xlsx"))
    info_FEAMP <- read_xlsx(file.path(DB, "Info_DBCOE_FEAMP1420.xlsx"))
    info_CTE   <- read_xlsx(file.path(DB, "Info_DBCOE_CTE1420.xlsx"))
    
    info <- info_FSC       %>%
      bind_rows(info_SIE)   %>%
      bind_rows(info_FEASR) %>% 
      bind_rows(info_CTE)   %>%
      bind_rows(info_FEAMP) %>%
      bind_rows(info_POC)  
    
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
                select(-AMBITO), 
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
                                         paste0(TIPO_DECISIONE_EN, " n. ", NUMERO_DECISIONE, " del ",format(DATA_DECISIONE, "%d/%m/%Y"))))
    
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
  
  # fix NA su LINK_DECISIONE
  info_last <- info_last %>% 
    mutate(LINK_DECISIONE = gsub("NA", "", LINK_DECISIONE))
  
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
  interventi <- init_programmazione_dati(use_temi = FALSE, use_sog=TRUE, use_eu=TRUE, use_713 = TRUE, use_ciclo = TRUE, use_flt = TRUE) %>%
    rename(x_PROGRAMMA = DESCRIZIONE_PROGRAMMA,
           x_GRUPPO = TIPOLOGIA_PROGRAMMA)
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
  }
  
  info_last <- init_programmazione_info(use_en = FALSE, use_713 = TRUE, sum_po = TRUE, sum_po_last = TRUE)
  
  # label da progetti pubblicati per allineamento a sito
  label_programmi <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>% 
    separate_rows(OC_DESCRIZIONE_PROGRAMMA, OC_CODICE_PROGRAMMA, sep = ":::")%>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
  
  # filtro pubblicati
  interventi <- interventi %>%
    mutate(PUB = if_else(OC_CODICE_PROGRAMMA %in% label_programmi$OC_CODICE_PROGRAMMA, TRUE, FALSE))
  
  # kill YEI (fonde tutto FSE del programma IOG in ambito YEI)
  interventi <- interventi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    # mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI",
    #                             TRUE ~ x_AMBITO)) %>%
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI", "CTE", "PAC")))
  
  # applica FLAG_MONITORAGGIO
  if (use_flt == TRUE) {
    interventi <- interventi %>%
      filter(FLAG_MONITORAGGIO == 1)
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
  }
  
  # summary (opzione 1: il programma pluri-fondo mostra il valore specifico di ogni ambito)
  # programmi <- interventi %>%
  #   # MEMO: con x_GRUPPO duplica programmi tipo Piombino e Dissesto perché viene da due cicli diversi lato risorse (ma hanno stesso x_CICLO lato strategia)
  #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, PUB) %>%
  #   left_join(interventi %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))
  
  # summary (opzione 2: il programma pluri-fondo è duplicato nei due ambiti e il valore esposto è sempre il totale) 
  programmi <- interventi %>%
    filter(x_GRUPPO != "PSC") %>% 
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, PUB) %>%
    left_join(interventi %>%
                filter(x_GRUPPO != "PSC") %>% 
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
                          RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA")
  # MEMO: questa versione è coerente con impostazione pagine aggregate sito ma poco informativa...

  # accoda PSC
  psc <- interventi %>%
    filter(x_GRUPPO == "PSC") %>% 
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, PUB) %>%
    left_join(interventi %>%
                filter(x_GRUPPO == "PSC") %>% 
                group_by(OC_CODICE_PROGRAMMA) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
                          RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
              by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(x_CICLO = "2014-2020")
  
  programmi <- programmi %>% 
    bind_rows(psc) 
  
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
    # temp <- programmi %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
    # chk <- programmi %>% semi_join(temp)
    print("Controlla se numerosità è invariata!!!")
    dim(programmi)[1] == dim(programmi_base)[1]
  }
  
  return(programmi)
  
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

    filename <- paste0("Interventi_DBCOE_CIS.xlsx") 
    
  } else {
    message("Non è ancora implementato")
    
  }
  
  # importa file excel
  appo <-  read_excel(file.path(DB, filename), guess_max = 5000)
  # VERSIONE  CON UNICO FILE - DA SISTEMARE: appo1 <-  read_excel(file.path(DB, "prova2.xlsx"), sheet = filename, guess_max = 5000)
  
  
  # switch varibili da tenere
  var_ls <- c("OC_CODICE_PROGRAMMA", "OC_TITOLO_PROGETTO",
              "AMBITO",
              "FINANZ_TOTALE_PUBBLICO")
  
  if (use_temi == TRUE) {
    var_ls <- c(var_ls,
                "COD_AREA_TEMATICA_PSC", "DESCR_AREA_TEMATICA_PSC", 
                "COD_SETTORE_INTERVENTO_PSC", "DESCR_SETTORE_INTERVENTO_PSC",
                "COD_RISULTATO_ATTESO", "DESCR_RISULTATO_ATTESO")
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
                                           FLAG_MONITORAGGIO == 9 ~ 9, # presente per FSC
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
    mutate(x_AMBITO = AMBITO) 
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
  po_riclass <- init_programmazione_dati(use_temi=FALSE, use_713=TRUE, use_flt=FALSE, 
                                    use_ciclo=TRUE, tipo_ciclo="CICLO_PROGRAMMAZIONE", use_location=FALSE) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
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



#' Esporta report con risorse coesione per ciclo, ambito e macroarea
#'
#' Esporta report con risorse coesione per ciclo, ambito e macroarea.
#'
#' @param ciclo Vuoi un ciclo specifico?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param use_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_eu Logico. vuoi vedere risorse UE ove previste?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @param force_yei Logico. Vuoi forzare FSE in YEI?
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e macroarea.
  make_report_risorse <- function(ciclo=NULL, use_meuro=FALSE, use_flt=FALSE, use_eu=FALSE, use_po_psc=FALSE, force_yei=FALSE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE) {
  
  programmi <- init_programmazione_dati(use_temi = FALSE, use_713 = TRUE, use_location = TRUE, use_ciclo = TRUE, use_eu=use_eu, 
                                        use_flt=use_flt, tipo_ciclo=tipo_ciclo, use_po_psc=use_po_psc) 
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2 | FLAG_MONITORAGGIO == 3)
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
      fname <- "risorse_coesione.csv"
    }
    
    write.csv2(out_2, file.path(TEMP, fname), row.names = FALSE)
  }
  
  return(out_2)
  
}



#' Report Fonti-Impieghi
#'
#' Crea Report Fonti-Impieghi con sintesi di CICLO_PROGRAMMAZIONE e CICLO_RISORSE per ambito di provenienza e utilizzo.
#'
#' @param use_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param export vuoi salvare il file?
#' @return Il Report Fonti-Impieghi
make_report_fonti_impieghi <- function(use_meuro=TRUE, export=TRUE) {
  
  appo <- init_programmazione_dati(use_713 = TRUE, use_flt = TRUE, use_ciclo = TRUE, use_po_psc = TRUE)
  
  # DEBUG:
  # appo %>% count(x_AMBITO, CICLO_PROGRAMMAZIONE, CICLO_RISORSE)
  # appo %>% filter(FLAG_MONITORAGGIO == 2) %>% count(x_AMBITO, CICLO_PROGRAMMAZIONE, CICLO_RISORSE)
  # x_AMBITO CICLO_PROGRAMMAZIONE CICLO_RISORSE n
  # 1      FSC            2007-2013     2007-2013 7
  # 2      POC            2014-2020     2014-2020 2
  
  temp <- appo %>% 
    filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
    mutate(CICLO_PROGRAMMAZIONE = factor(CICLO_PROGRAMMAZIONE, levels = c("2014-2020", "2007-2013", "2000-2006")),
           CICLO_RISORSE = factor(CICLO_RISORSE, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>% 
    mutate(x_AMBITO = case_when(#OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "FESR-FSE",
      x_AMBITO == "FESR" ~ "FESR-FSE",
      x_AMBITO == "FSE" ~ "FESR-FSE",
      x_AMBITO == "YEI" ~ "FESR-FSE",
      TRUE ~ x_AMBITO)) %>% 
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR-FSE", "CTE", "FEAD", "POC",	"PAC", "FSC", "SNAI"))) %>% 
    arrange(desc(CICLO_RISORSE)) %>% 
    mutate(y_AMBITO = x_AMBITO,
           FLAG_MONITORAGGIO = case_when(FLAG_MONITORAGGIO == 1 ~ "1.monit",
                                         FLAG_MONITORAGGIO == 0 ~ "2.no_monit",
                                         FLAG_MONITORAGGIO == 3 ~ "4.residuo",
                                         FLAG_MONITORAGGIO == 2 ~ "5.alt_monit", # riguarda interventi FSC 713  vengono da fondi e compensazioni ambientali + POC non programmato
                                         FLAG_MONITORAGGIO == 9 ~ "3.chk")) %>% 
    group_by(x_AMBITO, y_AMBITO, FLAG_MONITORAGGIO, CICLO_PROGRAMMAZIONE, CICLO_RISORSE) %>% 
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE)) 
  
  if (use_meuro == TRUE) {
    temp <- temp %>% 
      mutate(RISORSE = round(RISORSE/1000000, 1))
  }
  
  out <- temp %>% 
    arrange(FLAG_MONITORAGGIO, CICLO_PROGRAMMAZIONE, x_AMBITO) %>% 
    pivot_wider(id_cols = c("x_AMBITO", "FLAG_MONITORAGGIO", "CICLO_PROGRAMMAZIONE"), 
                names_from = c("CICLO_RISORSE", "y_AMBITO"), names_sort = TRUE,
                values_from = "RISORSE", values_fill = 0)
  
  if (export == TRUE){
    write_csv2(out, file.path(TEMP, "report_fonti_impieghi.csv"))
  }
  return(out)
}


#' Lista programmi per pagina dedicata
#'
#' Crea la lista dei programmi da pubblicare nella "pagina programmi" del sito di OC
#' 
#' @param programmi Dati di base da workflow_programmazione().
#' @param progetti Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)
#' @param export Vuoi salvare il file?
#' @return Lista dei programmi 2007-2013 e 2014-2020 applicando le convenzioni per la pubblicazione nella pagina "programmi" del sito.
make_pagina_programmi <- function(programmi=NULL, progetti=NULL, export=TRUE){
  
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_info=TRUE, use_flt=TRUE, progetti)
  }
  
  info_en <- init_programmazione_info(use_en = TRUE, use_713 = TRUE, sum_po = TRUE, sum_po_last = TRUE, use_po_psc = FALSE) %>% # TRUE
    select(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_EN)
  
  # programmi_en <- read_csv2(file.path(INPUT, "programmi_SIE_EN.csv")) %>%
  programmi_en <- read_xlsx(file.path(DB, "label_programmi_en.xlsx")) %>% 
    # select(-LABEL_PROGRAMMA_IT)
    distinct(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA_EN)
  
  # duplica psc
  psc <- programmi %>% 
    filter(x_GRUPPO == "PSC") %>% 
    mutate(x_CICLO = "2007-2013")
  
  programmi_2 <- programmi %>%
    bind_rows(psc)
  
  programmi <- programmi_2
  
  # DEV:
  # psc <- programmi %>% 
  #   filter(x_GRUPPO == "PSC") %>% 
  #   distinct(OC_CODICE_PROGRAMMA)
  # 
  # temp <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx")) %>% 
  #   mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE)) %>% 
  #   rename(AMBITO.x = AMBITO)
  # 
  # info_2 <- info %>% 
  #   anti_join(psc) %>% 
  #   bind_rows(temp)

  # label LABEL_PROGRAMMA_EN
  programmi <- programmi %>%
    left_join(info_en, by = "OC_CODICE_PROGRAMMA") %>%
    left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
           LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN))
  # CHK: duplicato, programmi incrementa di 4
  
  # temp <- programmi %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
  # chk <- programmi %>% semi_join(temp)
  # print("Controlla se numerosità è invariata!!!")
  # dim(programmi)[1] == dim(programmi_base)[1]
  # PON Inclusione
  
  # make URL_PROGRAMMA 
  programmi <- programmi %>%
    mutate(LINK_PROGRAMMA_IT = if_else(PUB == TRUE,
                                       paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/"),
                                       ""),
           LINK_PROGRAMMA_EN = if_else(PUB == TRUE,
                                       paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/"),
                                       "")) %>% 
    mutate(LINK_DOC_IT = paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"),
           LINK_DOC_EN = paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"))
    
  
  # LABEL_SITO_IT
  programmi <- programmi %>%
    mutate(LABEL_SITO_IT = if_else(is.na(LINK_SITO), "", "Sito web"),
           LABEL_SITO_EN = if_else(is.na(LINK_SITO), "", "Website"),
           LABEL_DOC_IT  = if_else(is.na(LINK_DOC_IT), "", "Documenti"),
           LABEL_DOC_EN  = if_else(is.na(LINK_DOC_EN), "", "Documents"),)
  
  programmi <- programmi %>% 
    mutate(LINK_DECISIONE = case_when(x_AMBITO == "FSC" ~ LINK_DECISIONE,
                                      x_AMBITO == "POC" ~ LINK_DECISIONE,
                                      # x_AMBITO == "PAC" ~ LINK_DECISIONE,
                                      TRUE ~ ""))
  
  # elimina "da programmare" e "non coesione" (fonte DEF 2020)
  # programmi <- programmi %>%
  #   filter(OC_CODICE_PROGRAMMA != "FSC_1420_CDD_40", # MEMO: da programmare FSC
  #          OC_CODICE_PROGRAMMA != "2020DAPROGR_1", # MEMO: sicilia e anpal
  #          OC_CODICE_PROGRAMMA != "2020DAPROGR_2", 
  #          OC_CODICE_PROGRAMMA != "TEMP_0713_007", # MEMO: debiti regioni 713
  #          OC_CODICE_PROGRAMMA != "DEBITI_CAM")
  # dovrà essere superato con OC_FLAG_MONITORAGGIO in init_programmazione_dati
  
  
  # label LABEL_AMBITO_EN
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_AMBITO_IT = case_when(x_AMBITO == "YEI" ~ "IOG",  # fix YEI
                                       # x_AMBITO == "SNAI" ~ "SNAI-SERVIZI",
                                       x_AMBITO == "SNAI" ~ "SNAI-Servizi",
                                       TRUE ~ x_AMBITO),
           LABEL_AMBITO_EN = case_when(x_AMBITO == "FESR" ~ "ERDF",
                                       x_AMBITO == "FSE" ~ "ESF",
                                       x_AMBITO == "FEASR" ~ "EAFRD",
                                       x_AMBITO == "FEAMP" ~ "MFF",
                                       x_AMBITO == "CTE" ~ "ETC",
                                       x_AMBITO == "FSC" ~ "DCF",
                                       x_AMBITO == "POC" ~ "COP",
                                       x_AMBITO == "PAC" ~ "CAP",
                                       x_AMBITO == "SNAI" ~ "IANS",
                                       x_AMBITO == "YEI" ~ "YEI"),
           LABEL_TIPO_IT = case_when(x_AMBITO == "SNAI" ~ "SNAI-SERVIZI",
                                     TRUE ~ x_GRUPPO),
           LABEL_TIPO_EN = case_when(x_GRUPPO == "PON" ~ "NOP",
                                     x_GRUPPO == "POR" ~ "ROP",
                                     x_GRUPPO == "PATTI" ~ "DEVELOPMENT PACT",
                                     x_GRUPPO == "PIANI STRALCIO" ~ "EXCERPT PLAN",
                                     x_GRUPPO == "PIANI OPERATIVI" ~ "NATIONAL PLAN",
                                     x_GRUPPO == "POC REGIONALI" ~ "REGIONAL COP",
                                     x_GRUPPO == "POC NAZIONALI" ~ "NATIONAL COP",
                                     x_AMBITO == "SNAI" ~ "IANS",
                                     TRUE ~ x_GRUPPO))
  
  # maiusc
  programmi <- programmi %>% 
    mutate(LABEL_PROGRAMMA_IT = toupper(LABEL_PROGRAMMA_IT),
           LABEL_PROGRAMMA_EN = toupper(LABEL_PROGRAMMA_EN))
  
  # FIX nomi psc ministeri creativi
  # programmi <- programmi %>% 
  #   mutate(LABEL_PROGRAMMA_IT = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
  #                                         OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
  #                                         TRUE ~ LABEL_PROGRAMMA_IT))
  
  # export #
  out <- programmi %>%
    filter(LABEL_AMBITO_IT != "FEASR", LABEL_AMBITO_IT != "FEAMP") %>% 
    mutate(LABEL_TIPO_IT = case_when(LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PATTI" ~ "PATTI",
                                    LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PSC" ~ "PSC",
                                    LABEL_AMBITO_IT == "FSC" ~ "VARI",
                                    LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale" ~ "NAZIONALI",
                                    LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
                                    LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale" ~ "REGIONALI",
                                    LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
                                    LABEL_AMBITO_IT == "SNAI-Servizi" ~ "SNAI-Servizi",
                                    LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "PAC Nazionale" ~ "NAZIONALI",
                                    LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "PAC Regionale" ~ "REGIONALI",
                                    LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT001FA005" ~ "NAZIONALI", # fix per direttrici ferroviarie
                                    TRUE ~ LABEL_TIPO_IT)) %>% 
    mutate(LABEL_TIPO_IT = factor(LABEL_TIPO_IT, levels = c("PSC", "PATTI", "VARI", "POR", "PON", "POIN", "CTE", "NAZIONALI", "REGIONALI", "COMPLETAMENTI", "SNAI-SERVIZI"))) %>%
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
    mutate(LABEL_TIPO_EN = factor(LABEL_TIPO_EN, levels = c("PSC", "PACTS", "OTHERS", "ROP", "NOP", "INOP","CTE", "NAZIONAL", "REGIONAL", "COMPLETAMENTI", "IANS"))) %>%
    mutate(LINK_DOC = paste0("../programmi/", OC_CODICE_PROGRAMMA, "/documenti/")) %>% # TEST
    mutate(RISORSE = round(RISORSE, 0),
           RISORSE_UE = round(RISORSE_UE, 0)) %>% 
    mutate(LINK_DOC = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #serve per non generare link su sito
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
                                           OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
                                           OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
                                           TRUE ~ LINK_DOC)) %>% 
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
                                TRUE ~ LABEL_DOC_IT)) %>% 
    # deve stare dopo
    # mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #serve per non generare link su sito
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
    #                                        OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
    #                                        TRUE ~ OC_CODICE_PROGRAMMA)) %>% 
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
           LINK_DOC, # TEST
           # LINK_DOC_IT, 
           # LINK_DOC_EN,
           LABEL_SITO_IT,
           LABEL_SITO_EN,
           # LINK_PROGRAMMA_IT, #generate automaticamente su sito
           # LINK_PROGRAMMA_EN, #generate automaticamente su sito
           LINK_SITO

    ) %>% 
    arrange(LABEL_TIPO_IT)
  
  

  
  
  # fix per CPT 713
  cpt <- programmi %>%
    filter(x_CICLO == "2007-2013", x_GRUPPO == "CPT")

  out2 <- out %>%
    anti_join(cpt, by = "OC_CODICE_PROGRAMMA") %>% 
    bind_rows(
      tibble(
        OC_CODICE_PROGRAMMA = "CPT_0713",
        LABEL_PROGRAMMA_IT = "CONTI PUBBLICI TERRITORIALI",
        LABEL_PROGRAMMA_EN = "PUBLIC TERRITORIAL ACCOUNTS",
        LABEL_CICLO = "2007-2013",
        LABEL_AMBITO_IT = "FSC",
        LABEL_AMBITO_EN = "DCF",
        LABEL_TIPO_IT = "VARI",
        LABEL_TIPO_EN = "OTHERS",
        RISORSE = sum(cpt$RISORSE),
        RISORSE_UE = 0,
        LABEL_DECISIONE_IT = "Delibera n. 42 del 23/03/2012",
        LABEL_DECISIONE_EN = "Resolution n. 42 - 23/03/2012",
        LINK_DECISIONE = "",
        LABEL_DOC_IT = "Documenti",
        LABEL_DOC_EN = "Documents",
        LINK_DOC = "",
        LABEL_SITO_IT = "",
        LABEL_SITO_EN = "",
        LINK_SITO = ""))
        
  # fix per caricamento su OC
  out3 <- out2 %>% 
    mutate(LABEL_TIPO_IT = as.character(LABEL_TIPO_IT),
           LABEL_TIPO_EN = as.character(LABEL_TIPO_EN)) %>% 
    mutate_if(is.character, list(~gsub("À", "A'", .))) %>% 
    mutate_if(is.character, list(~gsub("à", "a'", .))) %>% 
    mutate_if(is.character, list(~replace_na(., ""))) 
  
  out4 <- out3 %>% 
    # fix programmi anomali PAC
    filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007SA002FA016")) %>% 
    filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT001FA005")) %>% 
    filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT005FAMG1")) %>% 
    # fix programmi anomali FSC 713
    filter(OC_CODICE_PROGRAMMA != "TEMP_0713_006",
           OC_CODICE_PROGRAMMA != "TEMP_0713_999",
           OC_CODICE_PROGRAMMA != "TEMP_0713_005",
           OC_CODICE_PROGRAMMA != "CPT_0713")
  
  # OLD:
  # out <- out4
  # 
  # # split cicli
  # out_1420 <- out %>% 
  #   filter(LABEL_CICLO == "2014-2020")
  # 
  # out_713 <- out %>% 
  #   filter(LABEL_CICLO == "2007-2013") %>% 
  #   # scarta psc senza 713
  #   filter(OC_CODICE_PROGRAMMA != "PSC_MUR",
  #          OC_CODICE_PROGRAMMA != "PSC_LAZIO",
  #          OC_CODICE_PROGRAMMA != "PSC_MISE",
  #          OC_CODICE_PROGRAMMA != "PSC_MIT",
  #          OC_CODICE_PROGRAMMA != "PSC_MISALUTE",
  #          OC_CODICE_PROGRAMMA != "PSC_PCM-SPORT",
  #          OC_CODICE_PROGRAMMA != "PSC_MIPAAF")
  
  # NEW: elimina dupli di 713
  out <- out4 %>% 
    filter(!(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MUR"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_LAZIO"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISE"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIT"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISALUTE"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_PCM-SPORT"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIPAAF"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MITUR"),
           # fix per nuovi codici
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCTURISMO"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSVILECONOM"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCLAZIO"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSALUTE"),
           !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCUNIVRICERCA"))
  
  

  
  
  # split cicli
  out_1420 <- out %>% 
    filter(LABEL_CICLO == "2014-2020")
  
  out_713 <- out %>% 
    filter(LABEL_CICLO == "2007-2013")
  
  if (export == TRUE) {
    require(withr)
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_1420, file.path(TEMP, "programmi_1420.csv"), row.names = FALSE, na = "")
      )
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_713, file.path(TEMP, "programmi_0713.csv"), row.names = FALSE, na = "")
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
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return File opendata con le dotazioni per ambito e per i cicli 2007-2013 e 2014-2020. 
#' @note Usa dati finanziari da "interventi" ma sovrascrivo le convenzioni da "workflow" presenti in "programmi" (interventi sono N:1 su programmi)
#' @note ...
make_opendata_dotazioni <- function(programmi=NULL, progetti=NULL, export=TRUE, export_xls=TRUE) {
  
  # TODO: inserire controllo su totali finanziari per ciclo e ambito
  
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_info=TRUE, use_flt=TRUE, progetti)
  }

  
  
  # versione solo per 1420
  # interventi <- init_programmazione_dati(use_temi = FALSE, use_sog = TRUE, use_eu = TRUE, use_location = TRUE, 
  #                                   use_flt = TRUE, use_ciclo = TRUE)
  
  interventi <- init_programmazione_dati(use_temi = FALSE, use_sog = TRUE, use_eu = TRUE, use_location = TRUE, 
                                         use_flt = TRUE, use_ciclo = TRUE, use_713 = TRUE)
  
  # fix per psc in programmi
  psc <- programmi %>% 
    filter(x_GRUPPO == "PSC") %>% 
    bind_rows(programmi %>% 
                filter(x_GRUPPO == "PSC") %>% 
                mutate(x_CICLO = "2007-2013")) %>% 
    bind_rows(programmi %>% 
                filter(x_GRUPPO == "PSC") %>% 
                mutate(x_CICLO = "2000-2006"))
  
  programmi <- programmi %>% 
    filter(x_GRUPPO != "PSC") %>% 
    bind_rows(psc)
  
  
  # applica convenzione workflow a interventi
  appo1 <- interventi %>%
    # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2) %>% 
    filter(FLAG_MONITORAGGIO == 1) %>% 
    # MEMO: programmi è già filtrato da workflow
    select(-DESCRIZIONE_PROGRAMMA, -TIPOLOGIA_PROGRAMMA, -AMBITO,
           -CICLO_PROGRAMMAZIONE) %>%
    left_join(programmi %>%
                select(-RISORSE), 
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  # MEMO: 
  # usa dati finanziari da "interventi" 
  # ma sovrascrivo le convenzioni da "workflow" presenti in "programmi"
  # (interventi sono N:1 su programmi)
  
  out <- appo1 %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_LIVELLO = NA,
           LABEL_PROGRAMMA = toupper(x_PROGRAMMA),
           LABEL_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
                                    x_AMBITO == "SNAI" ~ "SNAI-Servizi",
                                    TRUE ~ x_AMBITO),
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
           RISORSE = FINANZ_TOTALE, 
           RISORSE_UE = FINANZ_UE) %>% 
    # aggrego perché interventi è più dettagliato (?)
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
  out <- out %>% 
    mutate(OC_TIPOLOGIA_PROGRAMMA = case_when(LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PATTI" ~ "PATTI",
                                              LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "PSC",
                                              # LABEL_AMBITO == "FSC" & grepl("PSC_", OC_CODICE_PROGRAMMA) ~ "PSC", # fix per NA
                                     LABEL_AMBITO == "FSC" ~ "VARI",
                                     LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "NAZIONALI",
                                     LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
                                     LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "REGIONALI",
                                     LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
                                     LABEL_AMBITO == "SNAI-Servizi" ~ "SNAI-SERVIZI",
                                     TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>% 
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
    
    looper <- tibble(
      LABEL_AMBITO = c("SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
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
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-SERVIZI", "CTE", "PAC"))) %>% 
      # appo per spostare psc
      mutate(LABEL_CICLO_2 = case_when(OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "2014-2020",
                                       TRUE ~ LABEL_CICLO)) %>% 
      mutate(AMMINISTRAZIONE = case_when(AMMINISTRAZIONE == "???" ~ "",
                                         TRUE ~ AMMINISTRAZIONE))
    
    
    # loop
    for (i in seq_along(rownames(looper))) {
      x_ambito <- looper[[i, "LABEL_AMBITO"]]
      x_ciclo <- looper[[i, "LABEL_CICLO"]]
      print(paste0("elaboro ", x_ciclo, "-", x_ambito))
      
      # filter
      if (x_ambito == "SIE" | x_ambito == "FS") {
        out_3 <- out_2 %>%  
          filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG", LABEL_CICLO_2 == x_ciclo)%>% 
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
    programmi <- workflow_programmazione(use_info=FALSE, use_flt=TRUE, progetti)
    # MEMO: use_info porta solo alcune variabili perché richiede sum_po = TRUE
  }
   
  info <- init_programmazione_info(use_713 = TRUE, sum_po = FALSE, use_po_psc = FALSE)
  # DEV: perché non lo prendo da workflow?
  
  # sovrascrive psc
  # psc <- programmi %>% 
  #   filter(x_GRUPPO == "PSC") %>% 
  #   distinct(OC_CODICE_PROGRAMMA)
  # 
  # temp <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx")) %>% 
  #   mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE)) %>% 
  #   rename(AMBITO.x = AMBITO)
  # 
  # info_2 <- info %>% 
  #   anti_join(psc) %>% 
  #   bind_rows(temp)
  
  
  appo1 <- programmi %>%
    left_join(info, by = "OC_CODICE_PROGRAMMA")
  # MEMO: uso convenzioni da "workflow" in "programmi" e aggiungo dati per singole delibere (che sono N:1 su programmi)
  
  # inglese
  appo2 <- appo1 # %>%
    # mutate(TIPO_DECISIONE_EN = case_when(TIPO_DECISIONE == "Delibera CIPE" ~ "Resolution CIPE",
    #                                      TIPO_DECISIONE == "Decisione CE" ~ "Decision EC",
    #                                      TIPO_DECISIONE == "Legge" ~ "Law")) %>%
    # mutate(LABEL_DECISIONE_IT = paste0(TIPO_DECISIONE,
    #                                    " n. ",
    #                                    NUMERO_DECISIONE,
    #                                    " del ",
    #                                    format(DATA_DECISIONE, "%d/%m/%Y")),
           # LABEL_DECISIONE_EN = paste0(TIPO_DECISIONE_EN,
           #                             " n. ",
           #                             NUMERO_DECISIONE,
           #                             " - ",
           #                             format(DATA_DECISIONE, "%m/%d/%Y")),
           # LINK_PROGRAMMA_IT = paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/"),
           # LINK_PROGRAMMA_EN = paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/")
           # )
  
  out <- appo2 %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_PROGRAMMA = toupper(x_PROGRAMMA),
           LABEL_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
                                    x_AMBITO == "SNAI" ~ "SNAI-Servizi",
                                    TRUE ~ x_AMBITO)) %>% 
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
           # LINK_PROGRAMMA_IT,
           # TIPO_DECISIONE_EN,
           # LABEL_DECISIONE_EN,
           # LINK_PROGRAMMA_EN,
           # LABEL_DOC,
           # LINK_DOC
           # LABEL_DOC_2,
           # LINK_DOC_2,
           # LABEL_DOC,
           # LINK_DOC,
           # LINK_SITO
    )
  
  # FIX nomi psc ministeri creativi
  # out <- out %>% 
  #   mutate(LABEL_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
  #                                      OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
  #                                      TRUE ~ LABEL_PROGRAMMA))
  
  # fix per export
  out <- out %>% 
    mutate(OC_TIPOLOGIA_PROGRAMMA = case_when(LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PATTI" ~ "PATTI",
                                              LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "PSC",
                                              # LABEL_AMBITO == "FSC" & grepl("PSC_", OC_CODICE_PROGRAMMA) ~ "PSC", # fix per NA
                                              LABEL_AMBITO == "FSC" ~ "VARI",
                                              LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "NAZIONALI",
                                              LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
                                              LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "REGIONALI",
                                              LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
                                              LABEL_AMBITO == "SNAI-Servizi" ~ "SNAI-SERVIZI",
                                              TRUE ~ OC_TIPOLOGIA_PROGRAMMA))
  
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
    
    looper <- tibble(
      LABEL_AMBITO = c("SIE", "POC", "FSC", "FSC", "FS", "PAC"),
      LABEL_CICLO = c("2014-2020", "2014-2020", "2014-2020", "2007-2013", "2007-2013", "2007-2013"),
    )
    
    # TODO: aggrego FESR e FSE in SIE solo per 1420, forse va fatto anche per 713 oppure nemmeno per 1420?
    
    # clean
    out_2 <- out %>%
      mutate(LABEL_AMBITO = factor(LABEL_AMBITO, levels = c("SIE", "FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "IOG", "SNAI-Servizi", "CTE", "PAC")))
    
    # loop
    for (i in seq_along(rownames(looper))) {
      x_ambito <- looper[[i, "LABEL_AMBITO"]]
      x_ciclo <- looper[[i, "LABEL_CICLO"]]
      print(paste0("elaboro ", x_ciclo, "-", x_ambito))
      
      # filter
      if (x_ambito == "SIE" | x_ambito == "FS") {
        out_3 <- out_2 %>%  
          filter(LABEL_AMBITO == "FSE" | LABEL_AMBITO == "FESR" | LABEL_AMBITO == "IOG", LABEL_CICLO == x_ciclo)
        
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



#' Dotazioni programmi in PSC
#'
#' Crea il file come quelli in opendata con le dotazioni dei programmi originari trasformati in PSC
#' 
#' @param programmi Dati di base da workflow_programmazione().
#' @param progetti Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return File opendata con le dotazioni per ambito e per i cicli 2007-2013 e 2014-2020. 
#' @note Usa dati finanziari da "interventi" ma sovrascrivo le convenzioni da "workflow" presenti in "programmi" (interventi sono N:1 su programmi)
make_opendata_dotazioni_popsc <- function(programmi=NULL, progetti=NULL, export=TRUE, export_xls=TRUE) {
  
  # TODO: inserire controllo su totali finanziari per ciclo e ambito
  
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_info=TRUE, use_flt=TRUE, progetti)
  }
  
  # interventi <- init_programmazione_dati(use_temi = FALSE, use_sog = TRUE, use_eu = TRUE, use_location = TRUE, 
  #                                   use_flt = TRUE, use_ciclo = TRUE, use_po_psc=TRUE)
  # interventi <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
  interventi <- read_xlsx(file.path(DB, "Fonti_DBCOE_PSC.xlsx")) %>% 
    group_by(AMBITO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, CICLO_PROGRAMMAZIONE, AMMINISTRAZIONE,
             TIPOLOGIA_PROGRAMMA,
             FINANZ_TOTALE, MACROAREA,
             CAT_REGIONE,               
             ID_PSC, PSC, FLAG_MONITORAGGIO) %>% 
    summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE)) %>% 
    # NEW: elimina programmi duplicati per nuovo codice
    filter(OC_CODICE_PROGRAMMA != "PSCCAMPANIA",
           OC_CODICE_PROGRAMMA != "PSCCIMTCAGLIARI",
           OC_CODICE_PROGRAMMA != "PSCEMILROMAGNA",
           OC_CODICE_PROGRAMMA != "PSCLAZIO",
           OC_CODICE_PROGRAMMA != "PSCPIEMONTE",
           OC_CODICE_PROGRAMMA != "PSCSALUTE",
           OC_CODICE_PROGRAMMA != "PSCSVILECONOM",
           OC_CODICE_PROGRAMMA != "PSCTRENTO",
           OC_CODICE_PROGRAMMA != "PSCTURISMO",
           OC_CODICE_PROGRAMMA != "PSCUNIVRICERCA",
           OC_CODICE_PROGRAMMA != "PSCVALLEAOSTA")
  

  
  
  # applica convenzione workflow a interventi
  appo1 <- interventi %>%
    # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2) %>% 
    filter(FLAG_MONITORAGGIO == 1) %>% 
    rename(x_AMBITO = AMBITO, 
           x_CICLO = CICLO_PROGRAMMAZIONE,
           x_MACROAREA = MACROAREA) %>%
    # MEMO: programmi è già filtrato 
    # select(-DESCRIZIONE_PROGRAMMA, -TIPOLOGIA_PROGRAMMA) %>%
    left_join(programmi %>%
                select(-RISORSE), 
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  # MEMO: 
  # usa dati finanziari da "interventi" 
  # ma sovrascrivo le convenzioni da "workflow" presenti in "programmi"
  # (interventi sono N:1 su programmi)
  
  out <- appo1 %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_LIVELLO = NA,
           LABEL_PROGRAMMA = toupper(DESCRIZIONE_PROGRAMMA),
           LABEL_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
                                    x_AMBITO == "SNAI" ~ "SNAI-Servizi",
                                    TRUE ~ x_AMBITO)) %>% 
    select(OC_CODICE_PROGRAMMA,
           LABEL_PROGRAMMA,
           LABEL_AMBITO,
           LABEL_CICLO = x_CICLO,
           OC_TIPOLOGIA_PROGRAMMA = TIPOLOGIA_PROGRAMMA,
           CATEGORIA_REGIONI = CAT_REGIONE,
           # LABEL_LIVELLO, # = OC_AREA_OBIETTIVO_UE,
           x_MACROAREA,
           AMMINISTRAZIONE,
           ID_PSC,
           PSC,
           RISORSE = FINANZ_TOTALE) %>% 
    # aggrego perché interventi è più dettagliato (?)
    group_by(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA, LABEL_AMBITO, LABEL_CICLO,
             OC_TIPOLOGIA_PROGRAMMA, 
             CATEGORIA_REGIONI,
             # LABEL_LIVELLO, 
             x_MACROAREA, AMMINISTRAZIONE,
             ID_PSC, PSC) %>% 
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>% 
    select(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA, LABEL_AMBITO, LABEL_CICLO,
           OC_TIPOLOGIA_PROGRAMMA, 
           CATEGORIA_REGIONI,
           # LABEL_LIVELLO, 
           x_MACROAREA, AMMINISTRAZIONE,
           RISORSE,
           ID_PSC, PSC)
  
  # FIX nomi psc ministeri creativi
  # out <- out %>% 
  #   mutate(TEMP = paste0("PSC ", PSC)) %>% 
  #   mutate(PSC = case_when(ID_PSC == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
  #                          ID_PSC == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
  #                          ID_PSC == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
  #                          ID_PSC == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
  #                          ID_PSC == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
  #                          ID_PSC == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
  #                          ID_PSC == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
  #                          ID_PSC == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
  #                          ID_PSC == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
  #                          TRUE ~ TEMP)) %>% 
  #   select(-TEMP)
  
  # clean
  out <- out %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "CN" ~ "Centro-Nord",
                                   x_MACROAREA == "SUD" ~ "Mezzogiorno"),
           # RISORSE = format(RISORSE, nsmall=2, big.mark=".", decimal.mark=","),
           CATEGORIA_REGIONI = ifelse(is.na(CATEGORIA_REGIONI), "-", as.character(CATEGORIA_REGIONI)),
           AMMINISTRAZIONE = ifelse(is.na(AMMINISTRAZIONE), "Amministrazioni varie", as.character(AMMINISTRAZIONE))) %>% 
    # aggiorna codici
    mutate(ID_PSC = case_when(ID_PSC == "PSC_CAMPANIA" ~ "PSCCAMPANIA",
                              ID_PSC == "PSC_CAGLIARI" ~ "PSCCIMTCAGLIARI",
                              ID_PSC == "PSC_EMILIA-ROMA" ~ "PSCEMILROMAGNA",
                              ID_PSC == "PSC_LAZIO" ~ "PSCLAZIO",
                              ID_PSC == "PSC_PIEMONTE" ~ "PSCPIEMONTE",
                              ID_PSC == "PSC_MISALUTE" ~ "PSCSALUTE",
                              ID_PSC == "PSC_MISE" ~ "PSCSVILECONOM",
                              ID_PSC == "PSC_PA_TRENTO" ~ "PSCTRENTO",
                              ID_PSC == "PSC_MTUR" ~ "PSCTURISMO",
                              ID_PSC == "PSC_MUR" ~ "PSCUNIVRICERCA",
                              ID_PSC == "PSC_VALLE_D_AOS" ~ "PSCVALLEAOSTA",
                              TRUE ~ ID_PSC))
  
  
  
  
  
  # export
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "dotazioni_psc.csv"), row.names = FALSE)
  }
  
  # export xls
  if (export_xls == TRUE) {
    # TODO: rivedere allineamento tra header template e variabili in export (teniamo un solo template)

    # xls
    require("openxlsx") 
    # wb <- loadWorkbook(file.path(INPUT, "TemplateDotazioni.xlsx"))
    wb <- loadWorkbook(system.file("extdata", "template_dotazioni_psc.xlsx", package="octk"))
    writeData(wb, x = out, sheet = "Dotazioni", startCol = 1, startRow = 2, colNames = FALSE)
    fname <- "Dotazioni_programmi_PSC.xlsx"
    saveWorkbook(wb, file = file.path(OUTPUT, fname), overwrite = TRUE)
  }
  
  return(out)
}



#' Decisioni programmi in PSC
#'
#' Crea il file come quelli in opendata con le decisioni dei programmi originari trasformati in PSC
#' 
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @return File opendata con le decisioni per ambito e per i cicli 2007-2013 e 2014-2020. 
#' @note ...
make_opendata_decisioni_popsc <- function(export=TRUE, export_xls=TRUE) {
  
  # dotazioni <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
  dotazioni <- read_xlsx(file.path(DB, "Fonti_DBCOE_PSC.xlsx")) %>% 
    group_by(AMBITO, OC_CODICE_PROGRAMMA, DESCRIZIONE_PROGRAMMA, CICLO_PROGRAMMAZIONE, AMMINISTRAZIONE,
             TIPOLOGIA_PROGRAMMA,
             FINANZ_TOTALE, MACROAREA,
             CAT_REGIONE,               
             ID_PSC, PSC, FLAG_MONITORAGGIO) %>% 
    summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE)) %>% 
    # NEW: elimina programmi duplicati per nuovo codice
    filter(OC_CODICE_PROGRAMMA != "PSCCAMPANIA",
           OC_CODICE_PROGRAMMA != "PSCCIMTCAGLIARI",
           OC_CODICE_PROGRAMMA != "PSCEMILROMAGNA",
           OC_CODICE_PROGRAMMA != "PSCLAZIO",
           OC_CODICE_PROGRAMMA != "PSCPIEMONTE",
           OC_CODICE_PROGRAMMA != "PSCSALUTE",
           OC_CODICE_PROGRAMMA != "PSCSVILECONOM",
           OC_CODICE_PROGRAMMA != "PSCTRENTO",
           OC_CODICE_PROGRAMMA != "PSCTURISMO",
           OC_CODICE_PROGRAMMA != "PSCUNIVRICERCA",
           OC_CODICE_PROGRAMMA != "PSCVALLEAOSTA")
  
  # decisioni <- read_xlsx(file.path(DB, "fsc_delibere_po_psc.xlsx"))
  decisioni <- read_xlsx(file.path(dirname(DB), "INFO", "PO-PSC", "fsc_delibere_po_psc.xlsx"))
  
  # chk_match(dotazioni, decisioni, "OC_CODICE_PROGRAMMA")
  
  appo1 <- dotazioni %>%
    # select(-NOTE) %>% 
    left_join(decisioni %>% 
                select(OC_CODICE_PROGRAMMA, DATA_DECISIONE, NUMERO_DECISIONE, FLAG_ULTIMA_DECISIONE,
                TIPO_DECISIONE, LINK_DECISIONE, VERSIONE, SEQ_DECISIONE, LINK_DOCUMENTO, NOTE),
              by = "OC_CODICE_PROGRAMMA")
  
  out <- appo1 %>%
    mutate(AMBITO = as.character(AMBITO)) %>% 
    mutate(LABEL_PROGRAMMA = toupper(DESCRIZIONE_PROGRAMMA),
           LABEL_AMBITO = AMBITO) %>% 
    select(OC_CODICE_PROGRAMMA,
           LABEL_PROGRAMMA,
           LABEL_AMBITO,
           LABEL_CICLO = CICLO_PROGRAMMAZIONE,
           OC_TIPOLOGIA_PROGRAMMA = TIPOLOGIA_PROGRAMMA,
           VERSIONE_PROGRAMMA= VERSIONE,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           SEQ_DECISIONE,
           FLAG_ULTIMA_DECISIONE,
           # LINK_DECISIONE,
           # LABEL_DECISIONE_IT = NOTE, 
           # LINK_PROGRAMMA_IT,
           # TIPO_DECISIONE_EN,
           # LABEL_DECISIONE_EN,
           # LINK_PROGRAMMA_EN,
           # LABEL_DOC,
           # LINK_DOC
           # LABEL_DOC_2,
           # LINK_DOC_2,
           # LABEL_DOC,
           # LINK_DOC,
           # LINK_SITO
           ID_PSC,
           PSC
    )
  
  # FIX nomi psc ministeri creativi
  # out <- out %>% 
  #   mutate(ID_PSC = case_when(ID_PSC == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
  #                          ID_PSC == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
  #                          ID_PSC == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
  #                          ID_PSC == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
  #                          ID_PSC == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
  #                          ID_PSC == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
  #                          ID_PSC == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
  #                          ID_PSC == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
  #                          ID_PSC == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
  #                          TRUE ~ ID_PSC))
  
  
  # out <- out %>% 
  #   mutate(LINK_DECISIONE = case_when(LABEL_AMBITO == "FSC" ~ LINK_DECISIONE,
  #                                     LABEL_AMBITO == "POC" ~ LINK_DECISIONE,
  #                                     LABEL_AMBITO == "PAC" ~ LINK_DECISIONE,
  #                                     TRUE ~ ""))
  # 
  
  # clean
  out <- out %>% 
    mutate(DATA_DECISIONE = format(DATA_DECISIONE, "%d/%m/%Y"),
           NUMERO_DECISIONE	= ifelse(is.na(NUMERO_DECISIONE), "n.d.", as.character(NUMERO_DECISIONE)),
           DATA_DECISIONE	= ifelse(is.na(DATA_DECISIONE), "n.d.", as.character(DATA_DECISIONE)),
           VERSIONE_PROGRAMMA	= ifelse(is.na(VERSIONE_PROGRAMMA), "-", as.character(VERSIONE_PROGRAMMA)),
           SEQ_DECISIONE	= ifelse(is.na(SEQ_DECISIONE), "-", as.character(SEQ_DECISIONE)))%>% 
    # aggiorna codici
    mutate(ID_PSC = case_when(ID_PSC == "PSC_CAMPANIA" ~ "PSCCAMPANIA",
                              ID_PSC == "PSC_CAGLIARI" ~ "PSCCIMTCAGLIARI",
                              ID_PSC == "PSC_EMILIA-ROMA" ~ "PSCEMILROMAGNA",
                              ID_PSC == "PSC_LAZIO" ~ "PSCLAZIO",
                              ID_PSC == "PSC_PIEMONTE" ~ "PSCPIEMONTE",
                              ID_PSC == "PSC_MISALUTE" ~ "PSCSALUTE",
                              ID_PSC == "PSC_MISE" ~ "PSCSVILECONOM",
                              ID_PSC == "PSC_PA_TRENTO" ~ "PSCTRENTO",
                              ID_PSC == "PSC_MTUR" ~ "PSCTURISMO",
                              ID_PSC == "PSC_MUR" ~ "PSCUNIVRICERCA",
                              ID_PSC == "PSC_VALLE_D_AOS" ~ "PSCVALLEAOSTA",
                              TRUE ~ ID_PSC))
  
  
  # export
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "decisioni_po_psc.csv"), row.names = FALSE, na = "")
  }
  
  # export xls
  if (export_xls == TRUE) {
    # TODO: rivedere allineamento tra header template e variabili in export (teniamo un solo template)
    
    # xls
    require("openxlsx") 
    # wb <- loadWorkbook(file.path(INPUT, "TemplateDotazioni.xlsx"))
    wb <- loadWorkbook(system.file("extdata", "template_decisioni_psc.xlsx", package="octk"))
    writeData(wb, x = out, sheet = "Decisioni", startCol = 1, startRow = 2, colNames = FALSE)
    fname <- "Decisioni_programmi_PSC.xlsx"
    saveWorkbook(wb, file = file.path(OUTPUT, fname), overwrite = TRUE)
  }
  
  return(out)
}


#' Crea file dati FSC per DBCOE
#'
#' Crea file dati FSC per DBCOE per i cicli 2000-2006, 2007-2013 e 2014-2020
#' 
#' @param file_evo Nome del file "assegnazioni_evo_XXXXXXXX.XX.xlsx"
#' @param file_temi Nome del file "PSC_articolazione_tematiche_chk_decimali_v.XX.xlsx"
#' @param psc_cm Logico. Vuoi convertire in PSC anche i Patti delle Città metropolitane?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx nel DBCOE?
#' @return File file dati FSC per DBCOE per i cicli 2007-2013 e 2014-2020. I PSC sono interamente nel file 2014-2020. Le risorse del ciclo 2000-2006 sono interamente riassorbite nei PSC, per questo non c'è un file dedicato.
#' @note Salva nella versione del DBCOE risultante come DB da __oc_init__.
setup_dbcoe_dati_fsc_psc <- function(file_evo, file_temi, psc_cm=FALSE, export=FALSE, export_xls=FALSE) {
  
  # file_evo <- "assegnazioni_evo_20210803.00.xlsx"
  # file_temi <- "PSC_articolazione_tematiche_chk_decimali_v.05.xlsx"
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  strumenti <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", file_evo), sheet = "strumenti")
  # articolazioni <- read_xlsx(file.path(INPUT, file_temi), sheet = "articolazioni")
  articolazioni <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", file_temi), sheet = "articolazioni")
  
  
  # ----------------------------------------------------------------------------------- #
  # clean strumenti
  
  strumenti <- strumenti %>%
    rename(OC_CODICE_PROGRAMMA = `Codice strumento`,
           # `Tipo codice strumento`,
           COD_ORIG = `Codice strumento originario`,
           CICLO_PROGRAMMAZIONE = `Ciclo di programmazione per strategia`, 
           OC_DESCRIZIONE_PROGRAMMA = `Descrizione strumento`,
           # PSC,
           OC_MACROTIPOLOGIA_PROGRAMMA = `Macro-tipologia strumento`,
           OC_TIPOLOGIA_PROGRAMMA = `Tipologia strumento`,
           AMMINISTRAZIONE_TITOLARE = `Amministrazione titolare`,
           # `Soggetto attuatore`,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE = `Tipologia di amministrazione titolare`,
           # `Label tavola informativa CIPE 05/2020`,
           # `Label tavola informativa CIPE 12/2019`,
           # `A cavallo di cicli`,
           # `Valore considerato per CIPE 05/2020`,
           # `Valore considerato per CIPE 05/2020 – Al netto di 2019`,
           RIS_PRE44_TOT = `Risorse FSC assegnate ante 44– Totale`,
           RIS_PRE44_0006 = `Risorse FSC assegnate ante 44 – 2000-2006`,
           RIS_PRE44_0713 = `Risorse FSC assegnate ante 44 – 2007-2013`,
           RIS_PRE44_1420 = `Risorse FSC assegnate ante 44 – 2014-2020`,
           RIS_44_NO_MONIT = `Art 44 – Tagli da economie (col H)`,
           RIS_44_TAGLI_7B = `Art 44 – Tagli da progetti 7.b (col. M)`,
           RIS_44_7A = `Art 44 – Progetti 7.a a dicembre 2019`,
           RIS_44_7B = `Art 44- Progetti e risorse in 7.b (calcolo)`,
           RIS_44_NO_VAL = `Art 44 – Risorse confermate senza valutazione`,
           RIS_TOT = `Risorse FSC post 44 – Totale`,
           RIS_0006 = `Risorse FSC post 44 – 2000-2006`,
           RIS_0713 = `Risorse FSC post 44 – 2007-2013`,
           RIS_1420 = `Risorse FSC post 44 – 2014-2020`,
           RIS_SUD = `Risorse FSC post 44 – Mezzogiorno`,
           RIS_CN = `Risorse FSC post 44 – Centro-Nord`,
           RIS_ND = `Risorse FSC post 44 – Non ripartite`,
           RIS_PSC_ORD = `Risorse FSC post 44 – confermate in PSC o altro`,
           RIS_PSC_SEZ_COVID = `Risorse FSC post 44 – sezione speciale COVID (art. 241)`,
           RIS_PSC_SEZ_POR = `Risorse FSC post 44 – sezione speciale copertura POR (art. 242)`,
           # FLAG_COESIONE = `Flag perimetro Coesione`,
           DELIBERE = `Delibere CIPE e Norme`,
           # NOTE = `Note su Risorse`,
           LISTA_INT  = `Lista interventi programmati`,
           # OC_FLAG_MONITORAGGIO = `Flag monitoraggio`
           OC_FLAG_MONITORAGGIO = `Flag perimetro Coesione`)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # elab programmi
  
  # MEMO: il flusso considera solo strumenti con risorse diverse da 0
  # alcuni strumenti con risorse 0 sono recuperati alla fine
  
  
  # po <- "2017POAMBIENFSC"
  
  temp_macro <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_SUD, RIS_CN, RIS_ND) %>%
    gather(key = "OC_MACROAREA", value = "FINANZ_FSC", RIS_SUD, RIS_CN, RIS_ND) %>%
    filter(abs(FINANZ_FSC) > 0) %>%
    mutate(OC_MACROAREA = case_when(OC_MACROAREA == "RIS_SUD" ~ "SUD", 
                                    OC_MACROAREA == "RIS_CN" ~ "CN", 
                                    OC_MACROAREA == "RIS_ND" ~ "ND"))
  
  temp_ciclo <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_0006, RIS_0713, RIS_1420) %>%
    gather(key = "CICLO_RISORSE", value = "FINANZ_FSC", RIS_0006, RIS_0713, RIS_1420)%>%
    filter(abs(FINANZ_FSC) > 0) %>%
    mutate(CICLO_RISORSE = case_when(CICLO_RISORSE == "RIS_0006" ~ "2000-2006",
                                     CICLO_RISORSE == "RIS_0713" ~ "2007-2013",
                                     CICLO_RISORSE == "RIS_1420" ~ "2014-2020"))
  
  appo <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_TOT, RIS_SUD, RIS_CN, RIS_ND, RIS_0006, RIS_0713, RIS_1420) %>%
    left_join(temp_macro  %>%
                count(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE) %>%
                rename(N_AREE = n), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    left_join(temp_ciclo %>%
                count(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE) %>%
                rename(N_CICLI = n), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    # filter(OC_CODICE_PROGRAMMA == po) %>%
    mutate(CHK = case_when(N_AREE > 1 & N_CICLI == 1 ~ "ciclo_unico",
                           N_AREE == 1 & N_CICLI > 1 ~ "area_unica",
                           N_AREE == 1 & N_CICLI == 1 ~ "tutto_unico",
                           is.na(N_AREE) & is.na(N_CICLI) ~ "vuoto",
                           is.na(N_AREE) ~ "vuoto",
                           is.na(N_CICLI) ~ "vuoto",
                           TRUE ~ "chk"))
  
  appo %>% count(CHK)
  # CHK             n
  # <chr>       <int>
  # 1 area_unica     32
  # 2 chk             1
  # 3 ciclo_unico    35
  # 4 tutto_unico   308
  # 5 vuoto          32
  
  # traspone macroaree e cicli contabili
  memo <- tibble()
  
  for (i in seq(1, dim(appo)[1])) {
    po <- as.character(appo[i, "OC_CODICE_PROGRAMMA"])
    print(po)
    
    chk <- as.character(appo[i, "CHK"])
    
    if (chk == "tutto_unico") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(-FINANZ_FSC) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "area_unica") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(-FINANZ_FSC) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "ciclo_unico") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po)  %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po) %>%
                    select(-FINANZ_FSC), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "vuoto") {
      temp <- tibble(OC_CODICE_PROGRAMMA = po,
                     CHK = chk)
      
    } else if (chk == "chk") {
      # patch per casi con split su ciclo e macroarea
      
      appo %>%
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(RIS_TOT)
      
      
      temp0 <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        left_join(appo %>%
                    filter(OC_CODICE_PROGRAMMA == po) %>%
                    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_TOT), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(X = FINANZ_FSC / RIS_TOT) %>%
        select(-FINANZ_FSC, -RIS_TOT) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(FINANZ_FSC_NEW = FINANZ_FSC * X)
      
      test <- sum(temp0$FINANZ_FSC_NEW) == as.numeric(appo[appo$OC_CODICE_PROGRAMMA == po, "RIS_TOT"])
      
      if (test == TRUE) {
        temp <- temp0 %>%
          select(OC_CODICE_PROGRAMMA,
                 CICLO_PROGRAMMAZIONE,
                 OC_MACROAREA,
                 FINANZ_FSC = FINANZ_FSC_NEW,
                 CICLO_RISORSE)
        
      } else {
        message("ATTENZIONE!!!")
        temp <- tibble(OC_CODICE_PROGRAMMA = po,
                       CHK = chk)
      }
    }
    
    #MEMO: lascio fuori i casi "vuoti"
    
    memo <- memo %>%
      bind_rows(temp)
    
  }
  
  
  
  
  # clean per export
  programmi <- strumenti %>%
    distinct(OC_CODICE_PROGRAMMA,
             COD_ORIG,
             OC_DESCRIZIONE_PROGRAMMA,
             CICLO_PROGRAMMAZIONE,
             # CICLO_RISORSE,
             # TIPOLOGIA_STRUMENTO,
             AMMINISTRAZIONE_TITOLARE,
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
             OC_TIPOLOGIA_PROGRAMMA,
             PSC,
             # ADDENDUM,
             # TIPO_DECISIONE,
             # NUMERO_DECISIONE,
             # DATA_DECISIONE,
             # OC_FLAG_ULTIMA_DECISIONE,
             # LINK_DECISIONE,
             # NOTE_DECISIONE,
             # OC_COD_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             # OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_TITOLO_PROGETTO,
             # FINANZ_UE,
             # FINANZ_FSC,
             # FINANZ_ALTRO,
             # FINANZ_TOTALE_PUBBLICO,
             # OC_MACROAREA,
             # CAT,
             # DEN_REGIONE,
             # TIPO_REGIONALIZZAZIONE,
             # NOTE_REGIONALIZZAZIONE,
             # OC_AREA_OBIETTIVO_UE,
             OC_FLAG_MONITORAGGIO
             # DESCR_SETTORE_STRATEGICO_FSC,
             # DESCR_ASSE_TEMATICO_FSC,
             # COD_RA,
             # DESCR_RA,
             # AREA_TEMATICA_PSC,
             # SETTORE_INTERVENTO_PSC,
             # NOTE_TEMATIZZAZIONE,
             # NOTE
    ) %>%
    mutate(OC_DESCR_FONTE = "FSC",
           ADDENDUM = NA,
           TIPOLOGIA_STRUMENTO = NA,
           TIPO_DECISIONE = NA,
           NUMERO_DECISIONE = NA,
           DATA_DECISIONE = NA,
           OC_FLAG_ULTIMA_DECISIONE = NA,
           LINK_DECISIONE = NA,
           NOTE_DECISIONE = NA,
           OC_COD_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA = NA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_TITOLO_PROGETTO = NA,
           FINANZ_UE = NA,
           # FINANZ_FSC,
           FINANZ_ALTRO = NA,
           FINANZ_TOTALE_PUBBLICO = NA,
           # OC_MACROAREA,
           CAT = NA,
           DEN_REGIONE = NA,
           TIPO_REGIONALIZZAZIONE = NA,
           NOTE_REGIONALIZZAZIONE = NA,
           OC_AREA_OBIETTIVO_UE = NA,
           DESCR_SETTORE_STRATEGICO_FSC = NA,
           DESCR_ASSE_TEMATICO_FSC = NA,
           COD_RA = NA,
           DESCR_RA = NA,
           AREA_TEMATICA_PSC = NA,
           SETTORE_INTERVENTO_PSC = NA,
           NOTE_TEMATIZZAZIONE = NA,
           NOTE = NA) %>%
    left_join(memo, 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    select(OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           COD_ORIG,
           OC_DESCRIZIONE_PROGRAMMA,
           PSC,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           TIPOLOGIA_STRUMENTO,
           AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           ADDENDUM,
           OC_TIPOLOGIA_PROGRAMMA,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           OC_FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE,
           NOTE_DECISIONE,
           OC_COD_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_TITOLO_PROGETTO,
           FINANZ_UE,
           FINANZ_FSC,
           FINANZ_ALTRO,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA,
           CAT,
           DEN_REGIONE,
           TIPO_REGIONALIZZAZIONE,
           NOTE_REGIONALIZZAZIONE,
           OC_AREA_OBIETTIVO_UE,
           OC_FLAG_MONITORAGGIO,
           DESCR_SETTORE_STRATEGICO_FSC,
           DESCR_ASSE_TEMATICO_FSC,
           COD_RA,
           DESCR_RA,
           AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO_PSC,
           NOTE_TEMATIZZAZIONE,
           NOTE) %>%
    filter(!is.na(FINANZ_FSC))
  
  # chk
  programmi %>% count(CICLO_RISORSE)
  programmi %>%
    count(CICLO_PROGRAMMAZIONE, OC_TIPOLOGIA_PROGRAMMA)
  
  # converte in euro
  programmi <- programmi %>%
    mutate(FINANZ_UE = FINANZ_UE * 1000000,
           FINANZ_FSC = FINANZ_FSC * 1000000,
           FINANZ_ALTRO = FINANZ_ALTRO * 1000000,
           FINANZ_TOTALE_PUBBLICO = FINANZ_TOTALE_PUBBLICO * 1000000)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # gestione CIS
  
  # programmi_2 <- programmi
  # MEMO: il blocco per riposizionare i cis nei programmi originali non ha senso quando passiamo a psc
  # QUESTO VALE SOLO SE IL CIS E' IN UN PSC, SE RESTA COME PROGRAMMA SEPARATO VA FATTO!!!!!
  
  #MEMO: questo blocco ricodifica codice programma per i CIS ripristinando il codice nel programma originariamente previsto nel monitoraggio
  # prende i dati sempre da strumenti, che conserva con risorse 0 i programmi interamente sostituiti dai programmi fittizi CIS 
  # a valle rimangono più righe per alcuni programmi (ad es. cambia amministrazione titolare) che sono gestite con summarise da normali funzioni del package
  
  temp <- strumenti %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA, x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA, x_CICLO = CICLO_PROGRAMMAZIONE)
  
  programmi_2 <- programmi %>%
    # mutate(OC_CODICE_PROGRAMMA = if_else(is.na(COD_ORIG), OC_CODICE_PROGRAMMA, COD_ORIG)) %>%
    mutate(OC_CODICE_PROGRAMMA = case_when(!is.na(COD_ORIG) & PSC == "DEAD" ~ COD_ORIG, # modifica solo CIS fuori da PSC
                                           TRUE ~ OC_CODICE_PROGRAMMA)) %>% 
    left_join(temp, 
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(OC_DESCRIZIONE_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_DESCRIZIONE_PROGRAMMA, x_PROGRAMMA),
           OC_TIPOLOGIA_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_TIPOLOGIA_PROGRAMMA, x_GRUPPO),
           CICLO_PROGRAMMAZIONE = ifelse(is.na(COD_ORIG), CICLO_PROGRAMMAZIONE, x_CICLO)) 
  
  # test per verificare se restano missing su x_GRUPPO
  programmi_2 %>%
    filter(!is.na(COD_ORIG), is.na(OC_TIPOLOGIA_PROGRAMMA))
  
  # fix se test negativo
  # programmi_2 <- programmi_2 %>%
  #   # select(-x_PROGRAMMA, -x_GRUPPO) %>%
  #   left_join(octk::po_riclass %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_GRUPPO), 
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   mutate(OC_DESCRIZIONE_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_DESCRIZIONE_PROGRAMMA, if_else(is.na(x_PROGRAMMA.x), x_PROGRAMMA.y, x_PROGRAMMA.x)),
  #          OC_TIPOLOGIA_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_TIPOLOGIA_PROGRAMMA, if_else(is.na(x_GRUPPO.x), x_GRUPPO.y, x_GRUPPO.x))) %>%
  #   select(-COD_ORIG, -x_PROGRAMMA.x, -x_GRUPPO.x, -x_PROGRAMMA.y, -x_GRUPPO.y)
  
  
  
  
  
  # ----------------------------------------------------------------------------------- #
  # recupera programmi con risorse 0
  
  # MEMO: questi programmi servono nel DB per compatibilità con i dati di monitoraggio
  
  appo <- strumenti %>%
    filter(is.na(COD_ORIG)) %>%
    anti_join(programmi_2, by = c("OC_CODICE_PROGRAMMA")) %>%
    distinct(OC_CODICE_PROGRAMMA,
             COD_ORIG,
             OC_DESCRIZIONE_PROGRAMMA,
             PSC,
             CICLO_PROGRAMMAZIONE,
             # CICLO_RISORSE,
             # TIPOLOGIA_STRUMENTO,
             AMMINISTRAZIONE_TITOLARE,
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
             OC_TIPOLOGIA_PROGRAMMA,
             # ADDENDUM,
             # TIPO_DECISIONE,
             # NUMERO_DECISIONE,
             # DATA_DECISIONE,
             # OC_FLAG_ULTIMA_DECISIONE,
             # LINK_DECISIONE,
             # NOTE_DECISIONE,
             # OC_COD_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             # OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_TITOLO_PROGETTO,
             # FINANZ_UE,
             # FINANZ_FSC,
             # FINANZ_ALTRO,
             # FINANZ_TOTALE_PUBBLICO,
             # OC_MACROAREA,
             # CAT,
             # DEN_REGIONE,
             # TIPO_REGIONALIZZAZIONE,
             # NOTE_REGIONALIZZAZIONE,
             # OC_AREA_OBIETTIVO_UE,
             OC_FLAG_MONITORAGGIO
             # DESCR_SETTORE_STRATEGICO_FSC,
             # DESCR_ASSE_TEMATICO_FSC,
             # COD_RA,
             # DESCR_RA,
             # AREA_TEMATICA_PSC,
             # SETTORE_INTERVENTO_PSC,
             # NOTE_TEMATIZZAZIONE,
             # NOTE
    ) %>%
    mutate(OC_DESCR_FONTE = "FSC",
           ADDENDUM = NA,
           TIPOLOGIA_STRUMENTO = NA,
           TIPO_DECISIONE = NA,
           NUMERO_DECISIONE = NA,
           DATA_DECISIONE = NA,
           OC_FLAG_ULTIMA_DECISIONE = NA,
           LINK_DECISIONE = NA,
           NOTE_DECISIONE = NA,
           OC_COD_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA = NA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_TITOLO_PROGETTO = NA,
           FINANZ_UE = NA,
           # FINANZ_FSC,
           FINANZ_ALTRO = NA,
           FINANZ_TOTALE_PUBBLICO = NA,
           # OC_MACROAREA,
           CAT = NA,
           DEN_REGIONE = NA,
           TIPO_REGIONALIZZAZIONE = NA,
           NOTE_REGIONALIZZAZIONE = NA,
           OC_AREA_OBIETTIVO_UE = NA,
           DESCR_SETTORE_STRATEGICO_FSC = NA,
           DESCR_ASSE_TEMATICO_FSC = NA,
           COD_RA = NA,
           DESCR_RA = NA,
           AREA_TEMATICA_PSC = NA,
           SETTORE_INTERVENTO_PSC = NA,
           NOTE_TEMATIZZAZIONE = NA,
           NOTE = NA) %>%
    mutate(OC_MACROAREA = case_when(AMMINISTRAZIONE_TITOLARE == "Regione Abruzzo" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Basilicata" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Marche" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Piemonte" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Toscana" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Umbria" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Liguria" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Emilia-Romagna" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Calabria" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Campania" ~ "SUD",
                                    TRUE ~ "ND"),
           CICLO_RISORSE = CICLO_PROGRAMMAZIONE, 
           FINANZ_FSC = 0,
           CHK = "zeri") %>%
    select(OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           COD_ORIG,
           OC_DESCRIZIONE_PROGRAMMA,
           PSC,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           TIPOLOGIA_STRUMENTO,
           AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           ADDENDUM,
           OC_TIPOLOGIA_PROGRAMMA,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           OC_FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE,
           NOTE_DECISIONE,
           OC_COD_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_TITOLO_PROGETTO,
           FINANZ_UE,
           FINANZ_FSC,
           FINANZ_ALTRO,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA,
           CAT,
           DEN_REGIONE,
           TIPO_REGIONALIZZAZIONE,
           NOTE_REGIONALIZZAZIONE,
           OC_AREA_OBIETTIVO_UE,
           OC_FLAG_MONITORAGGIO,
           DESCR_SETTORE_STRATEGICO_FSC,
           DESCR_ASSE_TEMATICO_FSC,
           COD_RA,
           DESCR_RA,
           AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO_PSC,
           NOTE_TEMATIZZAZIONE,
           NOTE) 
  
  programmi_3 <- programmi_2 %>%
    bind_rows(appo) %>%
    select(-COD_ORIG, 
           -OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           -TIPO_REGIONALIZZAZIONE,
           -AREA_TEMATICA_PSC,
           -SETTORE_INTERVENTO_PSC, 
           # -x_PROGRAMMA,
           # -x_GRUPPO,
           # -x_CICLO
    ) %>%
    rename(TERZA_ARTICOLAZIONE = OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA) %>%
    mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC) %>%
    select(-FINANZ_FSC, -FINANZ_ALTRO)
  
  
  # ----------------------------------------------------------------------------------- #
  # elab articolazioni
  
  appo <- articolazioni %>% 
    select(OC_MACROAREA = `Macroarea`,
           PSC = `Regione/Amministrazione`,
           CICLO_PROGRAMMAZIONE = `Ciclo di riferimento`,
           OC_DESCR_ARTICOLAZ_PROGRAMMA = `Finalità di assegnazione`,
           AREA_TEMATICA_PSC = `Area tematica`,
           SETTORE_INTERVENTO_PSC = `Settore di intervento preliminare`,
           FINANZ_FSC = `Totale arrotondato`,
           SEZ_SPEC_1_COVID = `Sezione speciale 1: risorse FSC contrasto effetti COVID1`,                            
           SEZ_SPEC_2_FS = `Sezione speciale 2: risorse FSC copertura interventi ex fondi strutturali 2014-20202`) %>%
    mutate(OC_MACROAREA = case_when(OC_MACROAREA == "CN" ~ "CN",
                                    OC_MACROAREA == "MZ" ~ "SUD"))
  
  appo2 <- appo %>% 
    filter(!is.na(OC_DESCR_ARTICOLAZ_PROGRAMMA)) %>%
    mutate(AREA_TEMATICA_PSC = OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
    select(-FINANZ_FSC, -OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
    pivot_longer(cols = starts_with("SEZ_SPEC"), names_to = "OC_DESCR_ARTICOLAZ_PROGRAMMA", values_to = "FINANZ_FSC")
  
  
  articolazioni_2 <- appo %>% 
    filter(is.na(OC_DESCR_ARTICOLAZ_PROGRAMMA)) %>%
    select(-SEZ_SPEC_1_COVID, -SEZ_SPEC_2_FS) %>%
    mutate(OC_DESCR_ARTICOLAZ_PROGRAMMA = "SEZ_ORD") %>%
    bind_rows(appo2) %>%
    mutate(OC_FLAG_MONITORAGGIO = 1)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # gestione PSC
  
  # totali psc
  # MEMO: filtro i PSC tranne quelli che non sono in articolazioni:
  # - act
  # - righe per compensazione di tagli eccessivi CSR (che non sono presenti in articolazioni)
  # - psc metropolitani
  if (psc_cm == FALSE) {
    appo_psc <- programmi_3 %>%
      filter(PSC != "DEAD", PSC != "ACT", OC_TIPOLOGIA_PROGRAMMA != "CSR", 
             # scarta PSC delle città metro
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE != "METROPOLITANA") %>%
      mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC)),
             OC_DESCRIZIONE_PROGRAMMA = paste0("PSC ", AMMINISTRAZIONE_TITOLARE),
             OC_TIPOLOGIA_PROGRAMMA = "PSC") %>%
      select(-PSC)
    
    appo_altro <- programmi_3 %>%
      # filter(PSC == "DEAD" | PSC == "ACT" | OC_TIPOLOGIA_PROGRAMMA == "CSR" | TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA") %>%
      # tiene PSC delle città metro come patti
      filter(PSC == "DEAD" | PSC == "ACT" | TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA") %>%
      select(-PSC)
    
  } else {
    appo_psc <- programmi_3 %>%
      filter(PSC != "DEAD", PSC != "ACT", OC_TIPOLOGIA_PROGRAMMA != "CSR") %>%
      mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC)),
             OC_DESCRIZIONE_PROGRAMMA = paste0("PSC ", AMMINISTRAZIONE_TITOLARE),
             OC_TIPOLOGIA_PROGRAMMA = "PSC") %>%
      select(-PSC)
    
    appo_altro <- programmi_3 %>%
      filter(PSC == "DEAD" | PSC == "ACT") %>%
      select(-PSC)
  }
  
  # MEMO;: il join per OC_FLAG_MONITORAGGIO è solo per controllo (es. compensazioni ambientali campania)
  
  # controllo totali psc vs articolazioni psc
  chk <- appo_psc %>%
    group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    mutate(RISORSE = round(RISORSE/1000000, 2)) %>%
    filter(abs(RISORSE) > 0) %>%
    # MEMO: qui usavo full_join ma devo mettere left_join perché altrimenti mi porta dietro sempre le città metropolitate
    # full_join(articolazioni_2 %>%
    left_join(articolazioni_2 %>%
                mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC))) %>%
                group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
                summarise(RISORSE = sum(FINANZ_FSC, na.rm = TRUE)) %>%
                mutate(RISORSE = round(RISORSE, 2)) %>%
                filter((abs(RISORSE) > 0)),
              by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO")) %>%
    mutate(CHK = RISORSE.x - RISORSE.y)
  
  # integra articolazioni psc
  appo_psc_2 <- appo_psc %>%
    mutate(CICLO_RISORSE = NA) %>% #MEMO: si azzera by def per funzionamento psc (non è nota per articolazioni)
    # select(-FINANZ_TOTALE_PUBBLICO, -OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
    select("OC_CODICE_PROGRAMMA", "OC_DESCRIZIONE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO",
           "OC_DESCR_FONTE", "AMMINISTRAZIONE_TITOLARE",	"TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE",	"OC_TIPOLOGIA_PROGRAMMA") %>%
    distinct() %>%
    # MEMO: qui usavo full_join ma devo mettere left_join perché altrimenti mi porta dietro sempre le città metropolitate
    # full_join(articolazioni_2 %>%
    left_join(articolazioni_2 %>%
                rename(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC) %>%
                mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_TOTALE_PUBBLICO * 1000000) %>%
                mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC))) %>%
                filter((abs(FINANZ_TOTALE_PUBBLICO) > 0)),
              by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO"))
  
  
  # chk finanziario
  # MEMO: attenzione perché qui perdo completamente decimali che ho troncato in articolazioni per fare i psc
  chk <- appo_psc %>%
    group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    mutate(RISORSE = round(RISORSE/1000000, 2)) %>%
    full_join(appo_psc_2 %>%
                group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO/1000000, na.rm = TRUE)) %>%
                mutate(RISORSE = round(RISORSE, 2)),
              by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO")) %>%
    mutate(CHK = RISORSE.x - RISORSE.y)
  
  appo_csr <- programmi_3 %>%
    filter(OC_TIPOLOGIA_PROGRAMMA == "CSR") %>%
    mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC)),
           OC_DESCRIZIONE_PROGRAMMA = paste0("PSC ", AMMINISTRAZIONE_TITOLARE),
           OC_DESCR_ARTICOLAZ_PROGRAMMA = OC_TIPOLOGIA_PROGRAMMA,
           OC_TIPOLOGIA_PROGRAMMA = "PSC",
           CICLO_RISORSE = NA,
           AREA_TEMATICA_PSC = "Risorse da compensazioni CSR") 
  # MEMO: queste sono gestite a parte perché assenti in "articolazioni" ma entrano in PSC
  
  # appo_altro <- programmi_3 %>%
  #   # filter(PSC == "DEAD" | PSC == "ACT" | OC_TIPOLOGIA_PROGRAMMA == "CSR" | TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA") %>%
  #   filter(PSC == "DEAD" | PSC == "ACT" | TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA") %>%
  #   select(-PSC)
  
  programmi_4 <- appo_psc_2 %>%
    bind_rows(appo_csr) %>% 
    bind_rows(appo_altro)
  
  
  # ----------------------------------------------------------------------------------- #
  # export programmi
  
  # MEMO: non ha più senso distinzione per ciclo perché diventano tutti programmi 2014-2020
  # CHK: verifica se CICLO_PROGRAMMAZIONE viene sovrascritto all'import del del DB (sembra di no...), altrimenti usiamo ciclo risorse 
  # MEMO: CICLO_RISORSE resta NA perché non è disponbile per articolazioni
  
  
  # programmi_1420 <- programmi_4 %>%
  #   select(-ADDENDUM, -TIPOLOGIA_STRUMENTO, -PSC) %>%
  #   # patch
  #   mutate(FINANZ_FSC = FINANZ_TOTALE_PUBBLICO)
  
  
  programmi <- programmi_4 %>%
    # MEMO: per PSC ho solo ciclo strategia di provenienza e lo duplico su risorse per non lasciare il vuoto
    mutate(CICLO_RISORSE = case_when(is.na(CICLO_RISORSE) & OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ CICLO_PROGRAMMAZIONE,
                                     TRUE ~ CICLO_RISORSE)) %>%
    mutate(FINANZ_UE = 0,
           FINANZ_ALTRO = 0,
           COD_OBIETTIVO_TEMATICO = NA,
           DESCR_OBIETTIVO_TEMATICO = NA,
           COD_RISULTATO_ATTESO = NA,
           DESCR_RISULTATO_ATTESO = NA,
           COD_AREA_TEMATICA_PSC = substr(AREA_TEMATICA_PSC, 1, 2),
           COD_SETTORE_INTERVENTO_PSC = substr(SETTORE_INTERVENTO_PSC, 1, 5)) %>%
    select(AMBITO	= OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           DESCRIZIONE_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           AMMINISTRAZIONE = AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_AMMINISTRAZIONE = TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_PROGRAMMA = OC_TIPOLOGIA_PROGRAMMA,
           COD_LIVELLO_1 = OC_COD_ARTICOLAZ_PROGRAMMA,
           DESCR_LIVELLO_1 = OC_DESCR_ARTICOLAZ_PROGRAMMA,
           FINANZ_UE,
           FINANZ_ALTRO,
           FINANZ_TOTALE =FINANZ_TOTALE_PUBBLICO,
           MACROAREA = OC_MACROAREA,
           CAT_REGIONE = CAT,
           DEN_REGIONE,
           COD_OBIETTIVO_TEMATICO,
           DESCR_OBIETTIVO_TEMATICO,
           COD_RISULTATO_ATTESO,
           DESCR_RISULTATO_ATTESO,
           COD_AREA_TEMATICA_PSC,
           DESCR_AREA_TEMATICA_PSC = AREA_TEMATICA_PSC,
           COD_SETTORE_INTERVENTO_PSC,
           DESCR_SETTORE_INTERVENTO_PSC = SETTORE_INTERVENTO_PSC,
           FLAG_MONITORAGGIO = OC_FLAG_MONITORAGGIO,
           NOTE,
           PSC_TEMP = PSC)
  
  
  # fix codici 15 char
  # programmi <- programmi %>% 
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
  #                                          OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
  #                                          OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  programmi <- fix_id_psc_15_digit(programmi, "OC_CODICE_PROGRAMMA")
  
  # fix nomi ministeri
  programmi <- fix_id_psc_ministeri(programmi, "DESCRIZIONE_PROGRAMMA")
  
  
  # export
  if (export == TRUE) {
    write.csv2(programmi, file.path(TEMP, "dati_fsc_psc.csv"), row.names = FALSE, na = "")
  }
  
  
  # export xls
  if (export_xls == TRUE) {

    # xls
    require("openxlsx") 

    #1420
    programmi_1420 <- programmi %>%
      filter(!is.na(PSC_TEMP) | CICLO_PROGRAMMAZIONE == "2014-2020") %>%
      select(-PSC_TEMP)
    
    # write.csv2(programmi_1420, file.path(TEMP, "DBPROG_FSC1420.csv"))
    
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = programmi_1420, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "Dati_DBCOE_FSC1420.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "Dati_DBCOE_FSC1420.xlsx"), to = file.path(DB, "Dati_DBCOE_FSC1420.xlsx"), overwrite = TRUE)
    
    # chk
    chk <- load_db("2014-2020", "FSC", use_ciclo = T)
    sum(chk$FINANZ_TOTALE, na.rm = T) - sum(programmi_1420$FINANZ_TOTALE, na.rm = T)
    chk %>% count(x_CICLO, CICLO_PROGRAMMAZIONE, CICLO_RISORSE)

    
    #713
    programmi_713 <- programmi %>%
      filter(is.na(PSC_TEMP), CICLO_PROGRAMMAZIONE == "2007-2013")  %>%
      select(-PSC_TEMP)
    
    # write.csv2(programmi_713, file.path(TEMP, "DBPROG_FSC0713.csv"))
    
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = programmi_713, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "Dati_DBCOE_FSC0713.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "Dati_DBCOE_FSC0713.xlsx"), to = file.path(DB, "Dati_DBCOE_FSC0713.xlsx"), overwrite = TRUE)
    
    # chk
    chk <- load_db("2007-2013", "FSC", use_ciclo = T)
    sum(chk$FINANZ_TOTALE, na.rm = T) - sum(programmi_713$FINANZ_TOTALE, na.rm = T)
    chk %>% count(x_CICLO, CICLO_PROGRAMMAZIONE, CICLO_RISORSE)

    # MEMO: il ciclo 06 è interamente riassorbito nei PSC
    # #06
    # programmi_06 <- programmi %>%
    #   filter(CICLO_PROGRAMMAZIONE == "2000-2006")
    # 
    # # write.csv2(programmi_713, file.path(TEMP, "DBPROG_FSC0713.csv"))
    # 
    # wb <- createWorkbook()
    # addWorksheet(wb, "FSC")
    # writeData(wb, sheet = "FSC", x = programmi_06, startCol = 1, startRow = 1, colNames = TRUE)
    # saveWorkbook(wb, file = file.path(OUTPUT, "20210331", "DBPROG_FSC0006.xlsx"), overwrite = TRUE)
    # 
    # file.copy(from = file.path(OUTPUT, "20210331", "DBCOE_FSC0006.xlsx"), to = file.path(DB, "DBPROG_FSC0006.xlsx"), overwrite = TRUE)
    
  }
  
  return(programmi)

}






#' Crea file matrice PO-PSC per DBCOE
#'
#' Crea file con matrice PO-PSC per DBCOE, contenente i programmi originari confluiti nei PSC
#' 
#' @param file_evo Nome del file "assegnazioni_evo_XXXXXXXX.XX.xlsx"
#' @param psc_cm Logico. Vuoi convertire in PSC anche i Patti delle Città metropolitane?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx nel DBCOE?
#' @return File con matrice PO-PSC per DBCOE per i cicli 2007-2013 e 201-2020. I dati per programma originario hanno valori al netto delle riprogrammazioni ex art. 44. Le risorse oggetto di riprogrammazione e le risorse aggiuntive parte della strategia anti COVID sono temporaneamente riportate in programmi fittizi
#' @note Salva nella versione del DBCOE risultante come DB da __oc_init__.
setup_dbcoe_dati_fsc_popsc <- function(file_evo, psc_cm=FALSE, export=FALSE, export_xls=FALSE) {
  
  # DA FINIRE
  # file_evo <- "assegnazioni_evo_20210803.00.xlsx"
  
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  strumenti <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", file_evo), sheet = "strumenti")
  # articolazioni <- read_xlsx(file.path(INPUT, "PSC_articolazione_tematiche_chk_decimali_v.05.xlsx"), sheet = "articolazioni")
  
  
  # ----------------------------------------------------------------------------------- #
  # clean strumenti
  
  strumenti <- strumenti %>%
    rename(OC_CODICE_PROGRAMMA = `Codice strumento`,
           # `Tipo codice strumento`,
           COD_ORIG = `Codice strumento originario`,
           CICLO_PROGRAMMAZIONE = `Ciclo di programmazione per strategia`, 
           OC_DESCRIZIONE_PROGRAMMA = `Descrizione strumento`,
           # PSC,
           OC_MACROTIPOLOGIA_PROGRAMMA = `Macro-tipologia strumento`,
           OC_TIPOLOGIA_PROGRAMMA = `Tipologia strumento`,
           AMMINISTRAZIONE_TITOLARE = `Amministrazione titolare`,
           # `Soggetto attuatore`,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE = `Tipologia di amministrazione titolare`,
           # `Label tavola informativa CIPE 05/2020`,
           # `Label tavola informativa CIPE 12/2019`,
           # `A cavallo di cicli`,
           # `Valore considerato per CIPE 05/2020`,
           # `Valore considerato per CIPE 05/2020 – Al netto di 2019`,
           RIS_PRE44_TOT = `Risorse FSC assegnate ante 44– Totale`,
           RIS_PRE44_0006 = `Risorse FSC assegnate ante 44 – 2000-2006`,
           RIS_PRE44_0713 = `Risorse FSC assegnate ante 44 – 2007-2013`,
           RIS_PRE44_1420 = `Risorse FSC assegnate ante 44 – 2014-2020`,
           RIS_44_NO_MONIT = `Art 44 – Tagli da economie (col H)`,
           RIS_44_TAGLI_7B = `Art 44 – Tagli da progetti 7.b (col. M)`,
           RIS_44_7A = `Art 44 – Progetti 7.a a dicembre 2019`,
           RIS_44_7B = `Art 44- Progetti e risorse in 7.b (calcolo)`,
           RIS_44_NO_VAL = `Art 44 – Risorse confermate senza valutazione`,
           RIS_TOT = `Risorse FSC post 44 – Totale`,
           RIS_0006 = `Risorse FSC post 44 – 2000-2006`,
           RIS_0713 = `Risorse FSC post 44 – 2007-2013`,
           RIS_1420 = `Risorse FSC post 44 – 2014-2020`,
           RIS_SUD = `Risorse FSC post 44 – Mezzogiorno`,
           RIS_CN = `Risorse FSC post 44 – Centro-Nord`,
           RIS_ND = `Risorse FSC post 44 – Non ripartite`,
           RIS_PSC_ORD = `Risorse FSC post 44 – confermate in PSC o altro`,
           RIS_PSC_SEZ_COVID = `Risorse FSC post 44 – sezione speciale COVID (art. 241)`,
           RIS_PSC_SEZ_POR = `Risorse FSC post 44 – sezione speciale copertura POR (art. 242)`,
           # FLAG_COESIONE = `Flag perimetro Coesione`,
           DELIBERE = `Delibere CIPE e Norme`,
           # NOTE = `Note su Risorse`,
           LISTA_INT  = `Lista interventi programmati`,
           # OC_FLAG_MONITORAGGIO = `Flag monitoraggio`
           OC_FLAG_MONITORAGGIO = `Flag perimetro Coesione`)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # elab programmi
  
  # MEMO: il flusso considera solo strumenti con risorse diverse da 0
  # alcuni strumenti con risorse 0 sono recuperati alla fine
  
  
  # po <- "2017POAMBIENFSC"
  
  temp_macro <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_SUD, RIS_CN, RIS_ND) %>%
    gather(key = "OC_MACROAREA", value = "FINANZ_FSC", RIS_SUD, RIS_CN, RIS_ND) %>%
    filter(abs(FINANZ_FSC) > 0) %>%
    mutate(OC_MACROAREA = case_when(OC_MACROAREA == "RIS_SUD" ~ "SUD", 
                                    OC_MACROAREA == "RIS_CN" ~ "CN", 
                                    OC_MACROAREA == "RIS_ND" ~ "ND"))
  
  temp_ciclo <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_0006, RIS_0713, RIS_1420) %>%
    gather(key = "CICLO_RISORSE", value = "FINANZ_FSC", RIS_0006, RIS_0713, RIS_1420)%>%
    filter(abs(FINANZ_FSC) > 0) %>%
    mutate(CICLO_RISORSE = case_when(CICLO_RISORSE == "RIS_0006" ~ "2000-2006",
                                     CICLO_RISORSE == "RIS_0713" ~ "2007-2013",
                                     CICLO_RISORSE == "RIS_1420" ~ "2014-2020"))
  
  appo <- strumenti %>%
    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_TOT, RIS_SUD, RIS_CN, RIS_ND, RIS_0006, RIS_0713, RIS_1420) %>%
    left_join(temp_macro  %>%
                count(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE) %>%
                rename(N_AREE = n), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    left_join(temp_ciclo %>%
                count(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE) %>%
                rename(N_CICLI = n), 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    # filter(OC_CODICE_PROGRAMMA == po) %>%
    mutate(CHK = case_when(N_AREE > 1 & N_CICLI == 1 ~ "ciclo_unico",
                           N_AREE == 1 & N_CICLI > 1 ~ "area_unica",
                           N_AREE == 1 & N_CICLI == 1 ~ "tutto_unico",
                           is.na(N_AREE) & is.na(N_CICLI) ~ "vuoto",
                           is.na(N_AREE) ~ "vuoto",
                           is.na(N_CICLI) ~ "vuoto",
                           TRUE ~ "chk"))
  
  appo %>% count(CHK)
  # CHK             n
  # <chr>       <int>
  # 1 area_unica     32
  # 2 chk             1
  # 3 ciclo_unico    35
  # 4 tutto_unico   308
  # 5 vuoto          32
  
  # traspone macroaree e cicli contabili
  memo <- tibble()
  
  for (i in seq(1, dim(appo)[1])) {
    po <- as.character(appo[i, "OC_CODICE_PROGRAMMA"])
    print(po)
    
    chk <- as.character(appo[i, "CHK"])
    
    if (chk == "tutto_unico") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(-FINANZ_FSC) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "area_unica") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(-FINANZ_FSC) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "ciclo_unico") {
      temp <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po)  %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po) %>%
                    select(-FINANZ_FSC), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(CHK = chk)
      
    } else if (chk == "vuoto") {
      temp <- tibble(OC_CODICE_PROGRAMMA = po,
                     CHK = chk)
      
    } else if (chk == "chk") {
      # patch per casi con split su ciclo e macroarea
      
      appo %>%
        filter(OC_CODICE_PROGRAMMA == po) %>%
        select(RIS_TOT)
      
      
      temp0 <- temp_macro %>% 
        filter(OC_CODICE_PROGRAMMA == po) %>%
        left_join(appo %>%
                    filter(OC_CODICE_PROGRAMMA == po) %>%
                    select(OC_CODICE_PROGRAMMA, CICLO_PROGRAMMAZIONE, RIS_TOT), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(X = FINANZ_FSC / RIS_TOT) %>%
        select(-FINANZ_FSC, -RIS_TOT) %>%
        left_join(temp_ciclo %>% 
                    filter(OC_CODICE_PROGRAMMA == po), 
                  by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
        mutate(FINANZ_FSC_NEW = FINANZ_FSC * X)
      
      test <- sum(temp0$FINANZ_FSC_NEW) == as.numeric(appo[appo$OC_CODICE_PROGRAMMA == po, "RIS_TOT"])
      
      if (test == TRUE) {
        temp <- temp0 %>%
          select(OC_CODICE_PROGRAMMA,
                 CICLO_PROGRAMMAZIONE,
                 OC_MACROAREA,
                 FINANZ_FSC = FINANZ_FSC_NEW,
                 CICLO_RISORSE)
        
      } else {
        message("ATTENZIONE!!!")
        temp <- tibble(OC_CODICE_PROGRAMMA = po,
                       CHK = chk)
      }
    }
    
    #MEMO: lascio fuori i casi "vuoti"
    
    memo <- memo %>%
      bind_rows(temp)
    
  }
  
  
  
  
  # clean per export
  programmi <- strumenti %>%
    distinct(OC_CODICE_PROGRAMMA,
             COD_ORIG,
             OC_DESCRIZIONE_PROGRAMMA,
             CICLO_PROGRAMMAZIONE,
             # CICLO_RISORSE,
             # TIPOLOGIA_STRUMENTO,
             AMMINISTRAZIONE_TITOLARE,
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
             OC_TIPOLOGIA_PROGRAMMA,
             PSC,
             # ADDENDUM,
             # TIPO_DECISIONE,
             # NUMERO_DECISIONE,
             # DATA_DECISIONE,
             # OC_FLAG_ULTIMA_DECISIONE,
             # LINK_DECISIONE,
             # NOTE_DECISIONE,
             # OC_COD_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             # OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_TITOLO_PROGETTO,
             # FINANZ_UE,
             # FINANZ_FSC,
             # FINANZ_ALTRO,
             # FINANZ_TOTALE_PUBBLICO,
             # OC_MACROAREA,
             # CAT,
             # DEN_REGIONE,
             # TIPO_REGIONALIZZAZIONE,
             # NOTE_REGIONALIZZAZIONE,
             # OC_AREA_OBIETTIVO_UE,
             OC_FLAG_MONITORAGGIO
             # DESCR_SETTORE_STRATEGICO_FSC,
             # DESCR_ASSE_TEMATICO_FSC,
             # COD_RA,
             # DESCR_RA,
             # AREA_TEMATICA_PSC,
             # SETTORE_INTERVENTO_PSC,
             # NOTE_TEMATIZZAZIONE,
             # NOTE
    ) %>%
    mutate(OC_DESCR_FONTE = "FSC",
           ADDENDUM = NA,
           TIPOLOGIA_STRUMENTO = NA,
           TIPO_DECISIONE = NA,
           NUMERO_DECISIONE = NA,
           DATA_DECISIONE = NA,
           OC_FLAG_ULTIMA_DECISIONE = NA,
           LINK_DECISIONE = NA,
           NOTE_DECISIONE = NA,
           OC_COD_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA = NA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_TITOLO_PROGETTO = NA,
           FINANZ_UE = NA,
           # FINANZ_FSC,
           FINANZ_ALTRO = NA,
           FINANZ_TOTALE_PUBBLICO = NA,
           # OC_MACROAREA,
           CAT = NA,
           DEN_REGIONE = NA,
           TIPO_REGIONALIZZAZIONE = NA,
           NOTE_REGIONALIZZAZIONE = NA,
           OC_AREA_OBIETTIVO_UE = NA,
           DESCR_SETTORE_STRATEGICO_FSC = NA,
           DESCR_ASSE_TEMATICO_FSC = NA,
           COD_RA = NA,
           DESCR_RA = NA,
           AREA_TEMATICA_PSC = NA,
           SETTORE_INTERVENTO_PSC = NA,
           NOTE_TEMATIZZAZIONE = NA,
           NOTE = NA) %>%
    left_join(memo, 
              by = c("OC_CODICE_PROGRAMMA", "CICLO_PROGRAMMAZIONE")) %>%
    select(OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           COD_ORIG,
           OC_DESCRIZIONE_PROGRAMMA,
           PSC,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           TIPOLOGIA_STRUMENTO,
           AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           ADDENDUM,
           OC_TIPOLOGIA_PROGRAMMA,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           OC_FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE,
           NOTE_DECISIONE,
           OC_COD_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_TITOLO_PROGETTO,
           FINANZ_UE,
           FINANZ_FSC,
           FINANZ_ALTRO,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA,
           CAT,
           DEN_REGIONE,
           TIPO_REGIONALIZZAZIONE,
           NOTE_REGIONALIZZAZIONE,
           OC_AREA_OBIETTIVO_UE,
           OC_FLAG_MONITORAGGIO,
           DESCR_SETTORE_STRATEGICO_FSC,
           DESCR_ASSE_TEMATICO_FSC,
           COD_RA,
           DESCR_RA,
           AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO_PSC,
           NOTE_TEMATIZZAZIONE,
           NOTE) %>%
    filter(!is.na(FINANZ_FSC))
  
  # chk
  programmi %>% count(CICLO_RISORSE)
  programmi %>%
    count(CICLO_PROGRAMMAZIONE, OC_TIPOLOGIA_PROGRAMMA)
  
  # converte in euro
  programmi <- programmi %>%
    mutate(FINANZ_UE = FINANZ_UE * 1000000,
           FINANZ_FSC = FINANZ_FSC * 1000000,
           FINANZ_ALTRO = FINANZ_ALTRO * 1000000,
           FINANZ_TOTALE_PUBBLICO = FINANZ_TOTALE_PUBBLICO * 1000000)
  
  
  
  # ----------------------------------------------------------------------------------- #
  # gestione CIS
  
  #MEMO: questo blocco ricodifica codice programma per i CIS ripristinando il codice nel programma originariamente previsto nel monitoraggio
  # prende i dati sempre da strumenti, che conserva con risorse 0 i programmi interamente sostituiti dai programmi fittizi CIS 
  # a valle rimangono più righe per alcuni programmi (ad es. cambia amministrazione titolare) che sono gestite con summarise da normali funzioni del package
  
  # PROVA ABOLITA
  # fix per CIS Taranto in PRA Puglia
  # strumenti <- strumenti %>% 
  #   mutate(OC_CODICE_PROGRAMMA = if_else(OC_CODICE_PROGRAMMA == "CIS_TA_PUG", "2007PU001FA010", OC_CODICE_PROGRAMMA),
  #          OC_DESCRIZIONE_PROGRAMMA = if_else(OC_CODICE_PROGRAMMA == "2007PU001FA010", , OC_DESCRIZIONE_PROGRAMMA)
  # MEMO: crea versione duplicata del PRA Puglia, con risorse su due cicli 
  
  temp <- strumenti %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA, x_GRUPPO = OC_TIPOLOGIA_PROGRAMMA, x_CICLO = CICLO_PROGRAMMAZIONE)
  
  # programmi_2 <- programmi %>%
  #   mutate(OC_CODICE_PROGRAMMA = if_else(is.na(COD_ORIG), OC_CODICE_PROGRAMMA, COD_ORIG)) %>%
  #   left_join(temp, 
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   mutate(OC_DESCRIZIONE_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_DESCRIZIONE_PROGRAMMA, x_PROGRAMMA),
  #          OC_TIPOLOGIA_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_TIPOLOGIA_PROGRAMMA, x_GRUPPO),
  #          CICLO_PROGRAMMAZIONE = ifelse(is.na(COD_ORIG), CICLO_PROGRAMMAZIONE, x_CICLO)) 
  
  programmi_2 <- programmi %>%
    mutate(CICLO_APPO = ifelse(OC_CODICE_PROGRAMMA == "CIS_TA_PUG", "2014-2020", NA)) %>% 
    mutate(OC_CODICE_PROGRAMMA = if_else(is.na(COD_ORIG), OC_CODICE_PROGRAMMA, COD_ORIG)) %>%
    left_join(temp, 
              by = "OC_CODICE_PROGRAMMA") %>%
    mutate(OC_DESCRIZIONE_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_DESCRIZIONE_PROGRAMMA, x_PROGRAMMA),
           OC_TIPOLOGIA_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_TIPOLOGIA_PROGRAMMA, x_GRUPPO),
           CICLO_PROGRAMMAZIONE = case_when(!is.na(CICLO_APPO) ~ CICLO_APPO,
                                            is.na(COD_ORIG) ~ CICLO_PROGRAMMAZIONE,
                                            TRUE ~ x_CICLO)) %>% 
    select(-CICLO_APPO)  
    # MEMO:
    # questa nuova verisone introduce fix per CIS Taranto in PRA Puglia, che era già separato in strumenti
    # in pratica crea versione duplicata del PRA Puglia, con risorse su due cicli 
    
    
  # test per verificare se restano missing su x_GRUPPO
  programmi_2 %>%
    filter(!is.na(COD_ORIG), is.na(OC_TIPOLOGIA_PROGRAMMA))
  
  # fix se test negativo
  # programmi_2 <- programmi_2 %>%
  #   # select(-x_PROGRAMMA, -x_GRUPPO) %>%
  #   left_join(octk::po_riclass %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_GRUPPO), 
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   mutate(OC_DESCRIZIONE_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_DESCRIZIONE_PROGRAMMA, if_else(is.na(x_PROGRAMMA.x), x_PROGRAMMA.y, x_PROGRAMMA.x)),
  #          OC_TIPOLOGIA_PROGRAMMA = ifelse(is.na(COD_ORIG), OC_TIPOLOGIA_PROGRAMMA, if_else(is.na(x_GRUPPO.x), x_GRUPPO.y, x_GRUPPO.x))) %>%
  #   select(-COD_ORIG, -x_PROGRAMMA.x, -x_GRUPPO.x, -x_PROGRAMMA.y, -x_GRUPPO.y)
  
  
  
  
  # ----------------------------------------------------------------------------------- #
  # recupera programmi con risorse 0
  
  # MEMO: questi programmi servono nel DB per compatibilità con i dati di monitoraggio
  
  appo <- strumenti %>%
    filter(is.na(COD_ORIG)) %>%
    anti_join(programmi_2, by = c("OC_CODICE_PROGRAMMA")) %>%
    distinct(OC_CODICE_PROGRAMMA,
             COD_ORIG,
             OC_DESCRIZIONE_PROGRAMMA,
             PSC,
             CICLO_PROGRAMMAZIONE,
             # CICLO_RISORSE,
             # TIPOLOGIA_STRUMENTO,
             AMMINISTRAZIONE_TITOLARE,
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
             OC_TIPOLOGIA_PROGRAMMA,
             # ADDENDUM,
             # TIPO_DECISIONE,
             # NUMERO_DECISIONE,
             # DATA_DECISIONE,
             # OC_FLAG_ULTIMA_DECISIONE,
             # LINK_DECISIONE,
             # NOTE_DECISIONE,
             # OC_COD_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
             # OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
             # OC_TITOLO_PROGETTO,
             # FINANZ_UE,
             # FINANZ_FSC,
             # FINANZ_ALTRO,
             # FINANZ_TOTALE_PUBBLICO,
             # OC_MACROAREA,
             # CAT,
             # DEN_REGIONE,
             # TIPO_REGIONALIZZAZIONE,
             # NOTE_REGIONALIZZAZIONE,
             # OC_AREA_OBIETTIVO_UE,
             OC_FLAG_MONITORAGGIO
             # DESCR_SETTORE_STRATEGICO_FSC,
             # DESCR_ASSE_TEMATICO_FSC,
             # COD_RA,
             # DESCR_RA,
             # AREA_TEMATICA_PSC,
             # SETTORE_INTERVENTO_PSC,
             # NOTE_TEMATIZZAZIONE,
             # NOTE
    ) %>%
    mutate(OC_DESCR_FONTE = "FSC",
           ADDENDUM = NA,
           TIPOLOGIA_STRUMENTO = NA,
           TIPO_DECISIONE = NA,
           NUMERO_DECISIONE = NA,
           DATA_DECISIONE = NA,
           OC_FLAG_ULTIMA_DECISIONE = NA,
           LINK_DECISIONE = NA,
           NOTE_DECISIONE = NA,
           OC_COD_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA = NA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA = NA,
           OC_TITOLO_PROGETTO = NA,
           FINANZ_UE = NA,
           # FINANZ_FSC,
           FINANZ_ALTRO = NA,
           FINANZ_TOTALE_PUBBLICO = NA,
           # OC_MACROAREA,
           CAT = NA,
           DEN_REGIONE = NA,
           TIPO_REGIONALIZZAZIONE = NA,
           NOTE_REGIONALIZZAZIONE = NA,
           OC_AREA_OBIETTIVO_UE = NA,
           DESCR_SETTORE_STRATEGICO_FSC = NA,
           DESCR_ASSE_TEMATICO_FSC = NA,
           COD_RA = NA,
           DESCR_RA = NA,
           AREA_TEMATICA_PSC = NA,
           SETTORE_INTERVENTO_PSC = NA,
           NOTE_TEMATIZZAZIONE = NA,
           NOTE = NA) %>%
    mutate(OC_MACROAREA = case_when(AMMINISTRAZIONE_TITOLARE == "Regione Abruzzo" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Basilicata" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Marche" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Piemonte" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Toscana" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Umbria" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Liguria" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Emilia-Romagna" ~ "CN",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Calabria" ~ "SUD",
                                    AMMINISTRAZIONE_TITOLARE == "Regione Campania" ~ "SUD",
                                    TRUE ~ "ND"),
           CICLO_RISORSE = CICLO_PROGRAMMAZIONE, 
           FINANZ_FSC = 0,
           CHK = "zeri") %>%
    select(OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           COD_ORIG,
           OC_DESCRIZIONE_PROGRAMMA,
           PSC,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           TIPOLOGIA_STRUMENTO,
           AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           ADDENDUM,
           OC_TIPOLOGIA_PROGRAMMA,
           TIPO_DECISIONE,
           NUMERO_DECISIONE,
           DATA_DECISIONE,
           OC_FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE,
           NOTE_DECISIONE,
           OC_COD_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_SUBARTICOLAZ_PROGRAMMA,
           OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA,
           OC_TITOLO_PROGETTO,
           FINANZ_UE,
           FINANZ_FSC,
           FINANZ_ALTRO,
           FINANZ_TOTALE_PUBBLICO,
           OC_MACROAREA,
           CAT,
           DEN_REGIONE,
           TIPO_REGIONALIZZAZIONE,
           NOTE_REGIONALIZZAZIONE,
           OC_AREA_OBIETTIVO_UE,
           OC_FLAG_MONITORAGGIO,
           DESCR_SETTORE_STRATEGICO_FSC,
           DESCR_ASSE_TEMATICO_FSC,
           COD_RA,
           DESCR_RA,
           AREA_TEMATICA_PSC,
           SETTORE_INTERVENTO_PSC,
           NOTE_TEMATIZZAZIONE,
           NOTE) 
  
  programmi_3 <- programmi_2 %>%
    bind_rows(appo) %>%
    select(-COD_ORIG, 
           -OC_COD_TERZA_ARTICOLAZ_PROGRAMMA,
           -TIPO_REGIONALIZZAZIONE,
           -AREA_TEMATICA_PSC,
           -SETTORE_INTERVENTO_PSC, 
           # -x_PROGRAMMA,
           # -x_GRUPPO,
           # -x_CICLO
    ) %>%
    rename(TERZA_ARTICOLAZIONE = OC_DESCR_TERZA_ARTICOLAZ_PROGRAMMA) %>%
    mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC) %>%
    select(-FINANZ_FSC, -FINANZ_ALTRO)
  
  
  # ----------------------------------------------------------------------------------- #
  # elab articolazioni
  # 
  # appo <- articolazioni %>% 
  #   select(OC_MACROAREA = `Macroarea`,
  #          PSC = `Regione/Amministrazione`,
  #          CICLO_PROGRAMMAZIONE = `Ciclo di riferimento`,
  #          OC_DESCR_ARTICOLAZ_PROGRAMMA = `Finalità di assegnazione`,
  #          AREA_TEMATICA_PSC = `Area tematica`,
  #          SETTORE_INTERVENTO_PSC = `Settore di intervento preliminare`,
  #          FINANZ_FSC = `Totale arrotondato`,
  #          SEZ_SPEC_1_COVID = `Sezione speciale 1: risorse FSC contrasto effetti COVID1`,                            
  #          SEZ_SPEC_2_FS = `Sezione speciale 2: risorse FSC copertura interventi ex fondi strutturali 2014-20202`) %>%
  #   mutate(OC_MACROAREA = case_when(OC_MACROAREA == "CN" ~ "CN",
  #                                   OC_MACROAREA == "MZ" ~ "SUD"))
  # 
  # appo2 <- appo %>% 
  #   filter(!is.na(OC_DESCR_ARTICOLAZ_PROGRAMMA)) %>%
  #   mutate(AREA_TEMATICA_PSC = OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
  #   select(-FINANZ_FSC, -OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
  #   pivot_longer(cols = starts_with("SEZ_SPEC"), names_to = "OC_DESCR_ARTICOLAZ_PROGRAMMA", values_to = "FINANZ_FSC")
  # 
  # 
  # articolazioni_2 <- appo %>% 
  #   filter(is.na(OC_DESCR_ARTICOLAZ_PROGRAMMA)) %>%
  #   select(-SEZ_SPEC_1_COVID, -SEZ_SPEC_2_FS) %>%
  #   mutate(OC_DESCR_ARTICOLAZ_PROGRAMMA = "SEZ_ORD") %>%
  #   bind_rows(appo2) %>%
  #   mutate(OC_FLAG_MONITORAGGIO = 1)
  # 
  # 
  # 
  # ----------------------------------------------------------------------------------- #
  # gestione PSC
  # 
  # # MEMO;: il join per OC_FLAG_MONITORAGGIO è solo per controllo (es. compensazioni ambientali campania)
  # 
  # # totali psc
  # # MEMO: filtro i PSC tranne quelli che non sono in articolazioni:
  # # - act
  # # - righe per compensazione di tagli eccessivi CSR
  # # - psc metropolitani
  # appo_psc <- programmi_3 %>%
  #   filter(PSC != "DEAD", PSC != "ACT", OC_TIPOLOGIA_PROGRAMMA != "CSR", TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE != "METROPOLITANA") %>%
  #   mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC)),
  #          OC_DESCRIZIONE_PROGRAMMA = paste0("PSC ", AMMINISTRAZIONE_TITOLARE),
  #          OC_TIPOLOGIA_PROGRAMMA = "PSC") %>%
  #   select(-PSC)
  # 
  # 
  # # controllo totali psc vs articolazioni psc
  # chk <- appo_psc %>%
  #   group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   mutate(RISORSE = round(RISORSE/1000000, 2)) %>%
  #   filter(abs(RISORSE) > 0) %>%
  #   full_join(articolazioni_2 %>%
  #               mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC))) %>%
  #               group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
  #               summarise(RISORSE = sum(FINANZ_FSC, na.rm = TRUE)) %>%
  #               mutate(RISORSE = round(RISORSE, 2)) %>%
  #               filter((abs(RISORSE) > 0)),
  #             by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO")) %>%
  #   mutate(CHK = RISORSE.x - RISORSE.y)
  # 
  # # integra articolazioni psc
  # appo_psc_2 <- appo_psc %>%
  #   mutate(CICLO_RISORSE = NA) %>% #MEMO: si azzera by def per funzionamento psc (non è nota per articolazioni)
  #   select(-FINANZ_TOTALE_PUBBLICO, -OC_DESCR_ARTICOLAZ_PROGRAMMA) %>%
  #   distinct() %>%
  #   full_join(articolazioni_2 %>%
  #               rename(FINANZ_TOTALE_PUBBLICO = FINANZ_FSC) %>%
  #               mutate(FINANZ_TOTALE_PUBBLICO = FINANZ_TOTALE_PUBBLICO * 1000000) %>%
  #               mutate(OC_CODICE_PROGRAMMA = paste0("PSC_", gsub(" ", "_", PSC))) %>%
  #               filter((abs(FINANZ_TOTALE_PUBBLICO) > 0)),
  #             by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO"))
  # 
  # 
  # # chk finanziario
  # # MEMO: attenzione perché qui perdo completamente decimali che ho troncato in articolazioni per fare i psc
  # chk <- appo_psc %>%
  #   group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   mutate(RISORSE = round(RISORSE/1000000, 2)) %>%
  #   full_join(appo_psc_2 %>%
  #               group_by(OC_CODICE_PROGRAMMA, OC_MACROAREA, CICLO_PROGRAMMAZIONE, OC_FLAG_MONITORAGGIO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO/1000000, na.rm = TRUE)) %>%
  #               mutate(RISORSE = round(RISORSE, 2)),
  #             by = c("OC_CODICE_PROGRAMMA", "OC_MACROAREA", "CICLO_PROGRAMMAZIONE", "OC_FLAG_MONITORAGGIO")) %>%
  #   mutate(CHK = RISORSE.x - RISORSE.y)
  # 
  # appo_altro <- programmi_3 %>%
  #   filter(PSC == "DEAD" | PSC == "ACT" | OC_TIPOLOGIA_PROGRAMMA == "CSR" | TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA") %>%
  #   select(-PSC)
  # 
  # programmi_4 <- appo_psc_2 %>%
  #   bind_rows(appo_altro)
  # 
  # 
  # 
  # ----------------------------------------------------------------------------------- #
  # export programmi
  
  # MEMO: non ha più senso distinzione per ciclo perché diventano tutti programmi 2014-2020
  # CHK: verifica se CICLO_PROGRAMMAZIONE viene sovrascritto all'import del del DB (sembra di no...), altrimenti usiamo ciclo risorse 
  # MEMO: CICLO_RISORSE resta NA perché non è disponbile per articolazioni
  
  
  # programmi_1420 <- programmi_4 %>%
  #   select(-ADDENDUM, -TIPOLOGIA_STRUMENTO, -PSC) %>%
  #   # patch
  #   mutate(FINANZ_FSC = FINANZ_TOTALE_PUBBLICO)
  
  programmi_4 <- programmi_3 %>% 
    # elimino CICLO_RISORSE
    group_by(OC_DESCR_FONTE, OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA, CICLO_PROGRAMMAZIONE, 
             AMMINISTRAZIONE_TITOLARE, TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE, OC_TIPOLOGIA_PROGRAMMA, 
             OC_COD_ARTICOLAZ_PROGRAMMA, OC_DESCR_ARTICOLAZ_PROGRAMMA, OC_MACROAREA, CAT, DEN_REGIONE, OC_FLAG_MONITORAGGIO, NOTE, PSC) %>% 
    summarise(FINANZ_TOTALE_PUBBLICO = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE))
  
  
  if (psc_cm == FALSE) {
    programmi_5 <- programmi_4 %>%
      filter(PSC != "DEAD", PSC !="ACT",
             TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE != "METROPOLITANA")
  } else {
    programmi_5 <- programmi_4 %>%
      filter(PSC != "DEAD", PSC !="ACT")
  }
  
  
  programmi <- programmi_5 %>%
    # filter(PSC != "DEAD", PSC !="ACT",
    #        TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE != "METROPOLITANA",
    #        # OC_TIPOLOGIA_PROGRAMMA != "CSR"
    # ) %>% 
    # MEMO: metto ciclo strategia di provenienza su ciclo risorse perché altrimenti si perde in load_db()
    mutate(CICLO_RISORSE = CICLO_PROGRAMMAZIONE) %>%
    mutate(FINANZ_UE = 0,
           FINANZ_ALTRO = 0,
           COD_OBIETTIVO_TEMATICO = NA,
           DESCR_OBIETTIVO_TEMATICO = NA,
           COD_RISULTATO_ATTESO = NA,
           DESCR_RISULTATO_ATTESO = NA,
           # COD_AREA_TEMATICA_PSC = substr(AREA_TEMATICA_PSC, 1, 2),
           # COD_SETTORE_INTERVENTO_PSC = substr(SETTORE_INTERVENTO_PSC, 1, 5)) %>%
           COD_AREA_TEMATICA_PSC = NA,
           COD_SETTORE_INTERVENTO_PSC = NA, 
           DESCR_AREA_TEMATICA_PSC = NA,
           DESCR_SETTORE_INTERVENTO_PSC = NA,
           ID_PSC = gsub(" ", "_", paste0("PSC_", PSC)),
           PSC = paste0("PSC ", toupper(AMMINISTRAZIONE_TITOLARE))) %>%
    select(AMBITO	= OC_DESCR_FONTE,
           OC_CODICE_PROGRAMMA,
           DESCRIZIONE_PROGRAMMA = OC_DESCRIZIONE_PROGRAMMA,
           CICLO_PROGRAMMAZIONE,
           CICLO_RISORSE,
           AMMINISTRAZIONE = AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_AMMINISTRAZIONE = TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE,
           TIPOLOGIA_PROGRAMMA = OC_TIPOLOGIA_PROGRAMMA,
           COD_LIVELLO_1 = OC_COD_ARTICOLAZ_PROGRAMMA,
           DESCR_LIVELLO_1 = OC_DESCR_ARTICOLAZ_PROGRAMMA,
           FINANZ_UE,
           FINANZ_ALTRO,
           FINANZ_TOTALE = FINANZ_TOTALE_PUBBLICO,
           MACROAREA = OC_MACROAREA,
           CAT_REGIONE = CAT,
           DEN_REGIONE,
           COD_OBIETTIVO_TEMATICO,
           DESCR_OBIETTIVO_TEMATICO,
           COD_RISULTATO_ATTESO,
           DESCR_RISULTATO_ATTESO,
           COD_AREA_TEMATICA_PSC,
           # DESCR_AREA_TEMATICA_PSC = AREA_TEMATICA_PSC,
           DESCR_AREA_TEMATICA_PSC,
           COD_SETTORE_INTERVENTO_PSC,
           # DESCR_SETTORE_INTERVENTO_PSC = SETTORE_INTERVENTO_PSC,
           DESCR_SETTORE_INTERVENTO_PSC,
           FLAG_MONITORAGGIO = OC_FLAG_MONITORAGGIO,
           NOTE,
           ID_PSC,
           PSC)
  
  # fix codici 15 char
  programmi <- fix_id_psc_15_digit(programmi, "ID_PSC")
  
  # fix nomi ministeri
  # programmi <- fix_id_psc_ministeri(programmi, "DESCRIZIONE_PROGRAMMA")
  programmi <- fix_id_psc_ministeri(programmi, "PSC")
  
  # export
  if (export == TRUE) {
    write.csv2(programmi, file.path(TEMP, "fsc_matrice_po_psc.csv"), row.names = FALSE, na = "")
  }
  
  
  # export xls
  if (export_xls == TRUE) {
    
    # xls
    require("openxlsx") 
    
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = programmi, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "fsc_matrice_po_psc.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "fsc_matrice_po_psc.xlsx"), to = file.path(DB, "fsc_matrice_po_psc.xlsx"), overwrite = TRUE)
  }
  return(programmi)
}




#' Crea file info FSC per DBCOE
#'
#' Crea file info FSC per DBCOE per i cicli 2000-2006, 2007-2013 e 2014-2020
#' 
#' @param file_evo Nome del file "assegnazioni_evo_XXXXXXXX.XX.xlsx"
#' @param file_temi Nome del file "PSC_articolazione_tematiche_chk_decimali_v.XX.xlsx"
#' @param psc_cm Logico. Vuoi convertire in PSC anche i Patti delle Città metropolitane?
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx nel DBCOE?
#' @return File file info FSC per DBCOE per i cicli 2007-2013 e 2014-2020. I PSC sono interamente nel file 2014-2020. Le risorse del ciclo 2000-2006 sono interamente riassorbite nei PSC, per questo non c'è un file dedicato.
#' @note Salva nella versione del DBCOE risultante come DB da __oc_init__.
setup_dbcoe_info_fsc_psc <- function(file_evo, file_temi, psc_cm=FALSE, export=FALSE, export_xls=FALSE) {
 
  # DEV: qui deve fornire solo delibere costitutive dei psc!
  
  # file_evo <- "assegnazioni_evo_20210803.00.xlsx"
  
  require("lubridate")
  # library("lubridate")
  
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  # strumenti <- read_xlsx(file.path(INPUT, "assegnazioni_evo_20210803.00.xlsx"), sheet = "strumenti")
  strumenti <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", file_evo), sheet = "strumenti")
  
  strumenti <- strumenti %>%
    rename(OC_CODICE_PROGRAMMA = `Codice strumento`,
           # `Tipo codice strumento`,
           COD_ORIG = `Codice strumento originario`,
           CICLO_PROGRAMMAZIONE = `Ciclo di programmazione per strategia`, 
           OC_DESCRIZIONE_PROGRAMMA = `Descrizione strumento`,
           # PSC,
           OC_MACROTIPOLOGIA_PROGRAMMA = `Macro-tipologia strumento`,
           OC_TIPOLOGIA_PROGRAMMA = `Tipologia strumento`,
           AMMINISTRAZIONE_TITOLARE = `Amministrazione titolare`,
           # `Soggetto attuatore`,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE = `Tipologia di amministrazione titolare`,
           # `Label tavola informativa CIPE 05/2020`,
           # `Label tavola informativa CIPE 12/2019`,
           # `A cavallo di cicli`,
           # `Valore considerato per CIPE 05/2020`,
           # `Valore considerato per CIPE 05/2020 – Al netto di 2019`,
           RIS_PRE44_TOT = `Risorse FSC assegnate ante 44– Totale`,
           RIS_PRE44_0006 = `Risorse FSC assegnate ante 44 – 2000-2006`,
           RIS_PRE44_0713 = `Risorse FSC assegnate ante 44 – 2007-2013`,
           RIS_PRE44_1420 = `Risorse FSC assegnate ante 44 – 2014-2020`,
           RIS_44_NO_MONIT = `Art 44 – Tagli da economie (col H)`,
           RIS_44_TAGLI_7B = `Art 44 – Tagli da progetti 7.b (col. M)`,
           RIS_44_7A = `Art 44 – Progetti 7.a a dicembre 2019`,
           RIS_44_7B = `Art 44- Progetti e risorse in 7.b (calcolo)`,
           RIS_44_NO_VAL = `Art 44 – Risorse confermate senza valutazione`,
           RIS_TOT = `Risorse FSC post 44 – Totale`,
           RIS_0006 = `Risorse FSC post 44 – 2000-2006`,
           RIS_0713 = `Risorse FSC post 44 – 2007-2013`,
           RIS_1420 = `Risorse FSC post 44 – 2014-2020`,
           RIS_SUD = `Risorse FSC post 44 – Mezzogiorno`,
           RIS_CN = `Risorse FSC post 44 – Centro-Nord`,
           RIS_ND = `Risorse FSC post 44 – Non ripartite`,
           RIS_PSC_ORD = `Risorse FSC post 44 – confermate in PSC o altro`,
           RIS_PSC_SEZ_COVID = `Risorse FSC post 44 – sezione speciale COVID (art. 241)`,
           RIS_PSC_SEZ_POR = `Risorse FSC post 44 – sezione speciale copertura POR (art. 242)`,
           # FLAG_COESIONE = `Flag perimetro Coesione`,
           DELIBERE = `Delibere CIPE e Norme`,
           # NOTE = `Note su Risorse`,
           LISTA_INT  = `Lista interventi programmati`,
           # OC_FLAG_MONITORAGGIO = `Flag monitoraggio`
           OC_FLAG_MONITORAGGIO = `Flag perimetro Coesione`)
  
  
  # ----------------------------------------------------------------------------------- #
  # crea file info
  
  # MEMO: qui forzo OC_CODICE_PROGRAMMA anche per per patti cm (a PSC metro approvati andrà ripristinato)
  
  if (psc_cm == FALSE) {
    appo <- strumenti %>%
      rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA) %>% 
      # mutate(ID_PSC = if_else(is.na(PSC), NA, paste0("PSC_", PSC))) %>% 
      mutate(PSC_2 = case_when(PSC == "DEAD" ~ "x",
                               PSC == "ACT" ~ "x",
                               # lascia patto cm
                               OC_TIPOLOGIA_PROGRAMMA == "PATTI" & TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA" ~ "x",
                               is.na(PSC) ~ "x",
                               TRUE ~ PSC)) %>% 
      # mutate(OC_CODICE_PROGRAMMA = if_else(PSC_2 == "x", ID_PROGRAMMA, paste0("PSC_", PSC_2))) %>% 
      mutate(OC_CODICE_PROGRAMMA = if_else(PSC_2 == "x", ID_PROGRAMMA, paste0("PSC_", gsub(" ", "_", PSC_2)))) %>% 
      select(OC_CODICE_PROGRAMMA, DELIBERE) %>%
      separate_rows(sep = ";\\s+", DELIBERE, convert = FALSE) 
  } else {
    appo <- strumenti %>%
      rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA) %>% 
      # mutate(ID_PSC = if_else(is.na(PSC), NA, paste0("PSC_", PSC))) %>% 
      mutate(PSC_2 = case_when(PSC == "DEAD" ~ "x",
                               PSC == "ACT" ~ "x",
                               # usa psc per cm
                               # OC_TIPOLOGIA_PROGRAMMA == "PATTI" & TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE == "METROPOLITANA" ~ "x",
                               is.na(PSC) ~ "x",
                               TRUE ~ PSC)) %>% 
      # mutate(OC_CODICE_PROGRAMMA = if_else(PSC_2 == "x", ID_PROGRAMMA, paste0("PSC_", PSC_2))) %>% 
      mutate(OC_CODICE_PROGRAMMA = if_else(PSC_2 == "x", ID_PROGRAMMA, paste0("PSC_", gsub(" ", "_", PSC_2)))) %>% 
      select(OC_CODICE_PROGRAMMA, DELIBERE) %>%
      separate_rows(sep = ";\\s+", DELIBERE, convert = FALSE) 
  }
  
  
  
  counter <- appo %>%
    count(OC_CODICE_PROGRAMMA)
  
  appo1 <- appo %>%
    left_join(counter, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LAST = if_else(n == 1, "X", "")) %>% 
    select(-n) %>% 
    mutate(DATA_DECISIONE = str_extract(DELIBERE, "\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}")) %>% 
    mutate(DATA_DECISIONE = dmy(DATA_DECISIONE)) %>% 
    mutate(TEMP = str_extract(DELIBERE, "n\\.\\s?\\d{1,3}\\s?del")) %>% 
    mutate(NUMERO_DECISIONE = str_extract(DELIBERE, "\\d{1,3}"))
  
  counter <- appo1 %>%
    group_by(OC_CODICE_PROGRAMMA) %>% 
    summarise(TEST = max(DATA_DECISIONE, na.rm = T))
  
  info <- appo1 %>%
    left_join(counter, by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(FLAG_ULTIMA_DECISIONE = case_when(LAST == "X" ~ "X",
                                             DATA_DECISIONE == TEST ~ "X",
                                             TRUE ~ "")) %>%
    mutate(TIPO_DECISIONE = case_when(grepl("Delibera", DELIBERE) ~ "Delibera", 
                                      grepl("Ordinanza", DELIBERE) ~ "Ordinanza", # TODO: distinguere!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                      grepl("Decreto Legge", DELIBERE) ~ "Decreto Legge",
                                      grepl("Legge", DELIBERE) ~ "Legge",
                                      TRUE ~ "Altra norma")) %>% 
    rename(NOTE_DECISIONE = DELIBERE) %>% 
    select(-TEMP, -TEST, -LAST) # %>% 
  # rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA)
  
  
  # fix codici 15 char
  # info <- info %>% 
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
  #                                          OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
  #                                          OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  info <- fix_id_psc_15_digit(info, "OC_CODICE_PROGRAMMA")


  # ----------------------------------------------------------------------------------- #
  # sovrascrive psc lato info
  
  # MEMO: fino a qui sopra ho ancora tutte le delibere per tutti i programmi
  
  # appo <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx"))
  if (psc_cm == FALSE) {
    appo <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", "fsc_delibere_psc.xlsx")) %>% 
      filter(TIPO != "CM")
  } else {
    appo <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", "fsc_delibere_psc.xlsx"))
  }

  psc <- appo %>% 
    distinct(OC_CODICE_PROGRAMMA)
  
  temp <- appo %>% 
    mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE)) %>% 
    select(names(info))
  
  info_2 <- info %>% 
    anti_join(psc) %>% 
    bind_rows(temp)
  
  info <- info_2
  
  

  # ----------------------------------------------------------------------------------- #
  # spalla con anagrafica programmi
  
  # 1420
  programmi_1420 <- load_db("2014-2020", "FSC", use_ciclo = T)
  programmi_713 <- load_db("2007-2013", "FSC", use_ciclo = T)
  
  programmi <- programmi_1420 %>% 
    bind_rows(programmi_713)
  
  # DEV: devo togliere ciclo?
  # DEV: aggiungi distinct alla fine

  # fix codici 15 char
  # programmi <- programmi %>% 
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
  #                                          OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
  #                                          OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  # MEMO: questo non serve perché DBCOE è già fixed

  programmi_info <- programmi %>% 
    # rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA) %>% 
    # mutate(OC_CODICE_PROGRAMMA = if_else(is.na(ID_PSC), ID_PROGRAMMA, ID_PSC), 
    #        DESCRIZIONE_PROGRAMMA = if_else(is.na(PSC), DESCRIZIONE_PROGRAMMA, PSC)) %>% 
    group_by(AMBITO,
             # ID_PROGRAMMA,
             OC_CODICE_PROGRAMMA,	
             DESCRIZIONE_PROGRAMMA,	
             # CICLO_PROGRAMMAZIONE # qui ciclo duplica
    ) %>% 
    summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE)) %>% 
    # integra info 
    left_join(info, by = "OC_CODICE_PROGRAMMA") %>% 
    # ripristina ciclo
    mutate(CICLO_PROGRAMMAZIONE = case_when(OC_CODICE_PROGRAMMA %in% programmi_1420$OC_CODICE_PROGRAMMA ~ "2014-2020",
                                            OC_CODICE_PROGRAMMA %in% programmi_713$OC_CODICE_PROGRAMMA ~ "2007-2013")) %>% 
    mutate(CICLO_RISORSE = "", # non è pertinente rispetto a decisioni
           FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE = "",
           NOTE_DECISIONE = "", 
           VERSIONE = "",
           SEQ_DECISIONE = "",
           LINK_DOCUMENTO = "", 
           NOTE = "") %>% 
    mutate(FINANZ_TOTALE = if_else(FLAG_ULTIMA_DECISIONE == "X", FINANZ_TOTALE, 0)) %>% 
    distinct(AMBITO,
             OC_CODICE_PROGRAMMA,	
             DESCRIZIONE_PROGRAMMA,	
             CICLO_PROGRAMMAZIONE,
             CICLO_RISORSE,
             TIPO_DECISIONE,
             NUMERO_DECISIONE,	
             DATA_DECISIONE,	
             FLAG_ULTIMA_DECISIONE,	
             LINK_DECISIONE,
             NOTE_DECISIONE,
             VERSIONE,
             SEQ_DECISIONE,	
             LINK_DOCUMENTO,
             FINANZ_TOTALE,	
             NOTE)
  
  
  
  # export
  if (export == TRUE) {
    write.csv2(programmi_info, file.path(TEMP, "info_fsc_psc.csv"), row.names = FALSE, na = "")
  }
  
  # export xls
  if (export_xls == TRUE) {
    
    require("openxlsx") 
    
    # 1420
    appo <- programmi_info %>% filter(CICLO_PROGRAMMAZIONE == "2014-2020")
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = appo, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "Info_DBCOE_FSC1420.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "Info_DBCOE_FSC1420.xlsx"), to = file.path(DB, "Info_DBCOE_FSC1420.xlsx"), overwrite = TRUE)
    
    # 713
    appo <- programmi_info %>% filter(CICLO_PROGRAMMAZIONE == "2007-2013")
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = appo, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "Info_DBCOE_FSC0713.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "Info_DBCOE_FSC0713.xlsx"), to = file.path(DB, "Info_DBCOE_FSC0713.xlsx"), overwrite = TRUE)
    
  }
  return(programmi_info)
}





#' Crea file info FSC per DBCOE
#'
#' Crea file info FSC per DBCOE per i cicli 2000-2006, 2007-2013 e 2014-2020
#' Crea file info di tipo PO-PSC per DBCOE, contenente tutte le delibere dei programmi originari confluiti nei PSC
#' 
#' @param file_evo Nome del file "assegnazioni_evo_XXXXXXXX.XX.xlsx"
#' @param export Vuoi salvare il file csv in TEMP?
#' @param export_xls Vuoi salvare i file xlsx nel DBCOE?
#' @return File file info FSC per DBCOE per i cicli 2007-2013 e 2014-2020. I PSC sono interamente nel file 2014-2020. Le risorse del ciclo 2000-2006 sono interamente riassorbite nei PSC, per questo non c'è un file dedicato.
#' @note Salva nella versione del DBCOE risultante come DB da __oc_init__. Diversamente dalle altre funzioni dello stesso gruppo, non provede il parametro **psc_cm** perchè l'elaborazione punta direttamente si adegua direttamente al DBCOE
setup_dbcoe_info_fsc_popsc <- function(file_evo, export=FALSE, export_xls=FALSE) {
  
  # DEV: 
  # al momento è uguale a setup_dbcoe_info_fsc_popsc(), ma va invertita la logica:
  # qui deve fornire solo delibere costitutive dei psc!
  
  # file_evo <- "assegnazioni_evo_20210803.00.xlsx"
  
  require("lubridate")
  # library("lubridate")
  
  
  # ----------------------------------------------------------------------------------- #
  # loads
  
  # strumenti <- read_xlsx(file.path(INPUT, "assegnazioni_evo_20210803.00.xlsx"), sheet = "strumenti")
  strumenti <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", file_evo), sheet = "strumenti")
  
  strumenti <- strumenti %>%
    rename(OC_CODICE_PROGRAMMA = `Codice strumento`,
           # `Tipo codice strumento`,
           COD_ORIG = `Codice strumento originario`,
           CICLO_PROGRAMMAZIONE = `Ciclo di programmazione per strategia`, 
           OC_DESCRIZIONE_PROGRAMMA = `Descrizione strumento`,
           # PSC,
           OC_MACROTIPOLOGIA_PROGRAMMA = `Macro-tipologia strumento`,
           OC_TIPOLOGIA_PROGRAMMA = `Tipologia strumento`,
           AMMINISTRAZIONE_TITOLARE = `Amministrazione titolare`,
           # `Soggetto attuatore`,
           TIPOLOGIA_DI_AMMINISTRAZIONE_TITOLARE = `Tipologia di amministrazione titolare`,
           # `Label tavola informativa CIPE 05/2020`,
           # `Label tavola informativa CIPE 12/2019`,
           # `A cavallo di cicli`,
           # `Valore considerato per CIPE 05/2020`,
           # `Valore considerato per CIPE 05/2020 – Al netto di 2019`,
           RIS_PRE44_TOT = `Risorse FSC assegnate ante 44– Totale`,
           RIS_PRE44_0006 = `Risorse FSC assegnate ante 44 – 2000-2006`,
           RIS_PRE44_0713 = `Risorse FSC assegnate ante 44 – 2007-2013`,
           RIS_PRE44_1420 = `Risorse FSC assegnate ante 44 – 2014-2020`,
           RIS_44_NO_MONIT = `Art 44 – Tagli da economie (col H)`,
           RIS_44_TAGLI_7B = `Art 44 – Tagli da progetti 7.b (col. M)`,
           RIS_44_7A = `Art 44 – Progetti 7.a a dicembre 2019`,
           RIS_44_7B = `Art 44- Progetti e risorse in 7.b (calcolo)`,
           RIS_44_NO_VAL = `Art 44 – Risorse confermate senza valutazione`,
           RIS_TOT = `Risorse FSC post 44 – Totale`,
           RIS_0006 = `Risorse FSC post 44 – 2000-2006`,
           RIS_0713 = `Risorse FSC post 44 – 2007-2013`,
           RIS_1420 = `Risorse FSC post 44 – 2014-2020`,
           RIS_SUD = `Risorse FSC post 44 – Mezzogiorno`,
           RIS_CN = `Risorse FSC post 44 – Centro-Nord`,
           RIS_ND = `Risorse FSC post 44 – Non ripartite`,
           RIS_PSC_ORD = `Risorse FSC post 44 – confermate in PSC o altro`,
           RIS_PSC_SEZ_COVID = `Risorse FSC post 44 – sezione speciale COVID (art. 241)`,
           RIS_PSC_SEZ_POR = `Risorse FSC post 44 – sezione speciale copertura POR (art. 242)`,
           # FLAG_COESIONE = `Flag perimetro Coesione`,
           DELIBERE = `Delibere CIPE e Norme`,
           # NOTE = `Note su Risorse`,
           LISTA_INT  = `Lista interventi programmati`,
           # OC_FLAG_MONITORAGGIO = `Flag monitoraggio`
           OC_FLAG_MONITORAGGIO = `Flag perimetro Coesione`)
  
  
  # ----------------------------------------------------------------------------------- #
  # crea file info
  
  
  appo <- strumenti %>%
    # rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA) %>% 
    # mutate(ID_PSC = if_else(is.na(PSC), NA, paste0("PSC_", PSC))) %>% 
    mutate(PSC_2 = case_when(PSC == "DEAD" ~ "x",
                             PSC == "ACT" ~ "x",
                             is.na(PSC) ~ "x",
                             TRUE ~ PSC)) %>% 
    # mutate(OC_CODICE_PROGRAMMA = if_else(PSC_2 == "x", ID_PROGRAMMA, paste0("PSC_", PSC_2))) %>% 
    mutate(PSC = if_else(PSC_2 == "x", "", paste0("PSC_", gsub(" ", "_", PSC_2)))) %>% 
    select(OC_CODICE_PROGRAMMA, DELIBERE, PSC) %>%
    separate_rows(sep = ";\\s+", DELIBERE, convert = FALSE) 
  
  counter <- appo %>%
    count(OC_CODICE_PROGRAMMA)
  
  appo1 <- appo %>%
    left_join(counter, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LAST = if_else(n == 1, "X", "")) %>% 
    select(-n) %>% 
    mutate(DATA_DECISIONE = str_extract(DELIBERE, "\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}")) %>% 
    mutate(DATA_DECISIONE = dmy(DATA_DECISIONE)) %>% 
    mutate(TEMP = str_extract(DELIBERE, "n\\.\\s?\\d{1,3}\\s?del")) %>% 
    mutate(NUMERO_DECISIONE = str_extract(DELIBERE, "\\d{1,3}"))
  
  counter <- appo1 %>%
    group_by(OC_CODICE_PROGRAMMA) %>% 
    summarise(TEST = max(DATA_DECISIONE, na.rm = T))
  
  info <- appo1 %>%
    left_join(counter, by = "OC_CODICE_PROGRAMMA") %>% 
    mutate(FLAG_ULTIMA_DECISIONE = case_when(LAST == "X" ~ "X",
                                             DATA_DECISIONE == TEST ~ "X",
                                             TRUE ~ "")) %>%
    mutate(TIPO_DECISIONE = case_when(grepl("Delibera", DELIBERE) ~ "Delibera", 
                                      grepl("Ordinanza", DELIBERE) ~ "Ordinanza", # TODO: distinguere!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                      grepl("Decreto Legge", DELIBERE) ~ "Decreto Legge",
                                      grepl("Legge", DELIBERE) ~ "Legge",
                                      TRUE ~ "Altra norma")) %>% 
    rename(NOTE_DECISIONE = DELIBERE) %>% 
    select(-TEMP, -TEST, -LAST) # %>% 
  # rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA)
  
  
  # fix codici 15 char
  # info <- info %>% 
  #   mutate(PSC = case_when(PSC == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
  #                          PSC == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
  #                          PSC == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
  #                          TRUE ~ PSC))
  # info <- fix_id_psc_15_digit(info, var1="ID_PSC")
  
  info <- info %>% 
    select(-PSC)
  
  
  # ----------------------------------------------------------------------------------- #
  # sovrascrive psc lato info
  
  # MEMO: fino a qui sopra ho ancora tutte le delibere per tutti i programmi
  
  # appo <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx"))
  # appo <- read_xlsx(file.path(DRIVE, "PROGRAMMAZIONE", "INFO", "PO-PSC", "fsc_delibere_psc.xlsx"))
  # 
  # psc <- appo %>% 
  #   distinct(OC_CODICE_PROGRAMMA)
  # 
  # temp <- appo %>% 
  #   mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE)) %>% 
  #   select(names(info))
  # 
  # info_2 <- info %>% 
  #   anti_join(psc) %>% 
  #   bind_rows(temp)
  # 
  # info <- info_2
  
  
  
  # ----------------------------------------------------------------------------------- #
  # spalla con anagrafica programmi
  
  # 1420
  # programmi_1420 <- load_db("2014-2020", "FSC", use_ciclo = T)
  # programmi_713 <- load_db("2007-2013", "FSC", use_ciclo = T)
  # 
  # programmi <- programmi_1420 %>% 
  #   bind_rows(programmi_713)
  
  programmi <- read_xlsx(file.path(DB, "fsc_matrice_po_psc.xlsx"))
  
  # DEV: devo togliere ciclo?
  # DEV: aggiungi distinct alla fine
  
  # fix codici 15 char
  # programmi <- programmi %>% 
  #   mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
  #                                          OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
  #                                          OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
  #                                          TRUE ~ OC_CODICE_PROGRAMMA))
  # MEMO: questo non serve perché DBCOE è già fixed
  
  programmi_info <- programmi %>% 
    # rename(ID_PROGRAMMA = OC_CODICE_PROGRAMMA) %>% 
    # mutate(OC_CODICE_PROGRAMMA = if_else(is.na(ID_PSC), ID_PROGRAMMA, ID_PSC), 
    #        DESCRIZIONE_PROGRAMMA = if_else(is.na(PSC), DESCRIZIONE_PROGRAMMA, PSC)) %>% 
    group_by(AMBITO,
             # ID_PROGRAMMA,
             OC_CODICE_PROGRAMMA,	
             DESCRIZIONE_PROGRAMMA,	
             CICLO_PROGRAMMAZIONE, # MEMO: qui ciclo non duplica più
             ID_PSC,
             PSC
    ) %>% 
    summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE)) %>% 
    # integra info 
    left_join(info, by = "OC_CODICE_PROGRAMMA") %>% 
    # ripristina ciclo
    # mutate(CICLO_PROGRAMMAZIONE = case_when(OC_CODICE_PROGRAMMA %in% programmi_1420$OC_CODICE_PROGRAMMA ~ "2014-2020",
    #                                         OC_CODICE_PROGRAMMA %in% programmi_713$OC_CODICE_PROGRAMMA ~ "2007-2013")) %>% 
    mutate(CICLO_RISORSE = "", # non è pertinente rispetto a decisioni
           FLAG_ULTIMA_DECISIONE,
           LINK_DECISIONE = "",
           NOTE_DECISIONE = "", 
           VERSIONE = "",
           SEQ_DECISIONE = "",
           LINK_DOCUMENTO = "", 
           NOTE = "") %>% 
    mutate(FINANZ_TOTALE = if_else(FLAG_ULTIMA_DECISIONE == "X", FINANZ_TOTALE, 0)) %>% 
    distinct(AMBITO,
             OC_CODICE_PROGRAMMA,	
             DESCRIZIONE_PROGRAMMA,	
             CICLO_PROGRAMMAZIONE,
             CICLO_RISORSE,
             TIPO_DECISIONE,
             NUMERO_DECISIONE,	
             DATA_DECISIONE,	
             FLAG_ULTIMA_DECISIONE,	
             LINK_DECISIONE,
             NOTE_DECISIONE,
             VERSIONE,
             SEQ_DECISIONE,	
             LINK_DOCUMENTO,
             FINANZ_TOTALE,	
             NOTE,
             ID_PSC,
             PSC)
  
  # export
  if (export == TRUE) {
    write.csv2(programmi_info, file.path(TEMP, "fsc_delibere_po_psc.csv"), row.names = FALSE, na = "")
  }
  
  # export xls
  if (export_xls == TRUE) {
    
    # xls
    require("openxlsx") 
    
    wb <- createWorkbook()
    addWorksheet(wb, "FSC")
    writeData(wb, sheet = "FSC", x = programmi_info, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(OUTPUT, "fsc_delibere_po_psc.xlsx"), overwrite = TRUE)
    
    file.copy(from = file.path(OUTPUT, "fsc_delibere_po_psc.xlsx"), to = file.path(DB, "fsc_delibere_po_psc.xlsx"), overwrite = TRUE)
  }
  
  return(programmi_info)
  
}


#' Correzione per codici PSC superiori a 15 digit
#'
#' Correzione per codici PSC superiori a 15 digit
#'  
#' @param df Dataframe
#' @param var1 Nome variabile da modificare (OC_CODICE_PROGRAMMA oppure ID_PSC)
#' @return Dataframe con valori modificati
fix_id_psc_15_digit <- function(df, var1="ID_PSC") {
  
  # fix_id_psc_15_digit(df, var1="ID_PSC")
  # fix_id_psc_15_digit(df, var1="OC_CODICE_PROGRAMMA")
  
  if (var1 == "OC_CODICE_PROGRAMMA") {
    df <- df %>% 
      mutate(OC_CODICE_PROGRAMMA = 
               case_when(OC_CODICE_PROGRAMMA == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
                         OC_CODICE_PROGRAMMA == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
                         OC_CODICE_PROGRAMMA == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
                         OC_CODICE_PROGRAMMA == "PSC_REGGIO_CALABRIA" ~ "PSC_REGGIO_CALA",
                         TRUE ~ OC_CODICE_PROGRAMMA))
  } else {
    df <- df %>% 
      mutate(ID_PSC = 
               case_when(ID_PSC == "PSC_EMILIA-ROMAGNA" ~ "PSC_EMILIA-ROMA",
                         ID_PSC == "PSC_FRIULI-VENEZIA_GIULIA" ~ "PSC_FRIULI-VENE",
                         ID_PSC == "PSC_VALLE_D_AOSTA" ~ "PSC_VALLE_D_AOS",
                         ID_PSC == "PSC_REGGIO_CALABRIA" ~ "PSC_REGGIO_CALA",
                         TRUE ~ ID_PSC))
  }
  return(df)
}



#' Correzione per denominazioni PSC ministeri
#'
#' Correzione per denominazioni PSC ministeri
#'  
#' @param df Dataframe con variabili 
#' @param var1 Nome variabile da modificare (PSC oppure DESCRIZIONE_PROGRAMMA)
#' @return Dataframe con valori modificati per la variabile PSC oppure DESCRIZIONE_PROGRAMMA in funzione dei valori diu ID_PSC oppure OC_CODICE_PROGRAMMA
fix_id_psc_ministeri <- function(df, var1="PSC") {
  
  # fix_id_psc_ministeri(df, "DESCRIZIONE_PROGRAMMA")

  if (var1 == "DESCRIZIONE_PROGRAMMA") {
    df <- df %>% 
      mutate(DESCRIZIONE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
                             OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
                             OC_CODICE_PROGRAMMA == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
                             # OC_CODICE_PROGRAMMA == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
                             OC_CODICE_PROGRAMMA == "PSC_MIC" ~ "PSC MINISTERO CULTURA",
                             OC_CODICE_PROGRAMMA == "PSC_MITUR" ~ "PSC MINISTERO TURISMO",
                             OC_CODICE_PROGRAMMA == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
                             OC_CODICE_PROGRAMMA == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
                             OC_CODICE_PROGRAMMA == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
                             OC_CODICE_PROGRAMMA == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
                             OC_CODICE_PROGRAMMA == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
                             TRUE ~ DESCRIZIONE_PROGRAMMA))
  } else {
    df <- df %>% 
      mutate(PSC = case_when(ID_PSC == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
                             ID_PSC == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
                             ID_PSC == "PSC_MI" ~ "PSC MINISTERO ISTRUZIONE",
                             # ID_PSC == "PSC_MIBACT" ~ "PSC MINISTERO CULTURA E TURISMO",
                             ID_PSC == "PSC_MIC" ~ "PSC MINISTERO CULTURA",
                             ID_PSC == "PSC_MITUR" ~ "PSC MINISTERO TURISMO",
                             ID_PSC == "PSC_MISE" ~ "PSC MINISTERO SVILUPPO ECONOMICO",
                             ID_PSC == "PSC_MUR" ~ "PSC MINISTERO UNIVERSITA' RICERCA SCIENTIFICA",
                             ID_PSC == "PSC_MIPAAF" ~ "PSC MINISTERO POLITICHE AGRICOLO ALIMENTARI FORESTALI",
                             ID_PSC == "PSC_SALUTE" ~ "PSC MINISTERO SALUTE",
                             ID_PSC == "PSC_PCM-SPORT" ~ "PSC PRESIDENZA CONSIGLIO MINISTRI DIPARTIMENTO SPORT",
                             TRUE ~ PSC))
  }
  
  return(df)
}





#' #' Lista programmi non monitorabili
#' #'
#' #' Crea la lista dei programmi programmi non monitorabili
#' #' 
#' #' @param programmi Dati di base da workflow_programmazione().
#' #' @param progetti Dataset di tipo 'progetti' (serve per denominazioni programmi da sito e non da DB)
#' #' @param export vuoi salvare il file?
#' #' @return Lista dei programmi non monitorabili
#' make_lista_nomonit <- function(programmi=NULL, progetti=NULL, export=TRUE){
#'   
#'   
#'   
#'   
#'   if (is.null(programmi)) {
#'     if (is.null(progetti)) {
#'       progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
#'     }
#'     programmi <- workflow_programmazione(use_info=TRUE, use_flt=TRUE, progetti)
#'   }
#'   
#'   info_en <- init_programmazione_info(use_en = TRUE, use_713 = TRUE, sum_po = TRUE, sum_po_last = TRUE, use_po_psc = TRUE) %>%
#'     select(OC_CODICE_PROGRAMMA, LABEL_DECISIONE_EN)
#'   
#'   # programmi_en <- read_csv2(file.path(INPUT, "programmi_SIE_EN.csv")) %>%
#'   programmi_en <- read_xlsx(file.path(DB, "label_programmi_en.xlsx")) %>% 
#'     # select(-LABEL_PROGRAMMA_IT)
#'     distinct(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA_EN)
#'   
#'   # DEV:
#'   # psc <- programmi %>% 
#'   #   filter(x_GRUPPO == "PSC") %>% 
#'   #   distinct(OC_CODICE_PROGRAMMA)
#'   # 
#'   # temp <- read_xlsx(file.path(DB, "fsc_delibere_psc.xlsx")) %>% 
#'   #   mutate(NUMERO_DECISIONE = as.character(NUMERO_DECISIONE)) %>% 
#'   #   rename(AMBITO.x = AMBITO)
#'   # 
#'   # info_2 <- info %>% 
#'   #   anti_join(psc) %>% 
#'   #   bind_rows(temp)
#'   
#'   # label LABEL_PROGRAMMA_EN
#'   programmi <- programmi %>%
#'     left_join(info_en, by = "OC_CODICE_PROGRAMMA") %>%
#'     left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
#'     mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
#'            LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN))
#'   # CHK: duplicato, programmi incrementa di 4
#'   
#'   # temp <- programmi %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
#'   # chk <- programmi %>% semi_join(temp)
#'   # print("Controlla se numerosità è invariata!!!")
#'   # dim(programmi)[1] == dim(programmi_base)[1]
#'   # PON Inclusione
#'   
#'   # make URL_PROGRAMMA 
#'   programmi <- programmi %>%
#'     mutate(LINK_PROGRAMMA_IT = if_else(PUB == TRUE,
#'                                        paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/"),
#'                                        ""),
#'            LINK_PROGRAMMA_EN = if_else(PUB == TRUE,
#'                                        paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/"),
#'                                        "")) %>% 
#'     mutate(LINK_DOC_IT = paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"),
#'            LINK_DOC_EN = paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"))
#'   
#'   
#'   # LABEL_SITO_IT
#'   programmi <- programmi %>%
#'     mutate(LABEL_SITO_IT = if_else(is.na(LINK_SITO), "", "Sito web"),
#'            LABEL_SITO_EN = if_else(is.na(LINK_SITO), "", "Website"),
#'            LABEL_DOC_IT  = if_else(is.na(LINK_DOC_IT), "", "Documenti"),
#'            LABEL_DOC_EN  = if_else(is.na(LINK_DOC_EN), "", "Documents"),)
#'   
#'   programmi <- programmi %>% 
#'     mutate(LINK_DECISIONE = case_when(x_AMBITO == "FSC" ~ LINK_DECISIONE,
#'                                       x_AMBITO == "POC" ~ LINK_DECISIONE,
#'                                       x_AMBITO == "PAC" ~ LINK_DECISIONE,
#'                                       TRUE ~ ""))
#'   
#'   # elimina "da programmare" e "non coesione" (fonte DEF 2020)
#'   # programmi <- programmi %>%
#'   #   filter(OC_CODICE_PROGRAMMA != "FSC_1420_CDD_40", # MEMO: da programmare FSC
#'   #          OC_CODICE_PROGRAMMA != "2020DAPROGR_1", # MEMO: sicilia e anpal
#'   #          OC_CODICE_PROGRAMMA != "2020DAPROGR_2", 
#'   #          OC_CODICE_PROGRAMMA != "TEMP_0713_007", # MEMO: debiti regioni 713
#'   #          OC_CODICE_PROGRAMMA != "DEBITI_CAM")
#'   # dovrà essere superato con OC_FLAG_MONITORAGGIO in init_programmazione_dati
#'   
#'   
#'   # label LABEL_AMBITO_EN
#'   programmi <- programmi %>%
#'     mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
#'     mutate(LABEL_AMBITO_IT = case_when(x_AMBITO == "YEI" ~ "IOG",  # fix YEI
#'                                        x_AMBITO == "SNAI" ~ "SNAI-Servizi",
#'                                        TRUE ~ x_AMBITO),
#'            LABEL_AMBITO_EN = case_when(x_AMBITO == "FESR" ~ "ERDF",
#'                                        x_AMBITO == "FSE" ~ "ESF",
#'                                        x_AMBITO == "FEASR" ~ "EAFRD",
#'                                        x_AMBITO == "FEAMP" ~ "MFF",
#'                                        x_AMBITO == "CTE" ~ "ETC",
#'                                        x_AMBITO == "FSC" ~ "DCF",
#'                                        x_AMBITO == "POC" ~ "COP",
#'                                        x_AMBITO == "SNAI" ~ "IANS",
#'                                        x_AMBITO == "YEI" ~ "YEI"),
#'            LABEL_TIPO_IT = x_GRUPPO,
#'            LABEL_TIPO_EN = case_when(x_GRUPPO == "PON" ~ "NOP",
#'                                      x_GRUPPO == "POR" ~ "ROP",
#'                                      x_GRUPPO == "PATTI" ~ "DEVELOPMENT PACT",
#'                                      x_GRUPPO == "PIANI STRALCIO" ~ "EXCERPT PLAN",
#'                                      x_GRUPPO == "PIANI OPERATIVI" ~ "NATIONAL PLAN",
#'                                      x_GRUPPO == "POC REGIONALI" ~ "REGIONAL COP",
#'                                      x_GRUPPO == "POC NAZIONALI" ~ "NATIONAL COP",
#'                                      x_GRUPPO == "SNAI" ~ "IANS",
#'                                      TRUE ~ x_GRUPPO))
#'   
#'   # maiusc
#'   programmi <- programmi %>% 
#'     mutate(LABEL_PROGRAMMA_IT = toupper(LABEL_PROGRAMMA_IT),
#'            LABEL_PROGRAMMA_EN = toupper(LABEL_PROGRAMMA_EN))
#'   
#'   # FIX nomi psc ministeri creativi
#'   # programmi <- programmi %>% 
#'   #   mutate(LABEL_PROGRAMMA_IT = case_when(OC_CODICE_PROGRAMMA == "PSC_MIT" ~ "PSC MINISTERO INFRASTRUTTURE E MOBILITA' SOSTENIBILE",
#'   #                                         OC_CODICE_PROGRAMMA == "PSC_MATTM" ~ "PSC MINISTERO TRANSIZIONE ECOLOGICA",
#'   #                                         TRUE ~ LABEL_PROGRAMMA_IT))
#'   
#'   # export #
#'   out <- programmi %>%
#'     filter(LABEL_AMBITO_IT != "FEASR", LABEL_AMBITO_IT != "FEAMP") %>% 
#'     mutate(LABEL_TIPO_IT = case_when(LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PATTI" ~ "PATTI",
#'                                      LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PSC" ~ "PSC",
#'                                      LABEL_AMBITO_IT == "FSC" ~ "VARI",
#'                                      LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale" ~ "NAZIONALI",
#'                                      LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
#'                                      LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale" ~ "REGIONALI",
#'                                      LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
#'                                      LABEL_AMBITO_IT == "SNAI-Servizi" ~ "SNAI-SERVIZI",
#'                                      TRUE ~ LABEL_TIPO_IT)) %>% 
#'     mutate(LABEL_TIPO_IT = factor(LABEL_TIPO_IT, levels = c("PSC", "PATTI", "VARI", "POR", "PON", "CTE", "NAZIONALI", "REGIONALI", "COMPLETAMENTI", "SNAI-SERVIZI"))) %>%
#'     mutate(LABEL_TIPO_EN = case_when(LABEL_AMBITO_EN == "DCF" & LABEL_TIPO_EN == "PACTS" ~ "PACTS",
#'                                      LABEL_AMBITO_EN == "DCF" & LABEL_TIPO_EN == "PSC" ~ "PSC",
#'                                      LABEL_AMBITO_EN == "DCF" ~ "OTHERS",
#'                                      LABEL_AMBITO_IT == "COP" & LABEL_TIPO_IT == "POC Nazionale" ~ "NAZIONALI",
#'                                      LABEL_AMBITO_IT == "COP" & LABEL_TIPO_IT == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
#'                                      LABEL_AMBITO_IT == "COP" & LABEL_TIPO_IT == "POC Regionale" ~ "REGIONALI",
#'                                      LABEL_AMBITO_IT == "COP" & LABEL_TIPO_IT == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
#'                                      TRUE ~ LABEL_TIPO_EN)) %>% 
#'     mutate(LABEL_TIPO_EN = factor(LABEL_TIPO_EN, levels = c("PSC", "PACTS", "OTHERS", "NOR", "NOP", "CTE", "NAZIONALI", "REGIONALI", "COMPLETAMENTI", "SNAI-SERVIZI"))) %>%
#'     mutate(LINK_DOC = paste0("../programmi/", OC_CODICE_PROGRAMMA, "/documenti/")) %>% # TEST
#'     mutate(RISORSE = round(RISORSE, 0),
#'            RISORSE_UE = round(RISORSE_UE, 0)) %>% 
#'     mutate(LINK_DOC = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #sere per non generare link su sito
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
#'                                 OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
#'                                 TRUE ~ LINK_DOC)) %>% 
#'     mutate(LABEL_DOC_EN = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #sere per non generare link su sito
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
#'                                     TRUE ~ LABEL_DOC_EN)) %>% 
#'     mutate(LABEL_DOC_IT = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #sere per non generare link su sito
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
#'                                     OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
#'                                     TRUE ~ LABEL_DOC_IT)) %>% 
#'     # deve stare dopo
#'     mutate(OC_CODICE_PROGRAMMA = case_when(OC_CODICE_PROGRAMMA == "TEMP_CTE_TRANS	" ~ "", #sere per non generare link su sito
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_CALABR" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_CAMPAN" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_CULTUR" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_ENERGI" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_SICILI" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "COMP_POC_LEGALI" ~ "",
#'                                            OC_CODICE_PROGRAMMA == "AREEINTASSTEC" ~ "",
#'                                            TRUE ~ OC_CODICE_PROGRAMMA)) %>% 
#'     select(OC_CODICE_PROGRAMMA,
#'            LABEL_PROGRAMMA_IT,
#'            LABEL_PROGRAMMA_EN,
#'            LABEL_CICLO = x_CICLO,
#'            LABEL_AMBITO_IT,
#'            LABEL_AMBITO_EN,
#'            LABEL_TIPO_IT,
#'            LABEL_TIPO_EN,
#'            RISORSE,
#'            RISORSE_UE,
#'            LABEL_DECISIONE_IT,
#'            LABEL_DECISIONE_EN,
#'            LINK_DECISIONE,
#'            LABEL_DOC_IT,
#'            LABEL_DOC_EN,
#'            LINK_DOC, # TEST
#'            # LINK_DOC_IT, 
#'            # LINK_DOC_EN,
#'            LABEL_SITO_IT,
#'            LABEL_SITO_EN,
#'            # LINK_PROGRAMMA_IT, #generate automaticamente su sito
#'            # LINK_PROGRAMMA_EN, #generate automaticamente su sito
#'            LINK_SITO
#'            
#'     ) %>% 
#'     arrange(LABEL_TIPO_IT)
#'   
#'   # split cicli
#'   out_1420 <- out %>% filter(LABEL_CICLO == "2014-2020")
#'   out_713 <- out %>% filter(LABEL_CICLO == "2007-2013")
#'   
#'   if (export == TRUE) {
#'     require(withr)
#'     withr::with_options(
#'       c(scipen = 10), 
#'       write.csv2(out_1420, file.path(TEMP, "programmi_1420.csv"), row.names = FALSE, na = "")
#'     )
#'     withr::with_options(
#'       c(scipen = 10), 
#'       write.csv2(out_713, file.path(TEMP, "programmi_0713.csv"), row.names = FALSE, na = "")
#'     )
#'   }
#'   
#'   return(out)
#' }

#' Copia file DBCOE in nuova versione
#'
#' Copia file DBCOE dalla cartella di una versione precedente a quella di nuova versione, per avviare il setup. Copia tutto tranne FSC.
#' 
#' @param db_path_old Path alla versione più antica del DBCOE.
#' @param db_path_new Path alla versione più recente del DBCOE.
#' @return File DBCOE nel folder della nuova versione
setup_dbcoe_no_fsc <- function(db_path_old, db_path_new=NULL) {
  if (is.null(db_path_new)) {
    db_path_new <- DB
  }
  
  appo <- c("Dati_DBCOE_CTE1420.xlsx",
            "Dati_DBCOE_FEAMP1420.xlsx",
            "Dati_DBCOE_FEASR1420.xlsx",
            # Dati_DBCOE_FSC0713.xlsx,
            # Dati_DBCOE_FSC1420.xlsx,
            "Dati_DBCOE_PAC0713.xlsx",
            "Dati_DBCOE_POC1420.xlsx",
            "Dati_DBCOE_SIE0713.xlsx",
            "Dati_DBCOE_SIE1420.xlsx",
            "Dati_DBCOE_SNAI1420.xlsx",
            # fsc_delibere_po_psc.xlsx,
            # fsc_matrice_po_psc.xlsx,
            "Info_DBCOE_CTE1420.xlsx",
            "Info_DBCOE_FEAMP1420.xlsx",
            "Info_DBCOE_FEASR1420.xlsx",
            # Info_DBCOE_FSC0713.xlsx,
            # Info_DBCOE_FSC1420.xlsx,
            "Info_DBCOE_PAC0713.xlsx",
            "Info_DBCOE_POC1420.xlsx",
            "Info_DBCOE_SIE0713.xlsx",
            "Info_DBCOE_SIE1420.xlsx",
            "Info_DBCOE_SNAI1420.xlsx",
            "label_programmi_en.xlsx",
            "link_sito_programmi.xlsx",
            "Metadati_DB_Programmazione.xlsx")
  
  for (temp in appo) {
    print(paste0("Moving... ", temp))
    file.copy(from = file.path(db_path_old, temp), to = file.path(db_path_new, temp), overwrite = TRUE)
  }
  
}


#' Verifica variazione risorse per ciclo e ambito
#'
#' Verifica variazione risorse per ciclo e ambito. Confronta due dataframe risultanti da make_report_risorse() o i file csv da questa esportati.
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
      risorse_old <- read_csv2(path_to_old)
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
    write.csv2(out, file.path(TEMP, "delta_risorse_ciclo_ambito.csv"), row.names = FALSE)
  }
  
  return(out)
  
}


#' Verifica variazione risorse per programma
#'
#' Verifica variazione risorse per programma. Confronta due dataframe risultanti da make_pagina_programmi() o i file csv da questa esportati.
#'
#' @param programmi_new Dataframe attuale da make_pagina_programmi()
#' @param programmi_old Dataframe precedente da make_pagina_programmi()
#' @param path_to_new Percorso ad attuale folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().
#' @param path_to_old Percorso a precedente folder in cui si trovano i file "programmi_0713.csv" e "programmi_1420.csv" generati con make_pagina_programmi().
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_risorse_programmi <- function(programmi_new=NULL, programmi_old=NULL, path_to_new=NULL, path_to_old=NULL, export=FALSE){
  
  if (is.null(programmi_new)) {
    if (is.null(path_to_new)) {
      message("Indica un file da confrontare")
    } else {
      programmi_new <- read_csv2(file.path(path_to_new, "programmi_0713.csv")) %>% 
        bind_rows(read_csv2(file.path(path_to_new, "programmi_1420.csv")))
    }
  }
  
  if (is.null(programmi_old)) {
    if (is.null(path_to_old)) {
      message("Indica il folder con i file da confrontare")
    } else {
      programmi_old <- read_csv2(file.path(path_to_old, "programmi_0713.csv")) %>% 
        bind_rows(read_csv2(file.path(path_to_old, "programmi_1420.csv")))
    }
  }
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    rename(x_PROGRAMMA = LABEL_PROGRAMMA_IT,
           x_CICLO = LABEL_CICLO,
           x_AMBITO = LABEL_AMBITO_IT,
           x_GRUPPO = LABEL_TIPO_IT) %>% 
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE) %>%
    full_join(programmi_old %>%
                # fix per "PAC"
                as_tibble(.) %>%
                rename(x_PROGRAMMA = LABEL_PROGRAMMA_IT,
                       x_CICLO = LABEL_CICLO,
                       x_AMBITO = LABEL_AMBITO_IT,
                       x_GRUPPO = LABEL_TIPO_IT) %>%
                mutate(x_AMBITO = case_when(x_CICLO == "2007-2013" & x_AMBITO == "POC" ~ "PAC",
                                            TRUE ~ x_AMBITO)) %>%
                refactor_ambito(.) %>%
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, RISORSE),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK = RISORSE.new - RISORSE.old) %>% 
    filter(abs(CHK) > 0)
  
  if (export==TRUE) {
    write.csv2(out, file.path(TEMP, "delta_risorse_programmi.csv"), row.names = FALSE)
  }
  
  return(out)
  
}


