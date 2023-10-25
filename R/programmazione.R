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
  # ciclo <- "2021-2027"
  # ambito <- "FSE"
  
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

#' Inizializza il database della programmazione
#'
#' Carica il databse della programmazione, con pulizia della codifica di aree tematiche e temi prioritari FSC.
#'
#' @param use_temi Vuoi caricare il DB con correzione dei temi prioritari?
#' @param use_sog Vuoi caricare il DB con il soggetto programmatore?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_713 Vuoi caricare anche il DB per il 2007-2013?
#' @param use_ciclo Voi caricare il ciclo?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_CICLO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_CICLO da DB)?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @return L'intero database dei programmazione, suddiviso in 'po_fesr', 'po_fse', 'po_fsc' e 'po_poc'.
init_programmazione_dati <- function(use_temi=FALSE, use_sog=FALSE, use_eu=FALSE, use_flt=FALSE, 
                                     use_713=FALSE, 
                                     use_articolaz=FALSE, use_location=FALSE, use_ciclo=TRUE, tipo_ciclo="CICLO_STRATEGIA", 
                                     # use_en=FALSE, 
                                     use_po_psc=FALSE,
                                     use_fix_siepoc=FALSE,
                                     stime_fix_siepoc=FALSE)
{
  # use_temi = FALSE
  # use_sog= TRUE
  # use_eu= FALSE
  # use_713 = TRUE
  # use_ciclo = FALSE
  # use_flt = FALSE
  # use_location = TRUE
  # use_articolaz = TRUE
  # use_po_psc=TRUE
  
  po_fsc <- load_db("2014-2020", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF aggiunto use_locatione che prima mancava
  po_fesr <- load_db("2014-2020", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_fse <- load_db("2014-2020", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_poc <- load_db("2014-2020", "POC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_yei <- load_db("2014-2020", "YEI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feamp <- load_db("2014-2020", "FEAMP", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_snai <- load_db("2014-2020", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_cte <- load_db("2014-2020", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_feasr <- load_db("2014-2020", "FEASR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) #AF mancava, aggiunto
  
  # NEW 2127
  po_fsc2127 <- load_db("2021-2027", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz) 
  po_fesr2127 <- load_db("2021-2027", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_fse2127 <- load_db("2021-2027", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_jtf2127 <- load_db("2021-2027", "JTF", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_snai2127 <- load_db("2021-2027", "SNAI", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  po_cte2127 <- load_db("2021-2027", "CTE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt, use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
  
  
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
    as.data.frame(.)
  
  if (use_713 == TRUE) {
    po_fsc713 <- load_db("2007-2013", "FSC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fesr713 <- load_db("2007-2013", "FESR", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_fse713 <- load_db("2007-2013", "FSE", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    po_pac713 <- load_db("2007-2013", "PAC", simplify_loc = TRUE, use_temi = use_temi, use_sog = use_sog, use_ue = use_eu, use_flt = use_flt,  use_location = use_location, use_ciclo = use_ciclo, use_articolaz = use_articolaz)
    
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
      ant_siepoc <- read_xlsx(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx")) 
    } else {
      message("errore, le anticipazioni non sono implementate")
    }
    
    # DEV: 
    if ("x_MACROAREA" %in% names(programmi)) {
      ant_siepoc <- ant_siepoc %>%
        rename(x_MACROAREA = MACROAREA) %>% 
        ricodifica_macroaree()
    }
    
    if (stime_fix_siepoc == TRUE) {
      ant_siepoc <- ant_siepoc %>%
        filter(FLAG_FONTE_FORMALE == "SI" | FLAG_FONTE_FORMALE == "NO")
    } else {
      ant_siepoc <- ant_siepoc %>%
        filter(FLAG_FONTE_FORMALE == "SI")
    }

    ant_siepoc <- ant_siepoc %>% 
      mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
             x_AMBITO = AMBITO,
             # x_PROGRAMMA,
             # x_GRUPPO,
             # RISORSE,
             # RISORSE_UE
             )
    
    # DEV: valuta se serve
    ant_siepoc <- ant_siepoc %>%
      select(names(programmi)) %>%
      group_by(across(-"FINANZ_TOTALE")) %>%
      summarise(FINANZ_TOTALE = sum(FINANZ_TOTALE, na.rm = TRUE))
    
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
init_programmazione_info <- function(use_en = FALSE, use_713 = FALSE, sum_po = FALSE, sum_po_last=FALSE, 
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
      bind_rows(info_SNAI)%>% 
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
      ant_siepoc <- read_xlsx(file.path(DB, "Correzioni_DBCOE_SIEPOC.xlsx")) 
      
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
workflow_programmazione <- function(use_flt=TRUE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_location=FALSE, progetti=NULL) {
  
  # DEBUG:
  # use_location=TRUE
  # use_fix_siepoc=FALSE
  
  #load
  interventi <- init_programmazione_dati(use_temi = FALSE, use_sog=TRUE, use_eu=TRUE, use_713 = TRUE, 
                                         use_location = use_location,
                                         use_ciclo = TRUE, use_flt = TRUE, 
                                         use_fix_siepoc = use_fix_siepoc, stime_fix_siepoc = stime_fix_siepoc) %>%
    rename(x_PROGRAMMA = DESCRIZIONE_PROGRAMMA,
           x_GRUPPO = TIPOLOGIA_PROGRAMMA)
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
  }
  
  
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
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI", "CTE", "PAC")))
    refactor_ambito(.)
  
  
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
  if (use_location == TRUE) {
    # programmi <- interventi %>%
    #   # filter(x_GRUPPO != "PSC") %>% # MEMO: qui perde casi 2127
    #   filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
    #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, AMMINISTRAZIONE, PUB, x_MACROAREA, CAT_REGIONE) %>%
    #   left_join(interventi %>%
    #               # filter(x_GRUPPO != "PSC") %>% 
    #               filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
    #               group_by(OC_CODICE_PROGRAMMA) %>%
    #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
    #                         RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
    #             by = "OC_CODICE_PROGRAMMA")
    # DEV: spostato in widget pagina programmi
    programmi <- interventi %>%
      # filter(x_GRUPPO != "PSC") %>% 
      # filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
      group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_GRUPPO, x_CICLO, AMMINISTRAZIONE, PUB, x_MACROAREA, CAT_REGIONE) %>%
      summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
                RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))

    # accoda PSC (sono scorporatoi sopra perché pluri-ciclo?)
    # psc <- interventi %>%
    #   filter(x_GRUPPO == "PSC") %>% 
    #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, PUB, x_MACROAREA) %>%
    #   left_join(interventi %>%
    #               filter(x_GRUPPO == "PSC") %>% 
    #               group_by(OC_CODICE_PROGRAMMA, x_MACROAREA) %>%
    #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
    #                         RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
    #             by = "OC_CODICE_PROGRAMMA") %>% 
    #   mutate(x_CICLO = "2014-2020")
    # psc <- interventi %>%
    #   filter(x_GRUPPO == "PSC") %>% 
    #   group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, AMMINISTRAZIONE, PUB, x_MACROAREA, CAT_REGIONE) %>%
    #   summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
    #             RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
    # DEV: non serve più lo split dei PSC, che ora sono tutti nel DBCOE e sono gestiti nel widget per sdoppiamento

    
  } else {
  # programmi <- interventi %>%
  #   # filter(x_GRUPPO != "PSC") %>% # MEMO: qui perde casi 2127
  #   filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
  #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, AMMINISTRAZIONE, PUB) %>%
  #   left_join(interventi %>%
  #               # filter(x_GRUPPO != "PSC") %>% 
  #               filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
  #                         RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA")
  # DEV: spostato in widget pagina programmi 
  programmi <- interventi %>%
    # filter(x_GRUPPO != "PSC") %>% 
    # filter(x_GRUPPO != "PSC" | is.na(x_GRUPPO)) %>% 
    group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_GRUPPO, x_CICLO, AMMINISTRAZIONE, PUB) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # accoda PSC (sono scorporatoi sopra perché pluri-ciclo?)
  # psc <- interventi %>%
  #   filter(x_GRUPPO == "PSC") %>% 
  #   distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, PUB) %>%
  #   left_join(interventi %>%
  #               filter(x_GRUPPO == "PSC") %>% 
  #               group_by(OC_CODICE_PROGRAMMA) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
  #                         RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE)),
  #             by = "OC_CODICE_PROGRAMMA") %>% 
  #   mutate(x_CICLO = "2014-2020")
  # psc <- interventi %>%
  #   filter(x_GRUPPO == "PSC") %>% 
  #   group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, AMMINISTRAZIONE, PUB) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
  #             RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  # DEV: non serve più lo split dei PSC, che ora sono tutti nel DBCOE e sono gestiti nel widget per sdoppiamento
  
  }
  
  # programmi <- programmi %>% 
  #   bind_rows(psc) 
  # DEV: non serve più lo split dei PSC, che ora sono tutti nel DBCOE e sono gestiti nel widget per sdoppiamento
  
  # ripristina x_GRUPPO (tolto sopra per evitare duplicazione in summarise sopra)
  # programmi <- programmi %>%
  #   left_join(interventi %>%
  #               # patch per programmi su due ambiti o su due cicli nello stesso ambito (con gruppi diversi)
  #               mutate(x_GRUPPO =  case_when(OC_CODICE_PROGRAMMA == "2007IT001FA005" ~ "NAZ-INF",
  #                                            OC_CODICE_PROGRAMMA == "2007IT005FAMG1" ~ "PAC Nazionale",
  #                                            OC_CODICE_PROGRAMMA == "2007SA002FA016" ~ "REG",
  #                                            OC_CODICE_PROGRAMMA == "2016XXAMPSAP00" ~ "Piani nazionali",
  #                                            OC_CODICE_PROGRAMMA == "2017TOPIOMBIFSC" ~ "Altre assegnazioni CIPE",
  #                                            OC_CODICE_PROGRAMMA == "CIS_TA_PUG" ~ "Altre assegnazioni CIPE",
  #                                            OC_CODICE_PROGRAMMA == "TEMP_0713_020" ~ "Altre assegnazioni CIPE",
  #                                            TRUE ~ x_GRUPPO)) %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_GRUPPO) %>%
  #               filter(!is.na(x_GRUPPO)),
  #             by = "OC_CODICE_PROGRAMMA")
  
  # rewrite x_PROGRAMMA su label sito
  programmi <- programmi %>%
    left_join(label_programmi) %>%
    mutate(x_PROGRAMMA = if_else(is.na(OC_DESCRIZIONE_PROGRAMMA), x_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)) %>%
    select(-OC_DESCRIZIONE_PROGRAMMA)
  
  # #aggiunge informazioni di supporto
  # if (use_info == TRUE) {
  #   info_last <- init_programmazione_info(use_en = FALSE, use_713 = TRUE, sum_po = TRUE, sum_po_last = TRUE)
  #   
  #   programmi_base <- programmi
  #   programmi <- programmi %>%
  #     left_join(info_last,
  #               by = "OC_CODICE_PROGRAMMA")
  #   # CHK: duplicato, programmi passa da 405 a 406
  #   # temp <- programmi %>% count(OC_CODICE_PROGRAMMA) %>% filter(n > 1)
  #   # chk <- programmi %>% semi_join(temp)
  #   print("Controlla se numerosità è invariata!!!")
  #   dim(programmi)[1] == dim(programmi_base)[1]
  # }
  # DEV: eliminata perché inutilizzata
  
  
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
                                x_AMBITO == "FSC" & x_GRUPPO != "PSC" ~ "VARI",
                                x_AMBITO == "PAC" & x_GRUPPO == "PAC Nazionale" ~ "NAZIONALI",
                                x_AMBITO == "PAC" & x_GRUPPO == "PAC Regionale" ~ "REGIONALI",
                                TRUE ~ x_GRUPPO))

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
#' @param use_ciclo Voi caricare il ciclo?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @return Il dataset di programmazione per l'ambito richiesto, con pulizia delle denominazioni territoriali e della codifica di aree tematiche e temi prioritari FSC.
load_db_interventi <- function(tipo, simplify_loc=FALSE, use_temi=FALSE, use_sog=FALSE, use_ue=FALSE, 
                               use_flt=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=TRUE, tipo_ciclo="CICLO_RISORSE"){
  
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
  var_ls <- c("ID", "OC_CODICE_PROGRAMMA", "OC_TITOLO_PROGETTO",
              "AMBITO",
              "FINANZ_TOTALE_PUBBLICO")
  
  if (tipo == "CIS") {
    var_ls <- c(var_ls, "FINANZ_FSC", "FINANZ_FS", "FINANZ_PAC", "FINANZ_ALTRO", "ID_PSC")
  }
  
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
    appo <- refactor_ciclo(appo) # DEV: spostato qui altrimenti manca x_CICLO
  }
  
  appo <- appo %>%
    mutate(x_AMBITO = AMBITO) 
  appo <- refactor_ambito(appo)
  # appo <- refactor_ciclo(appo)
  
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
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param force_yei Logico. Vuoi forzare FSE in YEI?
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e macroarea.
make_report_risorse <- function(ciclo=NULL, use_meuro=FALSE, use_flt=FALSE, use_eu=FALSE, use_po_psc=FALSE, 
                                use_fix_siepoc=TRUE, stime_fix_siepoc=FALSE, force_yei=FALSE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE) {
  
  # DEBUG:
  # use_po_psc=TRUE
  # use_fix_siepoc=TRUE
  # use_flt=TRUE
  # use_eu=TRUE
  # tipo_ciclo="CICLO_STRATEGIA"
  
  programmi <- init_programmazione_dati(use_temi = FALSE, use_713 = TRUE, use_location = TRUE, use_ciclo = TRUE, use_eu=use_eu, 
                                        use_flt=use_flt, tipo_ciclo=tipo_ciclo, use_po_psc=use_po_psc, 
                                        use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) 
  
  # NEW: split REACT-EU per allineamento a struttura tavole
  programmi <- programmi %>% 
    mutate (x_AMBITO=case_when(x_AMBITO=="FESR" & CAT_REGIONE=="REACT"~ "FESR_REACT",
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
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export Vuoi salvare il file?
#' @return Lista dei programmi 2007-2013 e 2014-2020 applicando le convenzioni per la pubblicazione nella pagina "programmi" del sito.
make_pagina_programmi <- function(programmi=NULL, progetti=NULL, use_fix_siepoc=TRUE, stime_fix_siepoc=FALSE,
                                  export=TRUE){
  
  # DEBUG:
  # use_fix_siepoc=TRUE
  
  if (is.null(programmi)) {
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
    }
    programmi <- workflow_programmazione(use_flt=TRUE, use_fix_siepoc=use_fix_siepoc, progetti=progetti)
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

  # integra info (compreso inglese)
  info_last <- init_programmazione_info(use_en = TRUE, use_713 = TRUE, 
                                        sum_po = TRUE, sum_po_last = TRUE,
                                        use_po_psc = FALSE, use_fix_siepoc=use_fix_siepoc)
  temp <- dim(programmi)[1]
  programmi <- programmi %>%
    left_join(info_last,
              by = "OC_CODICE_PROGRAMMA")
  x <- dim(programmi)[1] == temp
  if (x == FALSE) {
    print("Controlla la numerosità dell'elenco dei programmi, è variata nel join tra dati e info!!!")
  }
  rm(temp)

  # label programmi in inglese
  programmi_en <- read_xlsx(file.path(DB, "label_programmi_en.xlsx")) %>% 
    distinct(OC_CODICE_PROGRAMMA, LABEL_PROGRAMMA_EN)
  
  # # chk
  # programmi %>% 
  #   anti_join(programmi_en, by = "OC_CODICE_PROGRAMMA")

  # integra label inglese
  programmi <- programmi %>%
    left_join(programmi_en, by = "OC_CODICE_PROGRAMMA") %>%
    mutate(LABEL_PROGRAMMA_IT = x_PROGRAMMA,
           LABEL_PROGRAMMA_EN = if_else(is.na(LABEL_PROGRAMMA_EN), LABEL_PROGRAMMA_IT, LABEL_PROGRAMMA_EN))

  # make URL_PROGRAMMA 
  programmi <- programmi %>%
    # mutate(LINK_PROGRAMMA_IT = if_else(PUB == TRUE,
    #                                    paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/"),
    #                                    ""),
    #        LINK_PROGRAMMA_EN = if_else(PUB == TRUE,
    #                                    paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/"),
    #                                    "")) %>% 
    mutate(LINK_DOC_IT = paste0("https://opencoesione.gov.it/it/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"),
           LINK_DOC_EN = paste0("https://opencoesione.gov.it/en/programmi/", OC_CODICE_PROGRAMMA, "/documenti/"))
    
  
  # LABEL_SITO_IT
  programmi <- programmi %>%
    mutate(LABEL_SITO_IT = if_else(is.na(LINK_SITO), "", "Sito web"),
           LABEL_SITO_EN = if_else(is.na(LINK_SITO), "", "Website"),
           LABEL_DOC_IT  = if_else(is.na(LINK_DOC_IT), "", "Documenti"),
           LABEL_DOC_EN  = if_else(is.na(LINK_DOC_EN), "", "Documents"),)
  
  # link decisione
  programmi <- programmi %>% 
    mutate(LINK_DECISIONE = case_when(x_AMBITO == "FSC" ~ LINK_DECISIONE,
                                      x_AMBITO == "POC" ~ LINK_DECISIONE,
                                      # x_AMBITO == "PAC" ~ LINK_DECISIONE,
                                      TRUE ~ ""))
  
  # label ambito e tipo
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    mutate(LABEL_AMBITO_IT = x_AMBITO,
           # LABEL_AMBITO_IT = case_when(x_AMBITO == "YEI" ~ "IOG",  # fix YEI
                                       # # x_AMBITO == "SNAI" ~ "SNAI-SERVIZI",
                                       # x_AMBITO == "SNAI" ~ "SNAI-Servizi",
                                       # TRUE ~ x_AMBITO),
           # MEMO: qui x_AMBITO è già modificato da workflow
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
           # LABEL_TIPO_IT = case_when(x_AMBITO == "SNAI" ~ "SNAI-SERVIZI",
           #                           TRUE ~ x_GRUPPO),
           LABEL_TIPO_EN = case_when(x_GRUPPO == "PON" ~ "NOP",
                                     x_GRUPPO == "POR" ~ "ROP",
                                     x_GRUPPO == "PATTI" ~ "DEVELOPMENT PACT",
                                     x_GRUPPO == "PIANI STRALCIO" ~ "EXCERPT PLAN",
                                     x_GRUPPO == "PIANI OPERATIVI" ~ "NATIONAL PLAN",
                                     x_GRUPPO == "POC REGIONALI" ~ "REGIONAL COP",
                                     x_GRUPPO == "POC NAZIONALI" ~ "NATIONAL COP",
                                     x_AMBITO == "JTF" ~ "JTF",
                                     x_AMBITO == "IOG" ~ "YEI",
                                     x_AMBITO == "ALTRO" & grepl("SNAI", x_GRUPPO) ~ "IANS-SERVICES",
                                     x_AMBITO == "ALTRO" ~ "OTHERS",
                                     TRUE ~ x_GRUPPO))
  
  # maiusc
  programmi <- programmi %>%
    mutate(# LABEL_PROGRAMMA_IT = toupper(LABEL_PROGRAMMA_IT),
           LABEL_PROGRAMMA_EN = toupper(LABEL_PROGRAMMA_EN))
  # DEV: riportato in workflow

  # export #
  out <- programmi %>%
    filter(LABEL_AMBITO_IT != "FEASR", LABEL_AMBITO_IT != "FEAMP") %>% 
    # mutate(LABEL_TIPO_IT = case_when(LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PATTI" ~ "PATTI",
    #                                 LABEL_AMBITO_IT == "FSC" & LABEL_TIPO_IT == "PSC" ~ "PSC",
    #                                 LABEL_AMBITO_IT == "FSC" ~ "VARI",
    #                                 LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale" ~ "NAZIONALI",
    #                                 LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
    #                                 LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale" ~ "REGIONALI",
    #                                 LABEL_AMBITO_IT == "POC" & LABEL_TIPO_IT == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
    #                                 LABEL_AMBITO_IT == "SNAI-Servizi" ~ "SNAI-Servizi",
    #                                 LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "PAC Nazionale" ~ "NAZIONALI",
    #                                 LABEL_AMBITO_IT == "PAC" & LABEL_TIPO_IT == "PAC Regionale" ~ "REGIONALI",
    #                                 LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT001FA005" ~ "NAZIONALI", # fix per direttrici ferroviarie
    #                                 TRUE ~ LABEL_TIPO_IT)) %>% 
    # DEV: riportato in workflow
    # mutate(LABEL_TIPO_IT = factor(LABEL_TIPO_IT, levels = c("PSC", "PATTI", "VARI", "POR", "PON", "POIN", "CTE", "NAZIONALI", "REGIONALI", "COMPLETAMENTI", "SNAI-SERVIZI"))) %>%
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
    # mutate(LABEL_TIPO_EN = factor(LABEL_TIPO_EN, levels = c("PSC", "PACTS", "OTHERS", "ROP", "NOP", "INOP","CTE", "NAZIONAL", "REGIONAL", "COMPLETAMENTI", "IANS"))) %>%
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
                                TRUE ~ LABEL_DOC_IT)) # %>% 
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
    # select(OC_CODICE_PROGRAMMA,
    #        LABEL_PROGRAMMA_IT,
    #        LABEL_PROGRAMMA_EN,
    #        LABEL_CICLO = x_CICLO,
    #        LABEL_AMBITO_IT,
    #        LABEL_AMBITO_EN,
    #        LABEL_TIPO_IT,
    #        LABEL_TIPO_EN,
    #        RISORSE,
    #        RISORSE_UE,
    #        LABEL_DECISIONE_IT,
    #        LABEL_DECISIONE_EN,
    #        LINK_DECISIONE,
    #        LABEL_DOC_IT,
    #        LABEL_DOC_EN,
    #        LINK_DOC, # TEST
    #        # LINK_DOC_IT, 
    #        # LINK_DOC_EN,
    #        LABEL_SITO_IT,
    #        LABEL_SITO_EN,
    #        # LINK_PROGRAMMA_IT, #generate automaticamente su sito
    #        # LINK_PROGRAMMA_EN, #generate automaticamente su sito
    #        LINK_SITO
# 
#     ) %>% 
#     arrange(LABEL_TIPO_IT)
  
  # # fix per CPT 713
  # cpt <- programmi %>%
  #   filter(x_CICLO == "2007-2013", x_GRUPPO == "CPT")
  # DEV: deprecato, già modificato flag monitoraggio

  # out2 <- out %>%
  #   anti_join(cpt, by = "OC_CODICE_PROGRAMMA") %>% 
  #   bind_rows(
  #     tibble(
  #       OC_CODICE_PROGRAMMA = "CPT_0713",
  #       LABEL_PROGRAMMA_IT = "CONTI PUBBLICI TERRITORIALI",
  #       LABEL_PROGRAMMA_EN = "PUBLIC TERRITORIAL ACCOUNTS",
  #       LABEL_CICLO = "2007-2013",
  #       LABEL_AMBITO_IT = "FSC",
  #       LABEL_AMBITO_EN = "DCF",
  #       LABEL_TIPO_IT = "VARI",
  #       LABEL_TIPO_EN = "OTHERS",
  #       RISORSE = sum(cpt$RISORSE),
  #       RISORSE_UE = 0,
  #       LABEL_DECISIONE_IT = "Delibera n. 42 del 23/03/2012",
  #       LABEL_DECISIONE_EN = "Resolution n. 42 - 23/03/2012",
  #       LINK_DECISIONE = "",
  #       LABEL_DOC_IT = "Documenti",
  #       LABEL_DOC_EN = "Documents",
  #       LINK_DOC = "",
  #       LABEL_SITO_IT = "",
  #       LABEL_SITO_EN = "",
  #       LINK_SITO = ""))
  out2 <- out
  
  # fix per caricamento su OC
  out3 <- out2 # %>% 
    # mutate(LABEL_TIPO_IT = as.character(LABEL_TIPO_IT),
    #        LABEL_TIPO_EN = as.character(LABEL_TIPO_EN)) %>% 
    # mutate_if(is.character, list(~gsub("À", "A'", .))) %>% 
    # mutate_if(is.character, list(~gsub("à", "a'", .))) %>% 
    # mutate_if(is.character, list(~replace_na(., ""))) 
  
  out4 <- out3 # %>% 
    # fix programmi anomali PAC
    # filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007SA002FA016")) %>% 
    # filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT001FA005")) %>% 
    # filter(!(LABEL_AMBITO_IT == "PAC" & OC_CODICE_PROGRAMMA == "2007IT005FAMG1")) # %>% 
    # # fix programmi anomali FSC 713
    # filter(OC_CODICE_PROGRAMMA != "TEMP_0713_006",
    #        OC_CODICE_PROGRAMMA != "TEMP_0713_999",
    #        OC_CODICE_PROGRAMMA != "TEMP_0713_005",
    #        OC_CODICE_PROGRAMMA != "CPT_0713")
  # DEV: deprecato, già modificato flag monitoraggio
  
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
  
  # DEV: spostato sopra
  # # NEW: elimina dupli di 713
  # out <- out4 %>% 
  #   # filter(!(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MUR"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_LAZIO"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISE"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIT"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISALUTE"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_PCM-SPORT"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIPAAF"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MITUR"),
  #   #        # fix per nuovi codici
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCTURISMO"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSVILECONOM"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCLAZIO"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSALUTE"),
  #   #        !(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCUNIVRICERCA")) 
  #   mutate(TEMP = case_when(LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MUR" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_LAZIO" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISE" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIT" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MISALUTE" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_PCM-SPORT" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MIPAAF" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSC_MITUR" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCTURISMO" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSVILECONOM" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCLAZIO" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCSALUTE" ~ 0,
  #                           LABEL_CICLO == "2007-2013" & OC_CODICE_PROGRAMMA == "PSCUNIVRICERCA" ~ 0,
  #                           TRUE ~ 1)) %>% 
  #            filter(TEMP == 1) %>% 
  #   select(-TEMP)

  
  out <- out4 %>% 
    ungroup() %>% 
    # select(-x_PROGRAMMA, -x_AMBITO) %>% 
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
           LINK_SITO) %>% 
    arrange(LABEL_TIPO_IT)
  
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
  
  # NEW 2127
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
      write.csv2(out_1420, file.path(OUTPUT, "programmi_1420.csv"), row.names = FALSE, na = "")
      )
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_713, file.path(OUTPUT, "programmi_0713.csv"), row.names = FALSE, na = "")
    )
    # NEW 2127
    withr::with_options(
      c(scipen = 10), 
      write.csv2(out_2127, file.path(OUTPUT, "programmi_2127.csv"), row.names = FALSE, na = "")
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
    mutate(LABEL_PROGRAMMA = x_PROGRAMMA,
           LABEL_AMBITO = x_AMBITO) %>% 
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
    # mutate(x_AMBITO = as.character(x_AMBITO)) %>% 
    # mutate(LABEL_PROGRAMMA = toupper(x_PROGRAMMA),
    #        LABEL_AMBITO = case_when(x_AMBITO == "YEI" ~ "IOG",
    #                                 x_AMBITO == "SNAI" ~ "SNAI-Servizi",
    #                                 TRUE ~ x_AMBITO)) %>% 
    # DEV: riportato in workflow
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
  # out <- out %>% 
  #   mutate(OC_TIPOLOGIA_PROGRAMMA = case_when(LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PATTI" ~ "PATTI",
  #                                             LABEL_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "PSC" ~ "PSC",
  #                                             # LABEL_AMBITO == "FSC" & grepl("PSC_", OC_CODICE_PROGRAMMA) ~ "PSC", # fix per NA
  #                                             LABEL_AMBITO == "FSC" ~ "VARI",
  #                                             LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "NAZIONALI",
  #                                             LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "COMPLETAMENTI",
  #                                             LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "REGIONALI",
  #                                             LABEL_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "COMPLETAMENTI",
  #                                             LABEL_AMBITO == "SNAI-Servizi" ~ "SNAI-SERVIZI",
  #                                             TRUE ~ OC_TIPOLOGIA_PROGRAMMA))
  # DEV: riportato in workflow
  
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
        bind_rows(read_csv2(file.path(path_to_new, "programmi_1420.csv")))
    }
  }
  
  if (is.null(programmi_old)) {
    if (is.null(path_to_old)) {
      message("Indica il folder con i file da confrontare")
    } else {
      programmi_old <- read_csv2(file.path(path_to_old, "programmi_0713.csv"), locale=locale(encoding=encoding_old)) %>% 
        bind_rows(read_csv2(file.path(path_to_old, "programmi_1420.csv"), locale=locale(encoding=encoding_old)))
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
                select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,x_PROGRAMMA, x_GRUPPO, RISORSE),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_PROGRAMMA", "x_GRUPPO"),
              suffix = c(".new", ".old")) %>%
    mutate(RISORSE.old = if_else(is.na(RISORSE.old), 0, RISORSE.old),
           RISORSE.new = if_else(is.na(RISORSE.new), 0, RISORSE.new)) %>%
    mutate(CHK = RISORSE.new - RISORSE.old) %>% 
    filter(abs(CHK) > 0)
  
  # 2017AREAINTABRU
  
  if (export==TRUE) {
    write.csv2(out, file.path(TEMP, "delta_risorse_programmi.csv"), row.names = FALSE)
  }
  
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
  
  temp <- init_programmazione_dati(use_713 = TRUE, use_flt = TRUE)
  
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
#' @param progetti Dataset progetti importato con load_progetti
#' @return Il file "label_programmi_en.xlsx" viene copiato dalla versione precedente a quella corrente e integrato con i nuovi programmi
update_lista_programmi_sitiweb <- function(db_old, progetti=NULL) {
  
  # DEBUG:
  # db_new="20230630.00"
  # db_old="20230430.00"
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, visualizzati=TRUE, light=TRUE)
  }
  
  appo <- read_xlsx(file.path(dirname(DB), db_old, "link_sito_programmi.xlsx")) %>% 
    mutate(NUOVI = 0) # %>%
    # filter(!(x_AMBITO %in% c("FEAMP", "FEASR")))
    # mutate(toupper(DENOM_PROGRAMMA)) # DEV: primo giro
  
  temp <- init_programmazione_dati(use_713 = TRUE, use_flt = TRUE)
  
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
  
  # label da progetti pubblicati per allineamento a sito
  label_programmi <- progetti %>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA) %>% 
    separate_rows(OC_DESCRIZIONE_PROGRAMMA, OC_CODICE_PROGRAMMA, sep = ":::")%>%
    distinct(OC_CODICE_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)
  
  # rewrite x_PROGRAMMA su label sito
  appo2 <- appo1 %>%
    left_join(label_programmi) %>%
    mutate(DENOM_PROGRAMMA = if_else(is.na(OC_DESCRIZIONE_PROGRAMMA), DENOM_PROGRAMMA, OC_DESCRIZIONE_PROGRAMMA)) %>%
    select(-OC_DESCRIZIONE_PROGRAMMA)
  
  write.xlsx(appo2, file.path(DB, "link_sito_programmi.xlsx"))
  
}


