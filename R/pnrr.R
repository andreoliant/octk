# Funzioni per la gestione del PNRR

#' Carica dati di attuazione PNRR
#'
#' Carica dati di attuazione PNRR
#'
#' @param bimestre_pnrr Bimestre di riferimento per i dati PNRR
#' @param versione_pnrr Versione di riferimento dei dati (sono possibili più versioni per lo stesso bimestre)
#' @return Dataframe
load_progetti_pnrr <- function(bimestre_pnrr, versione_pnrr) {
  
  progetti_pnrr <- read_csv2(file.path(DRIVE, "DATI", "PNRR", "progetti_pnrr", paste0("progetti_pnrr_", bimestre_pnrr, "_", versione_pnrr, ".csv")),
                             guess_max = 1000000) %>%
    mutate(COD_PROCED_ATTIVAZIONE = as.character(COD_PROCED_ATTIVAZIONE))
  
  return(progetti_pnrr)
}


#' Carica dati di programmazione PNRR dal database della programmazione
#'
#' Carica dati di programmazione PNRR dal database della programmazione
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @return Dataframe
load_db_pnrr <- function(DB) {
  
  pnrr <- read_xlsx(file.path(DB, "Dati_DBCOE_PNRR.xlsx"))

  return(pnrr)
}


#' Carica dati di programmazione PNRR dal database della programmazione
#'
#' Carica dati di programmazione PNRR dal database della programmazione
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto.
#' @param scarico_italiadomani Data della versione del dataset programmazione su italiadomani in formato AAAAMMGG (https://www.italiadomani.gov.it/content/sogei-ng/it/it/catalogo-open-data/quadro-finanziario-pnrr.html).
#' @param export Vuoi esportare nel DBCOE?
#' @return Dataframe
setup_db_pnrr <- function(DB, scarico_italiadomani, export=TRUE) {
  
  appo <- read_csv2(file.path(DRIVE, "DATI", "PNRR", "opendata_italiadomani", scarico_italiadomani, "OpenData_PNRR_QuadroFinProgrammazione.csv"))

  appo1 <- appo %>%
  mutate(
  CICLO_PROGRAMMAZIONE = "2021-2027",
  TIPOLOGIA_AMMINISTRAZIONE = "NAZ",
  FLAG_MONITORAGGIO = 1,
  AMM_TIT = case_when(
    `Amministrazione Titolare` == "GIUST AMM.VA (CONSIGLIO DI STATO E TAR)" ~ "MGG",
    `Amministrazione Titolare` == "MIN AFFARI ESTERI E COOPERAZ INT" ~ "MAECI",
    `Amministrazione Titolare` == "MIN AGRIC. SOVRANITA' ALIM. E FORESTE" ~ "MASAF",
    `Amministrazione Titolare` == "MIN AMBIENTE E SICUREZZA ENERGETICA" ~ "MASE",
    `Amministrazione Titolare` == "MIN DELL'UNIVERSITA' E DELLA RICERCA" ~ "MUR",
    `Amministrazione Titolare` == "MINISTERO DEL LAVORO E POLITICHE SOCIALI" ~ "MLPS",
    `Amministrazione Titolare` == "MINISTERO DEL TURISMO" ~ "MiTur",
    `Amministrazione Titolare` == "MINISTERO DELL'ECONOMIA E DELLE FINANZE" ~ "MEF",
    `Amministrazione Titolare` == "MINISTERO DELL'INTERNO" ~ "MINT",
    `Amministrazione Titolare` == "MINISTERO DELL'ISTRUZIONE E MERITO" ~ "MIM",
    `Amministrazione Titolare` == "MINISTERO DELLA CULTURA" ~ "MIC",
    `Amministrazione Titolare` == "MINISTERO DELLA GIUSTIZIA" ~ "MGG",
    `Amministrazione Titolare` == "MINISTERO DELLA SALUTE" ~ "MS",
    `Amministrazione Titolare` == "MINISTERO IMPRESE E DEL MADE IN ITALY" ~ "MIMIT",
    `Amministrazione Titolare` == "MINISTERO INFRASTRUTTURE E TRASPORTI" ~ "MIT",
    `Amministrazione Titolare` == "PCM - DIP AFFARI REGIONALI E AUTONOMIE" ~ "PCM-DARA",
    `Amministrazione Titolare` == "PCM - DIP PARI OPPORTUNITA E FAMIGLIA" ~ "PCM-DPO",
    `Amministrazione Titolare` == "PCM - DIP POL GIOVAN E SERV CIV UNIVERS" ~ "PCM-DPGSCU",
    `Amministrazione Titolare` == "PCM - DIP PROTEZIONE CIVILE" ~ "PCM-DPC",
    `Amministrazione Titolare` == "PCM - DIPARTIM. TRASFORMAZIONE DIGITALE" ~ "DTD",
    `Amministrazione Titolare` == "PCM - DIPARTIMENTO FUNZIONE PUBBLICA" ~ "MinPA",
    `Amministrazione Titolare` == "PCM - DIPARTIMENTO PER LO SPORT" ~ "PCM-DS",
    `Amministrazione Titolare` == "PCM - PRESIDENZA CONSIGLIO MINISTRI" ~ "PCM-SG",
    `Amministrazione Titolare` == "PCM - STRUTTURA COMMISS. RICOSTRUZIONE" ~ "PCM-COMM",
    `Amministrazione Titolare` == "PCM - STRUTTURA DI MISSIONE PNRR" ~ "PCM-PNRR",
    `Amministrazione Titolare` == "PCM - UFF POL IN FAV PERS CON DISABILITA" ~ "PCM-DISA",
    `Amministrazione Titolare` == "PCM - DIP POLITICHE DI COESIONE" ~ "MinCOE",
  )) %>%
    select(
      AMBITO = Programma,
      OC_CODICE_PROGRAMMA = `Codice Univoco Submisura`,
      DESCRIZIONE_PROGRAMMA = `Descrizione Sub-Misura`,
      CICLO_PROGRAMMAZIONE,
      AMMINISTRAZIONE  = `Amministrazione Titolare`,
      TIPOLOGIA_AMMINISTRAZIONE,
      AMM_TIT,
      # id_regis         = "???",
      COD_MISSIONE      = Missione,
      DESCR_MISSIONE         = `Descrizione Missione`,
      COD_COMPONENTE    = Componente,
      DESCR_COMPONENTE       = `Descrizione Componente`,
      # id_misura        = `ID Misura`,
      COD_MISURA    = `Codice Univoco Misura`,
      DESCR_MISURA           = `Descrizione Misura`,
      # id_submisura     = `ID Sub-Misura`,
      CID              = `Codice CID`,
      TIPOLOGIA_PROGRAMMA = `Investimento o Riforma`,
      SUB_TIPOLOGIA_PROGRAMMA   = `Tipo Livello`,
      FONTE_PROGRAMMA     = `Prestito o Sovvenzione`,
      RISORSE          = `Importo Totale`,
      RISORSE_INESSERE = `Importo Progetti in essere`,
      RISORSE_NUOVI    = `Importo Progetti nuovi`,
      FLAG_MONITORAGGIO
    )

  risorse_pnrr <- appo1

  # export
  write.xlsx(risorse_pnrr, file.path(OUTPUT, "Dati_DBCOE_PNRR.xlsx"))

  if (export == TRUE) {
    file.copy(from = file.path(OUTPUT, "Dati_DBCOE_PNRR.xlsx"), 
                to = file.path(DB, "Dati_DBCOE_PNRR.xlsx"),
                overwrite = TRUE)
  }
}


#' Confronta due versioni del database PNRR
#'
#' Confronta due versioni del database PNRR identificando misure nuove, cancellate e con importi modificati
#'
#' @param DB_new Percorso al database più recente
#' @param DB_old Percorso al database precedente
#' @return Lista con i dataframe delle differenze
chk_delta_db_pnrr <- function(DB_new, DB_old) {
  
  # Carico i due database
  pnrr_new <- load_db_pnrr(DB_new)
  pnrr_old <- load_db_pnrr(DB_old)
  
  # Identifico misure nuove (presenti in new ma non in old)
  misure_nuove <- pnrr_new %>%
    anti_join(pnrr_old, by = "OC_CODICE_PROGRAMMA")
  
  # Identifico misure cancellate (presenti in old ma non in new)
  misure_cancellate <- pnrr_old %>%
    anti_join(pnrr_new, by = "OC_CODICE_PROGRAMMA")
  
  # Identifico misure con importi modificati
  misure_modificate <- pnrr_new %>%
    full_join(pnrr_old, by = "OC_CODICE_PROGRAMMA", 
              suffix = c("_new", "_old")) %>%
    filter(RISORSE_new != RISORSE_old |
           RISORSE_INESSERE_new != RISORSE_INESSERE_old |
           RISORSE_NUOVI_new != RISORSE_NUOVI_old) %>%
    select(
      OC_CODICE_PROGRAMMA,
      DESCRIZIONE_PROGRAMMA = DESCRIZIONE_PROGRAMMA_new,
      RISORSE_new, RISORSE_old,
      RISORSE_INESSERE_new, RISORSE_INESSERE_old,
      RISORSE_NUOVI_new, RISORSE_NUOVI_old,
      AMMINISTRAZIONE = AMMINISTRAZIONE_new
    ) %>%
    mutate(
      delta_RISORSE = RISORSE_new - RISORSE_old,
      delta_RISORSE_INESSERE = RISORSE_INESSERE_new - RISORSE_INESSERE_old,
      delta_RISORSE_NUOVI = RISORSE_NUOVI_new - RISORSE_NUOVI_old
    )
  
  # Creo lista con tutti i risultati
  risultati <- list(
    "Misure Nuove" = misure_nuove,
    "Misure Cancellate" = misure_cancellate,
    "Misure Modificate" = misure_modificate
  )
  
  # Salvo risultati in Excel
  write.xlsx(risultati, file.path(TEMP, paste0("delta_risorse_pnrr.xlsx")))
  
  # Stampo riepilogo
  print(paste("Numero di misure nuove:", nrow(misure_nuove)))
  print(paste("Numero di misure cancellate:", nrow(misure_cancellate)))
  print(paste("Numero di misure modificate:", nrow(misure_modificate)))
  
  return(risultati)
}


#' Carica dati progetti REGIS
#'
#' Carica i dati dei progetti dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati dei progetti
#' @export
load_progetti_regis <- function(scarico) {
  progetti <- read_delim(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__Progetti.csv"),
    delim = ";",
    locale = locale(encoding = "utf8", decimal_mark = "."),
    col_types = list(
      Programma = col_character(),
      Missione = col_character(),
      `Descrizione Missione` = col_character(),
      Componente = col_character(),
      `Descrizione Componente` = col_character(),
      `ID Misura` = col_character(),
      `Codice Univoco Misura` = col_character(),
      `Descrizione Misura` = col_character(),
      `ID Submisura` = col_character(),
      `Codice CID` = col_character(),
      `Codice Univoco Submisura` = col_character(),
      `Descrizione Submisura` = col_character(),
      `Amministrazione Titolare` = col_character(),
      `Codice Identificativo Procedura di Attivazione` = col_character(),
      `Titolo Procedura` = col_character(),
      `Tipologia Procedura di Attivazione` = col_character(),
      CUP = col_character(),
      `Codice Locale Progetto` = col_character(),
      `Stato CUP` = col_character(),
      `CUP Codice Natura` = col_character(),
      `CUP Descrizione Natura` = col_character(),
      `CUP Codice Tipologia` = col_character(),
      `CUP Descrizione Tipologia` = col_character(),
      `CUP Codice Settore` = col_character(),
      `CUP Descrizione Settore` = col_character(),
      `CUP Codice Sottosettore` = col_character(),
      `CUP Descrizione Sottosettore` = col_character(),
      `CUP Codice Categoria` = col_character(),
      `CUP Descrizione Categoria` = col_character(),
      `Titolo Progetto` = col_character(),
      `Sintesi Progetto` = col_character(),
      `Descrizione Tipo Aiuto` = col_character(),
      `Finanziamento - Stato` = col_number(),
      `Finanziamento Stato - FOI` = col_number(),
      `Finanziamento Prosecuzione Opere Pubbliche - FPOP` = col_number(),
      `Finanziamento UE (Diverso da PNRR)` = col_number(),
      `Finanziamento Regione` = col_number(),
      `Finanziamento Provincia` = col_number(),
      `Finanziamento Comune` = col_number(),
      `Finanziamento Altro Pubblico` = col_number(),
      `Finanziamento Privato` = col_number(),
      `Finanziamento da Reperire` = col_number(),
      `Finanziamento PNRR` = col_number(),
      `Finanziamento PNC` = col_number(),
      `Altri Fondi` = col_number(),
      `Finanziamento Totale` = col_number(),
      `Finanziamento Totale Pubblico` = col_number(),
      `Finanziamento Totale Pubblico Netto` = col_number(),
      `Soggetto Attuatore` = col_character(),
      `Codice Fiscale Soggetto Attuatore` = col_character(),
      `Flag Progetti in Essere` = col_character(),
      `Data Inizio Progetto Prevista` = col_character(),
      `Data Inizio Progetto Effettiva` = col_character(),
      `Data Fine Progetto Prevista` = col_character(),
      `Data Fine Progetto Effettiva` = col_character(),
      `Data di Estrazione` = col_character(),
      `Data Ultima Validazione` = col_character(),
      `Esito Ultima Validazione` = col_character()
    )
  ) %>%
    rename(
      ambito = `Programma`,
      id_missione = `Missione`,
      missione = `Descrizione Missione`,
      id_componente = `Componente`,
      componente = `Descrizione Componente`,
      id_misura = `ID Misura`,
      id_misura_uni = `Codice Univoco Misura`,
      misura = `Descrizione Misura`,
      id_submisura = `ID Submisura`,
      cid = `Codice CID`,
      id_submisura_uni = `Codice Univoco Submisura`,
      submisura = `Descrizione Submisura`,
      amm_tit_long = `Amministrazione Titolare`,
      id_pratt = `Codice Identificativo Procedura di Attivazione`,
      pratt = `Titolo Procedura`,
      tipo_pratt = `Tipologia Procedura di Attivazione`,
      cup = `CUP`,
      clp = `Codice Locale Progetto`,
      stato_cup = `Stato CUP`,
      id_natura = `CUP Codice Natura`,
      natura = `CUP Descrizione Natura`,
      id_tipologia = `CUP Codice Tipologia`,
      tipologia = `CUP Descrizione Tipologia`,
      id_settore = `CUP Codice Settore`,
      settore = `CUP Descrizione Settore`,
      id_sottosettore = `CUP Codice Sottosettore`,
      sottosettore = `CUP Descrizione Sottosettore`,
      id_categoria = `CUP Codice Categoria`,
      categoria = `CUP Descrizione Categoria`,
      titolo = `Titolo Progetto`,
      sintesi = `Sintesi Progetto`,
      tipo_aiuto = `Descrizione Tipo Aiuto`,
      fin_stato = `Finanziamento - Stato`,
      fin_foi = `Finanziamento Stato - FOI`,
      fin_fpop = `Finanziamento Prosecuzione Opere Pubbliche - FPOP`,
      fin_ue = `Finanziamento UE (Diverso da PNRR)`,
      fin_reg = `Finanziamento Regione`,
      fin_pro = `Finanziamento Provincia`,
      fin_com = `Finanziamento Comune`,
      fin_altro = `Finanziamento Altro Pubblico`,
      fin_priv = `Finanziamento Privato`,
      fin_drp = `Finanziamento da Reperire`,
      fin_pnrr = `Finanziamento PNRR`,
      fin_pnc = `Finanziamento PNC`,
      fin_altro2 = `Altri Fondi`,
      fin_tot = `Finanziamento Totale`,
      fin_tot_pa = `Finanziamento Totale Pubblico`,
      fin_tot_panet = `Finanziamento Totale Pubblico Netto`,
      sogg_att = `Soggetto Attuatore`,
      cf_sogg_att = `Codice Fiscale Soggetto Attuatore`,
      flag_inessere = `Flag Progetti in Essere`,
      data_inizio_prev = `Data Inizio Progetto Prevista`,
      data_inizio_eff = `Data Inizio Progetto Effettiva`,
      data_fine_prev = `Data Fine Progetto Prevista`,
      data_fine_eff = `Data Fine Progetto Effettiva`,
      ref_data = `Data di Estrazione`,
      valid_data = `Data Ultima Validazione`,
      valid = `Esito Ultima Validazione`
    )
  
  return(progetti)
}


#' Carica dati localizzazioni REGIS
#'
#' Carica i dati delle localizzazioni dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati delle localizzazioni
#' @export
load_localizzazioni_regis <- function(scarico) {
  localizzazioni <- read_delim(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__Localizzazione.csv"),
    delim = ";",
    locale = locale(encoding = "utf8", decimal_mark = "."),
    col_types = list(
      `Codice Univoco Submisura` = col_character(),
      `Descrizione Submisura` = col_character(),
      `CUP` = col_character(),
      `Codice Locale Progetto` = col_character(),
      `Regione` = col_character(),
      `Descrizione Regione` = col_character(),
      `Provincia` = col_character(),
      `Descrizione Provincia` = col_character(),
      `Comune` = col_character(),
      `Descrizione Comune` = col_character(),
      `Indirizzo` = col_character(),
      `CAP` = col_character(),
      `Percentuale di Localizzazione` = col_number(),
      `Data di Estrazione` = col_character()
    )
  ) %>%
    rename(
      id_submisura_uni = `Codice Univoco Submisura`,
      misura = `Descrizione Submisura`,
      cup = `CUP`,
      clp = `Codice Locale Progetto`,
      id_reg = `Regione`,
      reg = `Descrizione Regione`,
      id_pro = `Provincia`,
      pro = `Descrizione Provincia`,
      id_com = `Comune`,
      com = `Descrizione Comune`,
      address = `Indirizzo`,
      cap = `CAP`,
      quota_loc = `Percentuale di Localizzazione`,
      ref_data = `Data di Estrazione`
    )
  
  return(localizzazioni)
}


#' Carica dati target REGIS
#'
#' Carica i dati dei target dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati dei target
#' @export
load_target_regis <- function(scarico) {
  target <- read_csv2(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__Indicatori_Target.csv"),
    locale = locale(encoding = "utf8")
  ) %>%
    rename(
      id_submisura_uni = `Codice Univoco Submisura`,
      misura = `Descrizione Submisura`,
      cup = `CUP`,
      clp = `Codice Locale Progetto`,
      id_ind = `Codice Indicatore`,
      ind = `Descrizione Indicatore`,
      id_udm = `Unità di Misura`,
      udm = `Descrizione Unità di Misura`,
      planned = `Valore Programmato`,
      output = `Valore Realizzato`,
      mese = `Mese`,
      anno = `Anno`,
      ref_data = `Data di Estrazione`
    )
  
  return(target)
}


#' Carica dati iter REGIS
#'
#' Carica i dati degli iter dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati degli iter
#' @export
load_iter_regis <- function(scarico) {
  iter <- read_csv2(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__Iter_di_Progetto.csv"),
    locale = locale(encoding = "utf8")
  ) %>%
    rename(
      id_submisura_uni = `Codice Univoco Submisura`,
      misura = `Descrizione Submisura`,
      cup = `CUP`,
      clp = `Codice Locale Progetto`,
      cod_fase = `Codice Fase`,
      descr_fase = `Descrizione Fase`,
      data_inizio_prev = `Data Inizio Prevista`,
      data_inizio_eff = `Data Inizio Effettiva`,
      data_fine_prev = `Data Fine Prevista`,
      data_fine_eff = `Data Fine Effettiva`,
      ref_data = `Data di Estrazione`,
      valid = `Progetto Validato`
    )
  
  return(iter)
}


#' Carica dati pagamenti REGIS
#'
#' Carica i dati dei pagamenti dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati dei pagamenti
#' @export
load_pagamenti_regis <- function(scarico) {
  pagamenti <- read_delim(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__Pagamenti.csv"),
    delim = ";",
    locale = locale(encoding = "utf8", decimal_mark = ".")
  ) %>%
    rename(
      id_submisura_uni = `Codice Univoco Submisura`,
      misura = `Descrizione Submisura`,
      cup = `CUP`,
      clp = `Codice Locale Progetto`,
      pag_anno = `Pagamenti Totali per Anno`,
      anno = `Anno Pagamento`,
      imp_anno = `Impegni Totali per Anno`,
      cr_anno = `Piano dei Costi - Importo da Realizzare`,
      cr_todo_anno = `Piano dei Costi - Importo Realizzato nell’Anno`,
      ref_data = `Data di Estrazione`,
      valid = `Progetto Validato`
    )
  
  return(pagamenti)
}


#' Carica dati procedure REGIS
#'
#' Carica i dati delle procedure dal file CSV di REGIS
#'
#' @param scarico Data dello scarico in formato AAAAMMGG
#' @return Dataframe con i dati delle procedure
#' @export
load_procedure_regis <- function(scarico) {
  procedure <- read_delim(
    file.path(DRIVE, "DATI", "PNRR", "opendata_regis", scarico, "Z__PRATT_Convenzioni.csv"),
    delim = ";",
    locale = locale(encoding = "utf8", decimal_mark = "."),
    col_types = list(
      `Amministrazione Titolare` = col_character(),
      `Codice Univoco Submisura` = col_character(),
      `ID Submisura` = col_character(),
      `Descrizione Submisura` = col_character(),
      `Budget Submisura` = col_number(),
      `Codice PRATT` = col_character(),
      `Titolo Procedura` = col_character(),
      `Stato Procedura` = col_character(),
      `Tipo Procedura` = col_character(),
      `Importo Procedura` = col_number(),
      `Importo Impegnato` = col_number(),
      `Importo Disponibile` = col_number(),
      `Importo Convenzioni` = col_number(),
      `Numero Progetti` = col_number(),
      `Costo Ammesso Progetti` = col_number(),
      `Data di Estrazione` = col_character()
    )
  ) %>%
    rename(
      amm_tit_long_raw = `Amministrazione Titolare`,
      id_submisura_uni = `Codice Univoco Submisura`,
      id_submisura = `ID Submisura`,
      submisura = `Descrizione Submisura`,
      risorse_submisura = `Budget Submisura`,
      id_pratt = `Codice PRATT`,
      titolo_pratt = `Titolo Procedura`,
      stato_pratt = `Stato Procedura`,
      tipo_pratt = `Tipo Procedura`,
      risorse_pratt = `Importo Procedura`,
      imp_pratt = `Importo Impegnato`,
      noimp_pratt = `Importo Disponibile`,
      imp_convenzioni = `Importo Convenzioni`,
      n_prog = `Numero Progetti`,
      coamm_progetti = `Costo Ammesso Progetti`,
      ref_data = `Data di Estrazione`
    )
  
  return(procedure)
}


#' Carica e integra la struttura PNRR
#'
#' Carica i dati dal DB della programmazione e integra le misure mancanti dai dati di monitoraggio
#'
#' @param DB Percorso al database generato con oc_init() o sovrascritto
#' @param progetti Dataframe dei progetti REGIS (output di load_progetti_regis)
#' @param export Vuoi esportare le misure mancanti in Excel? Default TRUE
#' @return Dataframe con la struttura completa del PNRR
load_struttura_pnrr <- function(DB, progetti, export = TRUE) {
  # Carica dati dal DB
  appo <- load_db_pnrr(DB) %>% 
    mutate(id_submisura_uni = OC_CODICE_PROGRAMMA, #MEMO: serve per join in integra_progetti.R
      chk_misura = "dbcoe")
  
  # Identifica misure mancanti
  temp <- progetti %>%
    distinct(id_submisura_uni) %>%
    anti_join(appo, 
      by = "id_submisura_uni")
  
  # Crea righe complete
  misure_mancanti <- progetti %>% 
    semi_join(temp, by = "id_submisura_uni") %>% 
    distinct(id_submisura_uni, id_missione, missione, 
               id_componente, componente, 
               id_misura_uni, misura,
               submisura, amm_tit_long, cid)  %>%
    mutate(
      OC_CODICE_PROGRAMMA = id_submisura_uni, #MEMO: lo assegno prima perché dopo lo devo duplicare
      AMBITO = "PNRR",
      CICLO_PROGRAMMAZIONE = "2021-2027",
      TIPOLOGIA_AMMINISTRAZIONE = "NAZ",
      FLAG_MONITORAGGIO = 1,
      TIPOLOGIA_PROGRAMMA = NA_character_,
      SUB_TIPOLOGIA_PROGRAMMA = NA_character_,
      FONTE_PROGRAMMA      = NA_character_,
      RISORSE          = 0,
      RISORSE_INESSERE = 0,
      RISORSE_NUOVI    = 0,
      AMM_TIT = case_when(
        amm_tit_long == "GIUST AMM.VA (CONSIGLIO DI STATO E TAR)" ~ "MGG",
        amm_tit_long == "MIN AFFARI ESTERI E COOPERAZ INT" ~ "MAECI",
        amm_tit_long == "MIN AGRIC. SOVRANITA' ALIM. E FORESTE" ~ "MASAF",
        amm_tit_long == "MIN AMBIENTE E SICUREZZA ENERGETICA" ~ "MASE",
        amm_tit_long == "MIN DELL'UNIVERSITA' E DELLA RICERCA" ~ "MUR",
        amm_tit_long == "MINISTERO DEL LAVORO E POLITICHE SOCIALI" ~ "MLPS",
        amm_tit_long == "MINISTERO DEL TURISMO" ~ "MiTur",
        amm_tit_long == "MINISTERO DELL'ECONOMIA E DELLE FINANZE" ~ "MEF",
        amm_tit_long == "MINISTERO DELL'INTERNO" ~ "MINT",
        amm_tit_long == "MINISTERO DELL'ISTRUZIONE E MERITO" ~ "MIM",
        amm_tit_long == "MINISTERO DELLA CULTURA" ~ "MIC",
        amm_tit_long == "MINISTERO DELLA GIUSTIZIA" ~ "MGG",
        amm_tit_long == "MINISTERO DELLA SALUTE" ~ "MS",
        amm_tit_long == "MINISTERO IMPRESE E DEL MADE IN ITALY" ~ "MIMIT",
        amm_tit_long == "MINISTERO INFRASTRUTTURE E TRASPORTI" ~ "MIT",
        amm_tit_long == "PCM - DIP AFFARI REGIONALI E AUTONOMIE" ~ "PCM-DARA",
        amm_tit_long == "PCM - DIP PARI OPPORTUNITA E FAMIGLIA" ~ "PCM-DPO",
        amm_tit_long == "PCM - DIP POL GIOVAN E SERV CIV UNIVERS" ~ "PCM-DPGSCU",
        amm_tit_long == "PCM - DIP PROTEZIONE CIVILE" ~ "PCM-DPC",
        amm_tit_long == "PCM - DIPARTIM. TRASFORMAZIONE DIGITALE" ~ "DTD",
        amm_tit_long == "PCM - DIPARTIMENTO FUNZIONE PUBBLICA" ~ "MinPA",
        amm_tit_long == "PCM - DIPARTIMENTO PER LO SPORT" ~ "PCM-DS",
        amm_tit_long == "PCM - PRESIDENZA CONSIGLIO MINISTRI" ~ "PCM-SG",
        amm_tit_long == "PCM - STRUTTURA COMMISS. RICOSTRUZIONE" ~ "PCM-COMM",
        amm_tit_long == "PCM - STRUTTURA DI MISSIONE PNRR" ~ "PCM-PNRR",
        amm_tit_long == "PCM - UFF POL IN FAV PERS CON DISABILITA" ~ "PCM-DISA",
        amm_tit_long == "PCM - DIP POLITICHE DI COESIONE" ~ "MinCOE"
    )) %>%
    select(
      AMBITO,
      OC_CODICE_PROGRAMMA,
      DESCRIZIONE_PROGRAMMA = submisura,
      CICLO_PROGRAMMAZIONE,
      AMMINISTRAZIONE  = amm_tit_long,
      TIPOLOGIA_AMMINISTRAZIONE,
      AMM_TIT,
      # id_regis         = "???",
      COD_MISSIONE      = id_missione,
      DESCR_MISSIONE         = missione,
      COD_COMPONENTE    = id_componente,
      DESCR_COMPONENTE       = componente,
      # id_misura        = `ID Misura`,
      COD_MISURA    = id_misura_uni,
      DESCR_MISURA           = misura,
      # id_submisura     = `ID Sub-Misura`,
      CID              = cid,
      TIPOLOGIA_PROGRAMMA,
      SUB_TIPOLOGIA_PROGRAMMA,
      FONTE_PROGRAMMA,
      RISORSE,
      RISORSE_INESSERE,
      RISORSE_NUOVI,
      FLAG_MONITORAGGIO,
      id_submisura_uni
    ) %>% 
    mutate(chk_misura = "regis")

  # Export
  if (export == TRUE) {
    write.xlsx(misure_mancanti, file.path(TEMP, "misure_mancanti.xlsx"))
  }

  # Stampare un riepilogo
  message(paste("Numero di misure mancanti: ", nrow(misure_mancanti)))

  # Aggiungo le misure mancanti alla struttura
  struttura <- bind_rows(appo, misure_mancanti)
  
  return(struttura)
}


#' Setup dati di attuazione PNRR
#'
#' Setup dei dati di attuazione PNRR nello standard OpenCoesione
#'
#' @param struttura Dataframe della struttura programmatica del PNRR (output di load_struttura_pnrr())
#' @param progetti Dataframe dei progetti da REGIS (output di load_progetti_regis())
#' @param localizzazioni Dataframe delle localizzazioni dei progetti da REGIS (output di load_localizzazioni_regis())
#' @param iter Dataframe dell'iter procedurale dei progetti da REGIS (output di load_iter_regis())
#' @param pagamenti Dataframe dei pagamenti dei progetti da REGIS (output di load_pagamenti_regis())
#' @return Dataframe con dati di attuazione PNRR
setup_dati_pnrr <- function(struttura, progetti, localizzazioni, iter, pagamenti) {
  # integra dati per main script regis.R
  
  
  data_scarico <- ymd(scarico)
  
  
  # ----------------------------------------------------------------------------------- #
  # chk vari
  
  chk <- progetti %>% count(clp, cup) %>% filter(n>1)
  message(paste0("Sono presenti ", dim(chk)[1], " coppie CUP+CLP duplicate"))

  chk <- progetti %>% anti_join(localizzazioni, by = c("id_submisura_uni", "cup", "clp"))
  message(paste0("Sono presenti ", dim(chk)[1], " progetti senza localizzazioni"))

  chk <- progetti %>%  filter(valid == TRUE) %>%  anti_join(localizzazioni, by = c("id_submisura_uni", "cup", "clp"))
  message(paste0("Sono presenti ", dim(chk)[1], " progetti validati senza localizzazioni"))
  
  chk <- progetti %>% anti_join(iter, by = c("id_submisura_uni", "cup", "clp"))
  message(paste0("Sono presenti ", dim(chk)[1], " progetti senza iter procedurale"))

  chk <- progetti %>%  filter(valid == TRUE) %>%  anti_join(iter, by = c("id_submisura_uni", "cup", "clp"))
  message(paste0("Sono presenti ", dim(chk)[1], " progetti validati senza iter procedurale"))
  
  chk <- round(sum(progetti$fin_pnrr, na.rm=TRUE)/1000000000,1)
  message(paste0("Il valore complessivo dei progetti è di ", chk, " Meuro di risorse PNRR"))

  
  # ----------------------------------------------------------------------------------- #
  # localizzazioni
  
  # OLD:
  # temp_localizzazioni <- localizzazioni %>%
  #   semi_join(progetti, by = c("id_submisura_uni", "cup", "clp")) %>%
  #   arrange(id_reg, id_pro, id_com) %>%
  #   group_by(id_submisura_uni, cup, clp) %>%
  #   summarise(COD_REGIONE = paste0(id_reg, collapse = ":::"),
  #             DEN_REGIONE = paste0(reg, collapse = ":::"),
  #             COD_PROVINCIA = paste0(id_pro, collapse = ":::"),
  #             DEN_PROVINCIA = paste0(pro, collapse = ":::"),
  #             COD_COMUNE = paste0(id_com, collapse = ":::"),
  #             DEN_COMUNE = paste0(com, collapse = ":::")) %>%
  #   mutate(x_MACROAREA = case_when(COD_REGIONE %in% c("013", "014", "015", "016", "017", "018", "019", "020") ~ "Mezzogiorno",
  #                                  COD_REGIONE %in% c("001", "002", "003", "004", "005", "006", "007", "008",
  #                                                     "009", "010", "011", "012") ~ "Centro-Nord",
  #                                  COD_REGIONE == "999" ~ "Estero",
  #                                  COD_REGIONE == "000" ~ "Ambito nazionale",
  #                                  TRUE ~ "Altro"))
  # x_MACROAREA           n
  # <chr>             <int>
  # 1 Altro              6982
  # 2 Ambito nazionale    901
  # 3 Centro-Nord      263348
  # 4 Mezzogiorno      129959
  
  # NEW:
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")

  # funzione per controllare se tutti i casi multiregione rientrano nella stessa macroarea
  chk_regione <- function(data_vector, test_vector) {
    
    # DEBUG:
    # temp <- c("001:::002", "001:::003", "001:::020")
    # chk_regione(temp, reg_cn)
    
    sapply(data_vector, function(x) {all(unlist(str_split(x, pattern = ":::")) %in% test_vector)})
  }
  
  temp_localizzazioni <- localizzazioni %>%
    semi_join(progetti, by = c("id_submisura_uni", "cup", "clp")) %>%
    arrange(id_reg, id_pro, id_com) %>%
    group_by(id_submisura_uni, cup, clp) %>%
    summarise(COD_REGIONE = paste0(id_reg, collapse = ":::"),
              DEN_REGIONE = paste0(reg, collapse = ":::"),
              COD_PROVINCIA = paste0(id_pro, collapse = ":::"),
              DEN_PROVINCIA = paste0(pro, collapse = ":::"),
              COD_COMUNE = paste0(id_com, collapse = ":::"),
              DEN_COMUNE = paste0(com, collapse = ":::")) %>%
    mutate(x_MACROAREA = case_when(COD_REGIONE %in% reg_sud ~ "Mezzogiorno",
                                   COD_REGIONE %in% reg_cn ~ "Centro-Nord",
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_sud) == TRUE ~ "Mezzogiorno",
                                   grepl(":::", COD_REGIONE) & chk_regione(COD_REGIONE, reg_cn) == TRUE ~ "Centro-Nord",
                                   grepl(":::", COD_REGIONE) ~ "Trasversale",
                                   COD_REGIONE == "999" ~ "Estero",
                                   COD_REGIONE == "000" ~ "Ambito nazionale",
                                   TRUE ~ "chk"))
  
  temp_localizzazioni %>% ungroup() %>% count(x_MACROAREA)
  # x_MACROAREA           n
  # <chr>             <int>
  # 1 Ambito nazionale    901
  # 2 Centro-Nord      267410
  # 3 Mezzogiorno      132419
  # 4 Trasversale         460
 
  
  # ----------------------------------------------------------------------------------- #
  # iter
  
  # DEBUG: dominio
  # temp <- iter %>% 
  #   count(cod_fase, descr_fase)
  
  appo <- iter %>%
    # converte in date
    mutate(data_inizio_prev = dmy(data_inizio_prev),
           data_fine_prev = dmy(data_fine_prev),
           data_inizio_eff = dmy(data_inizio_eff),
           data_fine_eff = dmy(data_fine_eff)) %>% 
    # riclassifica fasi
    mutate(oc_cod_fase = case_when(cod_fase == '00101' ~ "STIP_ATTRIB", #PREDISPOSIZIONE CAPITOLATO E BANDO DI GARA
                                   cod_fase == '00102' ~ "STIP_ATTRIB", #PUBBLICAZIONE BANDO DI GARA
                                   cod_fase == '00103' ~ "STIP_ATTRIB", #AGGIUDICAZIONE
                                   cod_fase == '00104' ~ "STIP_ATTRIB", #STIPULA CONTRATTO
                                   cod_fase == '00105' ~ "ESECUZIONE", #ESECUZIONE FORNITURA
                                   cod_fase == '00106' ~ "COLLAUDO", #COLLAUDO
                                   cod_fase == '00201' ~ "STIP_ATTRIB", #PREDISPOSIZIONE CAPITOLATO E BANDO DI GARA
                                   cod_fase == '00202' ~ "STIP_ATTRIB", #PUBBLICAZIONE BANDO DI GARA
                                   cod_fase == '00203' ~ "STIP_ATTRIB", #AGGIUDICAZIONE
                                   cod_fase == '00204' ~ "STIP_ATTRIB", #STIPULA CONTRATTO
                                   cod_fase == '00205' ~ "ESECUZIONE", #ESECUZIONE FORNITURA
                                   cod_fase == '00206' ~ "COLLAUDO", #COLLAUDO
                                   cod_fase == '00301' ~ "STUDIO_FATT", #STUDIO DI FATTIBILITÀ
                                   cod_fase == '00302' ~ "PROG_PREL", #PROGETTAZIONE PRELIMINARE
                                   cod_fase == '00303' ~ "PROG_PREL", #PFTE (PROGETTO DI FATTIBILITÀ TECNICO ECONOMICA) -> chk
                                   cod_fase == '00304' ~ "PROG_PREL", #PFTE RAFFORZATO -> chk
                                   cod_fase == '00305' ~ "PROG_DEF", #PROGETTO DEFINITIVO
                                   cod_fase == '00306' ~ "PROG_DEF", #CONFERENZA DEI SERVIZI DECISORIA -> chk
                                   cod_fase == '00307' ~ "PROG_ESEC", #PROGETTAZIONE ESECUTIVA
                                   cod_fase == '00308' ~ "PROG_ESEC", #PROGETTAZIONE DEFINITIVA + ESECUTIVA -> chk
                                   cod_fase == '00309' ~ "STIP_ATTRIB", #PREDISPOSIZIONE CAPITOLATO E BANDO DI GARA
                                   cod_fase == '00310' ~ "STIP_ATTRIB", #PUBBLICAZIONE BANDO DI GARA
                                   cod_fase == '00311' ~ "STIP_ATTRIB", #AGGIUDICAZIONE
                                   cod_fase == '00312' ~ "STIP_ATTRIB", #STIPULA CONTRATTO
                                   cod_fase == '00313' ~ "ESECUZIONE", #ESECUZIONE LAVORI
                                   cod_fase == '00314' ~ "COLLAUDO", #COLLAUDO
                                   cod_fase == '00601' ~ "STIP_ATTRIB", #ATTRIBUZIONE FINANZIAMENTO
                                   cod_fase == '00602' ~ "ESECUZIONE", #ESECUZIONE INVESTIMENTI/ATTIVITÀ
                                   cod_fase == '00701' ~ "STIP_ATTRIB", #ATTRIBUZIONE FINANZIAMENTO
                                   cod_fase == '00702' ~ "ESECUZIONE", #ESECUZIONE INVESTIMENTI
                                   cod_fase == '00801' ~ "STIP_ATTRIB", #ATTRIBUZIONE FINANZIAMENTO
                                   cod_fase == '00802' ~ "ESECUZIONE", #ESECUZIONE INVESTIMENTI
                                   cod_fase == '01001' ~ "PROG_ESEC", #PROGETTAZIONE FORNITURE (ART. 41 DLGS 36/2023 E AR -> chk
                                   cod_fase == '01002' ~ "ESECUZIONE", #ESECUZIONE
                                   cod_fase == '01003' ~ "COLLAUDO", #VERIFICA DI CONFORMITÀ/REGOLARE ESECUZIONE
                                   cod_fase == '02001' ~ "PROG_ESEC", #PROGETTAZIONE SERVIZI (ART. 41 DLGS 36/2023 E ART. -> chk
                                   cod_fase == '02002' ~ "ESECUZIONE", #ESECUZIONE
                                   cod_fase == '02003' ~ "COLLAUDO", #VERIFICA DI CONFORMITÀ/REGOLARE ESECUZIONE
                                   cod_fase == '02004' ~ "PROG_PREL", #STUDIO DI FATTIBILITÀ O PROGETTO PRELIMINARE (FASI -> chk
                                   cod_fase == '02005' ~ "PROG_DEF", #PROGETTO DEFINITIVO (FASE PREVISTA DAL CODICE DEI
                                   cod_fase == '02006' ~ "PROG_PREL", #PROGETTO DI FATTIBILITÀ TECNICO ECONOMICA (ART. 41 -> chk
                                   cod_fase == '02007' ~ "PROG_ESEC", #PROGETTO ESECUTIVO (ART. 41 DLGS 36/2023 E PRECEDE
                                   cod_fase == '03001' ~ "PROG_PREL", #STUDIO DI FATTIBILITÀ O PROGETTO PRELIMINARE (FASI -> chk
                                   cod_fase == '03002' ~ "PROG_DEF", #PROGETTO DEFINITIVO (FASE PREVISTA DAL CODICE DEI
                                   cod_fase == '03003' ~ "PROG_PREL", #PROGETTO DI FATTIBILITÀ TECNICO ECONOMICA (ART. 41 -> chk
                                   cod_fase == '03004' ~ "PROG_ESEC", #PROGETTO ESECUTIVO (ART. 41 DLGS 36/2023 E PRECEDE
                                   cod_fase == '03005' ~ "ESECUZIONE", #ESECUZIONE LAVORI
                                   cod_fase == '03006' ~ "COLLAUDO" #COLLAUDO/REGOLARE ESECUZIONE
    )) %>% 
    mutate(oc_cod_fase = factor(oc_cod_fase, levels = c("STUDIO_FATT", "PROG_PREL", "PROG_DEF", "PROG_ESEC", "STIP_ATTRIB", "ESECUZIONE", "COLLAUDO")))
  
  # OLD:
  # accorpa fasi
  appo1_old <- appo %>% 
    group_by(id_submisura_uni, cup, clp, oc_cod_fase) %>% 
    summarise(DATA_INIZIO_PREV = min(data_inizio_prev, na.rm = TRUE),
              DATA_FINE_PREV   = max(data_fine_prev, na.rm = TRUE),
              DATA_INIZIO_EFF  = min(data_inizio_eff,  na.rm = TRUE),
              DATA_FINE_EFF    = max(data_fine_eff,  na.rm = TRUE))
  # TODO: qui dovrei controllare i progetti con anomalie
  # warnings()
  # In min.default(structure(NA_real_, class = "Date"), na.rm = TRUE) :
  # nessun argomento non-mancante al minimo; si restituisce Inf
  
  
  # NEW:
  
  # utility
  safe_min <- function(x) {
    if (all(is.na(x))) NA_Date_ else min(x, na.rm = TRUE)
  }
  safe_max <- function(x) {
    if (all(is.na(x))) NA_Date_ else max(x, na.rm = TRUE)
  }
  
  # accorpa fasi
  appo1 <- appo %>%
    group_by(id_submisura_uni, cup, clp, oc_cod_fase) %>%
    summarise(DATA_INIZIO_PREV = safe_min(data_inizio_prev),
              DATA_FINE_PREV   = safe_max(data_fine_prev),
              DATA_INIZIO_EFF  = safe_min(data_inizio_eff),
              DATA_FINE_EFF    = safe_max(data_fine_eff))

 temp_iter <- appo1 %>% 
    arrange(oc_cod_fase) %>% 
    # pivot su progetti
    pivot_wider(id_cols = c("id_submisura_uni", "cup", "clp"), names_from = c("oc_cod_fase"), 
                values_from = c("DATA_INIZIO_PREV", "DATA_FINE_PREV", "DATA_INIZIO_EFF", "DATA_FINE_EFF")) %>% 
    # crea stato procedurale
    mutate(OC_STATO_PROCEDURALE = case_when(DATA_FINE_EFF_COLLAUDO > 0 & DATA_FINE_EFF_COLLAUDO <= data_scarico ~ "Eseguito",
                                            DATA_INIZIO_EFF_COLLAUDO > 0 & DATA_INIZIO_EFF_COLLAUDO <= data_scarico ~ "Eseguito",
                                            DATA_FINE_EFF_ESECUZIONE > 0 & DATA_FINE_EFF_ESECUZIONE <= data_scarico ~ "Eseguito",
                                            DATA_INIZIO_EFF_ESECUZIONE > 0 & DATA_INIZIO_EFF_ESECUZIONE <= data_scarico ~ "In esecuzione",
                                            DATA_FINE_EFF_STIP_ATTRIB > 0 & DATA_FINE_EFF_STIP_ATTRIB <= data_scarico ~ "In affidamento",
                                            DATA_INIZIO_EFF_STIP_ATTRIB > 0 & DATA_INIZIO_EFF_STIP_ATTRIB <= data_scarico ~ "In affidamento",
                                            DATA_FINE_EFF_PROG_ESEC > 0 & DATA_FINE_EFF_PROG_ESEC <= data_scarico ~ "In affidamento",
                                            DATA_INIZIO_EFF_PROG_ESEC > 0 & DATA_INIZIO_EFF_PROG_ESEC <= data_scarico ~ "In corso di progettazione",
                                            DATA_FINE_EFF_PROG_DEF > 0 & DATA_FINE_EFF_PROG_DEF <= data_scarico ~ "In corso di progettazione",
                                            DATA_INIZIO_EFF_PROG_DEF > 0 & DATA_INIZIO_EFF_PROG_DEF <= data_scarico ~ "In corso di progettazione",
                                            DATA_FINE_EFF_PROG_PREL > 0 & DATA_FINE_EFF_PROG_PREL <= data_scarico ~ "In corso di progettazione",
                                            DATA_INIZIO_EFF_PROG_PREL > 0 & DATA_INIZIO_EFF_PROG_PREL <= data_scarico ~ "In corso di progettazione",
                                            DATA_FINE_EFF_STUDIO_FATT > 0 & DATA_FINE_EFF_STUDIO_FATT <= data_scarico ~ "In corso di progettazione",
                                            DATA_INIZIO_EFF_STUDIO_FATT > 0 & DATA_INIZIO_EFF_STUDIO_FATT <= data_scarico ~ "In avvio di progettazione",
                                            TRUE ~ "Non avviato"))
  
  
  # ----------------------------------------------------------------------------------- #
  # integrazione
  
  # integrazione regis
  appo_regis <- progetti %>% 
    # integra struttura
    # MEMO: tengo solo id_submisura_uni per match con struttura, che sovrascrive
    select(-id_missione, -missione, -id_componente, -componente, 
           -id_misura, -id_misura_uni, -misura, 
           -id_submisura, -submisura, -cid, 
           -amm_tit_long) %>% 
    left_join(struttura, by = "id_submisura_uni") %>% 
    # integra amministrazioni 
    # left_join(amministrazioni %>% 
    #             select(id_AMM_TIT, AMM_TIT), by = "id_AMM_TIT") %>% 
    # integra impegni e pagamenti
    left_join(pagamenti %>% 
                group_by(id_submisura_uni, cup, clp) %>% 
                summarise(imp = sum(imp_anno, na.rm=TRUE),
                          cr = sum(cr_anno, na.rm=TRUE),
                          pag = sum(pag_anno, na.rm=TRUE)),
              by = c("id_submisura_uni", "cup", "clp")) %>% 
    # TODO: controllare se questi vanno filtrati (ad es. per scontanre recuperi)
    # integra localizzazioni
    left_join(temp_localizzazioni,
              by = c("id_submisura_uni", "cup", "clp")) %>%
    # integra iter
    left_join(temp_iter,
              by = c("id_submisura_uni", "cup", "clp")) %>% 
    # converte altre date (fuori da iter)
    mutate(data_inizio_prev = dmy(data_inizio_prev),
           data_fine_prev = dmy(data_fine_prev),
           data_inizio_eff = dmy(data_inizio_eff),
           data_fine_eff = dmy(data_fine_eff),
           valid_data = dmy(valid_data),
           ref_data = dmy(ref_data)) %>% 
    # calcola variabili equivalenti coesione
    mutate(COE = fin_pnrr,
           COE_IMP = imp * fin_pnrr/fin_tot,
           COE_CR = cr * fin_pnrr/fin_tot,
           COE_PAG = pag * fin_pnrr/fin_tot) %>% 
    # integra altre variabili coesione
    mutate(x_CICLO = CICLO_PROGRAMMAZIONE,
           x_AMBITO = AMBITO,
           x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>% 
    # TODO: forse serve una nuova colonna in struttura con titolo short
    # flag
    mutate(OC_FLAG_VISUALIZZAZIONE = if_else(valid == "Validato", 1, 0))
  
  regis <- appo_regis %>% 
    # clean
    select(COD_LOCALE_PROGETTO = clp,
           CUP = cup,
           OC_TITOLO_PROGETTO = titolo,
           x_CICLO,
           x_AMBITO,
           x_GRUPPO,
           x_PROGRAMMA, 
           OC_CODICE_PROGRAMMA,
           OC_DESCRIZIONE_PROGRAMMA = DESCRIZIONE_PROGRAMMA,
           COD_MISSIONE,
           DESCR_MISSIONE,
           COD_COMPONENTE,
           DESCR_COMPONENTE, 
           COD_MISURA,
           DESCR_MISURA,
           CUP_COD_NATURA = id_natura,
           CUP_DESCR_NATURA = natura,
           CUP_COD_TIPOLOGIA = id_tipologia,
           CUP_DESCR_TIPOLOGIA = tipologia,
           CUP_COD_SETTORE = id_settore,
           CUP_DESCR_SETTORE = settore,
           CUP_COD_SOTTOSETTORE = id_sottosettore,
           CUP_DESCR_SOTTOSETTORE = sottosettore,
           CUP_COD_CATEGORIA = id_categoria,
           CUP_DESCR_CATEGORIA = categoria,
           FIN_PNRR = fin_pnrr,
           FINANZ_UE = fin_ue,
           FINANZ_STATO_PNC = fin_pnc,
           FINANZ_STATO_ALTRO = fin_stato,
           FINANZ_STATO_FOI = fin_foi,
           FINANZ_STATO_FPOP = fin_fpop,
           FINANZ_REGIONE = fin_reg,
           FINANZ_PROVINCIA = fin_pro,
           FINANZ_COMUNE = fin_com,
           FINANZ_ALTRO_PUBBLICO = fin_altro,
           FINANZ_ALTRO = fin_altro2,
           FINANZ_PRIVATO = fin_priv,
           FINANZ_DA_REPERIRE = fin_drp,
           FINANZ_TOTALE = fin_tot,
           FINANZ_TOTALE_PUBBLICO = fin_tot_pa,
           OC_FINANZ_TOT_PUB_NETTO = fin_tot_panet,
           COE,
           IMPEGNI = imp,
           COE_IMP,
           COSTO_REALIZZATO = cr,
           COE_CR,
           TOT_PAGAMENTI = pag,
           COE_PAG,
           OC_DATA_INIZIO_PROGETTO_PREVISTA = data_inizio_prev,
           OC_DATA_INIZIO_PROGETTO_EFFETTIVA = data_inizio_eff,
           OC_DATA_FINE_PROGETTO_PREVISTA = data_fine_prev,
           OC_DATA_FINE_PROGETTO_EFFETTIVA = data_fine_eff,
           DATA_INIZIO_PREV_STUDIO_FATT,
           DATA_INIZIO_EFF_STUDIO_FATT,
           DATA_FINE_PREV_STUDIO_FATT,
           DATA_FINE_EFF_STUDIO_FATT,
           DATA_INIZIO_PREV_PROG_PREL,
           DATA_INIZIO_EFF_PROG_PREL,
           DATA_FINE_PREV_PROG_PREL,
           DATA_FINE_EFF_PROG_PREL,
           DATA_INIZIO_PREV_PROG_DEF,
           DATA_INIZIO_EFF_PROG_DEF,
           DATA_FINE_PREV_PROG_DEF,
           DATA_FINE_EFF_PROG_DEF,
           DATA_INIZIO_PREV_PROG_ESEC,
           DATA_INIZIO_EFF_PROG_ESEC,
           DATA_FINE_PREV_PROG_ESEC,
           DATA_FINE_EFF_PROG_ESEC,
           DATA_INIZIO_PREV_STIP_ATTRIB,
           DATA_INIZIO_EFF_STIP_ATTRIB,
           DATA_FINE_PREV_STIP_ATTRIB,
           DATA_FINE_EFF_STIP_ATTRIB,
           DATA_INIZIO_PREV_ESECUZIONE,
           DATA_INIZIO_EFF_ESECUZIONE,
           DATA_FINE_PREV_ESECUZIONE,
           DATA_FINE_EFF_ESECUZIONE,
           DATA_INIZIO_PREV_COLLAUDO,
           DATA_INIZIO_EFF_COLLAUDO,
           DATA_FINE_PREV_COLLAUDO,
           DATA_FINE_EFF_COLLAUDO,
           OC_COD_FASE_CORRENTE = `Codice Fase Iter di Progetto`,
           OC_DESCR_FASE_CORRENTE = `Descrizione Fase Iter di Progetto`,
           OC_STATO_PROCEDURALE,
           COD_PROCED_ATTIVAZIONE = id_pratt,
           DESCR_PROCED_ATTIVAZIONE = pratt,
           # COD_TIPO_PROCED_ATTIVAZIONE,
           DESCR_TIPO_PROCED_ATTIVAZIONE = tipo_pratt,
           OC_DENOM_PROGRAMMATORE = AMM_TIT,
           OC_CODFISC_BENEFICIARIO = cf_sogg_att,
           OC_DENOM_BENEFICIARIO = sogg_att,
           x_MACROAREA,
           COD_REGIONE,
           DEN_REGIONE,
           COD_PROVINCIA, 
           DEN_PROVINCIA,
           COD_COMUNE,
           DEN_COMUNE,
           FLAG_IN_ESSERE = flag_inessere,
           OC_FLAG_VISUALIZZAZIONE,
           ESITO_VALIDAZIONE = valid,
           DATA_VALIDAZIONE = valid_data,
           DATA_AGGIORNAMENTO = ref_data)
  
  # chk molteplicità
  dim(regis)[1] == dim(progetti)[1]
  
  return(regis)
  
}


#' Confronta versioni dati PNRR
#'
#' Utility di confronta tra la versione attuale dei dati PNRR e una precedente
#'
#' @param progetti_pnrr Dataframe dei progetti da REGIS di riferimento (output di setup_dati_pnrr())
#' @param bimestre_pnrr_old Bimestre di confronto per i dati PNRR
#' @param versione_pnrr_old Versione di confronto dei dati (sono possibili più versioni per lo stesso bimestre)
#' @param pagamenti Dataframe dei pagamenti dei progetti da REGIS (output di load_pagamenti_regis)
#' @param export Vuoi esportare le misure mancanti in Excel? Default TRUE
#' @return Dataframe con dati di attuazione PNRR
chk_delta_scarico <- function(progetti_pnrr, bimestre_pnrr_old, versione_pnrr_old, export=TRUE) {
  # calcola delta tra due versioni dei dati
  
  regis <- progetti_pnrr
  
  # legge versione precedente
  temp_old <- load_progetti_pnrr(bimestre_pnrr_old, versione_pnrr_old)
  
  # confronto a livello di progetto
  appo <- regis %>% 
    select(OC_DENOM_PROGRAMMATORE, CUP, COD_LOCALE_PROGETTO, COE, COE_IMP, COE_PAG) %>% 
    # left_join(temp_old %>% 
    #             select(CUP, COD_LOCALE_PROGETTO, COE, COE_IMP, COE_PAG), 
    #           by = c("CUP", "COD_LOCALE_PROGETTO"), 
    #           suffix = c(".new", ".old")) %>% 
    full_join(temp_old %>% 
                select(CUP, COD_LOCALE_PROGETTO, COE, COE_IMP, COE_PAG), 
              by = c("CUP", "COD_LOCALE_PROGETTO"), 
              suffix = c(".new", ".old")) %>% 
    mutate(delta_COE = COE.new - COE.old,
           delta_COE_IMP = COE_IMP.new - COE_IMP.old,
           delta_COE_PAG = COE_PAG.new - COE_PAG.old) %>% 
    # flag per progetti con variazioni
    mutate(has_delta = abs(delta_COE) > 0 | abs(delta_COE_IMP) > 0 | abs(delta_COE_PAG) > 0)
  
  # sintesi per amministrazione
  appo_amm <- appo %>% 
    group_by(OC_DENOM_PROGRAMMATORE) %>% 
    summarise(N.new = n(),
              N.old = sum(!is.na(COE.old)),
              delta_N = N.new - N.old,
              COE.new = sum(COE.new, na.rm = TRUE),
              COE.old = sum(COE.old, na.rm = TRUE),
              delta_COE = COE.new - COE.old,
              COE_IMP.new = sum(COE_IMP.new, na.rm = TRUE),
              COE_IMP.old = sum(COE_IMP.old, na.rm = TRUE),
              delta_COE_IMP = COE_IMP.new - COE_IMP.old,
              COE_PAG.new = sum(COE_PAG.new, na.rm = TRUE),
              COE_PAG.old = sum(COE_PAG.old, na.rm = TRUE),
              delta_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  # sintesi totale
  appo_tot <- appo %>% 
    summarise(N.new = n(),
              N.old = sum(!is.na(COE.old)),
              delta_N = N.new - N.old,
              COE.new = sum(COE.new, na.rm = TRUE),
              COE.old = sum(COE.old, na.rm = TRUE),
              delta_COE = COE.new - COE.old,
              COE_IMP.new = sum(COE_IMP.new, na.rm = TRUE),
              COE_IMP.old = sum(COE_IMP.old, na.rm = TRUE),
              delta_COE_IMP = COE_IMP.new - COE_IMP.old,
              COE_PAG.new = sum(COE_PAG.new, na.rm = TRUE),
              COE_PAG.old = sum(COE_PAG.old, na.rm = TRUE),
              delta_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  # dettaglio progetti con variazioni
  appo_var <- appo %>% 
    filter(has_delta == TRUE) %>% 
    select(OC_DENOM_PROGRAMMATORE, CUP, COD_LOCALE_PROGETTO,
           COE.new, COE.old, delta_COE,
           COE_IMP.new, COE_IMP.old, delta_COE_IMP,
           COE_PAG.new, COE_PAG.old, delta_COE_PAG)
  
  # export
  if (export == TRUE) {
    temp <- list(sintesi_totale = appo_tot,
                 sintesi_amministrazioni = appo_amm,
                 progetti_con_variazioni = appo_var)
    write.xlsx(temp, file.path(TEMP, "chk_delta_scarico.xlsx"), 
               asTable = TRUE, colWidths = "auto", firstRow = TRUE)
  }
  
  return(appo_tot)
  
}