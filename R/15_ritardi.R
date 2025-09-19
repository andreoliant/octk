# ritardi



#' Setup analisi ritardi
#'
#' Crea dataset di base per analisi ritardi iter procedurale. Trova fase corrente, verifica ritardo su data prevista per prossima data effettiva assnte.
#' Ricalcola x_STATO_PROCEDURALE con get_stato_attuazione()
#'  
#' @param bimestre Bimestre di OpenCoesione
#' @param progetti Dataset da load_progetti(bimestre, light = FALSE, visualizzati = FALSE)
#' @param progetti_old Dataset da progetti_light riferito a un bimestre precedente (serve per controllo variazioni date previste)
#' @param chk_today Parametro da passare a get_stato_attuazione(), con formato "2021-02-28"
#' @param perimetro Elenco di CLP per focus specifico
#' @param debug Vuoi verificare ...?
#' @param export Vuoi esportare in TEMP in xlsx?
#' @return Dataset di base per analisi ritardi iter procedurale
setup_ritardi <- function(bimestre, progetti, progetti_old, chk_today, perimetro=NULL, debug=FALSE, export=FALSE) {
  
  # MEMO: 
  # considero come fasi le fasi e le interfasi
  # la fase seguente è quella per cui si misura l'eventuale ritardo, sulla data di inizio di tale fase, che fa da milestone
  # se la prima fase seguente teorica è assente, forzo in avanti per cercare date previste successive
  # alcune opere usano SDF e altre PP per primo livello post DL 50
  # alcuni progetti entrano come SdF, altri direttamente come PP, PD, PE o STIP
  # tutte le nature non lavori entrano in STIP
  # se manca data di inizio manca anche data di fine, se c'è data di inizio può mancare data di fine
  
  # elab
  max_date <- as.numeric(bimestre)
  
  # switch
  if (!is.null(perimetro)) {
    message("Uso perimetro per focus specifico")
    
    appo0 <- progetti %>% 
      semi_join(perimetro, by = "COD_LOCALE_PROGETTO") %>% 
      select(COD_LOCALE_PROGETTO)
    
  } else {
    appo0 <- progetti %>% 
      select(COD_LOCALE_PROGETTO)
    
  }
  
  appo <- appo0 %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO,
                       DATA_INIZIO_PREV_STUDIO_FATT, DATA_INIZIO_EFF_STUDIO_FATT, DATA_FINE_PREV_STUDIO_FATT, DATA_FINE_EFF_STUDIO_FATT, 
                       DATA_INIZIO_PREV_PROG_PREL, DATA_INIZIO_EFF_PROG_PREL, DATA_FINE_PREV_PROG_PREL, DATA_FINE_EFF_PROG_PREL,
                       DATA_INIZIO_PREV_PROG_DEF, DATA_INIZIO_EFF_PROG_DEF, DATA_FINE_PREV_PROG_DEF, DATA_FINE_EFF_PROG_DEF, 
                       DATA_INIZIO_PREV_PROG_ESEC, DATA_INIZIO_EFF_PROG_ESEC, DATA_FINE_PREV_PROG_ESEC, DATA_FINE_EFF_PROG_ESEC, 
                       DATA_INIZIO_PREV_AGG_BANDO, DATA_INIZIO_EFF_AGG_BANDO, DATA_FINE_PREV_AGG_BANDO, DATA_FINE_EFF_AGG_BANDO, #MEMO: eliminate perché non provengono da iter procedurale
                       DATA_INIZIO_PREV_STIP_ATTRIB, DATA_INIZIO_EFF_STIP_ATTRIB, DATA_FINE_PREV_STIP_ATTRIB, DATA_FINE_EFF_STIP_ATTRIB, 
                       DATA_INIZIO_PREV_ESECUZIONE, DATA_INIZIO_EFF_ESECUZIONE, DATA_FINE_PREV_ESECUZIONE, DATA_FINE_EFF_ESECUZIONE, 
                       DATA_INIZIO_PREV_COLLAUDO, DATA_INIZIO_EFF_COLLAUDO, DATA_FINE_PREV_COLLAUDO, DATA_FINE_EFF_COLLAUDO),
              by = "COD_LOCALE_PROGETTO") 
  
  # calcolo fase corrente
  appo1 <- appo %>% 
    mutate(FASE_CORRENTE = case_when(DATA_FINE_EFF_COLLAUDO > 0 & DATA_FINE_EFF_COLLAUDO <= max_date & DATA_FINE_EFF_ESECUZIONE > 0 ~ "END", #"COL_END" #MEMO: ci sono pochi casi in cui il collaudo sembra riferito a fasi precedenti o avviato in corso di esecuzione
                                     DATA_INIZIO_EFF_COLLAUDO > 0 & DATA_INIZIO_EFF_COLLAUDO <= max_date & DATA_FINE_EFF_ESECUZIONE > 0 ~ "END", #"COL"
                                     DATA_FINE_EFF_ESECUZIONE > 0 & DATA_FINE_EFF_ESECUZIONE <= max_date ~ "END", #"ESEC_COL"
                                     DATA_INIZIO_EFF_ESECUZIONE > 0 & DATA_INIZIO_EFF_ESECUZIONE <= max_date ~ "ESEC",
                                     DATA_FINE_EFF_STIP_ATTRIB > 0 & DATA_FINE_EFF_STIP_ATTRIB <= max_date ~ "STIP_ESEC",
                                     DATA_INIZIO_EFF_STIP_ATTRIB > 0 & DATA_INIZIO_EFF_STIP_ATTRIB <= max_date ~ "STIP",
                                     DATA_FINE_EFF_PROG_ESEC > 0 & DATA_FINE_EFF_PROG_ESEC <= max_date ~ "PE_STIP", 
                                     DATA_INIZIO_EFF_PROG_ESEC > 0 & DATA_INIZIO_EFF_PROG_ESEC <= max_date ~ "PE",
                                     DATA_FINE_EFF_PROG_DEF > 0 & DATA_FINE_EFF_PROG_DEF <= max_date ~ "PD_PE",
                                     DATA_INIZIO_EFF_PROG_DEF > 0 & DATA_INIZIO_EFF_PROG_DEF <= max_date ~ "PD",
                                     DATA_FINE_EFF_PROG_PREL > 0 & DATA_FINE_EFF_PROG_PREL <= max_date ~ "PP_PD",
                                     DATA_INIZIO_EFF_PROG_PREL > 0 & DATA_INIZIO_EFF_PROG_PREL <= max_date ~ "PP",
                                     DATA_FINE_EFF_STUDIO_FATT > 0 & DATA_FINE_EFF_STUDIO_FATT <= max_date ~ "SDF_PP",
                                     DATA_INIZIO_EFF_STUDIO_FATT > 0 & DATA_INIZIO_EFF_STUDIO_FATT <= max_date ~ "SDF",
                                     TRUE ~ "START"))
  
  # calcolo fase seguente
  appo2 <- appo1 %>% 
    mutate(FASE_SEGUENTE = case_when(FASE_CORRENTE == "END" ~ "END",
                                     FASE_CORRENTE == "COL_END" ~ "END",
                                     FASE_CORRENTE == "COL" ~ "COL_END",
                                     # FASE_CORRENTE == "ESEC_COL" & is.na(DATA_INIZIO_PREV_COLLAUDO) ~ "END",
                                     FASE_CORRENTE == "ESEC_COL" ~ "COL",
                                     FASE_CORRENTE == "ESEC" ~ "ESEC_COL",
                                     FASE_CORRENTE == "STIP_ESEC" ~ "ESEC",
                                     FASE_CORRENTE == "STIP" ~ "STIP_ESEC",
                                     
                                     FASE_CORRENTE == "PE_STIP" & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PE_STIP" & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PE_STIP" ~ "STIP", 
                                     
                                     FASE_CORRENTE == "PE" & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PE" & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PE" & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP",
                                     FASE_CORRENTE == "PE" ~ "PE_STIP",
                                     
                                     FASE_CORRENTE == "PD_PE" & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PD_PE" & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PD_PE" & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP",
                                     FASE_CORRENTE == "PD_PE" & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP",
                                     FASE_CORRENTE == "PD_PE" ~ "PE",
                                     
                                     FASE_CORRENTE == "PD" & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PD" & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PD" & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "PD" & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "PD" & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE",
                                     FASE_CORRENTE == "PD" ~ "PD_PE",
                                     
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE", 
                                     FASE_CORRENTE == "PP_PD" & is.na(DATA_INIZIO_PREV_PROG_DEF) ~ "PD_PE",
                                     FASE_CORRENTE == "PP_PD" ~ "PD",
                                     
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE", 
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) ~ "PD_PE", 
                                     FASE_CORRENTE == "PP" & is.na(DATA_FINE_PREV_PROG_PREL) ~ "PD",
                                     FASE_CORRENTE == "PP" ~ "PP_PD",
                                     
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE", 
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) ~ "PD_PE",
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) ~ "PD",
                                     FASE_CORRENTE == "SDF_PP" & is.na(DATA_INIZIO_PREV_PROG_PREL) ~ "PP_PD",
                                     FASE_CORRENTE == "SDF_PP" ~ "PP",
                                     
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE", 
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) ~ "PD_PE", 
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) ~ "PD",
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) ~ "PP_PD",
                                     FASE_CORRENTE == "SDF" & is.na(DATA_FINE_PREV_STUDIO_FATT) ~ "PP",
                                     FASE_CORRENTE == "SDF" ~ "SDF_PP",
                                     
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) & is.na(DATA_INIZIO_PREV_ESECUZIONE) & is.na(DATA_FINE_PREV_ESECUZIONE) & is.na(DATA_INIZIO_PREV_COLLAUDO) & is.na(DATA_FINE_PREV_COLLAUDO) ~ "END",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) & is.na(DATA_INIZIO_PREV_ESECUZIONE) & is.na(DATA_FINE_PREV_ESECUZIONE) & is.na(DATA_INIZIO_PREV_COLLAUDO) ~ "COL_END",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) & is.na(DATA_INIZIO_PREV_ESECUZIONE) & is.na(DATA_FINE_PREV_ESECUZIONE) ~ "COL",
                                     
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) & is.na(DATA_INIZIO_PREV_ESECUZIONE) ~ "ESEC_COL",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) & is.na(DATA_FINE_PREV_STIP_ATTRIB) ~ "ESEC",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) & is.na(DATA_INIZIO_PREV_STIP_ATTRIB) ~ "STIP_ESEC",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) & is.na(DATA_FINE_PREV_PROG_ESEC) ~ "STIP", 
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) & is.na(DATA_INIZIO_PREV_PROG_ESEC) ~ "PE_STIP", 
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) & is.na(DATA_FINE_PREV_PROG_DEF) ~ "PE", 
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) & is.na(DATA_INIZIO_PREV_PROG_DEF) ~ "PD_PE", 
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_PROG_PREL) ~ "PD",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) & is.na(DATA_INIZIO_PREV_PROG_PREL) ~ "PP_PD",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) & is.na(DATA_FINE_PREV_STUDIO_FATT) ~ "PP",
                                     FASE_CORRENTE == "START" & is.na(DATA_INIZIO_PREV_STUDIO_FATT) ~ "SDF_PP",
                                     FASE_CORRENTE == "START" ~ "SDF")) %>% 
    mutate(FASE_CORRENTE = factor(FASE_CORRENTE, levels = c("START", "SDF", "SDF_PP", "PP", "PP_PD", "PD", "PD_PE", "PE", "PE_STIP", "STIP", "STIP_ESEC", "ESEC", "ESEC_COL", "COL", "COL_END", "END")),
           FASE_SEGUENTE = factor(FASE_SEGUENTE, levels = c("START", "SDF", "SDF_PP", "PP", "PP_PD", "PD", "PD_PE", "PE", "PE_STIP", "STIP", "STIP_ESEC", "ESEC", "ESEC_COL", "COL", "COL_END", "END")))
  
  
  # calcolo data di controllo (prima data della fase seguente)
  appo3 <- appo2 %>% 
    mutate(FASE_SEGUENTE_DATA = case_when(FASE_SEGUENTE == "END" ~ as.numeric(bimestre), #MEMO: posto per definizione pari a bimestre
                                          FASE_SEGUENTE == "COL_END" ~ DATA_FINE_PREV_COLLAUDO,
                                          FASE_SEGUENTE == "COL" ~ DATA_INIZIO_PREV_COLLAUDO,
                                          FASE_SEGUENTE == "ESEC_COL" ~ DATA_FINE_PREV_ESECUZIONE,
                                          FASE_SEGUENTE == "ESEC" ~ DATA_INIZIO_PREV_ESECUZIONE,
                                          FASE_SEGUENTE == "STIP_ESEC" ~ DATA_FINE_PREV_STIP_ATTRIB,
                                          FASE_SEGUENTE == "STIP" ~ DATA_INIZIO_PREV_STIP_ATTRIB,
                                          FASE_SEGUENTE == "PE_STIP" ~ DATA_FINE_PREV_PROG_ESEC,
                                          FASE_SEGUENTE == "PE" ~ DATA_INIZIO_PREV_PROG_ESEC,
                                          FASE_SEGUENTE == "PD_PE" ~ DATA_FINE_PREV_PROG_DEF,
                                          FASE_SEGUENTE == "PD" ~ DATA_INIZIO_PREV_PROG_DEF,
                                          FASE_SEGUENTE == "PP_PD" ~ DATA_FINE_PREV_PROG_PREL,
                                          FASE_SEGUENTE == "PP" ~ DATA_INIZIO_PREV_PROG_PREL,
                                          FASE_SEGUENTE == "SDF_PP" ~ DATA_FINE_PREV_STUDIO_FATT,
                                          FASE_SEGUENTE == "SDF" ~ DATA_INIZIO_PREV_STUDIO_FATT))

  
  # verifica variazione data prevista di riferimento
  temp <- progetti_old %>% 
    inner_join(appo3 %>%
                 select(COD_LOCALE_PROGETTO, FASE_SEGUENTE), 
               by = "COD_LOCALE_PROGETTO") %>% 
    mutate(FASE_SEGUENTE_DATA_OLD = case_when(FASE_SEGUENTE == "END" ~ as.numeric(bimestre), #MEMO: posto per definizione pari a bimestre
                                              FASE_SEGUENTE == "COL_END" ~ DATA_FINE_PREV_COLLAUDO,
                                              FASE_SEGUENTE == "COL" ~ DATA_INIZIO_PREV_COLLAUDO,
                                              FASE_SEGUENTE == "ESEC_COL" ~ DATA_FINE_PREV_ESECUZIONE,
                                              FASE_SEGUENTE == "ESEC" ~ DATA_INIZIO_PREV_ESECUZIONE,
                                              FASE_SEGUENTE == "STIP_ESEC" ~ DATA_FINE_PREV_STIP_ATTRIB,
                                              FASE_SEGUENTE == "STIP" ~ DATA_INIZIO_PREV_STIP_ATTRIB,
                                              FASE_SEGUENTE == "PE_STIP" ~ DATA_FINE_PREV_PROG_ESEC,
                                              FASE_SEGUENTE == "PE" ~ DATA_INIZIO_PREV_PROG_ESEC,
                                              FASE_SEGUENTE == "PD_PE" ~ DATA_FINE_PREV_PROG_DEF,
                                              FASE_SEGUENTE == "PD" ~ DATA_INIZIO_PREV_PROG_DEF,
                                              FASE_SEGUENTE == "PP_PD" ~ DATA_FINE_PREV_PROG_PREL,
                                              FASE_SEGUENTE == "PP" ~ DATA_INIZIO_PREV_PROG_PREL,
                                              FASE_SEGUENTE == "SDF_PP" ~ DATA_FINE_PREV_STUDIO_FATT,
                                              FASE_SEGUENTE == "SDF" ~ DATA_INIZIO_PREV_STUDIO_FATT))
  
  appo4 <- appo3 %>%
    left_join(temp %>%
                select(COD_LOCALE_PROGETTO, FASE_SEGUENTE_DATA_OLD), 
              by = "COD_LOCALE_PROGETTO") %>% 
    mutate(DELTA_DATA_SEGUENTE = ymd(as.character(FASE_SEGUENTE_DATA)) - ymd(as.character(FASE_SEGUENTE_DATA_OLD)),
           CHK_DATA = case_when(DELTA_DATA_SEGUENTE > 0 ~ "posticipata",
                                DELTA_DATA_SEGUENTE == 0 ~ "invariata",
                                DELTA_DATA_SEGUENTE < 0 ~ "anticipata",
                                is.na(FASE_SEGUENTE_DATA_OLD) ~ "nuova",
                                TRUE ~ "chk"))
  
  # calcola ritardo
  # MEMO: chk su bimestre di monitoraggio di 60 giorni
  appo5 <- appo4 %>% 
    mutate(FASE_SEGUENTE_RITARDO = ymd(bimestre) - ymd(as.character(FASE_SEGUENTE_DATA)),
           # FASE_SEGUENTE_RITARDO = if_else(TEMP >= 0, TEMP, as.difftime(0, units = "days")),
           CHK_RITARDO = case_when(FASE_SEGUENTE_RITARDO >= 365 ~ "ritardo grave",
                                   FASE_SEGUENTE_RITARDO >= 60 ~ "ritardo",
                                   FASE_SEGUENTE_RITARDO > 0 ~ "scaduti",
                                   FASE_SEGUENTE_RITARDO <= -60 ~ "regolare",
                                   FASE_SEGUENTE_RITARDO == 0 & FASE_SEGUENTE == "END" ~ "concluso",
                                   FASE_SEGUENTE_RITARDO <= 0 ~ "in scadenza",
                                   is.na(FASE_SEGUENTE_RITARDO) ~ "chk_no_date")) %>% 
    mutate(CHK_RITARDO = factor(CHK_RITARDO, levels = c("concluso", "regolare", "in scadenza", "scaduti", "ritardo", "ritardo grave", "chk_no_date"))) %>% 
    mutate(SUM_RITARDO = case_when(CHK_RITARDO == "ritardo grave" ~ "ritardo",
                                   CHK_RITARDO == "ritardo" ~ "ritardo",
                                   CHK_RITARDO == "scaduti" ~ "ritardo",
                                   CHK_RITARDO == "chk_no_date" ~ "ritardo",
                                   TRUE ~ "regolare")) %>% 
    mutate(SUM_RITARDO = factor(SUM_RITARDO, levels = c("regolare", "ritardo")))
  
  appo6 <- appo5
  
  appo7 <- appo6 %>% 
    left_join(progetti %>% 
                mutate(REGIONE = paste0(COD_REGIONE, "_", DEN_REGIONE)) %>% 
                select(COD_LOCALE_PROGETTO, CUP_COD_NATURA, REGIONE, DATA_AGGIORNAMENTO, IMPEGNI, TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO") 
  
  appo8 <- appo7 %>% 
    get_stato_attuazione(., chk_today = chk_today) %>% 
    rename(x_STATO_PROCEDURALE = STATO_PROCED)
  
  ritardi <- appo8
  
  
  # debug
  if (debug == TRUE) {
    message("Avvio debug:")
  
    # chk su fase corrente
    chk <- ritardi %>% count(FASE_CORRENTE, x_STATO_PROCEDURALE)
    write.xlsx(chk, file.path(TEMP, "chk_fase_corrente_vs_stato.xlsx"))
    message("Allineamento tra FASE_CORRENTE e x_STATO_PROCEDURALE:")
    print(chk)
    
    
    # chk coerenza fase seguente su fase corrente
    chk <- ritardi %>% count(FASE_CORRENTE, FASE_SEGUENTE)
    write.xlsx(chk, file.path(TEMP, "chk_fase_corrente_vs_seguente.xlsx"))
    message("Allineamento tra FASE_CORRENTE e FASE_SEGUENTE:")
    print(chk)
    
    # chk date missing
    chk <- ritardi %>% filter(is.na(FASE_CORRENTE))
    write.xlsx(chk, file.path(TEMP, "chk_fase_corrente_missing.xlsx"))
    message("Assenza FASE_CORRENTE:")
    print(chk)
    
    # chk date missing
    chk <- ritardi %>% filter(is.na(FASE_SEGUENTE))
    write.xlsx(chk, file.path(TEMP, "chk_fase_seguente_missing.xlsx"))
    message("Assenza FASE_SEGUENTE:")
    print(chk)
    
    # chk date missing
    chk <- ritardi %>% filter(is.na(FASE_SEGUENTE_DATA))
    write.xlsx(chk, file.path(TEMP, "chk_fase_seguente_data_missing.xlsx"))
    message("Assenza FASE_SEGUENTE_DATA:")
    print(chk)
    
    # chk data fase seguente variate
    chk <- ritardi %>% count(CHK_DATA) 
    write.xlsx(chk, file.path(TEMP, "chk_variazione_fase_seguente_data.xlsx"))
    message("Variazione FASE_SEGUENTE_DATA:")
    print(chk)
    
    # chk data fase seguente variate
    chk <- ritardi %>% count(CHK_DATA) 
    write.xlsx(chk, file.path(TEMP, "chk_variazione_fase_seguente_data.xlsx"))
    message("Variazione FASE_SEGUENTE_DATA:")
    print(chk)
   
    # sintesi ritardo
    chk <- ritardi %>%
      group_by(CHK_DATA, CHK_RITARDO) %>%
      summarise(N = sum(n(), na.rm = TRUE)) %>%
      pivot_wider(id_cols = CHK_DATA, names_from = CHK_RITARDO, values_from = N, values_fill = 0)
    write.xlsx(chk, file.path(TEMP, "chk_sintesi_ritardo.xlsx"))
    message("Sintesi CHK_DATA per CHK_RITARDO:")
    print(chk)
    
    message("Fine debug")
  }

  # export
  if (export == TRUE) {
    write.xlsx(ritardi, file.path(TEMP, paste0("analisi_ritardi_progetti_", bimestre, ".xlsx")))    
  }  
  
  return(ritardi)
  
}