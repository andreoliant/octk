#' Workflow per analisi CIS
#' @param bimestre Bimestre di OpenCoesione
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param operazioni Dataset da operazioni_light
#' @param progetti Dataset da progetti_light
#' @param debug Vuoi verificare il DB e il mappging con i clp?
#' @param export Vuoi esportare in TEMP in xlsx?
workflow_cis <- function(bimestre, interventi=NULL, operazioni=NULL, progetti=NULL, debug=TRUE, export=TRUE) {
  
  # TODO: calcolare report programmi - cis in coesione.R
  
  if (is.null(interventi)) {
    interventi <- load_db_interventi(tipo = "CIS", simplify_loc=FALSE, use_temi=FALSE, use_sog=FALSE, 
                              use_ue=FALSE, use_flt=FALSE, use_articolaz=FALSE, use_location=FALSE, use_ciclo=TRUE, tipo_ciclo="CICLO_STRATEGIA")
  }

  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, usa_meuro=FALSE) 
  }
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, light = TRUE)
  }
  
  # lista clp
  lista_clp <- interventi %>%
    rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
    select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
    separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
    filter(!is.na(COD_LOCALE_PROGETTO))
  
  
  # mapping clp in operaizoni su lista clp in DB
  mapping_clp <- lista_clp %>%
    left_join(operazioni %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COE, COE_IMP, COE_PAG), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  
  
  if (debug == TRUE) {
    # verifica db
    message("Inizio verifica DB")
    
    # chk duplicati in lista clp
    print("Duplicati presenti in lista clp:")
    chk <- lista_clp %>%
      count(x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO) %>%
      filter(n > 1)
    print(chk)
    # MEMO: ora i duplicati sono risolti lato DB
    # x_AMBITO x_CICLO   COD_LOCALE_PROGETTO     n
    # <fct>    <fct>     <chr>               <int>
    # 1 FESR     2007-2013 1MTRA12214              2
    # 2 FSC      2007-2013 1MISESSOT05             2
    # 3 FSC      2007-2013 1MISESSOT09             2
    # 4 ORD      2007-2013 1MTRA12111              2
    
    
    # clp presenti in db con mismatch su operazioni per ciclo, ambito e programma
    print("Orfani di lista clp da matching per ciclo, ambito e programma:")
    chk <- lista_clp %>%
      anti_join(lista_clp %>%
                  inner_join(operazioni %>%
                               select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO), 
                             by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")),
                by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
      filter(x_AMBITO != "ORD")
    print(chk)
    
    # lista integrata interventi e operazioni
    operazioni_cis <- lista_cis_interventi_operazioni(interventi, lista_clp, operazioni, progetti, export)

  }
  
  
  # elab
  message("Inizio elaborazione")
  memo <- list()
  
  # aggregati
  out <- report_cis(interventi, lista_clp, operazioni, export)
  
  memo[["chk_cis.risorse-db.fin"]] <- sum(out$RISORSE, na.rm = TRUE) - sum(cis$FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)
  
  
  # aggregati per titolare
  out_titolari <- report_cis_titolari(interventi, lista_clp, operazioni, export)
  
  memo[["chk_titolari.risorse-db.fin"]] <- sum(out_titolari$RISORSE, na.rm = TRUE) - sum(cis$FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)
  memo[["chk_titolari.coe-cis.coe"]] <- sum(out_titolari$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  
  
  
  # aggregati per intervento
  out_interventi <- report_cis_interventi(interventi, lista_clp, operazioni, export)
  
  # chk
  memo[["chk_interventi.risorse-db.fin"]] <- sum(out_interventi$RISORSE, na.rm = TRUE) - sum(cis$FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)
  memo[["chk_interventi.coe-cis.coe"]] <- sum(out_interventi$COE, na.rm = TRUE) - sum(out$COE, na.rm = TRUE)
  
  
  # aggregati per cis su progetti
  out_totali <- report_cis_totali(interventi, lista_clp, progetti, export)
  
  # chk
  memo[["chk_totali.risorse-db.fin"]] <- sum(out_totali$RISORSE, na.rm = TRUE) - sum(cis$FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)
  
  
  message("Quality check:")
  print(memo)
  
  # lista integrata interventi e operazioni
  # progetti_cis <- lista_clp %>%
  #   distinct(CIS, COD_CIS, OC_TITOLO_INTERVENTO, COD_LOCALE_PROGETTO) %>% #MEMO: distinct necessario per clp su ambiti diversi
  #   left_join(progetti %>%
  #               distinct(COD_LOCALE_PROGETTO, CUP, OC_TITOLO_PROGETTO, x_CICLO, x_AMBITO, OC_CODICE_PROGRAMMA, OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI), 
  #             by = "COD_LOCALE_PROGETTO") %>%
  #   select(CIS, COD_CIS, OC_TITOLO_INTERVENTO, CUP, 
  #          x_CICLO, x_AMBITO, OC_CODICE_PROGRAMMA, COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
  #          CP = OC_FINANZ_TOT_PUB_NETTO, 
  #          IMP = IMPEGNI, 
  #          PAG = TOT_PAGAMENTI)
  # 
  # chk
  # sum(out_totali$CP, na.rm = TRUE) - sum(progetti_cis$CP, na.rm = TRUE)
  
}


#' Report con aggregati per cis (in risorse coesione)
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param lista_clp Mapping tra interventi CIS e CLP...
#' @param operazioni Dataset da operazioni_light
#' @param export Vuoi esportare in TEMP in xlsx?
report_cis <- function(interventi, lista_clp=NULL, operazioni=NULL, export=TRUE) {
  
  cis <- interventi
  
  if (is.null(lista_clp)) {
    lista_clp <- cis %>%
      rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
      select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
      separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
      filter(!is.na(COD_LOCALE_PROGETTO))
  }
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, usa_meuro=FALSE)
  }
  
  # aggregati per cis
  out <- cis %>%
    group_by(CIS, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    full_join(lista_clp %>%
                left_join(operazioni %>%
                            select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COE, COE_IMP, COE_PAG), 
                          by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
                group_by(CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO) %>% 
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE), 
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("CIS", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, "CIS")
    writeData(wb, sheet = "CIS", x = out, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(TEMP, "report_cis.xlsx"), overwrite = TRUE)
  }
  
  return(out)
}


#' Report con aggregati per cis e titolare (in risorse coesione)
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param lista_clp Mapping tra interventi CIS e CLP...
#' @param operazioni Dataset da operazioni_light
#' @param export Vuoi esportare in TEMP in xlsx?
report_cis_titolari <- function(interventi, lista_clp=NULL, operazioni=NULL, export=TRUE) {
  
  cis <- interventi
  
  if (is.null(lista_clp)) {
    lista_clp <- cis %>%
      rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
      select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
      separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
      filter(!is.na(COD_LOCALE_PROGETTO))
  }
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, usa_meuro=FALSE)
  }
  
  # aggregati per cis
  out_titolari <- cis %>%
    group_by(COD_CIS, CIS, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    full_join(lista_clp %>%
                left_join(operazioni %>%
                            select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COE, COE_IMP, COE_PAG), 
                          by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
                group_by(COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO) %>% 
                summarise(COE = sum(COE, na.rm = TRUE), 
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("COD_CIS", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, "CIS")
    writeData(wb, sheet = "CIS", x = out_titolari, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(TEMP, "report_cis_titolari.xlsx"), overwrite = TRUE)
  }
  
  return(out_titolari)
}


#' Report con dettaglio per cis, titolare e intervento (in risorse coesione)
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param lista_clp Mapping tra interventi CIS e CLP...
#' @param operazioni Dataset da operazioni_light
#' @param export Vuoi esportare in TEMP in xlsx?
report_cis_interventi <- function(interventi, lista_clp=NULL, operazioni=NULL, export=TRUE) {
  
  cis <- interventi
  
  if (is.null(lista_clp)) {
    lista_clp <- cis %>%
      rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
      select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
      separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
      filter(!is.na(COD_LOCALE_PROGETTO))
  }
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, usa_meuro=FALSE)
  }
  
  # operazioni aggregate per intervento cis
  out_interventi <- cis %>%
    rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
    group_by(COD_CIS, CIS, OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_AMBITO, x_CICLO, OC_TITOLO_INTERVENTO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    full_join(lista_clp %>%
                left_join(operazioni %>%
                            select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COE, COE_IMP, COE_PAG), 
                          by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
                group_by(COD_CIS, OC_TITOLO_INTERVENTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO) %>% 
                summarise(COE = sum(COE, na.rm = TRUE), 
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("COD_CIS", "OC_TITOLO_INTERVENTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO"))
  
  
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, "CIS")
    writeData(wb, sheet = "CIS", x = out_interventi, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(TEMP, "report_cis_interventi.xlsx"), overwrite = TRUE)
  }
  
  return(out_interventi)
}


#' Report con aggregati per cis (in risorse totali)
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param lista_clp Mapping tra interventi CIS e CLP...
#' @param progetti Dataset da progetti_light
#' @param export Vuoi esportare in TEMP in xlsx?
report_cis_totali <- function(interventi, lista_clp=NULL, progetti=NULL, export=TRUE) {
  
  cis <- interventi
  
  if (is.null(lista_clp)) {
    lista_clp <- cis %>%
      rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
      select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
      separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
      filter(!is.na(COD_LOCALE_PROGETTO))
  }
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, light = TRUE)
  }
  
  # aggregati per cis su progetti
  out_totali <- cis %>%
    group_by(CIS) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    full_join(lista_clp %>%
                distinct(CIS, COD_LOCALE_PROGETTO) %>% #MEMO: distinct necessario per clp su ambiti diversi
                left_join(progetti %>%
                            distinct(COD_LOCALE_PROGETTO, OC_FINANZ_TOT_PUB_NETTO, IMPEGNI, TOT_PAGAMENTI), 
                          by = "COD_LOCALE_PROGETTO") %>%
                group_by(CIS) %>% 
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE), 
                          IMP = sum(IMPEGNI, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)),
              by = "CIS")
  
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, "CIS")
    writeData(wb, sheet = "CIS", x = out_totali, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(TEMP, "report_cis_totali.xlsx"), overwrite = TRUE)
  }
  
  return(out_totali)
}


#' Lista interventi cis con mapping a operazioni
#' @param interventi Interventi CIS da load_db_interventi(tipo = "CIS)
#' @param lista_clp Mapping tra interventi CIS e CLP...
#' @param operazioni Dataset da operazioni_light
#' @param progetti Dataset da progetti_light
#' @param export Vuoi esportare in TEMP in xlsx?
lista_cis_interventi_operazioni <- function(interventi, lista_clp=NULL, operazioni=NULL, progetti=NULL, export=TRUE) {
  
  cis <- interventi
  
  if (is.null(lista_clp)) {
    lista_clp <- cis %>%
      rename(OC_TITOLO_INTERVENTO = OC_TITOLO_PROGETTO) %>%
      select(CIS, COD_CIS, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COD_LOCALE_PROGETTO, OC_TITOLO_INTERVENTO) %>%
      separate_rows(COD_LOCALE_PROGETTO, sep = ":::") %>%
      filter(!is.na(COD_LOCALE_PROGETTO))
  }
  
  if (is.null(operazioni)) {
    operazioni <- load_operazioni(bimestre, usa_meuro=FALSE)
  }
  
  if (is.null(progetti)) {
    progetti <- load_progetti(bimestre, light = TRUE)
  }
  
  # lista integrata interventi e operazioni
  operazioni_cis <- lista_clp %>%
    left_join(operazioni %>%
                select(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_AMBITO, x_CICLO, COE, COE_IMP, COE_PAG), 
              by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO", "x_CICLO")) %>%
    # recupera CUP e titolo progetto
    left_join(progetti %>%
                select(COD_LOCALE_PROGETTO, CUP, OC_TITOLO_PROGETTO),
              by = "COD_LOCALE_PROGETTO") %>%
    select(CIS, COD_CIS, OC_TITOLO_INTERVENTO, CUP, 
           x_CICLO, x_AMBITO, OC_CODICE_PROGRAMMA, COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO,
           COE, COE_IMP, COE_PAG)
  
  if (export == TRUE) {
    wb <- createWorkbook()
    addWorksheet(wb, "CIS")
    writeData(wb, sheet = "CIS", x = operazioni_cis, startCol = 1, startRow = 1, colNames = TRUE)
    saveWorkbook(wb, file = file.path(TEMP, "lista_cis_interventi_operazioni.xlsx"), overwrite = TRUE)
  }
  
  return(operazioni_cis)
}



