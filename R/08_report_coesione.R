#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe operazioni
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @param add_totali Vuoi aggiungere valori calcolati in termini di costo pubblico?
#' @param use_cp2 Se add_totali == TRUE, vuoi raddoppiare i valori relativi ai progetti multi-programma?  
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
OLD_make_report_programmi_coesione <- function(perimetro, usa_meuro=FALSE, use_713=FALSE, use_eu=FALSE, use_flt=FALSE, use_po_psc=FALSE,
                                               add_totali=FALSE, use_cp2=FALSE, cut_no_risorse=FALSE,
                                               tipo_ciclo="CICLO_STRATEGIA",
                                               focus="report", export=FALSE, export_xls=FALSE, progetti=NULL, po_riclass=NULL) {
  
  # perimetro <- operazioni
  # DEBUG: 
  # use_713 <- TRUE
  # use_flt <- TRUE
  # tipo_ciclo <- "CICLO_STRATEGIA"
  # use_po_psc <- TRUE
  
  # OLD:
  # programmi <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_po_psc=use_po_psc) %>%
  #   rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
  #          x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  programmi <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_eu=TRUE, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_po_psc=use_po_psc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  message("programmi caricato")
  
  if (use_flt == TRUE) {
    
    # fix per 2007IT005FAMG1
    programmi <- programmi %>% 
      mutate(FLAG_MONITORAGGIO = if_else(OC_CODICE_PROGRAMMA == "2007IT005FAMG1", 1, FLAG_MONITORAGGIO))
    
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }
  
  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI anche per perimetro 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  # MEMO: c'è anche su progetti sotto
  
  
  # MEMO: patch per factor di x_AMBITO e x_CICLO
  # perimetro <- perimetro %>%
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  # perimetro <- refactor_ambito(perimetro)
  # perimetro <- refactor_ciclo(perimetro)
  
  # if (usa_meuro == TRUE) {
  #   programmi <- programmi %>%
  #     mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  # }
  
  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")
  
  spalla <- programmi %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
    #                                               "FEAD", "FAMI", "CTE", "ORD")),
    #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  # MEMO: questo group_by è sufficiente a gestire direttrici ferroviarie con use_po_psc = TRUE
  
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  } 
  
  
  if (is.null(po_riclass)) {
    po_riclass <- octk::po_riclass
  }
  
  
  # spalla <- po_riclass %>%
  #   filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
  #          x_CICLO != "2000-2006",
  #          x_AMBITO != "FEASR") %>%
  #   filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
  #   separate_rows(x_AMBITO, sep="-") %>%
  #   # MEMO: patch per factor di x_AMBITO e x_CICLO
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #   arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   left_join(programmi %>%
  #               # MEMO: patch per factor di x_AMBITO e x_CICLO
  #               mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                             "FEAD", "FAMI", "CTE", "ORD")),
  #                      x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
  
  # report
  out <- spalla %>%
    full_join(perimetro %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    left_join(po_riclass %>%
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == TRUE) {
    out <- out %>%
      select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG,
             `Non avviato`,
             `In avvio di progettazione`,
             `In corso di progettazione`,
             `In affidamento`,
             `In esecuzione`,
             `Eseguito`,
             `Eseguito`)
    
  } else {
    out <- out %>%
      select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, COE, COE_IMP, COE_PAG,
             `Non avviato`,
             `In avvio di progettazione`,
             `In corso di progettazione`,
             `In affidamento`,
             `In esecuzione`,
             `Eseguito`,
             `Eseguito`)
    
  }
  
  
  # aggiunge valori calcolati in termini di FTPN
  if (add_totali == TRUE) {
    message("integrazione risorse totali")
    if (is.null(progetti)) {
      progetti <- load_progetti(bimestre, light = TRUE, refactor = TRUE)
    }
    
    # patch YEI anche per perimetro 
    progetti <- progetti  %>%
      mutate(x_AMBITO = as.character(x_AMBITO)) %>%
      mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                  TRUE ~ x_AMBITO)) %>%
      refactor_ambito(.)
    
    progetti <- progetti %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.) %>%
      # MEMO: patch per factor di x_AMBITO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE", "ORD")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,
             CP = OC_FINANZ_TOT_PUB_NETTO,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
    
    # if (usa_meuro == TRUE) {
    #   progetti <- progetti %>%
    #     mutate(CP = CP / 1000000,
    #            IMP = IMP / 1000000,
    #            PAG = PAG / 1000000)
    # }
    
    out <- out %>%
      left_join(progetti %>%
                  group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                  summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE)),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>% # MEMO: qui perdo multi ":::" in progetti
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    
    # recupero multi
    appo <- progetti %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
      summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE)) %>%
      anti_join(out,
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
      # riempie NA con 0
      # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # recupera x_vars
      left_join(po_riclass %>%
                  refactor_ambito(.) %>%
                  refactor_ciclo(.) %>%
                  # MEMO: patch per factor di x_AMBITO e x_CICLO
                  # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                  #                                               "FEAD", "FAMI", "CTE", "ORD")),
                  #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
                  select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA),
                by = c("OC_CODICE_PROGRAMMA", "x_CICLO"))
    
    # bind di multi
    out <- out %>%
      bind_rows(appo) %>%
      as_tibble(.) %>%
      # riempie NA con 0
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      refactor_ambito(.) %>%
      refactor_ciclo(.)
    
    # CHK: QUI ACCODA RIGHE ":::" A QUELLE BASE
    
    # versione con cp2
    if (use_cp2 == TRUE) {
      
      appo <- out %>%
        filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        as_tibble(.) %>%
        select(OC_CODICE_PROGRAMMA, x_AMBITO_FSE_FESR = x_AMBITO, N_CLP, CP, IMP, PAG) %>%
        # CHK: forse N_CLP non va sommato...
        mutate(x_AMBITO_FSE_FESR = as.character(x_AMBITO_FSE_FESR)) %>%
        separate_rows(OC_CODICE_PROGRAMMA, sep = ":::") %>%
        # recupera x_vars
        left_join(po_riclass %>%
                    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO),
                  by = "OC_CODICE_PROGRAMMA") %>%
        # left_join(programmi %>%
        #             as_tibble(.) %>%
        #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
        #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
        # modifica x_AMBITO
        mutate(x_AMBITO = case_when(x_AMBITO == "FESR-FSE" ~ x_AMBITO_FSE_FESR, # MEMO: split per programmi pluri-fondo
                                    x_AMBITO == "YEI-FSE" ~ x_AMBITO_FSE_FESR,
                                    x_AMBITO == "FSC-POC" ~ "FSC",  # MEMO: forzo su FSC
                                    TRUE ~ x_AMBITO)) %>%
        refactor_ambito(.) %>%
        refactor_ciclo(.) %>%
        left_join(programmi %>%
                    as_tibble(.) %>%
                    distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
                  by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))# %>%
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE", "ORD")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      # ripristina struttura per bindrows
      
      if (use_eu == TRUE) {
        appo <- appo %>% 
          mutate(RISORSE = 0,
                 RISORSE_UE = 0,
                 N = 0,
                 COE = 0,
                 COE_IMP = 0,
                 COE_PAG = 0,
                 `Non avviato` = 0,
                 `In avvio di progettazione` = 0,
                 `In corso di progettazione` = 0,
                 `In affidamento` = 0,
                 `In esecuzione` = 0,
                 `Eseguito` = 0) %>%
          select(names(out))
      } else {
        appo <- appo %>% 
          mutate(RISORSE = 0,
                 N = 0,
                 COE = 0,
                 COE_IMP = 0,
                 COE_PAG = 0,
                 `Non avviato` = 0,
                 `In avvio di progettazione` = 0,
                 `In corso di progettazione` = 0,
                 `In affidamento` = 0,
                 `In esecuzione` = 0,
                 `Eseguito` = 0) %>%
          select(names(out))
      }
      
      
      # TODO: qui va aggiustata la parte PAC di direttrici ferroviarie e di giustizia digitale perché CP resta vuoto (o forse dentro report programmi)
      
      
      programmi_2 <- out %>%
        filter(!grepl(":::", OC_CODICE_PROGRAMMA)) %>%
        bind_rows(appo)
      dim(out)[1] + dim(appo)[1]/2 == dim(programmi_2)[1]
      
      # sovrascrive out
      out <- programmi_2 %>%
        group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE)
      
      if (usa_meuro == TRUE) {
        out <- out %>%
          mutate(CP = round(CP / 1000000, 1),
                 IMP = round(IMP / 1000000, 1),
                 PAG = round(PAG / 1000000, 1))
      }
      
    }
  }
  
  if (cut_no_risorse == TRUE) {
    out <- out %>%
      filter(RISORSE > 0)
  }
  
  if (export == TRUE) {
    if (use_cp2 == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi.csv")), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (use_cp2 == TRUE) {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_cp2.xlsx")))
    } else {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi.xlsx")))
    }
  }
  
  
  return(out)
}




#' Integra report per Programmi con delta da un bimestre precedente (per coesione)
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
#' @note DA COMPLETARE
add_delta_programmi_coesione <- function(bimestre, programmi, last_bimestre, last_data_path=NULL,
                                         usa_meuro=FALSE, focus="delta", export=FALSE) {
  
  # bimestre precedente
  if (is.null(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }
  
  # loads
  last_perimetro <- read_csv2(file.path(last_data_path, paste0("operazioni_light_", last_bimestre, ".csv")), guess_max = 1000000)
  
  # switch variabile COE
  # last_perimetro <- last_progetti %>%
  #   mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #   mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
  #          COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
  #                          x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
  #                          x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
  #                          TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(COE = COE / 1000000,
             COE_IMP = COE_IMP / 1000000,
             COE_PAG = COE_PAG / 1000000)
  }
  
  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)
  
  appo <- last_perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                               "FEAD", "FAMI", "CTE")),
  #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  
  # report
  out <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
    #                                               "FEAD", "FAMI", "CTE")),
    #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    # refactor_ambito(.) %>%
    # refactor_ciclo(.) %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (COE_PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  
  # if (add_totali == TRUE) {
  #
  #   last_progetti <- load_progetti(bimestre = last_bimestre,
  #                                  data_path = last_data_path,
  #                                  visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE) %>%
  #     select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO,
  #            CP = OC_FINANZ_TOT_PUB_NETTO,
  #            IMP = IMPEGNI,
  #            PAG = TOT_PAGAMENTI)
  #
  #   if (usa_meuro == TRUE) {
  #     last_progetti <- last_progetti %>%
  #       mutate(CP = CP / 1000000,
  #              IMP = IMP / 1000000,
  #              PAG = PAG / 1000000)
  #
  # TODO: aggiungere il calcolo
  #
  #   }
  # } 
  
  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}



#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono come in 'programmi', vale solo se 'programmi' è in euro
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre
  
  # fix
  programmi <- programmi %>%
    mutate(N_CLP = N) #,
  # IMP = 0,
  # PAG = 0)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
    
  } else {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
  }
  
  
  
  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }
  
  return(report)
}

#' Esporta report per ciclo, ambito e macroarea con dati coesione
#'
#' Report con apertura ciclo, ambito e macroarea.
#' E' costruito su operazioni e dati coesione.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_macroaree_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo, ambto e macroarea.
make_report_macroaree_coesione <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEBUG:
  # programmi <- programmi_macroaree
  
  report <- programmi  %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    arrange(x_CICLO, x_AMBITO, x_MACROAREA)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N) 
      
    }
  } else {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N) 
      
    }
  }
  
  
  
  
  out <- report %>% 
    filter(# x_AMBITO != "FEAMP", 
      x_AMBITO != "FEASR") %>% 
    arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA)
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "report_macroaree.csv"))
  }
  
  return(out)
}



#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe operazioni
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC? [FUNZIONALITA' DEPRECATA]
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_coesione <- function(perimetro, usa_meuro=FALSE, show_cp=FALSE, use_713=FALSE, use_eu=FALSE, use_flt=FALSE, 
                                           use_po_psc=FALSE,
                                           use_cicli_psc=FALSE,
                                           use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                           cut_no_risorse=FALSE,
                                           tipo_ciclo="CICLO_STRATEGIA",
                                           focus="report", export=FALSE, export_xls=FALSE, progetti=NULL, po_riclass=NULL, DB) {
  
  # perimetro <- operazioni
  # DEBUG: 
  # use_713 <- TRUE
  # use_flt <- TRUE
  # tipo_ciclo <- "CICLO_STRATEGIA"
  # use_po_psc <- FALSE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- FALSE
  # stime_fix_siepoc <- FALSE
  
  # OLD:
  # programmi <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_po_psc=use_po_psc) %>%
  #   rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
  #          x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  message("programmi caricato")
  
  if (use_flt == TRUE) {
    
    # fix per 2007IT005FAMG1
    programmi <- programmi %>% 
      mutate(FLAG_MONITORAGGIO = if_else(OC_CODICE_PROGRAMMA == "2007IT005FAMG1", 1, FLAG_MONITORAGGIO))
    
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
    
    # CHK: perché non c'era? forse all'inizio filtravo solo visualizzati in operazioni...
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 10)
  }
  
  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI anche per perimetro 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  # MEMO: c'è anche su progetti sotto
  
  
  # MEMO: patch per factor di x_AMBITO e x_CICLO
  # perimetro <- perimetro %>%
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  # perimetro <- refactor_ambito(perimetro)
  # perimetro <- refactor_ciclo(perimetro)
  
  # if (usa_meuro == TRUE) {
  #   programmi <- programmi %>%
  #     mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  # }
  
  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")
  
  spalla <- programmi %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
    #                                               "FEAD", "FAMI", "CTE", "ORD")),
    #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  # MEMO: questo group_by è sufficiente a gestire direttrici ferroviarie con use_po_psc = TRUE
  
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  } 
  
  
  if (is.null(po_riclass)) {
    po_riclass <- octk::po_riclass
  }
  
  
  # spalla <- po_riclass %>%
  #   filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
  #          x_CICLO != "2000-2006",
  #          x_AMBITO != "FEASR") %>%
  #   filter(!(grepl(":::", OC_CODICE_PROGRAMMA))) %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
  #   separate_rows(x_AMBITO, sep="-") %>%
  #   # MEMO: patch per factor di x_AMBITO e x_CICLO
  #   mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                 "FEAD", "FAMI", "CTE", "ORD")),
  #          x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #   arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   left_join(programmi %>%
  #               # MEMO: patch per factor di x_AMBITO e x_CICLO
  #               mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                                                             "FEAD", "FAMI", "CTE", "ORD")),
  #                      x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
  
  
  if (show_cp == TRUE) {
    appo <- perimetro %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG, na.rm = TRUE))
  } else {
    appo <- perimetro %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE))
  }
  
  
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    # left_join(po_riclass %>%
    #             refactor_ambito(.) %>%
    #             refactor_ciclo(.) %>%
    #             select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
    #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # as_tibble(.) %>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    left_join(po_riclass %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    mutate(x_PROGRAMMA = if_else(OC_CODICE_PROGRAMMA == "2007IT001FA005" & x_AMBITO == "FSC", "DIRETTRICI FERROVIARIE", x_PROGRAMMA),
           x_GRUPPO = if_else(OC_CODICE_PROGRAMMA == "2007IT001FA005" & x_AMBITO == "FSC", "PRA_NAZ", x_GRUPPO))
  
  if (show_cp == TRUE) {
    if (usa_meuro == TRUE) {
      out <- out %>%
        mutate(COE = round(COE / 1000000, 1),
               COE_IMP = round(COE_IMP / 1000000, 1),
               COE_PAG = round(COE_PAG / 1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1),
               `Non avviato` = round(`Non avviato` / 1000000, 1),
               `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
               `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
               `In affidamento` = round(`In affidamento` / 1000000, 1),
               `In esecuzione` = round(`In esecuzione` / 1000000, 1),
               `Eseguito` = round(`Eseguito` / 1000000, 1))
    }
  } else {
    if (usa_meuro == TRUE) {
      out <- out %>%
        mutate(COE = round(COE / 1000000, 1),
               COE_IMP = round(COE_IMP / 1000000, 1),
               COE_PAG = round(COE_PAG / 1000000, 1),
               `Non avviato` = round(`Non avviato` / 1000000, 1),
               `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
               `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
               `In affidamento` = round(`In affidamento` / 1000000, 1),
               `In esecuzione` = round(`In esecuzione` / 1000000, 1),
               `Eseguito` = round(`Eseguito` / 1000000, 1))
    } 
  }
  
  if (show_cp == TRUE) {
    if (use_eu == TRUE) {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    } else {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG, 
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    }
  } else {
    if (use_eu == TRUE) {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    } else {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, COE, COE_IMP, COE_PAG,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    }
  }
  
  
  
  
  if (cut_no_risorse == TRUE) {
    out <- out %>%
      filter(RISORSE > 0)
  }
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi.csv")), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_cp2.xlsx")))
    } else {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi.xlsx")))
    }
  }
  
  
  return(out)
}




#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe operazioni
#' @param perimetro_sie Dataset con valori per macroaree dai livelli gerarchici
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC? [FUNZIONALITA' DEPRECATA]
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx in OUTPUT?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_macroaree_coesione <- function(perimetro, perimetro_sie=NULL, usa_meuro=FALSE, use_713=FALSE, use_eu=FALSE,
                                                     use_flt=FALSE, use_po_psc=FALSE, use_cicli_psc=FALSE,
                                                     use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                     show_cp=FALSE, cut_no_risorse=FALSE,
                                                     tipo_ciclo="CICLO_STRATEGIA",
                                                     focus="report", export=FALSE, export_xls=FALSE, progetti=NULL, po_riclass=NULL, DB) {
  
  # DEBUG: 
  # use_713 <- TRUE
  # use_flt <- TRUE
  # use_po_psc <- TRUE
  # tipo_ciclo <- "CICLO_STRATEGIA"
  # perimetro <- operazioni
  
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  
  if (use_flt == TRUE) {
    # fix per 2007IT005FAMG1
    programmi <- programmi %>% 
      mutate(FLAG_MONITORAGGIO = if_else(OC_CODICE_PROGRAMMA == "2007IT005FAMG1", 1, FLAG_MONITORAGGIO))
    
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }
  
  # NEW: switch per dati da livelli gerarchici
  if (!is.null(perimetro_sie)) {
    perimetro_2 <- perimetro %>% 
      # scarta le righe sie di cui uso dati da livelli gerarchici
      anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
      # accoda nuove righe sie
      bind_rows(perimetro_sie)
    
    # chk
    temp <- sum(perimetro_2$COE, na.rm = TRUE) - sum(perimetro$COE, na.rm = TRUE)
    message(paste0("Questo è il delta usando i dati per livelli gerarchici di FESR, FSE e YEI: ", temp))
    
  } else {
    # perimetro_sie <- setup_macroaree_sie(bimestre, operazioni)
    perimetro_sie <- setup_macroaree_sie(operazioni = perimetro)
    
    perimetro_2 <- perimetro %>% 
      # scarta le righe sie di cui uso dati da livelli gerarchici
      anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
      # accoda nuove righe sie
      bind_rows(perimetro_sie)
    
    # chk
    sum(perimetro$COE, na.rm = T) - sum(perimetro_2$COE, na.rm = T)
    # 0.08990479
    # -11.89011
  }
  
  perimetro <- perimetro_2
  
  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI anche per perimetro 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  # MEMO: c'è anche su progetti sotto
  
  # ricodifica x_MACROAREA
  # programmi <- ricodifica_macroaree(programmi)
  # MEMO: ora è spostato in init_programmazione()
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  } 
  
  if (is.null(po_riclass)) {
    # po_riclass <- octk::po_riclass
    # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
    
    po_riclass <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
      rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
      distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
    
  }
  
  # refactor
  spalla <- refactor_macroarea(spalla)
  perimetro <- refactor_macroarea(perimetro)
  
  if (show_cp == TRUE) {
    appo <- perimetro %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE))
  } else {
    appo <- perimetro %>%
      group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
      summarise(N = n(),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE))
  }
  
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea()%>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    left_join(po_riclass %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    mutate(x_PROGRAMMA = if_else(OC_CODICE_PROGRAMMA == "2007IT001FA005" & x_AMBITO == "FSC", "DIRETTRICI FERROVIARIE", x_PROGRAMMA),
           x_GRUPPO = if_else(OC_CODICE_PROGRAMMA == "2007IT001FA005" & x_AMBITO == "FSC", "PRA_NAZ", x_GRUPPO))
  
  
  if (show_cp == TRUE) {
    if (usa_meuro == TRUE) {
      out <- out %>%
        mutate(COE = round(COE / 1000000, 1),
               COE_IMP = round(COE_IMP / 1000000, 1),
               COE_PAG = round(COE_PAG / 1000000, 1),
               CP = round(CP/1000000, 1),
               `Non avviato` = round(`Non avviato` / 1000000, 1),
               `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
               `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
               `In affidamento` = round(`In affidamento` / 1000000, 1),
               `In esecuzione` = round(`In esecuzione` / 1000000, 1),
               `Eseguito` = round(`Eseguito` / 1000000, 1))
    }
  } else {
    if (usa_meuro == TRUE) {
      out <- out %>%
        mutate(COE = round(COE / 1000000, 1),
               COE_IMP = round(COE_IMP / 1000000, 1),
               COE_PAG = round(COE_PAG / 1000000, 1),
               `Non avviato` = round(`Non avviato` / 1000000, 1),
               `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
               `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
               `In affidamento` = round(`In affidamento` / 1000000, 1),
               `In esecuzione` = round(`In esecuzione` / 1000000, 1),
               `Eseguito` = round(`Eseguito` / 1000000, 1))
    } 
  }
  
  
  if (show_cp == TRUE) {
    if (use_eu == TRUE) {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    } else {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, N, COE, COE_IMP, COE_PAG, CP,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    }
  } else {
    if (use_eu == TRUE) {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    } else {
      out <- out %>%
        select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, N, COE, COE_IMP, COE_PAG,
               `Non avviato`,
               `In avvio di progettazione`,
               `In corso di progettazione`,
               `In affidamento`,
               `In esecuzione`,
               `Eseguito`,
               `Eseguito`)
      
    }
  }
  
  
  # DEBUG:
  # memo_out <- out
  
  
  # ricodifica
  # out <- out %>% 
  #   mutate(x_MACROAREA = as.character(x_MACROAREA)) %>% 
  #   mutate(x_MACROAREA = case_when(x_MACROAREA == "Ambito nazionale" ~ "Non classificato",
  #                                  TRUE ~ x_MACROAREA))
  
  
  if (cut_no_risorse == TRUE) {
    # out <- out %>%
    #   filter(RISORSE > 0)
    messagge("Con cut_no_risorse=TRUE si escudono casi con RISORSE 0 ma progetti monitorati su ambito nazionale, Sei sicuro?")
    
    appo <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepocE) %>%
      rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
    
    if (use_flt == TRUE) {
      appo <- appo %>%
        filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      # MEMO: in FSC resta anche tipo 9 che viene scartato
    }
    
    out <- out %>% 
      semi_join(appo, by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
    
  }
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree.csv")), row.names = FALSE)
    }
  }
  
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_macroaree_cp2.xlsx")), rowNames = FALSE)
    } else {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_macroaree.xlsx")), rowNames = FALSE)
    }
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
chk_variazione_risorse_programmi_coesione <- function(programmi_new=NULL, programmi_old=NULL, path_to_new=NULL, path_to_old=NULL, export=FALSE){
  
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
    write.csv2(out, file.path(TEMP, "delta_risorse_programmi.csv"), row.names = FALSE)
  }
  
  return(out)
  
}


#' Integra report bimestre con bimestre precedente a scelta
#'
#' Integra report bimestre con bimestre precedente a scelta.
#'
#' @param report Report da make_report_bimestre_coesione.
#' @param path_to_old Percorso a elaborazione per bimestre precedente fino a versione, senza nome file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo e ambito.
add_delta_report_bimestre_coesione <- function(report, path_to_old, export=FALSE) {
  
  # path_to_old <- file.path(DRIVE, "ELAB", "20210430", "STATO", "stato", "V.01")
  
  report_old <- read_csv2(file.path(path_to_old, "temp", "report.csv")) %>% 
    select(x_CICLO, x_AMBITO, COE_OLD = COE, COE_IMP_OLD = COE_IMP, COE_PAG_OLD = COE_PAG)
  
  out <- report %>% 
    left_join(report_old, by = c("x_CICLO", "x_AMBITO"))
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "report.csv"))
  }
  
  return(out)
  
}



#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe operazioni
#' @param perimetro_sie Dataset con valori per macroaree dai livelli gerarchici
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param use_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_po_psc Vuoi usare i dati di programmazione per PO ante art. 44 e non per PSC?
#' @param add_totali Vuoi aggiungere valori calcolati in termini di costo pubblico?
#' @param use_cp2 Se add_totali == TRUE, vuoi raddoppiare i valori relativi ai progetti multi-programma?
#' @param cut_no_risorse Vuoi eliminare i programmi monitorati senza risorse lato DB?
#' @param tipo_ciclo Vuoi usare CICLO_STRATEGIA (default in x_AMBITO nel DB) o CICCLO_RISORSE in senso contabile (sovrascrive x_AMBITO da DB)?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx in OUTPUT?
#' @param  progetti dataset di tipo "progetti" da utilizzare per con add_totali == TRUE
#' @param  po_riclass dataset di tipo "po_riclass" da utilizzare (altrimenti usa default nel package)
#' @return Un file csv con apertura per programma e fase procedurale.
OLD_make_report_programmi_macroaree_coesione <- function(perimetro, perimetro_sie=NULL, usa_meuro=FALSE, use_713=FALSE, use_eu=FALSE,
                                                         use_flt=FALSE, use_po_psc=FALSE,
                                                         add_totali=FALSE, use_cp2=FALSE, cut_no_risorse=FALSE,
                                                         tipo_ciclo="CICLO_STRATEGIA",
                                                         focus="report", export=FALSE, export_xls=FALSE, progetti=NULL, po_riclass=NULL) {
  
  # DEBUG: 
  # use_713 <- TRUE
  # use_flt <- TRUE
  # use_po_psc <- TRUE
  # tipo_ciclo <- "CICLO_STRATEGIA"
  # perimetro <- operazioni
  
  programmi <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_eu=TRUE, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=TRUE, use_po_psc=use_po_psc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  
  if (use_flt == TRUE) {
    # fix per 2007IT005FAMG1
    programmi <- programmi %>% 
      mutate(FLAG_MONITORAGGIO = if_else(OC_CODICE_PROGRAMMA == "2007IT005FAMG1", 1, FLAG_MONITORAGGIO))
    
    programmi <- programmi %>%
      # filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      filter(FLAG_MONITORAGGIO == 1)
    # MEMO: in FSC resta anche tipo 9 che viene scartato
  }
  
  # NEW: switch per dati da livelli gerarchici
  if (!is.null(perimetro_sie)) {
    perimetro_2 <- perimetro %>% 
      # scarta le righe sie di cui uso dati da livelli gerarchici
      anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
      # accoda nuove righe sie
      bind_rows(perimetro_sie)
    
    # chk
    temp <- sum(perimetro_2$COE, na.rm = TRUE) - sum(perimetro$COE, na.rm = TRUE)
    message(paste0("Questo è il delta usando i dati per livelli gerarchici di FESR, FSE e YEI: ", temp))
    
  } else {
    perimetro_sie <- setup_macroaree_sie(bimestre, operazioni)
    
    perimetro_2 <- perimetro %>% 
      # scarta le righe sie di cui uso dati da livelli gerarchici
      anti_join(perimetro_sie, by = c("COD_LOCALE_PROGETTO", "OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% 
      # accoda nuove righe sie
      bind_rows(perimetro_sie)
    
    # chk
    sum(perimetro$COE, na.rm = T) - sum(perimetro_2$COE, na.rm = T)
    # 0.08990479
  }
  
  perimetro <- perimetro_2
  
  # patch YEI
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI anche per perimetro 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  # MEMO: c'è anche su progetti sotto
  
  # ricodifica x_MACROAREA
  # programmi <- ricodifica_macroaree(programmi)
  # MEMO: ora è spostato in init_programmazione()
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  if (usa_meuro == TRUE) {
    spalla <- spalla %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  } 
  
  if (is.null(po_riclass)) {
    # po_riclass <- octk::po_riclass
    # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
    
    po_riclass <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=FALSE, use_po_psc=use_po_psc) %>%
      rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
      distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
    
  }
  
  # refactor
  spalla <- refactor_macroarea(spalla)
  perimetro <- refactor_macroarea(perimetro)
  
  # report
  out <- spalla %>%
    full_join(perimetro %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
                summarise(N = n(),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              # MEMO: patch per factor di x_AMBITO
              # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
              #                                               "FEAD", "FAMI", "CTE", "ORD")),
              #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea() %>%
    # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
    # TODO: questo non funziona
    left_join(po_riclass %>%
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  
  if (use_eu == TRUE) {
    out <- out %>%
      select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG,
             `Non avviato`,
             `In avvio di progettazione`,
             `In corso di progettazione`,
             `In affidamento`,
             `In esecuzione`,
             `Eseguito`,
             `Eseguito`)
  } else {
    out <- out %>%
      select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, RISORSE, N, COE, COE_IMP, COE_PAG,
             `Non avviato`,
             `In avvio di progettazione`,
             `In corso di progettazione`,
             `In affidamento`,
             `In esecuzione`,
             `Eseguito`,
             `Eseguito`)
  }
  
  
  # DEBUG:
  # memo_out <- out
  
  
  if (add_totali == TRUE) {
    message("Il caclolo di CP Non è implementato")
  }
  
  # aggiunge valori calcolati in termini di FTPN
  # if (add_totali == TRUE) {
  #   # MEMO: 
  #   # se uso add_totali ho valori confrontabili con totale su sito (e quindi con progetti)
  #   # se uso cp2 ho valori confrontabili con singole pagine programmi su sito (ma non con totali sito o file progetti)
  #   
  #   message("Inizio integrazione totali")
  #   
  #   if (is.null(progetti)) {
  #     progetti <- load_progetti(bimestre, light = TRUE, refactor = TRUE)
  #   }
  #   
  #   # patch YEI anche per perimetro 
  #   progetti_2 <- progetti  %>%
  #     # DEV:
  #     mutate(x_AMBITO = as.character(x_AMBITO)) %>% # MEMO: qui perdo informazioni perché ho forzato
  #     # FONDO_COMUNITARIO
  #     mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
  #                                 TRUE ~ x_AMBITO)) %>%
  #     refactor_ambito(.)
  # 
  #   progetti_2 <- progetti_2 %>%
  #     refactor_ambito(.) %>%
  #     refactor_ciclo(.) %>%
  #     # MEMO: patch per factor di x_AMBITO
  #     # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #     #                                               "FEAD", "FAMI", "CTE", "ORD")),
  #     #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #     select(COD_LOCALE_PROGETTO, 
  #            OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA,
  #            CP = OC_FINANZ_TOT_PUB_NETTO,
  #            IMP = IMPEGNI,
  #            PAG = TOT_PAGAMENTI)
  #   
  #   
  #   fix_sie <- fix_macroaree_progetti_sie(bimestre, progetti, operazioni)
  #   
  #   # patch YEI anche per fix_sie 
  #   fix_sie <- fix_sie  %>%
  #     mutate(x_AMBITO = as.character(x_AMBITO)) %>%
  #     mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
  #                                 TRUE ~ x_AMBITO)) %>%
  #     refactor_ambito(.)
  #   
  #   progetti_3 <- progetti_2 %>% 
  #     # scarta le righe sie di cui uso dati da livelli gerarchici
  #     anti_join(fix_sie, by = "COD_LOCALE_PROGETTO") %>% 
  #     # accoda nuove righe sie
  #     bind_rows(fix_sie)
  #   
  #   # chk
  #   sum(progetti_3$CP, na.rm = T) - sum(progetti_2$CP, na.rm = T)
  #   # -3.051758e-05
  #   sum(progetti_3$CP, na.rm = T) - sum(progetti$OC_FINANZ_TOT_PUB_NETTO, na.rm = T)
  #   # -3.051758e-05
  #   
  #   
  #   # semplifica altro 
  #   # CHK: codifica diversa tra operazioni e progetti che compare solo qui
  #   progetti_3 <- progetti_3 %>%
  #     mutate(x_MACROAREA = as.character(x_MACROAREA)) %>%
  #     mutate(x_MACROAREA = case_when (x_MACROAREA == "Estero" ~ "Ambito nazionale",
  #                                     x_MACROAREA == "Trasversale" ~ "Ambito nazionale",
  #                                     x_MACROAREA == "Ambito nazionale" ~ "Ambito nazionale",
  #                                     TRUE ~ x_MACROAREA)) %>%
  #     refactor_macroarea()
  #   
  #   # DEV:
  #   # progetti_3 <- progetti_3 %>% 
  #   #   filter(x_AMBITO != "FEASR")
  #   
  #   # if (usa_meuro == TRUE) {
  #   #   progetti <- progetti %>%
  #   #     mutate(CP = CP / 1000000,
  #   #            IMP = IMP / 1000000,
  #   #            PAG = PAG / 1000000)
  #   # }
  #   
  #   out_2 <- out %>%
  #     left_join(progetti_3 %>% # MEMO: join perde ":::" che recupero dopo
  #     # full_join(progetti_3 %>%
  #                 group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
  #                 summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
  #                           CP = sum(CP, na.rm = TRUE),
  #                           IMP = sum(IMP, na.rm = TRUE),
  #                           PAG = sum(PAG, na.rm = TRUE)),
  #               by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>% # MEMO: qui perdo multi ":::" in progetti
  #     as_tibble(.) %>%
  #     # riempie NA con 0
  #     mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #     refactor_ambito(.) %>%
  #     refactor_ciclo(.)
  #   
  #   # sum(out_2$CP, na.rm = T) - sum(progetti_3$CP, na.rm = T)
  #   # -6.309.154.899
  #   
  #   # chk <- progetti_3 %>%
  #   #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
  #   #   summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
  #   #             CP = sum(CP, na.rm = TRUE),
  #   #             IMP = sum(IMP, na.rm = TRUE),
  #   #             PAG = sum(PAG, na.rm = TRUE)) %>%
  #   #  anti_join(out,
  #   #             by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>% 
  #   #   filter(!(grepl(":::", OC_CODICE_PROGRAMMA)))
  #   # MEMO: 
  #   # da questo join restano fuori anche programmi SIE con localizzazione su ambito nazionale ma livelli gerarchici con macroaree
  #   # risolto con fix sie sopra
  # 
  #   # recupero multi
  #   appo <- progetti_3 %>%
  #     group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
  #     summarise(N_CLP = n(), # MEMO: questa serve per contare il numero netto di progetti
  #               CP = sum(CP, na.rm = TRUE),
  #               IMP = sum(IMP, na.rm = TRUE),
  #               PAG = sum(PAG, na.rm = TRUE)) %>%
  #     anti_join(out_2,
  #               by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
  #     # riempie NA con 0
  #     mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #     # recupera x_vars
  #     left_join(po_riclass %>%
  #                 refactor_ambito(.) %>%
  #                 refactor_ciclo(.) %>%
  #                 # MEMO: patch per factor di x_AMBITO e x_CICLO
  #                 # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #                 #                                               "FEAD", "FAMI", "CTE", "ORD")),
  #                 #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #                 select(OC_CODICE_PROGRAMMA, x_CICLO, x_GRUPPO, x_PROGRAMMA),
  #               by = c("OC_CODICE_PROGRAMMA", "x_CICLO"))
  #   
  #   
  #   
  #   # DEV: li lascio vuoti perché sto usando po_riclass da dbcoe che non contiene multi (e non posso usare la versione in octk perché contiene nomi diversi?)
  #   # patch per x_PROGRAMMA e x_GRUPPO assenti in DB ma presenti in po_riclass
  #   # left_join(po_riclass %>%
  #   #             refactor_ambito(.) %>%
  #   #             refactor_ciclo(.) %>%
  #   #             select(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
  #   #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
  #   #   as_tibble(.) %>%
  #   #   mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
  #   #          x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
  #   #   select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
  #   #   refactor_ambito(.) %>%
  #   #   refactor_ciclo(.)
  #     
  #   
  #   # bind di multi
  #   out_3 <- out_2 %>%
  #     bind_rows(appo) %>%
  #     as_tibble(.) %>%
  #     # riempie NA con 0
  #     mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  #     refactor_ambito(.) %>%
  #     refactor_ciclo(.)
  #   
  #   # sum(out_3$CP, na.rm = T) - sum(progetti_3$CP, na.rm = T)
  #   # 0
  #   
  #   # if (usa_meuro == TRUE) {
  #   #   out <- out %>%
  #   #     mutate(CP = CP / 1000000,
  #   #            IMP = IMP / 1000000,
  #   #            PAG = PAG / 1000000)
  #   # }
  #   # MEMO: lo faccio dopo perché entrano in somme di cp2
  #   
  #   # verifica totali finanziari con progetti
  #   # test <- progetti %>%
  #   #   filter(OC_FLAG_VISUALIZZAZIONE == 0)
  #   # sum(out_3$CP, na.rm = T) - sum(test$OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)
  #   # -3.051758e-05
  #   
  #   # DEBUG:
  #   # memo_out_3 <- out_3
  #   
  #   # versione con cp2
  #   if (use_cp2 == TRUE) {
  #     message("Inizio calcolo cp2")
  #     
  #     # chk <- po_riclass %>%
  #     #   distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA, x_GRUPPO) %>%
  #     #   count(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
  #     #   filter(n>1)
  #     # 
  #     # chk2 <- po_riclass %>% 
  #     #   semi_join(chk)
  #     
  #     appo <- out_3 %>%
  #       filter(grepl(":::", OC_CODICE_PROGRAMMA)) %>%
  #       as_tibble(.) %>%
  #       select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO_FSE_FESR = x_AMBITO, x_MACROAREA, N_CLP, CP, IMP, PAG) %>%
  #       # CHK: forse N_CLP non va sommato...
  #       mutate(x_AMBITO_FSE_FESR = as.character(x_AMBITO_FSE_FESR)) %>%
  #       separate_rows(OC_CODICE_PROGRAMMA, sep = ":::") %>%
  #       rename(x_AMBITO = x_AMBITO_FSE_FESR) %>% # MEMO: contiene x_ambito sbagliato
  #       # recupera x_vars (programmi plurifondo)
  #       left_join(po_riclass %>%
  #                   distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_PROGRAMMA, x_GRUPPO),
  #                 # by = "OC_CODICE_PROGRAMMA") %>% #TODO: qui genera duplicazioni
  #                 by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>% # TODO: qui però non risolve NA su x_vars, e x_ambito può essere anomalo
  #       refactor_ambito(.) %>%
  #       refactor_ciclo(.) %>%
  #       # left_join(programmi %>%
  #       #             refactor_macroarea(.) %>%
  #       #             # as_tibble(.) %>%
  #       #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA),
  #       #           by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
  #       # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
  #       #                                               "FEAD", "FAMI", "CTE", "ORD")),
  #       #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
  #       # ripristina struttura per bindrows
  #       mutate(RISORSE = 0,
  #              N = 0,
  #              COE = 0,
  #              COE_IMP = 0,
  #              COE_PAG = 0,
  #              `Non avviato` = 0,
  #              `In avvio di progettazione` = 0,
  #              `In corso di progettazione` = 0,
  #              `In affidamento` = 0,
  #              `In esecuzione` = 0,
  #              `Eseguito` = 0)
  #     # %>%
  #     #   select(names(out)) # MEMO: qui perdeva CP
  #     
  #     # DEV:
  #     # 
  #     # temp %>% 
  #     #   count(OC_CODICE_PROGRAMMA) %>% filter(n>1) %>% 
  #     #   anti_join(memo_temp %>% 
  #     #               count(OC_CODICE_PROGRAMMA) %>% filter(n>1))
  #     # 
  #     # 
  #     # appo %>% 
  #     #   count(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>% filter(n>1) %>% 
  #     #   anti_join(memo_appo %>% 
  #     #               count(OC_CODICE_PROGRAMMA) %>% filter(n>1))
  #     # 
  #     
  #     # TODO: qui va aggiustata la parte PAC di direttrici ferroviarie e di giustizia digitale perché CP resta vuoto (o forse dentro report programmi)
  #     
  #     sum(appo$CP, na.rm = T)
  #     
  #     programmi_2 <- out_3 %>%
  #       filter(!grepl(":::", OC_CODICE_PROGRAMMA)) %>%
  #       bind_rows(appo)
  #     dim(out_3)[1] + dim(appo)[1]/2 - dim(programmi_2)[1]
  #     # 0
  # 
  #     # semplifica altro 
  #     # CHK: codifica diversa tra operazioni e progetti che compare solo qui
  #     # programmi_2 <- programmi_2 %>%
  #     #   mutate(x_MACROAREA = as.character(x_MACROAREA)) %>%
  #     #   mutate(x_MACROAREA = case_when (x_MACROAREA == "Estero" ~ "Ambito nazionale",
  #     #                                   x_MACROAREA == "Trasversale" ~ "Ambito nazionale",
  #     #                                   x_MACROAREA == "Ambito nazionale" ~ "Ambito nazionale",
  #     #                                   TRUE ~ x_MACROAREA)) %>%
  #     #   refactor_macroarea()
  #     
  #     # sovrascrive out
  #     out_3 <- programmi_2 %>%
  #       group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA) %>%
  #       summarise_if(is.numeric, sum, na.rm = TRUE)
  #     
  #   }
  #   if (usa_meuro == TRUE) {
  #     out <- out_3 %>%
  #       mutate(CP = round(CP / 1000000, 1),
  #              IMP = round(IMP / 1000000, 1),
  #              PAG = round(PAG / 1000000, 1))
  #   }
  #   # MEMO: vale sia per cp2 che per add_totali semplice
  # }
  
  # DEBUG:
  # sum(out$COE, na.rm = TRUE) - sum(memo_out$COE, na.rm = TRUE)
  # sum(out$COE, na.rm = TRUE) - sum(memo_out_3$COE, na.rm = TRUE)
  # sum(out$CP, na.rm = TRUE) - sum(memo_out_3$CP, na.rm = TRUE)/1000000
  # 6497.946
  # sum(out$CP, na.rm = TRUE) - sum(memo_out_3$CP, na.rm = TRUE)/1000000 -(sum(appo$CP, na.rm = TRUE)/1000000)/2 
  # 0.09144618 -> CGHK: dovrebbe essere 0
  
  # ricodifica
  out <- out %>% 
    mutate(x_MACROAREA = as.character(x_MACROAREA)) %>% 
    mutate(x_MACROAREA = case_when(x_MACROAREA == "Ambito nazionale" ~ "Non classificato",
                                   TRUE ~ x_MACROAREA))
  
  
  if (cut_no_risorse == TRUE) {
    # out <- out %>%
    #   filter(RISORSE > 0)
    messagge("Con cut_no_risorse=TRUE si escudono casi con RISORSE 0 ma progetti monitorati su ambito nazionale, Sei sicuro?")
    
    appo <- init_programmazione_dati(use_temi=FALSE, use_713=use_713, use_flt=use_flt, use_ciclo=TRUE, tipo_ciclo=tipo_ciclo, use_location=TRUE) %>%
      rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
             x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
    
    if (use_flt == TRUE) {
      appo <- appo %>%
        filter(FLAG_MONITORAGGIO == 1 | FLAG_MONITORAGGIO == 2)
      # MEMO: in FSC resta anche tipo 9 che viene scartato
    }
    
    out <- out %>% 
      semi_join(appo, by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
    
  }
  
  if (export == TRUE) {
    if (use_cp2 == TRUE) {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree_cp2.csv")), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, paste0(focus, "_programmi_macroaree.csv")), row.names = FALSE)
    }
  }
  
  
  if (export_xls == TRUE) {
    if (use_cp2 == TRUE) {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_macroaree_cp2.xlsx")), rowNames = FALSE)
    } else {
      write.xlsx(out, file.path(OUTPUT, paste0(focus, "_programmi_macroaree.xlsx")), rowNames = FALSE)
    }
  }
  
  return(out)
}

#' Esporta report per Programmi con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx per ciclo e ambito in OUTPUT?
#' @param progetti dataset di tipo "progetti" che serve per integrare CP, impegni e pagamenti totali.
#' @return Un file csv con apertura per programma, con RISORSE, COE, COE_IMPe, COE_PAG e con COE per fase procedurale.
make_report_programmi_coesione_dataiku <- function(perimetro, usa_meuro=FALSE, show_cp=FALSE, use_eu=FALSE, use_flt=TRUE, 
                                                   use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                   export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  po <- octk::po_riclass
  
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)
  message("programmi caricato")
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)%>%
      mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
  }
  
  # patch YEI su programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # patch YEI su attuazione
  perimetro <- perimetro %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # programmazione
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  
  # attuazione
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    # mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  # chk programmi con attuazione e risorse 0
  chk <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`) %>%
    filter(RISORSE == 0 | is.na(RISORSE)) 
  write.xlsx(chk, file.path(TEMP, "chk_programmi_risorse_0.xlsx"))
  
  if (dim(chk)[1]>0){
    message("WARNING: sono presenti righe con risore 0 e attuazione valorizzata!")
  }
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP, IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`) %>%
    filter(RISORSE > 0)
  
  
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    out <- out %>% 
      select(-RISORSE_UE)
  } 
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_cp2.xlsx"))
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi.xlsx"))
    }
  }
  
  return(out)
}




#' Esporta report per Programmi e Macroaree con dati coesione
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato.
#' E' costruito su operaizoni e dati coesione.
#'
#' @param perimetro Dataset di classe macroaree
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param show_cp Logico. Vuoi calcolare anche il costo pubblico (CP)?
#' @param use_eu Vuoi caricare il dataset SIE del DB con le risorse UE e la categoria di regione? (solo per SIE)
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni? 
#' @param stime_fix_siepoc Per correggere i dati SIE e POC 1420 con le anticipazioni vuoi usare anche le stime? 
#' @param export vuoi salvare il file?
#' @param export_xls Vuoi salvare i file xlsx in OUTPUT?
#' @param progetti dataset di tipo "progetti" da utilizzare per show_cp == TRUE
#' @return Un file csv con apertura per programma e fase procedurale.
#' @note Nel report restano per definizione righe con risorse 0, che derivano da errate imputazioni di macroaree.
make_report_programmi_macroaree_coesione_dataiku <- function(perimetro, usa_meuro=FALSE, use_eu=FALSE,
                                                             use_flt=FALSE, use_cicli_psc=FALSE,
                                                             use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE,
                                                             show_cp=FALSE, export=FALSE, export_xls=FALSE, progetti=NULL, DB) {
  
  # DEBUG: 
  # perimetro <- macroaree
  # use_flt <- TRUE
  # use_cicli_psc <- TRUE
  # use_fix_siepoc <- TRUE
  # stime_fix_siepoc <- TRUE
  
  # OLD:
  # po <- octk::po_riclass
  # MEMO: questa soluzione porta a deniminazioni divergenti per lo stesso codice po
  
  # NEW:
  po <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))%>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA) %>%
    distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO)
  
  # programmazione
  programmi <- init_programmazione_dati(DB=DB, use_cicli_psc=use_cicli_psc, use_fix_siepoc=use_fix_siepoc, stime_fix_siepoc=stime_fix_siepoc) %>%
    rename(x_GRUPPO = TIPOLOGIA_PROGRAMMA,
           x_PROGRAMMA = DESCRIZIONE_PROGRAMMA)%>%
    mutate(OC_CODICE_PROGRAMMA = ifelse(OC_CODICE_PROGRAMMA == "ACCOESBASILICATA", "ACCOESBASILICAT", OC_CODICE_PROGRAMMA))
  
  if (use_flt == TRUE) {
    programmi <- programmi %>%
      filter(FLAG_MONITORAGGIO == 1)
    
    perimetro <- perimetro %>% 
      filter(OC_FLAG_VISUALIZZAZIONE %in% c(0, 10))
    
  }
  
  # patch YEI programmazione
  programmi <- programmi %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # refactor macroarea
  programmi <- refactor_macroarea(programmi)
  perimetro <- refactor_macroarea(perimetro)
  
  # patch YEI attuazione 
  perimetro <- perimetro  %>%
    mutate(x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_AMBITO = case_when(OC_CODICE_PROGRAMMA == "2014IT05M9OP001" ~ "YEI", # sovrascrive FSE
                                TRUE ~ x_AMBITO)) %>%
    refactor_ambito(.)
  
  # crea spalla
  spalla <- programmi %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, x_MACROAREA) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE, na.rm = TRUE),
              RISORSE_UE = sum(FINANZ_UE, na.rm = TRUE))
  
  # integra totali
  appo0 <- perimetro %>% 
    group_by(COD_LOCALE_PROGETTO, OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    left_join(progetti %>% 
                select(COD_LOCALE_PROGETTO, CP=OC_FINANZ_TOT_PUB_NETTO, IMP=IMPEGNI, PAG=TOT_PAGAMENTI),
              by = "COD_LOCALE_PROGETTO")
  # DEV: qui raddoppia per ogni progetto su più macroaree
  
  appo <- appo0 %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise(N = n(),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE),
              CP = sum(CP, na.rm = TRUE),
              IMP = sum(IMP, na.rm = TRUE),
              PAG = sum(PAG, na.rm = TRUE))
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                left_join(perimetro %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_MACROAREA, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
    as_tibble(.) %>%
    # riempie NA con 0
    mutate_if(is.numeric, replace_na, replace=0) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea(.)%>%
    left_join(spalla %>% 
                ungroup() %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: senza x_CICLO funziona anche per PSC pluriciclo
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>% 
    # ripristina denominazioni mancanti
    left_join(po %>% 
                refactor_ambito(.) %>%
                refactor_ciclo(.) %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA_2 = x_PROGRAMMA, x_AMBITO, x_GRUPPO_2 = x_GRUPPO),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>% # MEMO: priorità a DBCOE sopra, ma restano casi fuori da gestire con po_riclass
    as_tibble(.) %>%
    mutate(x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA_2, x_PROGRAMMA),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO_2, x_GRUPPO)) %>%
    select(-x_PROGRAMMA_2, -x_GRUPPO_2) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.)
  
  
  out <- out %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_MACROAREA, 
           RISORSE, RISORSE_UE, N, COE, COE_IMP, COE_PAG, CP,IMP, PAG,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`)
  
  if (usa_meuro == TRUE) {
    out <- out %>%
      mutate(RISORSE = round(RISORSE / 1000000, 1),
             RISORSE_UE = round(RISORSE_UE / 1000000, 1),
             COE = round(COE / 1000000, 1),
             COE_IMP = round(COE_IMP / 1000000, 1),
             COE_PAG = round(COE_PAG / 1000000, 1),
             CP = round(CP/1000000, 1),
             IMP = round(IMP/1000000, 1),
             PAG = round(PAG/1000000, 1),
             `Non avviato` = round(`Non avviato` / 1000000, 1),
             `In avvio di progettazione` = round(`In avvio di progettazione` / 1000000, 1),
             `In corso di progettazione` = round(`In corso di progettazione` / 1000000, 1),
             `In affidamento` = round(`In affidamento` / 1000000, 1),
             `In esecuzione` = round(`In esecuzione` / 1000000, 1),
             `Eseguito` = round(`Eseguito` / 1000000, 1))
  }
  
  if (use_eu == FALSE) {
    spalla <- spalla %>% 
      select(-RISORSE_UE)
  }
  
  if (show_cp == FALSE) {
    out <- out %>% 
      select(-CP, -IMP, -PAG)
  } 
  
  if (export == TRUE) {
    if (show_cp == TRUE) {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree_cp2.csv"), row.names = FALSE)
    } else {
      write.csv2(out, file.path(TEMP, "report_programmi_macroaree.csv"), row.names = FALSE)
    }
  }
  
  if (export_xls == TRUE) {
    if (show_cp == TRUE) {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree_cp2.xlsx"), rowNames = FALSE)
    } else {
      write.xlsx(out, file.path(OUTPUT, "report_programmi_macroaree.xlsx"), rowNames = FALSE)
    }
  }
  
  return(out)
}

#' Crea report bimestre in modalita' "coesione"
#'
#' Crea report sintetico bimestrale con risorse coesione calcolate su operazioni.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono come in 'programmi', vale solo se 'programmi' è in euro
#' @export vuoi salvare il file?
#' @return Il report bimestre.
make_report_bimestre_coesione_dataiku <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEV:
  # if (is.null(programmi)) {
  # temp <- paste0("report_meuro", "_programmi.csv")
  #   programmi <- read_csv2(file.path(TEMP, temp), row.names = FALSE)
  # }
  # MEMO: serve bimestre
  
  # fix
  programmi <- programmi %>%
    mutate(N_CLP = N) #,
  # IMP = 0,
  # PAG = 0)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                RISORSE_UE = sum(RISORSE_UE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
    
  } else {
    report <- programmi  %>%
      # MEMO: patch per factor di x_AMBITO e x_CICLO
      # mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
      #                                               "FEAD", "FAMI", "CTE")),
      #        x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
      group_by(x_CICLO, x_AMBITO) %>%
      summarise(N =  sum(N, na.rm = TRUE),
                RISORSE = sum(RISORSE, na.rm = TRUE),
                COE = sum(COE, na.rm = TRUE),
                COE_IMP = sum(COE_IMP, na.rm = TRUE),
                COE_PAG = sum(COE_PAG, na.rm = TRUE),
                N_CLP = sum(N_CLP, na.rm = TRUE),
                CP = sum(CP, na.rm = TRUE),
                IMP = sum(IMP, na.rm = TRUE),
                PAG = sum(PAG)) %>%
      arrange(x_CICLO, x_AMBITO)
    
    if (usa_meuro == TRUE) {
      report <- report%>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1),
               CP = round(CP/1000000, 1),
               IMP = round(IMP/1000000, 1),
               PAG = round(PAG/1000000, 1))
    }
    
    # arrange per template
    report <- report %>% 
      filter(x_AMBITO != "FEAMP", x_AMBITO != "FEASR") %>% 
      select(x_CICLO,	x_AMBITO,	RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP) %>% 
      arrange(desc(x_CICLO), x_AMBITO)
  }
  
  
  
  if (export == TRUE) {
    write_csv2(report, file.path(TEMP, "report.csv"))
  }
  
  return(report)
}


#' Esporta report per ciclo, ambito e macroarea con dati coesione
#'
#' Report con apertura ciclo, ambito e macroarea.
#' E' costruito su operazioni e dati coesione.
#'
#' @param programmi Dataset in formato "programmi" da make_report_programmi_macroaree_coesione()
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro. Attenzione: per usare Meuro il perimetro deve essere in euro, viene arrotondato dopo
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per ciclo, ambto e macroarea.
make_report_macroaree_coesione_dataiku <- function(programmi, usa_meuro=TRUE, export=TRUE) {
  
  # DEBUG:
  # programmi <- programmi_macroaree
  
  report <- programmi  %>%
    group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
    arrange(x_CICLO, x_AMBITO, x_MACROAREA)
  
  if ("RISORSE_UE" %in% names(programmi)) {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               RISORSE_UE = round(RISORSE_UE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, RISORSE_UE, COE, COE_IMP, COE_PAG, N) 
      
    }
  } else {
    if (usa_meuro == TRUE) {
      report <- report %>%
        mutate(RISORSE = round(RISORSE/1000000, 1),
               COE = round(COE/1000000, 1),
               COE_IMP = round(COE_IMP/1000000, 1),
               COE_PAG = round(COE_PAG/1000000, 1))
    }
    
    if ("N_CLP" %in% names(programmi)) {
      if (usa_meuro == TRUE) {
        report <- report %>%
          mutate(CP = round(CP/1000000, 1),
                 IMP = round(IMP/1000000, 1),
                 PAG = round(PAG/1000000, 1))
      }
      
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N, CP, IMP, PAG, N_CLP)
      
    } else {
      # arrange per template
      report <- report %>% 
        select(x_CICLO,	x_AMBITO,	x_MACROAREA, RISORSE, COE, COE_IMP, COE_PAG, N) 
      
    }
  }
  
  
  
  
  out <- report %>% 
    filter(# x_AMBITO != "FEAMP", 
      x_AMBITO != "FEASR") %>% 
    arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA)
  
  if (export == TRUE) {
    write_csv2(out, file.path(TEMP, "report_macroaree.csv"))
  }
  
  return(out)
}



#' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_programmi_coesione_dataiku <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                      use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO", "x_GRUPPO", "x_PROGRAMMA"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}


' Verifica variazione variabili coesione per programma
#'
#' Verifica variazione variabili coesione per programma. Confronta RISORSE per due versioni del DBCOE e COE, COE_IMP e COE_PAG pert due bimestri.
#'
#' @param dati_new Versione attuale dei dati. Di default è quella configurata in oc_init(), coincide con "bimestre".
#' @param dbcoe_new Versione attuale del DBCOE. Di default è quella configurata in oc_init().
#' @param dati_old Versione precedente dei dati (espressa come bimestre).
#' @param dbcoe_old Versione precedente del DBCOE.
#' @param use_cicli_psc Vuoi usare i dati di programmazione per cicli dei PSC?
#' @param use_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le anticipazioni di decisioni in base alle delibere sui POC? 
#' @param stime_fix_siepoc Vuoi correggere i dati SIE e POC 1420 con le stime di chiusura dei programmi? 
#' @param use_flt Logico. Vuoi utilizzare solo i programmi che rientrano nel perimetro coesione monitorabile?
#' @param export vuoi salvare il file?
#' @return Un dataframe per programma, ciclo e ambito.
chk_variazione_sintesi_coesione_dataiku <- function(dati_new, dbcoe_new, dati_old, dbcoe_old, 
                                                    use_cicli_psc=FALSE, use_fix_siepoc=FALSE, stime_fix_siepoc=FALSE, use_flt=TRUE, export=FALSE){
  
  # DEBUG:
  # dati_new = "20250228"
  # dbcoe_new="20250228.00"
  # dati_old = "20241231"
  # dbcoe_old="20241231.02"
  # use_cicli_psc=TRUE
  # use_fix_siepoc=TRUE
  # stime_fix_siepoc=TRUE
  # export=TRUE
  
  # print(DB)
  
  progetti <- tibble(COD_LOCALE_PROGETTO = "XXXX",
                     OC_FINANZ_TOT_PUB_NETTO  = 0, 
                     IMPEGNI  = 0, 
                     TOT_PAGAMENTI = 0)
  
  DATA1 <- file.path(dirname(DATA), dati_new)
  macroaree1 <- load_macroaree(bimestre=dati_new, visualizzati=TRUE, DATA=DATA1)
  DB1 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_new)
  programmi_new <- make_report_programmi_coesione_dataiku(macroaree1, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB1)
  
  DATA2 <- file.path(dirname(DATA), dati_old)
  macroaree2 <- load_macroaree(bimestre=dati_old, visualizzati=TRUE, DATA=DATA2)
  DB2 <- file.path(DRIVE, "PROGRAMMAZIONE", dbcoe_old)
  programmi_old <- make_report_programmi_coesione_dataiku(macroaree2, usa_meuro=TRUE, use_eu=FALSE, use_flt=use_flt, show_cp=FALSE, 
                                                          use_cicli_psc=TRUE, use_fix_siepoc=TRUE, stime_fix_siepoc = TRUE,
                                                          export=FALSE, export_xls=FALSE, progetti=progetti, DB=DB2)
  
  out <- programmi_new %>%
    as_tibble(.) %>%
    ungroup(.) %>% 
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
              COE = sum(COE, na.rm = TRUE),
              COE_IMP = sum(COE_IMP, na.rm = TRUE),
              COE_PAG = sum(COE_PAG, na.rm = TRUE)) %>% 
    full_join(programmi_old %>%
                ungroup(.) %>% 
                group_by(x_CICLO, x_AMBITO)%>%
                summarise(RISORSE = sum(RISORSE, na.rm=TRUE),
                          COE = sum(COE, na.rm = TRUE),
                          COE_IMP = sum(COE_IMP, na.rm = TRUE),
                          COE_PAG = sum(COE_PAG, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO"),
              suffix = c(".new", ".old")) %>%
    mutate_if(is.numeric, replace_na, replace=0) %>% 
    mutate(CHK_RISORSE = RISORSE.new - RISORSE.old,
           CHK_COE = COE.new - COE.old,
           CHK_COE_IMP = COE_IMP.new - COE_IMP.old,
           CHK_COE_PAG = COE_PAG.new - COE_PAG.old)
  
  if (export==TRUE) {
    write.xlsx(out, file.path(TEMP, paste0("chk_delta_coesione_sintesi_", dbcoe_new, "_", dbcoe_old, ".xlsx")))
  }
  
  return(out)
  
}

#' Varifica totale di RISORSE
#'
#' Report con apertura ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
#'
#' @param report Dataframe da make_report_bimestre_coesione()
#' @param programmi Dataframe da make_report_programmi_coesione()
#' @param report_macroaree Dataframe da make_report_macroaree_coesione()
#' @param risorse Dataframe da make_report_risorse().E' opzionale
#' @return Dataframe per ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
chk_allineamento_risorse <- function(report, programmi, report_macroaree, risorse=NULL) {
  
  if (is.null(risorse)) {
    risorse <- make_report_risorse(use_meuro=TRUE, use_flt=TRUE, use_eu=FALSE, force_yei=TRUE, tipo_ciclo="CICLO_STRATEGIA", export=FALSE)
    message("Attenzione: 'risorse' è in meuro e potrebbe essere in valuta diversa dagli altri report confrontati")
  }
  
  out <- report %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(RISORSE = sum(RISORSE, na.rm = TRUE)) %>%
    full_join(programmi %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(report_macroaree %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(risorse %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(RISORSE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) 
  
  names(out) <- c("x_CICLO", "x_AMBITO", "report", "programmi", "report_macroaree", "risorse")
  
  return(out)
  
}


#' Varifica totale di COE
#'
#' Report con apertura ciclo e ambito con confronto del totale di COE in diversi report di octk.
#'
#' @param report Dataframe da make_report_bimestre_coesione()
#' @param programmi Dataframe da make_report_programmi_coesione()
#' @param report_macroaree Dataframe da make_report_macroaree_coesione()
#' @param perimetro Dataframe da prep_perimetro_bimestre_coesione().E' opzionale
#' @return Dataframe per ciclo e ambito con confronto del totale di RISORSE in diversi report di octk.
chk_allineamento_costo_coe <- function(report, programmi, report_macroaree, perimetro=NULL) {
  
  if (is.null(perimetro)) {
    perimetro <- prep_perimetro_bimestre_coesione(bimestre, usa_meuro=TRUE) 
    message("Attenzione: 'perimetro' è in meuro e potrebbe essere in valuta diversa dagli altri report confrontati")
  }
  
  out <- report %>%
    group_by(x_CICLO, x_AMBITO) %>%
    summarise(COE = sum(COE, na.rm = TRUE)) %>%
    full_join(programmi %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(report_macroaree %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) %>%
    full_join(perimetro %>%
                group_by(x_CICLO, x_AMBITO) %>%
                summarise(COE = sum(COE, na.rm = TRUE)),
              by = c("x_CICLO", "x_AMBITO")) 
  
  names(out) <- c("x_CICLO", "x_AMBITO", "report", "programmi", "report_macroaree", "perimetro")
  
  return(out)
  
}

# REPORT ANTICHI----


#' Prepara perimetor progetti
#'
#' Prepara perimetor progetti
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
prep_perimetro_bimestre <- function(bimestre, usa_meuro=TRUE) {
  
  # TODO: inserire switch per ciclo con calcolo di CP/COE direttamente su operazioni per 1420 (ora si può fare solo per impegni e pagamenti)
  
  # loads
  progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  
  # switch variabile COE
  # MEMO: COSTO_RENDICONTABILE_UE e OC_FINANZ_STATO_FSC_NETTO contengono NA quindi converto numerici in 0
  perimetro <- progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  if (usa_meuro == TRUE) {
    perimetro <- perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    perimetro <- perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }
  
  # simply
  perimetro <- get_simply_non_loc(perimetro)
  
  return(perimetro)
}

make_report_bimestre <- function(bimestre, perimetro, last_bimestre, last_data_path,
                                 usa_macroaree=FALSE, usa_meuro=TRUE, export=FALSE) {
  
  # last_bimestre <- "20181231"
  # last_data_path = "/Users/aa/dati/oc/20181231"
  
  # progetti
  if (missing(perimetro)) {
    perimetro <- prep_perimetro_bimestre(bimestre = bimestre, usa_meuro=usa_meuro)
  }
  
  # risorse
  risorse <- make_risorse(usa_713=TRUE, usa_macroaree=usa_macroaree) %>%
    mutate(RIS = round(RIS / 1000000, 1))
  
  
  # ----------------- #
  # report base
  
  # prep
  if (usa_macroaree == FALSE) {
    report <- risorse %>%
      full_join(perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO) %>%
                  summarise(N = n(),
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE),
                            COE = sum(COE, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO")) %>%
      arrange(desc(x_CICLO), x_AMBITO) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
  } else {
    report <- risorse %>%
      full_join(perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
                  summarise(N = n(),
                            CP = sum(CP, na.rm = TRUE),
                            IMP = sum(IMP, na.rm = TRUE),
                            PAG = sum(PAG, na.rm = TRUE),
                            COE = sum(COE, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
      arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  }
  
  # ----------------- #
  # bimestre precedente
  
  # setup
  # rm(progetti)
  # last_bimestre <- "20181231"
  # last_data_path = "/Users/aa/dati/oc/20181231"
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }
  
  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  
  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  last_perimetro <- last_perimetro %>%
    mutate(CP = CP / 1000000,
           COE = COE / 1000000,
           IMP = IMPEGNI / 1000000,
           PAG = TOT_PAGAMENTI / 1000000)
  
  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)
  
  
  # ----------------- #
  # report def
  
  if (usa_macroaree == FALSE) {
    report_last <- report %>%
      full_join(last_perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO) %>%
                  summarise(COE_LAST = sum(COE, na.rm = TRUE),
                            PAG_LAST = sum(PAG, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO")) %>%
      arrange(desc(x_CICLO), x_AMBITO) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      # mutate(COE_DELTA = (COE - COE_LAST) / COE,
      #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
      mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
             PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
      mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
             PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
      mutate(COE_DELTA = if_else(COE_DELTA > 1, 1, COE_DELTA),
             PAG_DELTA = if_else(PAG_DELTA > 1, 1, PAG_DELTA)) %>%
      select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)
    
  } else {
    report_last <- report %>%
      full_join(last_perimetro %>%
                  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
                  group_by(x_CICLO, x_AMBITO, x_MACROAREA) %>%
                  summarise(COE_LAST = sum(COE, na.rm = TRUE),
                            PAG_LAST = sum(PAG, na.rm = TRUE)),
                by = c("x_CICLO", "x_AMBITO", "x_MACROAREA")) %>%
      arrange(desc(x_CICLO), x_AMBITO, x_MACROAREA) %>%
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
      mutate(COE_DELTA = (COE - COE_LAST) / COE,
             PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
      mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
             PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
      mutate(COE_DELTA = if_else(COE_DELTA > 1, 1, COE_DELTA),
             PAG_DELTA = if_else(PAG_DELTA > 1, 1, PAG_DELTA)) %>%
      select(x_CICLO,	x_AMBITO,	x_MACROAREA, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)
  }
  
  if (export == TRUE) {
    if (usa_macroaree == FALSE) {
      write.csv2(report_last, file.path(TEMP, "report.csv"), row.names = FALSE)
    } else {
      write.csv2(report_last, file.path(TEMP, "report_macoaree.csv"), row.names = FALSE)
    }
  }
  
  return(report_last)
}






#' Esporta report per Programmi
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param add_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi <- function(perimetro, usa_meuro=FALSE, add_713=FALSE, focus="elab", export=FALSE) {
  
  # TODO:
  # correggere "FESR-FSE"
  # inserire verifica bimestre precedente
  
  # appo <- perimetro %>%
  #   mutate(x_PROGRAMMA = factor(x_PROGRAMMA, levels = c("PATTO EMILIA-ROMAGNA", "PATTO LOMBARDIA", "PATTO LAZIO",
  #                                                       "PATTO BOLOGNA", "PATTO FIRENZE", "PATTO GENOVA", "PATTO MILANO", "PATTO VENEZIA",
  #                                                       "PATTO ABRUZZO", "PATTO BASILICATA", "PATTO CALABRIA",  "PATTO CAMPANIA",
  #                                                       "PATTO MOLISE", "PATTO PUGLIA", "PATTO SARDEGNA",  "PATTO SICILIA",
  #                                                       "PATTO BARI", "PATTO CAGLIARI", "PATTO CATANIA",  "PATTO MESSINA",
  #                                                       "PATTO NAPOLI", "PATTO PALERMO", "PATTO REGGIO CALABRIA")))
  
  appo <- perimetro
  
  temp <- paste0(focus, "_programmi.csv")
  
  programmi <- init_programmazione(usa_temi=FALSE, add_713=add_713, export=FALSE)
  
  if (usa_meuro == TRUE) {
    programmi<- programmi %>%
      mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  }
  
  # spalla con risorse (per tutti i programmi)
  # spalla <- programmi %>%
  #   group_by(OC_CODICE_PROGRAMMA) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, RISORSE)
  
  # OLD:
  # spalla <- octk::po_riclass %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR" #,
  #          # x_AMBITO != "FEAMP"
  #          ) %>%
  #   left_join(programmi %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))
  
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2014-2020")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2014-2020")
  programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")
  
  spalla <- programmi %>%
    mutate(x_GRUPPO = case_when(
      x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "POCN",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "POCN",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "POCR",
      x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "POCR",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Piani nazionali" ~ "PIANI",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Patti per lo sviluppo" ~ "PATTI",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altre assegnazioni CIPE" ~ "ALTRI CIPE",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altro" ~ "ALTRO",
      x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Cofinanziamento SIE" ~ "ALTRO",
      x_CICLO == "2014-2020" & x_AMBITO == "FEAMP" ~ "PON",
      x_CICLO == "2014-2020" & x_AMBITO == "YEI" ~ "YEI",
      x_CICLO == "2014-2020" & x_AMBITO == "SNAI" ~ "SNAI",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POIN" ~ "POIN",
      x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
      x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
      x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Nazionale" ~ "PACN",
      x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Regionale" ~ "PACR",
      # MEMO: per il momento il blocco FSC resta come si trova (e dovrebbe essere coerente con po_riclass)
      TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>%
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, FINANZ_TOTALE_PUBBLICO) %>%
    filter(x_CICLO != "2000-2006",
           x_AMBITO != "CTE",
           x_AMBITO != "FEASR" #,
           # x_AMBITO != "FEAMP"
    ) %>%
    group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
    summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA")
  
  
  # report
  out <- spalla %>%
    # TODO: inserire filtro sopra!!! Non posso usare solo quello con right_join su attuazione
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(OC_CODICE_PROGRAMMA, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  out <- out %>%
    left_join(octk::po_riclass %>%
                distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
              by = "OC_CODICE_PROGRAMMA", suffix = c("", ".x")) %>%
    as_tibble(.) %>%
    mutate(x_CICLO = as.character(x_CICLO),
           x_AMBITO = as.character(x_AMBITO)) %>%
    mutate(x_CICLO = if_else(is.na(x_CICLO), x_CICLO.x, x_CICLO),
           x_AMBITO = if_else(is.na(x_AMBITO), x_AMBITO.x, x_AMBITO),
           x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO.x, x_GRUPPO),
           x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA.x, x_PROGRAMMA)) %>%
    select(-x_CICLO.x, -x_AMBITO.x, -x_GRUPPO.x, -x_PROGRAMMA.x) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, CP, IMP, PAG, COE,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}



#' Integra report per Programmi con delta da un bimestre precedente
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
add_delta_programmi <- function(bimestre, programmi, last_bimestre, last_data_path,
                                usa_meuro=FALSE, focus="delta", export=FALSE) {
  
  # if (missing(programmi)) {
  #
  #   programmi <- make_report_programmi(perimetro, focus="chk", usa_meuro=usa_meuro, export=FALSE)
  # }
  # DEV: eliminato perché non è definito "perimetro"
  
  # ----------------- #
  # bimestre precedente
  
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }
  
  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  
  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }
  
  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)
  
  appo <- last_perimetro
  
  # temp <- paste0(focus, "_programmi.csv")
  #
  # programmi <- init_programmazione(usa_temi=FALSE, export=FALSE)
  #
  # if (usa_meuro == TRUE) {
  #   programmi<- programmi  %>%
  #     mutate(RISORSE = round(RISORSE / 1000000, 1))
  # }
  
  # spalla con risorse (per tutti i programmi)
  # spalla <- programmi %>%
  #   group_by(OC_CODICE_PROGRAMMA) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA") %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA, RISORSE)
  
  # spalla <- octk::po_riclass %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR",
  #          x_AMBITO != "FEAMP")
  # %>%
  #   left_join(programmi %>%
  #               group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
  #               summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
  #             by = c("OC_CODICE_PROGRAMMA", "x_AMBITO"))
  
  # MEMO: non serve ripassare per "report_cd_programmi()" perché ho già risorse
  
  # report
  out <- programmi %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  # %>%
  #   select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)
  
  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}


#' Esporta report per Programmi (NEW)
#'
#' Report con apertura per programma e fase procedurale rispetto al focus selezionato
#'
#' @param perimetro Dataset di classe perimetro.
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param add_713 Vuoi caricare anche i dati di programmaizone per il 2007-2013?
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma e fase procedurale.
make_report_programmi_2 <- function(perimetro, usa_meuro=FALSE, add_713=FALSE, focus="elab", export=FALSE) {
  
  appo <- perimetro
  
  temp <- paste0(focus, "_programmi.csv")
  
  # DEBUG: add_713 <- TRUE
  programmi <- init_programmazione(usa_temi=FALSE, add_713=add_713, export=FALSE)
  
  if (usa_meuro == TRUE) {
    programmi<- programmi %>%
      mutate(FINANZ_TOTALE_PUBBLICO = round(FINANZ_TOTALE_PUBBLICO / 1000000, 1))
  }
  
  # CHK
  # programmi %>% count(x_CICLO, x_AMBITO, OC_TIPOLOGIA_PROGRAMMA) %>% filter(x_CICLO == "2007-2013")
  # perimetro %>% count(x_CICLO, x_AMBITO, x_GRUPPO) %>% filter(x_CICLO == "2007-2013")
  
  spalla <- octk::po_riclass %>%
    filter(TIPO != 2 & TIPO != 3 & TIPO != 9, # MEMO: elimino programmi accorpati e disttivati
           x_CICLO != "2000-2006",
           # x_AMBITO != "CTE",
           x_AMBITO != "FEASR") %>%
    select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    # MEMO: risolvo programmi plurifondo ("FESR-FSE" e "FSE-YEI")
    separate_rows(x_AMBITO, sep="-") %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    # OLD:
    # separate(x_AMBITO, c("F1", "F2"), sep = "-", remove=FALSE, fill = "right") %>%
    # gather("x_AMBITO", )
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA) %>%
    left_join(programmi %>%
                # MEMO: patch per factor di x_AMBITO
                mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                              "FEAD", "FAMI", "CTE"))) %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO"))
  
  # spalla <- programmi %>%
  #   mutate(x_GRUPPO = case_when(
  #     x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale" ~ "POCN",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Nazionale Completamenti" ~ "POCN",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale" ~ "POCR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "POC Regionale Completamenti" ~ "POCR",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Piani nazionali" ~ "PIANI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Patti per lo sviluppo" ~ "PATTI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altre assegnazioni CIPE" ~ "ALTRI CIPE",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Altro" ~ "ALTRO",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FSC" & OC_TIPOLOGIA_PROGRAMMA == "Cofinanziamento SIE" ~ "ALTRO",
  #     x_CICLO == "2014-2020" & x_AMBITO == "FEAMP" ~ "PON",
  #     x_CICLO == "2014-2020" & x_AMBITO == "YEI" ~ "YEI",
  #     x_CICLO == "2014-2020" & x_AMBITO == "SNAI" ~ "SNAI",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FESR" & OC_TIPOLOGIA_PROGRAMMA == "POIN" ~ "POIN",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "PON" ~ "PON",
  #     x_CICLO == "2007-2013" & x_AMBITO == "FSE" & OC_TIPOLOGIA_PROGRAMMA == "POR" ~ "POR",
  #     x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Nazionale" ~ "PACN",
  #     x_CICLO == "2007-2013" & x_AMBITO == "POC" & OC_TIPOLOGIA_PROGRAMMA == "PAC Regionale" ~ "PACR",
  #     # MEMO: per il momento il blocco FSC resta come si trova (e dovrebbe essere coerente con po_riclass)
  #     TRUE ~ OC_TIPOLOGIA_PROGRAMMA)) %>%
  #   select(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, FINANZ_TOTALE_PUBBLICO) %>%
  #   filter(x_CICLO != "2000-2006",
  #          x_AMBITO != "CTE",
  #          x_AMBITO != "FEASR" #,
  #          # x_AMBITO != "FEAMP"
  #   ) %>%
  #   group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO) %>%
  #   summarise(RISORSE = sum(FINANZ_TOTALE_PUBBLICO, na.rm = TRUE)) %>%
  #   left_join(octk::po_riclass %>%
  #               distinct(OC_CODICE_PROGRAMMA, x_PROGRAMMA),
  #             by = "OC_CODICE_PROGRAMMA")
  
  # spalla %>% count(x_CICLO, x_AMBITO, x_GRUPPO)
  
  # report
  out <- spalla %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(N = n(),
                          CP = sum(CP, na.rm = TRUE),
                          IMP = sum(IMP, na.rm = TRUE),
                          PAG = sum(PAG, na.rm = TRUE),
                          COE = sum(COE, na.rm = TRUE)) %>%
                left_join(appo %>%
                            group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, OC_STATO_PROCEDURALE) %>%
                            summarise(COE = sum(COE, na.rm = TRUE)) %>%
                            spread(OC_STATO_PROCEDURALE, COE, fill = 0, drop = FALSE),
                          by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
                # MEMO: patch per factor di x_AMBITO
                mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                              "FEAD", "FAMI", "CTE"))),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
  
  out <- out %>%
    # left_join(octk::po_riclass %>%
    #             distinct(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, x_PROGRAMMA),
    #           by = "OC_CODICE_PROGRAMMA", suffix = c("", ".x")) %>%
    as_tibble(.) %>%
    mutate(x_CICLO = as.character(x_CICLO),
           x_AMBITO = as.character(x_AMBITO)) %>%
    # mutate(x_CICLO = if_else(is.na(x_CICLO), x_CICLO.x, x_CICLO),
    #        x_AMBITO = if_else(is.na(x_AMBITO), x_AMBITO.x, x_AMBITO),
    #        x_GRUPPO = if_else(is.na(x_GRUPPO), x_GRUPPO.x, x_GRUPPO),
    #        x_PROGRAMMA = if_else(is.na(x_PROGRAMMA), x_PROGRAMMA.x, x_PROGRAMMA)) %>%
    # select(-x_CICLO.x, -x_AMBITO.x, -x_GRUPPO.x, -x_PROGRAMMA.x) %>%
    select(OC_CODICE_PROGRAMMA, x_PROGRAMMA, x_CICLO, x_AMBITO, x_GRUPPO, RISORSE, N, CP, IMP, PAG, COE,
           `Non avviato`,
           `In avvio di progettazione`,
           `In corso di progettazione`,
           `In affidamento`,
           `In esecuzione`,
           `Eseguito`,
           `Eseguito`)
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}


#' Integra report per Programmi con delta da un bimestre precedente (NEW)
#'
#' Integra il report da make_report_programmi() con il confronto con il bimestre indicato.
#'
#' @param bimestre Bimestre di riferimento (in formato OC tipo "20181231").
#' @param programmi Dataset generato con make_report_programmi().
#' @param last_bimestre Bimestre con cui effettuare il confronto (in formato OC tipo "20181231").
#' @param usa_meuro Vuoi i dati in Meuro? Di default sono in euro.
#' @param focus nome per file.
#' @param export vuoi salvare il file?
#' @return Un file csv con apertura per programma, fase procedurale e bimestre.
add_delta_programmi_2 <- function(bimestre, programmi, last_bimestre, last_data_path,
                                  usa_meuro=FALSE, focus="delta", export=FALSE) {
  
  # bimestre precedente
  if (missing(last_data_path)) {
    last_data_path <- sub(bimestre, last_bimestre, DATA)
  }
  
  # loads
  last_progetti <- load_progetti(bimestre = last_bimestre,
                                 data_path = last_data_path,
                                 visualizzati = TRUE, debug = TRUE, light = TRUE, refactor = TRUE)
  
  # switch variabile COE
  last_perimetro <- last_progetti %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(CP = OC_FINANZ_TOT_PUB_NETTO,
           COE = case_when(x_AMBITO == "FSC" ~ OC_FINANZ_STATO_FSC_NETTO,
                           x_AMBITO == "FSE" ~ COSTO_RENDICONTABILE_UE,
                           x_AMBITO == "FESR" ~ COSTO_RENDICONTABILE_UE + OC_FINANZ_STATO_FSC_NETTO,
                           TRUE ~ OC_FINANZ_TOT_PUB_NETTO))
  
  # meuro
  if (usa_meuro == TRUE) {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP / 1000000,
             COE = COE / 1000000,
             IMP = IMPEGNI / 1000000,
             PAG = TOT_PAGAMENTI / 1000000)
  } else {
    last_perimetro <- last_perimetro %>%
      mutate(CP = CP,
             COE = COE,
             IMP = IMPEGNI,
             PAG = TOT_PAGAMENTI)
  }
  
  # simply
  last_perimetro <- get_simply_non_loc(last_perimetro)
  
  appo <- last_perimetro %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")))
  
  # report
  out <- programmi  %>%
    # MEMO: patch per factor di x_AMBITO e x_CICLO
    mutate(x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                                                  "FEAD", "FAMI", "CTE")),
           x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006"))) %>%
    full_join(appo %>%
                group_by(OC_CODICE_PROGRAMMA, x_CICLO, x_AMBITO) %>%
                summarise(COE_LAST = sum(COE, na.rm = TRUE),
                          PAG_LAST = sum(PAG, na.rm = TRUE)),
              by = c("OC_CODICE_PROGRAMMA", "x_CICLO", "x_AMBITO")) %>%
    # riempie NA con 0
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # mutate(COE_DELTA = (COE - COE_LAST) / COE,
    #        PAG_DELTA = (PAG - PAG_LAST) / PAG) %>%
    mutate(COE_DELTA = (COE - COE_LAST) / COE_LAST,
           PAG_DELTA = (PAG - PAG_LAST) / PAG_LAST) %>%
    mutate(COE_DELTA = if_else(is.infinite(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.infinite(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(is.nan(COE_DELTA), 0, COE_DELTA),
           PAG_DELTA = if_else(is.nan(PAG_DELTA), 0, PAG_DELTA)) %>%
    mutate(COE_DELTA = if_else(COE_DELTA > 1, 1.01, COE_DELTA),
           PAG_DELTA = if_else(PAG_DELTA > 1, 1.01, PAG_DELTA))
  # %>%
  #   select(x_CICLO,	x_AMBITO, RIS,	N,	CP,	IMP,	PAG,	COE,	COE_LAST,	COE_DELTA, PAG_LAST, PAG_DELTA)
  
  if (export == TRUE) {
    temp <- paste0(focus, "_programmi.csv")
    write.csv2(out, file.path(TEMP, temp), row.names = FALSE)
  }
  return(out)
}

