# OC > Explorer > Perimetri
# Classificazione per tematismi

#' Crea nuova base per riclassificazione lato categorie CUP
#'
#' ...
#'
#' @param pseudo Dataset "pseudo".
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_cup.csv".
#' #' @section Warning:
#' Da rinominare in "classi_cup.csv" e modificare.
setup_classi_cup <- function(pseudo, progetti, file_name="classi_cup_NEW.csv") {

  # intgera pseudo
  out <- pseudo %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "CUP_COD_SETTORE", "CUP_DESCR_SETTORE",
                       "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE",
                       "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA"),
              by = "COD_LOCALE_PROGETTO") %>%
    filter(PERI == 1) %>% # isola scarti
    count(CUP_COD_SETTORE, CUP_DESCR_SETTORE, CUP_COD_SOTTOSETTORE, CUP_DESCR_SOTTOSETTORE,
          CUP_COD_CATEGORIA, CUP_DESCR_CATEGORIA) %>%
    arrange(desc(n)) %>%
    mutate(CLASSE = NA)

  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE)
  
  if (file_name == "classi_cup_NEW.csv") {
    message("Integra e rinomina classi_cup_NEW.csv")
  }

}


#' Crea nuova base per riclassificazione lato temi/campi UE
#'
#' ...
#'
#' @param pseudo Dataset "pseudo".
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_ue.csv".
#' #' @section Warning:
#' Da rinominare in "classi_ue.csv" e modificare.
setup_classi_ue <- function(pseudo, progetti, file_name="classi_ue_NEW.csv") {

  # intgera pseudo
  out <- pseudo %>%
    left_join(progetti %>%
                select("COD_LOCALE_PROGETTO", "OC_COD_CATEGORIA_SPESA", "OC_DESCR_CATEGORIA_SPESA"),
              by = "COD_LOCALE_PROGETTO") %>%
    filter(PERI == 1) %>% # isola scarti
    count(OC_COD_CATEGORIA_SPESA, OC_DESCR_CATEGORIA_SPESA) %>%
    arrange(desc(n)) %>%
    mutate(CLASSE = NA)

  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE)
  
  if (file_name == "classi_ue_NEW.csv") {
    message("Integra e rinomina classi_ue_NEW.csv")
  }

}


#' Setup fixlist per classificazione
#'
#' Salva fixlist.csv
#'
#' @return...
setup_fixlist <- function() {

  # DEV: mettere qui dentro anche setup_classi_cup() e setup_classi_ue()

  write.csv2(fixlist, file.path(INPUT, "fixlist.csv"), row.names = FALSE)

}


#' Classificazione per categoria CUP e campo d'intervento UE
#'
#' Classificazione per categoria CUP e campo d'intervento UE
#'
#' @param pseudo Dataset "pseudo".
#' @param classe_jolly Nome per classe da assegnare ai casi non mappati in classi_cup e classi_ue
#' @param livelli_classe Nomi per factor di classe
#' @param export Logico. Vuoi salvare?
#' @param debug Logico. Vuoi una matrice con freq cup x ue
#' @return Un file "pseudo".
make_classi <- function(pseudo, classe_jolly="Altro", livelli_classe=NULL, export=TRUE, debug=FALSE) {

  # if (is.null(var_ls)) {
  #   var_ls <- c("COD_LOCALE_PROGETTO", "CUP", "OC_TITOLO_PROGETTO",
  #               "OC_COD_CICLO", "OC_COD_FONTE", "FONDO_COMUNITARIO",
  #               "CUP_COD_SETTORE",  "CUP_DESCR_SETTORE",  "CUP_COD_SOTTOSETTORE", "CUP_DESCR_SOTTOSETTORE", "CUP_COD_CATEGORIA", "CUP_DESCR_CATEGORIA",
  #               "OC_DESCRIZIONE_PROGRAMMA", "OC_CODICE_PROGRAMMA",
  #               "OC_COD_ARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA", "OC_COD_SUBARTICOLAZ_PROGRAMMA", "OC_DESCR_ARTICOLAZ_PROGRAMMA",
  #               "OC_FINANZ_TOT_PUB_NETTO", "IMPEGNI", "TOT_PAGAMENTI")
  # }
  # MEMO: forse va tolto anche sopra

  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro

  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    # select(-CLASSE) %>%
    # left_join(progetti %>%
    #             select(var_ls)) %>%
    filter(PERI == 1) # isola scarti

  # aggiunge categorie UE
  # appo <- get_categorie_UE(appo)

  # intgera pseudo
  appo <- appo %>%
    left_join(progetti %>%
                select(CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA,
                        COD_LOCALE_PROGETTO, CUP_COD_NATURA, CUP_DESCR_NATURA,
                       OC_COD_CATEGORIA_SPESA),
              by = "COD_LOCALE_PROGETTO")
  # MEMO: recupera natura per modifica aiuti con categoria UE


  # ----------------------------------------------------------------------------------- #
  # Classificazione

  # load
  classi_cup <- read_csv2(file.path(INPUT, "classi_cup.csv")) %>%
    select(CUP_COD_SETTORE, CUP_COD_SOTTOSETTORE, CUP_COD_CATEGORIA, CLASSE_CUP = CLASSE)

  classi_ue <- read_csv2(file.path(INPUT, "classi_ue.csv")) %>%
    select(OC_COD_CATEGORIA_SPESA, CLASSE_UE = CLASSE)

  # switch per livelli_classe e classe_jolly
  if (is.null(livelli_classe)) {
    livelli_classe <- unique(c(unique(classi_cup$CLASSE_CUP), c(unique(classi_ue$CLASSE_UE))))
    classe_jolly <- "NA"
  }

  # merge
  appo <- appo %>%
    # merge lato CUP
    left_join(classi_cup,
              by = c("CUP_COD_SETTORE", "CUP_COD_SOTTOSETTORE","CUP_COD_CATEGORIA")) %>%
    # merge lato UE
    left_join(classi_ue,
              by = "OC_COD_CATEGORIA_SPESA") %>%
    # MEMO: risolve NA per nuovi progetti con categoria CUP anomala e mai censita >>> CHK!!!
    # MEMO: risolve NA per progetti 1420 senza tema UE
    mutate(CLASSE_CUP = ifelse(is.na(CLASSE_CUP), "Altro", CLASSE_CUP),
           CLASSE_UE = ifelse(is.na(CLASSE_UE), "Altro", CLASSE_UE))

  # MEMO: se vengono NA sono lato CUP e andrebbe rifatto classi_cup.csv
  # appo %>% count(CLASSE_CUP)
  # appo %>% count(CLASSE_UE)

  # crea campo CLASSE e CLASSE_CHK
  appo <- appo %>%
    mutate(CLASSE_CUP = factor(CLASSE_CUP, levels = c(livelli_classe, "Altro")),
           CLASSE_UE = factor(CLASSE_UE, levels = c(livelli_classe, "Altro"))) %>%
    mutate(CHK_CLASSE = CLASSE_CUP == CLASSE_UE)

  # analisi
  # chk <- appo %>%
  #   filter(CLASSE_CUP == "Cultura", CLASSE_UE == "Altro") %>%
  #   arrange(desc(OC_FINANZ_TOT_PUB_NETTO)) %>%
  #   select(TIPO_QUERY, COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO, CUP_DESCR_NATURA, CUP_DESCR_CATEGORIA, DESCR_TEMA_CAMPO)
  # View(chk)

  # MEMO:
  # "Altro" + "Altro" vengono per definizione da "OLD" oppure da "PO" >>> come li gestisco?

  # consolida CLASSE
  appo <- appo %>%
    mutate(CLASSE = case_when((CLASSE_CUP == "Altro") & (CLASSE_UE == "Altro") ~ classe_jolly,
                              CLASSE_CUP == CLASSE_UE ~ as.character(CLASSE_CUP),
                              CLASSE_CUP == "Altro" ~ as.character(CLASSE_UE),
                              CLASSE_UE == "Altro" ~ as.character(CLASSE_CUP),
                              # MEMO: preferenza a CUP per opere e UE per altre nature
                              CUP_COD_NATURA == "03" ~ as.character(CLASSE_CUP),
                              TRUE ~ as.character(CLASSE_UE))) %>%
    # MEMO: la categoria "Altro" qui non esiste più
    mutate(CLASSE = factor(CLASSE, levels = livelli_classe))


  # appo %>%
  #   count(CLASSE)


  # ----------------------------------------------------------------------------------- #
  # Fix manuale

  # load fix
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>%
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    select(COD_LOCALE_PROGETTO, CLASSE)

  chk <- fixlist %>%
    count(COD_LOCALE_PROGETTO) %>%
    filter(n > 1)
  print(paste0("verifica se ci sono duplicati in fixlist: ", dim(chk)[1]))
  # WARNING: se questo contiene duplicati poi ritrovo i duplicati in pseudo

  # fix
  appo <- appo %>%
    mutate(CLASSE = as.character(CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y)


  # ----------------------------------------------------------------------------------- #
  # Integra pseudo

  # integra appo in pseudo
  pseudo <- pseudo %>%
    left_join(appo %>%
                select("COD_LOCALE_PROGETTO", "CLASSE"),
              by = "COD_LOCALE_PROGETTO")
  # MEMO: restano NA in CLASSE per gli scarti...

  if (debug == TRUE) {

    # matrice cup x ue
    appo %>%
      count(CLASSE_CUP, CLASSE_UE) %>%
      spread(CLASSE_UE, n) %>%
      rename("CUP/UE" = CLASSE_CUP) %>%
      write.csv2(file.path(TEMP, "matrix_classi.csv"), na = "", row.names = FALSE)

  }

  if (export == TRUE) {

    write.csv2(pseudo, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)

  }


  return(pseudo)

}


#' Classificazione Hard-Soft
#'
#' Classificazione Hard-Soft da nautra CUP
#'
#' @param pseudo Dataset "pseudo".
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_hard_soft <- function (pseudo, export = TRUE) {
  
  out <- pseudo %>% 
    filter(PERI == 1) %>% # isola scarti
    mutate(CLASSE = case_when(CUP_COD_NATURA == "03" ~ "hard",
                              TRUE ~ "soft"))
  
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  out <- out %>% 
    mutate(CLASSE = as.character(CLASSE)) %>% 
    left_join(fixlist, by = "COD_LOCALE_PROGETTO") %>% 
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>% 
    mutate(CLASSE = factor(CLASSE, levels = c("hard", "soft"))) %>% 
    select(-CLASSE.x, -CLASSE.y) %>%
    select(-CUP_DESCR_NATURA, -CUP_COD_NATURA)
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}




#' Crea nuova base per riclassificazione per forma giuridica soggetti
#'
#' Crea nuova base per riclassificazione per forma giuridica soggetti
#'
#' @param file_name Nome file da salvare in INPUT.
#' @return Un file "classi_soggetti.csv".
setup_classi_soggetti <- function(file_name="classi_soggetti_NEW.csv") {
  
  out <- octk::forma_giuridica_soggetti
  
  write.csv2(out, file.path(INPUT, file_name), row.names = FALSE, quote = c(1))
  
  if (file_name == "classi_soggetti_NEW.csv") {
    message("Integra e rinomina classi_soggetti_NEW.csv")
    message("Attenzione ad aprire il csv con la colonna 1 come testo")
  }
}


#' Classificazione per soggetti
#'
#' Classificazione per tipologie di soggetti
#'
#' @param pseudo Dataset "pseudo".
#' @param livelli_classe Nomi per factor di classe
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_soggetti <- function (pseudo, livelli_classe=NULL, progetti, export = TRUE) {
  
  if (!("OC_COD_FORMA_GIU_BENEFICIARIO" %in% names(progetti))) {
    progetti <- load_progetti(bimestre, light = FALSE)
  }
  
  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro
  
  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    filter(PERI == 1) # isola scarti

  # intgera pseudo
  appo <- appo %>%
    left_join(progetti %>%
              select(COD_LOCALE_PROGETTO, OC_COD_FORMA_GIU_BENEFICIARIO),
              by = "COD_LOCALE_PROGETTO") %>% 
    # isola primo soggetto
    mutate(OC_COD_FORMA_GIU_BENEFICIARIO = substr(OC_COD_FORMA_GIU_BENEFICIARIO, 1, 6))
  # MEMO: ci sono casi spuri diversi da 9.9.99  (es. 1.2)

  
  # ----------------------------------------------------------------------------------- #
  # Classificazione
  
  # load
  classi_soggetti <- read_csv2(file.path(INPUT, "classi_soggetti.csv"), col_types = "ccccc") %>%
    select(OC_COD_FORMA_GIU_BENEFICIARIO, CLASSE)
  
  
  # switch per livelli_classe e classe_jolly
  if (is.null(livelli_classe)) {
    livelli_classe <- unique(classi_soggetti$CLASSE)
    livelli_classe <- livelli_classe[which(!is.na(livelli_classe))]
    if (!("Altro" %in% livelli_classe)){
      livelli_classe <- c(livelli_classe, "Altro")
    }
  }
  
  # merge
  appo <- appo %>%
    left_join(classi_soggetti,
              by = "OC_COD_FORMA_GIU_BENEFICIARIO")

    
  # ----------------------------------------------------------------------------------- #
  # Fix manuale

  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  # fix
  appo <- appo %>%
    # gestione missing
    mutate(CLASSE = ifelse(is.na(CLASSE), "Altro", CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    mutate(CLASSE = factor(CLASSE, levels = livelli_classe)) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y) 
  
  out <- appo
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}


#' Classificazione per ambiti comunali
#'
#' Classificazione per tipologie di soggetti
#'
#' @param pseudo Dataset "pseudo".
#' @param export Logico. Vuoi salvare?
#' @return Un file "pseudo".
make_classi_comuni <- function (pseudo, progetti, export = TRUE) {
  
  # load matrix
  comuni <- read_xlsx(file.path(INPUT, paste0("input_query.xlsx")), sheet = "comuni") %>% 
    filter(QUERY == 1) %>% 
    select(COD_COMUNE, DEN_COMUNE, AMBITO, AMBITO_SUB)
  
  # ----------------------------------------------------------------------------------- #
  # Integra dati perimetro
  
  # filtra pseudo (in appo e senza scarti)
  appo <- pseudo %>%
    filter(PERI == 1) # isola scarti
  
  # corregge comuni
  temp <- progetti %>%
    select(COD_LOCALE_PROGETTO, COD_COMUNE)%>%
    semi_join(appo,
              by = "COD_LOCALE_PROGETTO") %>% 
    separate_rows(COD_COMUNE, sep = ":::") %>%
    # allinea codici oc a matrix (senza regione)
    mutate(chk = nchar(COD_COMUNE)) %>% 
    filter(chk == 9) %>% 
    mutate(COD_COMUNE = substr(COD_COMUNE, 4, 9)) %>%
    select(-chk) %>% 
    left_join(comuni, by = "COD_COMUNE")
  
  # aggrega comuni
  temp1 <- temp %>%
    group_by(COD_LOCALE_PROGETTO) %>%
    summarise(COD_COMUNE = paste(COD_COMUNE, collapse=':::'),
              DEN_COMUNE = paste(DEN_COMUNE, collapse=':::')) %>% 
    left_join(temp %>%
                # elimina ":::NA" da localizzazioni senza ambito
                filter(!is.na(AMBITO)) %>% 
                # elimina dupli da localizzazioni multiple nello stesso ambito
                distinct(COD_LOCALE_PROGETTO, AMBITO, AMBITO_SUB) %>% 
                group_by(COD_LOCALE_PROGETTO) %>%
                summarise(AMBITO = paste(AMBITO, collapse=':::'),
                          AMBITO_SUB = paste(AMBITO_SUB, collapse=':::')),
              by = "COD_LOCALE_PROGETTO")
  
  
  
  # intgera pseudo
  appo <- appo  %>% 
    left_join(temp1, by = "COD_LOCALE_PROGETTO")

  
  # ----------------------------------------------------------------------------------- #
  # Classificazione
  
  # merge
  appo <- appo %>%
    mutate(CLASSE = AMBITO)
  
  # TODO: gestire anvhe ambito_sub
  
  # ----------------------------------------------------------------------------------- #
  # Fix manuale
  
  fixlist <- read_csv2(file.path(INPUT, "fixlist.csv")) %>% 
    filter(!is.na(COD_LOCALE_PROGETTO), CHK == 1) %>% 
    select(COD_LOCALE_PROGETTO, CLASSE)
  
  chk <- fixlist %>% 
    count(COD_LOCALE_PROGETTO) %>% 
    filter(n > 1)
  
  print(paste0("Numero di duplicati in fixlist: ", dim(chk)[1]))
  
  # fix
  appo <- appo %>%
    mutate(CLASSE = as.character(CLASSE)) %>%
    left_join(fixlist,
              by = "COD_LOCALE_PROGETTO") %>%
    # mutate(CLASSE = ifelse(is.na(CLASSE.y), as.factor(CLASSE.x), as.factor(CLASSE.y))) %>%
    mutate(CLASSE = ifelse(is.na(CLASSE.y), CLASSE.x, CLASSE.y)) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Altro"))) %>%
    # mutate(CLASSE = factor(CLASSE, levels = c(1, 2, 3), labels = livelli_classe)) %>%
    # CHK: VERIFICARE LABELS
    select(-CLASSE.x, -CLASSE.y)
  
  
  out <- appo
  
  if (export == TRUE) {
    write.csv2(out, file.path(TEMP, "pseudo.csv"), na = "", row.names = FALSE)
  }
  
  return(out)
}