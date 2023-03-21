# OC > Toolkit
# Loader

#' Carica il dataset progetti
#'
#' Carica il file progetti_esteso_$BIMESTRE.csv dal folder DATI.
#'
#' @param bimestre Stringa in formato "20180630" come da standard per le date in OC.
#' @param visualizzati Logico. Vuoi solo i progetti visualizzati sul portale OC?
#' @param debug Logico. Vuoi vedere i totali di progetti e costo pubblico per controllo sul portale OC?
#' @param light Logico. Vuoi usare la versione light di "progetti.csv"?
#' @return Il dataset viene caricato come "progetti" nel Global Environment. Se "progetti" è gia presente compare una notifica.
load_progetti <- function(bimestre, data_path=NULL, visualizzati=TRUE, debug=FALSE, light=FALSE, refactor=FALSE)
{
  # if (exists("progetti", envir = .GlobalEnv)) {
  #   print("Progetti esteso è gia caricato")
  #   progetti <- progetti
  #
  # } else {

    # switch di filename per progetti_light
    if (light == TRUE) {
      temp <- paste0("progetti_light_", bimestre, ".csv")
    } else {
      if (as.numeric(bimestre) <= 20181231) {
        temp <- paste0("progetti_esteso_", bimestre, ".csv")
      } else {
        temp <- "PROGETTI_PREESTESO.csv"
      }
    }

    # switch
    if (!is.null(data_path)) {
      DATA <- data_path
      # MEMO: sovrascrive data_path a DATA
    } else {
      # #OLD: questo non aveva senso perché era pleonastico perché risultava "/home/antonio/dati/oc/20210228/../20210228"
      # data_path=file.path(DATA, "..", bimestre)
      # DATA <- data_path
      # # MEMO: questo serve per puntare a bimestre specifico senza modificare data_path
      DATA <- DATA
    }

    # load progetti
    if (visualizzati == TRUE) {
      # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000) %>%
      #   filter(OC_FLAG_VISUALIZZAZIONE == 0)
      # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1200000) %>%
      #   filter(OC_FLAG_VISUALIZZAZIONE == 0)
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000) %>%
        filter(OC_FLAG_VISUALIZZAZIONE == 0 | OC_FLAG_VISUALIZZAZIONE == 9) # include progetti FEASR per SNAI
      # CHK: progetti %>% filter(OC_FLAG_VISUALIZZAZIONE == 9) %>% count(X_AMBITO)
    } else {
      # progetti <- read_csv2(file.path(DATA, temp), guess_max = 1000000)
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 1800000)
      # MEMO: qui prende anche non visualizzati
    }

    # Warning: 143229 parsing failures.
    # no trailing characters
    # number of columns of result is not a multiple of vector length
    # MEMO: risolto incrementando guess_max

    # analisi tipologia colonne
    # sapply(names(progetti), function(x) {print(paste0(x, " = ", class(progetti[[x]])))})

    # refactor
    # MEMO: si applica solo a light
    if (light == TRUE & refactor == TRUE) {
      progetti <- refactor_progetti(progetti)
    }

    # debug
    if (debug == TRUE) {
      msg <- progetti %>%
        summarise(N = n(),
                  CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
      message(paste0("Progetti esteso contiene ", format(msg$N, big.mark = ".", decimal.mark = ","),
                     " progetti per un costo pubblico totale di ",
                     format(round(msg$CP/1000000000, 1), big.mark = ".", decimal.mark = ","),
                     " miliardi di euro."))
    }
    return(progetti)
  # }
}




#' Refactor per perimetro di progetti
#'
#' Integra un perimetro di progetti appena caricato con i factor per x_MACROAREA, x_CICLO, x_AMBITO e OC_STATO_PROCEDURALE.
#'
#' @param perimetro Dataset in formato standard.
#' @return Il dataset integrato.
refactor_progetti <- function(perimetro) {

  perimetro <- perimetro %>%
    mutate(# x_MACROAREA = factor(x_MACROAREA, levels = c("Centro-Nord", "Sud", "Trasversale", "Nazionale", "Estero")),
           # x_AMBITO = factor(x_AMBITO, levels = c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
           #                                        "FEAD", "FAMI", "CTE")),
           # x_CICLO = factor(x_CICLO, levels = c("2014-2020", "2007-2013", "2000-2006")),
           OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE, levels = c("Non avviato",
                                                                          "In avvio di progettazione",
                                                                          "In corso di progettazione",
                                                                          "In affidamento",
                                                                          "In esecuzione",
                                                                          "Eseguito",
                                                                          "Non determinabile"))) %>%
    refactor_ambito(.) %>%
    refactor_ciclo(.) %>%
    refactor_macroarea(.)
  
  return(perimetro)
}



#' Refactor di x_AMBITO
#'
#' Integra un perimetro di progetti con factor di x_AMBITO.
#'
#' @param df Dataset progetti in formato standard.
#' @return Il dataset integrato.
refactor_ambito <- function(df) {
  
  levels_ambito <- c("FESR", "FSE", "POC", "FSC", "FEASR", "FEAMP", "YEI", "SNAI",
                     "FEAD", "FAMI", "CTE", "ENI", "ORD", "PAC")
  
  df <- df %>%
    mutate(x_AMBITO = factor(x_AMBITO, levels = levels_ambito))
  
  return(df)
  
}


#' Refactor di x_CICLO
#'
#' Integra un perimetro di progetti con factor di x_CICLO.
#'
#' @param df Dataset progetti in formato standard.
#' @return Il dataset integrato.
refactor_ciclo <- function(df) {
  
  levels_ciclo <- c("2021-2027", "2014-2020", "2007-2013", "2000-2006")
  
  df <- df %>%
    mutate(x_CICLO = factor(x_CICLO, levels = levels_ciclo))
  
  return(df)
  
}


#' Refactor di x_MACROAREA
#'
#' Integra un perimetro di progetti con factor di x_MACROAREA
#'
#' @param df Dataset progetti in formato standard.
#' @return Il dataset integrato.
refactor_macroarea <- function(df) {
  
  levels_macroarea <- c("Mezzogiorno", "Centro-Nord", "Ambito nazionale", "Trasversale", "Estero")
  
  df <- df %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels = levels_macroarea))
  
  return(df)
  
}