# OC > Explorer > Perimetri
# Loader da "progetti_esteso"

load_progetti <- function(bimestre, visualizzati=TRUE) {
  # DEBUG:
  # bimestre <- "20180630"

  if (exists("progetti")) {
    print("progetti_esteso is ready in progetti!")

  } else {
    # load progetti_esteso
    # ver
    # bimestre <- "20180430"

    # paths
    # setwd("/Users/aa/coding/oc_explorer/")
    # DATA <- file.path("/Users/aa/dati", paste0("oc_", bimestre))
    temp <- paste0("progetti_esteso_", bimestre, ".csv")

    # explore
    # progetti <- read_csv2(file.path(DATA, temp), n_max = 5)

    # load progetti
    if (visualizzati == TRUE) {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 50000) %>%
        filter(OC_FLAG_VISUALIZZAZIONE == 0)
    } else {
      progetti <- read_csv2(file.path(DATA, temp), guess_max = 50000)
      # MEMO: qui prende anche non visualizzati
    }

    # analisi tipologia colonne
    # sapply(names(progetti), function(x) {print(paste0(x, " = ", class(progetti[[x]])))})
    # DEV: fare qualcosa per assegnare ""col_types" sopra

    # Warning:
    # 143229 parsing failures.
    # no trailing characters
    # number of columns of result is not a multiple of vector length

    # chk
    msg <- progetti %>%
      summarise(N = n(),
                FTP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))
    message(msg)


    # HAND: verificare FTP e Progetti su portale OC
    # MEMO: FTP = 117279067953 su portale

    # TODO: inserire automazione per controllo con scraping su portale (fare somma di natura cup)
    # TODO: riscrivere come funzione?

    # DEV: mettere rm(loader)?

    return(progetti)

  }

}



