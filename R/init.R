# OC > Toolkit
# Setup

oc_init <- function() {
  # libs
  library(tidyverse)
  # CHK: questo dovrebbe venire da DESCRIPTION...?

  # print(environment())
  # print(parent.env(environment()))

  # workarea
  if (!exists("workarea")) {
    # MEMO la workarea è come una wd ma è diversa da quella del progetto di RStudio
    workarea <- getwd()
    DEV_MODE <- TRUE
    message("La DEV_MODE è attiva! La workarea è in ./test")
  } else {
    DEV_MODE <- FALSE
  }

  # focus
  if (!exists("focus")) {
    # MEMO: focus regola naming per export
    if (DEV_MODE) {
      focus <- "prova"
      # MEMO: in DEV_MODE il nome in "appo" (sotto) è "oc" per definzione
    } else {
      temp <- stringr::str_split(workarea, .Platform$file.sep)[[1]]
      appo <- temp[length(temp)]
      focus <- appo
      # MEMO: prende nome da ultimo item di workarea
    }
  }

  # paths workarea
  if (DEV_MODE) {
    # MEMO:
    # qui lavoro nel folder del package e uso la cartella "test" di sviluppo
    # oppure lavoro in un folder nella root di un progetto
    test_dir <- file.path(workarea, "test")
    if (!dir.exists(test_dir)) {
      dir.create(test_dir)
    }
    INPUT <<- file.path(test_dir, "input")
    TEMP <<- file.path(test_dir, "temp")
    OUTPUT <<- file.path(test_dir, "output")
  } else {
    INPUT <<- file.path(workarea, "input")
    TEMP <<- file.path(workarea, "temp")
    OUTPUT <<- file.path(workarea, "output")
  }

  # wizard workarea
  if (!dir.exists(INPUT)) {
    message(paste0("Creo e connetto il folder INPUT in ", INPUT))
    dir.create(INPUT)
  } else {
    message(paste0("Connetto il folder INPUT in ", INPUT))
  }

  if (!dir.exists(TEMP)) {
    message(paste0("Creo e connetto il folder TEMP in ", TEMP))
    dir.create(TEMP)
  } else {
    message(paste0("Connetto il folder TEMP in ", TEMP))
  }

  if (!dir.exists(OUTPUT)) {
    message(paste0("Creo e connetto il folder OUTPUT in ", OUTPUT))
    dir.create(OUTPUT)
  } else {
    message(paste0("Connetto il folder OUTPUT in ", OUTPUT))
  }

  # wizard dati attuazione
  if (!exists("data_path")) {
    data_path <- readline("Quale path per la fonte dati (scrivi senza \"...\")? ")
  }

  # wizard db programmazione
  if (!exists("db_path")) {
    db_path <- readline("Quale path per il db di programmazione (scrivi senza \"...\")? ")
  }

  if (!exists("bimestre")) {
    bimestre <- list.files(data_path) %>%
      as.numeric() %>%
      max() %>%
      as.character()
    # MEMO: si aggiorna da solo con nuovo bimestre
  }

  # "/Users/aa/dati"
  # DATA <<- file.path(data_path, bimestre) # MEMO: versione prima di GoogleDrive
  DATA <<- file.path(data_path)
  message(paste0("Connetto la fonte dati in ", DATA))
  DB <<- file.path(db_path)
  message(paste0("Connetto il db di programmzione in ", DB))
}


