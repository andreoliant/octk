# OC > Toolkit
# Setup

# CHK: getwd() è sempre all'interno di oc anche se parto da altro progetto
# INFO: https://gist.github.com/jennybc/362f52446fe1ebc4c49f

# libs
library(tidyverse)
# CHK: questo dovrebbe venire da DESCRIPTION...?

# print(environment())
# print(parent.env(environment()))

# development mode
# print(getwd())
# temp <- stringr::str_split(getwd(), "/")[[1]]
# appo <- temp[length(temp)]
# print(appo)
# MEMO: prende nome da ultimo item di wd
# if (appo == "oc") {
#   DEV_MODE <- TRUE
#   # MEMO: attiva DEV_MODE se nome è uguale a "oc"
# } else {
#   DEV_MODE <- FALSE
# }
# rm(temp)
if (!exists("workarea")) {
  workarea <- getwd()
  DEV_MODE <- TRUE
  message("La DEVMODE è attiva!")
} else {
  DEV_MODE <- FALSE
}
temp <- stringr::str_split(workarea, "/")[[1]]
appo <- temp[length(temp)]
# MEMO: prende nome da ultimo item di workarea

# defaults
if (!exists("bimestre")) {
  bimestre <- "20180630"
  # MEMO: da aggiornare con nuova release per nuovo bimestre
  # DEV: mettere elenco in /data e usare max() qui
}

if (!exists("focus")) {
  # MEMO: focus regola sia workarea che filename di export
  if (DEV_MODE) {
    focus <- "test"
    # MEMO: usa test perché in DEV_MODE nome è uguale a "oc"
  } else {
    focus <- appo
    # MEMO: usa nome perché diverso da "oc"
  }
}
rm(appo)

# if (!exists("data_path")) {
#   data_path <- "/Users/aa/dati"
#   # DEV: qui potrebbe andare readline ma andrebbe inserito solo all'installazione
#   # x <- readline("What is the value of x?")
# }

# paths workarea
# print(DEV_MODE)
# if (DEV_MODE) {
#   INPUT <- file.path(getwd(), "tests", "input")
#   TEMP <- file.path(getwd(), "tests", "temp")
#   OUTPUT <- file.path(getwd(), "tests", "output")
# } else {
#   INPUT <- file.path(getwd(),  "input")
#   TEMP <- file.path(getwd(), "temp")
#   OUTPUT <- file.path(getwd(), "output")
# }
if (DEV_MODE) {
  # MEMO: qui lavoro nel folder del package e uso una nuova cartella
  INPUT <- file.path(workarea, "tests", "input")
  TEMP <- file.path(workarea, "tests", "temp")
  OUTPUT <- file.path(workarea, "tests", "output")
} else {
  INPUT <- file.path(workarea,  "input")
  TEMP <- file.path(workarea, "temp")
  OUTPUT <- file.path(workarea, "output")
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

# wizard dati
if (!exists("data_path")) {
  data_path <<- readline("Quale path per la fonte dati? ")
}
# "/Users/aa/dati"
DATA <- file.path(data_path, paste0("oc_", bimestre))
message(paste0("Connetto la fonte dati in ", DATA))


