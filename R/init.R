# OC > Toolkit
# Setup

#' Inizializza OCTK
#'
#' Definisce i puntamenti ai folder di lavoro e ai dati.
#'
#' @param bimestre Versione dei dati di attuazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param db_ver  Versione dei dati di programmazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param data_path Percorso allla fonte dati.
#' @param db_path Percorso al bd di programmazione.
#' @param workarea Percorso per la cartella di lavoro. Se vuoto, va di default nella wd.
#' @param elab Nome della cartella dedicata al tipo di elebaorazione (solo con use_drive=TRUE)
#' @param focus Nome della sotto-cartella dedicata all'elebaorazione (solo con use_drive=TRUE). Il nome viene usato anche nel naming dei file in output.
#' @param ver Nome della sotto-sotto-cartella dedicata alla versione dell'elebaorazione (solo con use_drive=TRUE).
#' @param use_drive Logico. Vuoi usare GoogleDrive?
#' @param drive_root Percorso per la cartella di lavoro su GoogleDrive.
#' @param user Nome utente da cui deriva drive_root
#' @param DEV_MODE Logico. Vuoi usare la DEV_MODE?
#' @return I path della workarea (WORK, con INPUT, OUTPUT e TEMP), dei dati di attuazione (DATA)
#' e del database di programmazione (DB) sono disponibili nel Global Environment. Se non esistono, vengono creati i folder
#' "input", "output" e "temp" nella workarea di progetto denominata come "focus" all'interno di "...".
oc_init <- function (bimestre, db_ver, data_path = NULL, db_path = NULL, 
                     workarea = NULL, elab = NULL, focus = NULL, ver = NULL, use_drive = TRUE, 
                     drive_root = NULL, user = NULL, DEV_MODE = FALSE) 
{
  library("tidyverse")
  library("haven")
  library("readxl")
  library("openxlsx")
  library("lubridate")
  
  # configurazione con drive
  if (use_drive == TRUE) {
    message("GoogleDrive in uso")
    if (is.null(drive_root)) {
      if (is.null(user)) {
        ROOT <- "/Volumes/GoogleDrive/Drive condivisi"  
      }
      else {ROOT <- set_developer(user)} 
    }
    else {
      ROOT <- drive_root
    }
    if (is.null(data_path)) {
      data_path <- file.path(ROOT,"DATI", bimestre ,"DASAS","DATAMART")
    } else {
      data_path <- file.path(data_path, bimestre)
    }
    #se non è nullo è un percorso che punta in locale 
    db_path <- file.path(ROOT, "PROGRAMMAZIONE", db_ver)
    if (is.null(elab) & is.null(focus)) {
      if (DEV_MODE == TRUE) {
        WORK <- file.path(getwd(), "test")
        focus <- "test"
        message("La DEV_MODE è attiva! La workarea è in locale in oc/test ma i dati sono in GoogleDrive")
      }
      else {
        WORK <- file.path(getwd())
        temp <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
        focus <<- temp[length(temp)]
      }
    }
    else {
      if (!is.null(ver)) {
        WORK <- file.path(ROOT, "ELAB", bimestre, elab, 
                          focus, ver)
      }
      else {
        WORK <- file.path(ROOT, "ELAB", bimestre, elab, 
                          focus)
      }
    }
  }
  else {
    message("Lavoro in locale")
    if (is.null(data_path)) {
      appo <- readline("Quale path per la fonte dati? ")
      data_path <- gsub("\\\"", "", appo)
    }
    if (is.null(db_path)) {
      appo <- readline("Quale path per il db di programmazione? ")
      db_path <- gsub("\\\"", "", appo)
    }
    if (is.null(workarea)) {
      if (is.null(elab) & is.null(focus)) {
        if (DEV_MODE == TRUE) {
          WORK <- file.path(getwd(), "test")
          message("La DEV_MODE è attiva! La workarea è in oc/test")
          focus <- "test"
        }
        else {
          WORK <- file.path(getwd())
          temp <- stringr::str_split(getwd(), .Platform$file.sep)[[1]]
          focus <<- temp[length(temp)]
        }
      }
      else {
        if (!is.null(ver)) {
          WORK <- file.path(getwd(), elab, focus, ver)
        }
        else {
          WORK <- file.path(getwd(), elab, focus)
        }
      }
    }
    else {
      WORK <- workarea
    }
  }
  # DATA <<- file.path(data_path, bimestre)
  DATA <<- file.path(data_path)
  message(paste0("Connetto la fonte dati in ", DATA))
  DB <<- file.path(db_path)
  message(paste0("Connetto il db di programmazione in ", DB))
  INPUT <<- file.path(WORK, "input")
  if (!dir.exists(INPUT)) {
    message(paste0("Creo e connetto il folder INPUT in ", 
                   INPUT))
    dir.create(INPUT, recursive = TRUE)
  }
  else {
    message(paste0("Connetto il folder INPUT in ", INPUT))
  }
  TEMP <<- file.path(WORK, "temp")
  if (!dir.exists(TEMP)) {
    message(paste0("Creo e connetto il folder TEMP in ", 
                   TEMP))
    dir.create(TEMP, recursive = TRUE)
  }
  else {
    message(paste0("Connetto il folder TEMP in ", TEMP))
  }
  OUTPUT <<- file.path(WORK, "output")
  if (!dir.exists(OUTPUT)) {
    message(paste0("Creo e connetto il folder OUTPUT in ", 
                   OUTPUT))
    dir.create(OUTPUT, recursive = TRUE)
  }
  else {
    message(paste0("Connetto il folder OUTPUT in ", OUTPUT))
  }
  WORK <<- WORK
  focus <<- focus
  bimestre <<- bimestre
  DRIVE <<- ROOT
}

#' Copia i dati di OC da GoogleDrive
#'
#' Copia i dati di OC da GoogleDrive.
#'
#' @param bimestre Versione dei dati di attuazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param data_path Percorso allla fonte dati (senza folder del bimestre).
#' @return I file di "progetti_preesteso" sono copiati nel folder DATA.
oc_init_data <- function(bimestre, data_path=NULL) {

  # finanziamenti_preesteso.sas7bdat
  # indicatori_pucok.sas7bdat
  # operazioni_pucok.sas7bdat
  # PROGETTI_PREESTESO.csv
  # PROGETTI_PREESTESO.zip

  ROOT <- "/Volumes/GoogleDrive/Drive condivisi"

  # wizard dati attuazione
  if (is.null(data_path)) {
    appo <- readline("Quale path per la fonte dati? ")
    data_path <- gsub("\\\"", "", appo)
  }

  DATA <- file.path(data_path, bimestre)

  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "PROGETTI_PREESTESO.zip"),
            to = file.path(DATA, "PROGETTI_PREESTESO.zip"))
  unzip(zipfile = file.path(DATA, "PROGETTI_PREESTESO.zip"),
        exdir = file.path(DATA))

  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "finanziamenti_preesteso.sas7bdat"),
            to = file.path(DATA, "finanziamenti_preesteso.sas7bdat"))

  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "indicatori_pucok.sas7bdat"),
            to = file.path(DATA, "indicatori_pucok.sas7bdat"))

  file.copy(from = file.path(ROOT, "DATI", bimestre, "DASAS", "DATAMART", "operazioni_pucok.sas7bdat"),
            to = file.path(DATA, "operazioni_pucok.sas7bdat"))

}



#' Gestione utenti OCTK su GoogleDrive
#'
#' Definisce il puntamento a GoogleDrive per ogni utente.
#'
#' @param user Nome utente da cui deriva drive_root
#' @return Puntamento allla root di GoogleDrive per ogni utente (drive_root)
set_developer <- function(user) {
  developer <- tibble(developer = c("Flora", 
                                    "Luca",
                                    "Andrea",
                                    "Nicola", 
                                    "Antonio",
                                    "Antonio2",
                                    "Daniela",
                                    "Paolo",
                                    "AndreaT"),
                          path = c(file.path("G:","Drive condivisi"), # "G:/Drive condivisi"
                                   file.path("G:","Drive condivisi"),
                                   file.path("G:","Drive condivisi"),
                                   # file.path("/Volumes", "GoogleDrive", "Drive condivisi"), # "/Volumes/GoogleDrive/Drive condivisi"
                                   file.path("/Users", "nicoladechiara", "Library", "CloudStorage", "GoogleDrive-nicola.dechiara@opencoesione.team", "Drive condivisi"), # "/Users/nicoladechiara/Library/CloudStorage/GoogleDrive-nicola.dechiara@opencoesione.team/Drive condivisi"
                                   file.path("/home", "antonio", "ExpanDrive", "OC", "Shared Drives"),
                                   file.path("/home", "aa", "oc_drive"),
                                   file.path("G:","Shared drives"),
                                   file.path("G:","Drive condivisi"),
                                   file.path("/Users", "andreataddei", "Google Drive", "Drive condivisi"))) 
  
  drive_root <- developer%>%
    filter(developer == user)%>%
    select(path)%>%
    as.character()
  
  return(drive_root)
}




