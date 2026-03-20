#' Inizializza OCTK
#'
#' Definisce i puntamenti ai folder di lavoro e ai dati.
#'
#' @param bimestre Versione dei dati di attuazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param db_ver  Versione dei dati di programmazione da utilizzare. Stringa in formato "20180630" come da standard per le date in OC.
#' @param data_path Percorso alla fonte dati. Se NULL, viene ricavato dalla configurazione utente.
#' @param db_path Percorso al db di programmazione. Se NULL, viene ricavato come DRIVE/PROGRAMMAZIONE/db_ver.
#' @param elab Nome della cartella dedicata al tipo di elaborazione.
#' @param focus Nome della sotto-cartella dedicata all'elaborazione. Il nome viene usato anche nel naming dei file in output.
#' @param ver Nome della sotto-sotto-cartella dedicata alla versione dell'elaborazione.
#' @param use_drive Logico. Mantenuto per retrocompatibilità. Se mode non è specificato, determina drive/local.
#' @param drive_root Percorso per la root di Google Drive montato.
#' @param user Nome utente da cui derivano drive_root e data_root.
#' @param mode Modalità operativa: "drive", "local", "hybrid".
#'
#' @return I path della workarea (WORK, con INPUT, OUTPUT e TEMP), dei dati di attuazione (DATA),
#' del database di programmazione (DB), della root di Drive (DRIVE), della wd locale (LOCAL)
#' e della configurazione runtime (CONFIG) sono disponibili nel Global Environment.
#' In modalità "hybrid" sono inoltre disponibili SYNC_WORK, SYNC_INPUT, SYNC_TEMP e SYNC_OUTPUT.
#'
#' @export
oc_init <- function(bimestre,
                    db_ver,
                    data_path = NULL,
                    db_path = NULL,
                    elab = NULL,
                    focus = NULL,
                    ver = NULL,
                    use_drive = TRUE,
                    drive_root = NULL,
                    user = NULL,
                    mode = NULL) {
  
  library("tidyverse")
  library("haven")
  library("arrow")
  library("readxl")
  library("openxlsx")
  library("lubridate")
  library("readr")
  
  mode <- .oc_resolve_mode(mode = mode, use_drive = use_drive)
  
  LOCAL <- getwd()
  
  dev_cfg <- .oc_get_developer_config(user = user)
  
  DRIVE <- .oc_resolve_drive_root(
    drive_root = drive_root,
    dev_cfg = dev_cfg,
    user = user
  )
  
  DATA <- .oc_resolve_data_path(
    data_path = data_path,
    dev_cfg = dev_cfg,
    bimestre = bimestre,
    user = user
  )
  
  DB <- .oc_resolve_db_path(
    db_path = db_path,
    DRIVE = DRIVE,
    db_ver = db_ver
  )
  
  focus <- .oc_resolve_focus(focus = focus, local_wd = LOCAL)
  
  WORK <- .oc_build_work_path(
    mode = mode,
    LOCAL = LOCAL,
    DRIVE = DRIVE,
    bimestre = bimestre,
    elab = elab,
    focus = focus,
    ver = ver
  )
  
  INPUT  <- file.path(WORK, "input")
  TEMP   <- file.path(WORK, "temp")
  OUTPUT <- file.path(WORK, "output")
  
  .oc_ensure_dir(INPUT)
  .oc_ensure_dir(TEMP)
  .oc_ensure_dir(OUTPUT)
  
  SYNC_WORK <- NULL
  SYNC_INPUT <- NULL
  SYNC_TEMP <- NULL
  SYNC_OUTPUT <- NULL
  
  if (identical(mode, "hybrid")) {
    SYNC_WORK <- .oc_build_sync_work_path(
      DRIVE = DRIVE,
      bimestre = bimestre,
      elab = elab,
      focus = focus,
      ver = ver
    )
    
    SYNC_INPUT  <- file.path(SYNC_WORK, "input")
    SYNC_TEMP   <- file.path(SYNC_WORK, "temp")
    SYNC_OUTPUT <- file.path(SYNC_WORK, "output")
    
    .oc_ensure_dir(SYNC_INPUT)
    .oc_ensure_dir(SYNC_TEMP)
    .oc_ensure_dir(SYNC_OUTPUT)
  }
  
  message(paste0("Modalità: ", mode))
  message(paste0("Connetto la root Drive in ", DRIVE))
  message(paste0("Connetto la fonte dati in ", DATA))
  message(paste0("Connetto il db di programmazione in ", DB))
  message(paste0("Connetto la workarea in ", WORK))
  message(paste0("Connetto il folder INPUT in ", INPUT))
  message(paste0("Connetto il folder TEMP in ", TEMP))
  message(paste0("Connetto il folder OUTPUT in ", OUTPUT))
  
  if (identical(mode, "hybrid")) {
    message(paste0("Connetto il folder SYNC_WORK in ", SYNC_WORK))
    message(paste0("Connetto il folder SYNC_INPUT in ", SYNC_INPUT))
    message(paste0("Connetto il folder SYNC_TEMP in ", SYNC_TEMP))
    message(paste0("Connetto il folder SYNC_OUTPUT in ", SYNC_OUTPUT))
  }
  
  CONFIG <<- list(
    mode = mode,
    user = user,
    os = dev_cfg$os,
    LOCAL = LOCAL,
    DRIVE = DRIVE,
    DATA = DATA,
    DB = DB,
    WORK = WORK,
    INPUT = INPUT,
    TEMP = TEMP,
    OUTPUT = OUTPUT,
    SYNC_WORK = SYNC_WORK,
    SYNC_INPUT = SYNC_INPUT,
    SYNC_TEMP = SYNC_TEMP,
    SYNC_OUTPUT = SYNC_OUTPUT,
    bimestre = bimestre,
    db_ver = db_ver,
    elab = elab,
    focus = focus,
    ver = ver,
    use_drive = use_drive,
    drive_root_param = drive_root,
    data_path_param = data_path,
    db_path_param = db_path
  )
  
  DRIVE <<- DRIVE
  LOCAL <<- LOCAL
  DATA <<- DATA
  DB <<- DB
  WORK <<- WORK
  INPUT <<- INPUT
  TEMP <<- TEMP
  OUTPUT <<- OUTPUT
  focus <<- focus
  bimestre <<- bimestre
  
  if (identical(mode, "hybrid")) {
    SYNC_WORK <<- SYNC_WORK
    SYNC_INPUT <<- SYNC_INPUT
    SYNC_TEMP <<- SYNC_TEMP
    SYNC_OUTPUT <<- SYNC_OUTPUT
  } else {
    if (exists("SYNC_WORK", envir = .GlobalEnv, inherits = FALSE)) rm("SYNC_WORK", envir = .GlobalEnv)
    if (exists("SYNC_INPUT", envir = .GlobalEnv, inherits = FALSE)) rm("SYNC_INPUT", envir = .GlobalEnv)
    if (exists("SYNC_TEMP", envir = .GlobalEnv, inherits = FALSE)) rm("SYNC_TEMP", envir = .GlobalEnv)
    if (exists("SYNC_OUTPUT", envir = .GlobalEnv, inherits = FALSE)) rm("SYNC_OUTPUT", envir = .GlobalEnv)
  }
  
  invisible(CONFIG)
}


.oc_resolve_mode <- function(mode = NULL, use_drive = TRUE) {
  if (is.null(mode)) {
    return(if (isTRUE(use_drive)) "drive" else "local")
  }
  
  mode <- match.arg(mode, choices = c("drive", "local", "hybrid"))
  mode
}


.oc_get_developer_config <- function(user = NULL) {
  if (is.null(user) || identical(user, "")) {
    return(list(user = NULL, os = NULL, drive_root = NULL, data_root = NULL))
  }
  
  cfg_file <- system.file("config", "developers.yml", package = utils::packageName())
  
  if (!nzchar(cfg_file) || !file.exists(cfg_file)) {
    stop("File di configurazione developer non trovato in inst/config/developers.yml", call. = FALSE)
  }
  
  cfg <- yaml::read_yaml(cfg_file)
  
  if (is.null(cfg$developers) || length(cfg$developers) == 0) {
    stop("Il file developers.yml non contiene la sezione 'developers'.", call. = FALSE)
  }
  
  idx <- vapply(cfg$developers, function(x) identical(x$user, user), logical(1))
  
  if (!any(idx)) {
    stop(paste0("Utente '", user, "' non trovato nel file developers.yml."), call. = FALSE)
  }
  
  dev <- cfg$developers[[which(idx)[1]]]
  
  list(
    user = dev$user %||% NULL,
    os = .oc_null_if_empty(dev$os %||% NULL),
    drive_root = .oc_null_if_empty(dev$drive_root %||% NULL),
    data_root = .oc_null_if_empty(dev$data_root %||% NULL)
  )
}


.oc_resolve_drive_root <- function(drive_root = NULL, dev_cfg = NULL, user = NULL) {
  root <- .oc_null_if_empty(drive_root)
  
  if (is.null(root) && !is.null(dev_cfg)) {
    root <- .oc_null_if_empty(dev_cfg$drive_root)
  }
  
  if (is.null(root)) {
    if (is.null(user)) {
      stop("drive_root non specificato e user mancante: impossibile risolvere DRIVE.", call. = FALSE)
    } else {
      stop(paste0("drive_root non risolvibile per user '", user, "'."), call. = FALSE)
    }
  }
  
  root
}


.oc_resolve_data_path <- function(data_path = NULL, dev_cfg = NULL, bimestre, user = NULL) {
  root <- .oc_null_if_empty(data_path)
  
  if (is.null(root) && !is.null(dev_cfg)) {
    root <- .oc_null_if_empty(dev_cfg$data_root)
  }
  
  if (is.null(root)) {
    if (is.null(user)) {
      stop("data_path non specificato e user mancante: impossibile risolvere DATA.", call. = FALSE)
    } else {
      stop(paste0("data_path/data_root non risolvibile per user '", user, "'."), call. = FALSE)
    }
  }
  
  file.path(root, bimestre)
}


.oc_resolve_db_path <- function(db_path = NULL, DRIVE, db_ver) {
  root <- .oc_null_if_empty(db_path)
  
  if (is.null(root)) {
    root <- file.path(DRIVE, "PROGRAMMAZIONE", db_ver)
  }
  
  root
}


.oc_resolve_focus <- function(focus = NULL, local_wd) {
  if (!is.null(focus) && !identical(focus, "")) {
    return(focus)
  }
  
  basename(local_wd)
}


.oc_build_work_path <- function(mode, LOCAL, DRIVE, bimestre, elab = NULL, focus = NULL, ver = NULL) {
  if (identical(mode, "drive")) {
    root <- file.path(DRIVE, "ELAB")
  } else {
    root <- LOCAL
  }
  
  .oc_path_compose(root, bimestre, elab, focus, ver)
}


.oc_build_sync_work_path <- function(DRIVE, bimestre, elab = NULL, focus = NULL, ver = NULL) {
  .oc_path_compose(file.path(DRIVE, "ELAB"), bimestre, elab, focus, ver)
}


.oc_path_compose <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, function(x) is.null(x) || identical(x, ""), logical(1))]
  do.call(file.path, parts)
}


.oc_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}


.oc_null_if_empty <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (length(x) == 0) {
    return(NULL)
  }
  
  if (is.character(x) && !nzchar(x)) {
    return(NULL)
  }
  
  x
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}