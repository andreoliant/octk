# funzioni per reportistica excel standard


# ──────────────────────────────────────────────────────────────────────────
# Funzione: write_table_to_wb  – revisione 2025-07-05
# ──────────────────────────────────────────────────────────────────────────
#' Scrive e formatta una tabella in un workbook **openxlsx**
#'
#' Crea (o aggiorna) un foglio Excel con:
#' \itemize{
#'   \item Titolo e sottotitolo nelle prime due righe;
#'   \item Un doppio header (riga blu con descrizioni e riga grigia con nomi tecnici);
#'   \item Una terza riga “formule” con lettere \emph{A, B, C…} per
#'         le sole colonne numeric/Date/percentuale;
#'   \item La tabella dati, bordata, con formati:
#'         interi ``#,##0``, decimali ``#,##0.00``, percentuali ``0%``;
#'   \item Una riga “Totale” con somma automatica esclusa sulle percentuali;
#'   \item Fonte e nota finali, senza fusione celle;
#'   \item Larghezze calcolate sui dati (con minimi globali) e griglia disattivata.
#' }
#'
#' Etichette di variabile possono essere passate via \code{header_df} e, se
#' presenti, sovrascrivono i nomi tecnici nella riga blu.
#'
#' @param wb        \code{openxlsx::Workbook}. Oggetto workbook aperto o creato.
#' @param df        \code{data.frame}. Dataset da esportare.
#' @param title     \code{character(1)}. Titolo principale (riga 1).
#' @param subtitle  \code{character(1)}. Sottotitolo (riga 2).
#' @param source    \code{character(1)}. Testo “Fonte: …”.
#' @param note      \code{character(1)}. Nota esplicativa.
#' @param sheet_name \code{character(1)}. Nome del foglio (default ``"Foglio1"``).
#'                   Se non esiste viene creato.
#' @param start_row \code{integer(1)}. Riga da cui far partire l’header
#'                  (default 4).
#' @param header_df \code{data.frame} opzionale con colonne \code{name}/\code{label}
#'                  per applicare etichette descrittive.
#' @param apply_labels \code{logical(1)}. Se \code{TRUE} (default) applica i
#'                  label presenti in \code{header_df}.
#'
#' @details
#' \strong{Formati colonna}\cr
#' \itemize{
#'   \item Colonne \code{integer}: \verb{#,##0}
#'   \item Colonne \code{numeric}: \verb{#,##0.00}
#'   \item Colonne percentuale (prefisso ``"p_"``): \verb{0%}
#'   \item Colonne \code{Date}: formato \verb{DD/MM/YYYY}
#' }
#'
#' \strong{Totali}\cr
#' Per ogni colonna numerica diversa da percentuale viene scritta la formula
#' \code{SUM()} nella riga dei totali. Le percentuali restano vuote.
#'
#' @return Invisibilmente, lo stesso \code{wb} modificato.
#'
#' @seealso \code{\link[openxlsx]{createWorkbook}}, \code{is_pct_col},
#'          \code{is_int_col}
#'
#' @examples
#' \dontrun{
#' library(openxlsx)
#' wb <- createWorkbook()
#'
#' write_table_to_wb(
#'   wb        = wb,
#'   df        = iris,
#'   title     = "Analisi Iris",
#'   subtitle  = "Esempio di tabella formattata",
#'   source    = "Fonte: dataset iris",
#'   note      = "Nota: misure in centimetri.",
#'   sheet_name = "Iris",
#'   start_row  = 4
#' )
#'
#' saveWorkbook(wb, "iris_formattata.xlsx", overwrite = TRUE)
#' }
#'
#' @importFrom openxlsx createStyle addWorksheet writeData addStyle
#'   writeFormula setColWidths setRowHeights int2col sheets showGridLines
#' @keywords internal
write_table_to_wb <- function(
    wb,
    df,
    title,
    subtitle,
    source=NULL,
    note=NULL,
    sheet_name = "Foglio1",
    start_row  = 4,
    header_df  = NULL,
    apply_labels = TRUE
) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Serve il pacchetto 'openxlsx'.")
  
  `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
  
  # ── 0) label opzionali ---------------------------------------------------
  if (!is.null(header_df) && apply_labels) {
    if (!all(c("name", "label") %in% names(header_df)))
      stop("`header_df` deve avere colonne 'name' e 'label'.")
    header_df <- header_df[header_df$name %in% names(df), , drop = FALSE]
    mapply(function(nm, lb) attr(df[[nm]], "label") <<- lb,
           header_df$name, header_df$label)
  }
  
  # ── 1) foglio ------------------------------------------------------------
  if (!(sheet_name %in% openxlsx::sheets(wb)))
    openxlsx::addWorksheet(wb, sheet_name)
  
  # elimina linee griglia
  openxlsx::showGridLines(
    wb, sheet = sheet_name,
    showGridLines = FALSE
  )
  
  # ── 2) stili -------------------------------------------------------------
  titleStyle    <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold",
                                         halign = "left", wrapText = FALSE)
  subtitleStyle <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
                                         halign = "left", wrapText = FALSE)
  
  headBlue <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                    fgFill = "#4F81BD", fontColour = "#FFFFFF",
                                    halign = "center", valign = "center",
                                    wrapText = TRUE,
                                    border = c("top","bottom","left","right"))
  headGrey <- openxlsx::createStyle(fontSize = 10, textDecoration = "bold",
                                    fgFill = "#D9D9D9",
                                    halign = "center", valign = "center",
                                    wrapText = TRUE,
                                    border = c("top","bottom","left","right"))
  formulaStyle <- openxlsx::createStyle(fontSize = 10,
                                    textDecoration = "italic",
                                    fgFill = "#EEEEEE",                   
                                    halign = "center",
                                    valign = "center",
                                    border = c("top", "bottom", "left", "right"))

  dataText <- openxlsx::createStyle(border = c("top","bottom","left","right"))
  dataInt  <- openxlsx::createStyle(numFmt = "#,##0",
                                    border = c("top","bottom","left","right"))
  dataNum  <- openxlsx::createStyle(numFmt = "#,##0.00",
                                    border = c("top","bottom","left","right"))
  
  totalBorder <- openxlsx::createStyle(border = c("top","bottom","left","right"))
  totalText   <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       halign = "left")
  totalInt    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       numFmt = "#,##0",   halign = "right")
  totalNum    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       numFmt = "#,##0.00", halign = "right")
  
  dataPct  <- openxlsx::createStyle(numFmt = "0%",
                                    border = c("top","bottom","left","right"))
  totalPct <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                    numFmt   = "0%",
                                    halign   = "right",
                                    border   = c("top","bottom","left","right"))

  sourceStyle <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
                                       halign = "left", wrapText = FALSE)
  noteStyle   <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
                                       halign = "left", wrapText = FALSE)
  
  # ── 3) titoli ------------------------------------------------------------
  openxlsx::writeData(wb, sheet_name, title,    startRow = 1, startCol = 1)
  openxlsx::writeData(wb, sheet_name, subtitle, startRow = 2, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, titleStyle,    rows = 1, cols = 1)
  openxlsx::addStyle(wb, sheet_name, subtitleStyle, rows = 2, cols = 1)
  
  # ── 4) header doppio -----------------------------------------------------
  hdr1        <- start_row          # riga blu
  hdr2        <- start_row + 1      # riga grigia
  formula_row <- start_row + 2      # nuova riga “formule”
  data_start  <- start_row + 3      # i dati iniziano una riga più sotto
  
  desc <- vapply(seq_along(df), function(j) {
    lb <- attr(df[[j]], "label", exact = TRUE)
    if (!is.null(lb) && nzchar(lb)) lb else toupper(names(df)[j])
  }, character(1))
  names_uc <- toupper(names(df))
  
  openxlsx::writeData(wb, sheet_name, as.list(desc),
                      startRow = hdr1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, as.list(names_uc),
                      startRow = hdr2, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, headBlue,
                     rows = hdr1, cols = 1:ncol(df), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, headGrey,
                     rows = hdr2, cols = 1:ncol(df), gridExpand = TRUE)
  
  # ── 4b) riga “formule” ---------------------------------------------------
  # indice delle colonne candidabili
  # eligible <- which(vapply(df, function(x)
  #   is.numeric(x) || inherits(x, "Date"), logical(1)))
  eligible <- which(vapply(seq_along(df), function(j)
    is.numeric(df[[j]]) || inherits(df[[j]], "Date") || is_pct_col(names(df)[j], df[[j]]),
    logical(1)))
  
  # vettore lettere maiuscole per le sole colonne eleggibili
  letters_for_formulas <- LETTERS[seq_along(eligible)]   # "A", "B", …
  
  # inizializza con stringhe vuote
  formula_labels <- rep("", ncol(df))
  formula_labels[eligible] <- letters_for_formulas
  
  openxlsx::writeData(
    wb, sheet_name,
    as.list(formula_labels),
    startRow = formula_row,
    startCol = 1,
    colNames = FALSE
  )
  openxlsx::addStyle(
    wb, sheet_name,
    formulaStyle,
    rows = formula_row,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # ── 5) dati --------------------------------------------------------------
  openxlsx::writeData(wb, sheet_name, df,
                      startRow = data_start, startCol = 1,
                      colNames = FALSE, rowNames = FALSE)
  
  data_end  <- data_start + nrow(df) - 1
  total_row <- data_end + 1
  
  # for (j in seq_along(df)) {
  #   st <- if (is.numeric(df[[j]])) {
  #     # if (toupper(names(df)[j]) == "N") dataInt else dataNum
  #     if (is_int_col(df[[j]])) dataInt else dataNum
  #   } else dataText
  #   openxlsx::addStyle(wb, sheet_name, st,
  #                      rows = data_start:data_end, cols = j,
  #                      gridExpand = TRUE, stack = TRUE)
  # }
  
  for (j in seq_along(df)) {
    nm <- names(df)[j]
    x  <- df[[j]]
    
    st <- if (is_pct_col(nm, x)) {
      dataPct
    } else if (is.numeric(x)) {
      if (is_int_col(x)) dataInt else dataNum
    } else {
      dataText
    }
    
    openxlsx::addStyle(
      wb, sheet_name, st,
      rows = data_start:data_end, cols = j,
      gridExpand = TRUE, stack = TRUE
    )
  }
  
  # ── 5b) totali -----------------------------------------------------------
  # for (j in seq_along(df)) {
  #   if (is.numeric(df[[j]])) {
  #     colLtr <- openxlsx::int2col(j)
  #     openxlsx::writeFormula(
  #       wb, sheet_name,
  #       sprintf("SUM(%s%d:%s%d)", colLtr, data_start, colLtr, data_end),
  #       startRow = total_row, startCol = j
  #     )
  #   }
  # }
  # 
  for (j in seq_along(df)) {
    nm <- names(df)[j]
    x  <- df[[j]]
    
    if (is.numeric(x) && !is_pct_col(nm, x)) {   # ← ESCLUDE le percentuali
      colLtr <- openxlsx::int2col(j)
      openxlsx::writeFormula(
        wb, sheet_name,
        sprintf("SUM(%s%d:%s%d)", colLtr, data_start, colLtr, data_end),
        startRow = total_row, startCol = j
      )
    }
  }
  
  openxlsx::writeData(wb, sheet_name, "Totale",
                      startRow = total_row, startCol = 1, colNames = FALSE)
  openxlsx::addStyle(wb, sheet_name, totalBorder,
                     rows = total_row, cols = 1:ncol(df), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, totalText,
                     rows = total_row, cols = 1, stack = TRUE)
  
  # for (j in 2:ncol(df)) {
  #   if (is.numeric(df[[j]])) {
  #     # st <- if (toupper(names(df)[j]) == "N") totalInt else totalNum
  #     st <- if (is_int_col(df[[j]])) totalInt else totalNum
  #     openxlsx::addStyle(wb, sheet_name, st,
  #                        rows = total_row, cols = j, stack = TRUE)
  #   }
  # }
  
  for (j in 2:ncol(df)) {
    nm <- names(df)[j]
    if (is.numeric(df[[j]])) {
      st <- if (is_pct_col(nm, df[[j]])) {
        totalPct
      } else if (is_int_col(df[[j]])) {
        totalInt
      } else {
        totalNum
      }
      openxlsx::addStyle(wb, sheet_name, st,
                         rows = total_row, cols = j, stack = TRUE)
    }
  }
  
  
  # ── 6) fonte & nota ------------------------------------------------------
  
  if (!is.null(source)) {
    src_row  <- total_row + 1
    openxlsx::writeData(wb, sheet_name, paste0("Fonte: ", source),
                        startRow = src_row,  startCol = 1)
    openxlsx::addStyle(wb, sheet_name, sourceStyle, rows = src_row,  cols = 1)
  }
  
  if (!is.null(note)) {
    note_row <- src_row + 2
    openxlsx::writeData(wb, sheet_name, note,
                        startRow = note_row, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, noteStyle,   rows = note_row, cols = 1)
  }
  # openxlsx::mergeCells(wb, sheet_name, rows = src_row,  cols = 1:ncol(df))
  # openxlsx::mergeCells(wb, sheet_name, rows = note_row, cols = 1:ncol(df))
  
  # ── 7) larghezze colonne (solo dati) -------------------------------------
  # fmt_val <- function(x, colname) {
  #   if (is.numeric(x)) {
  #     if (toupper(colname) == "N") {
  #       formatC(x, digits = 0, format = "f",
  #               big.mark = ",", decimal.mark = ".")
  #     } else {
  #       formatC(x, digits = 2, format = "f",
  #               big.mark = ",", decimal.mark = ".")
  #     }
  #   } else as.character(x)
  # }
  # fmt_val <- function(x) {
  #   if (is_int_col(x)) {
  #     formatC(x, digits = 0, format = "f",
  #             big.mark = ".", decimal.mark = "")
  #   } else if (is.numeric(x)) {
  #     formatC(x, digits = 2, format = "f",
  #             big.mark = ".", decimal.mark = ",")
  #   } else {
  #     as.character(x)
  #   }
  # }
  
  fmt_val <- function(x, name) {
    if (is_pct_col(name, x)) {
      paste0(round(x * 100), "%")
    } else if (is_int_col(x)) {
      # formatC(x, digits = 0, format = "f",
      #         big.mark = ".", decimal.mark = "")
      formatC(x, format = "d", big.mark = ".")
    } else if (is.numeric(x)) {
      formatC(x, digits = 2, format = "f",
              big.mark = ".", decimal.mark = ",")
    } else {
      as.character(x)
    }
  }
  
  
  # widths <- numeric(ncol(df))
  # for (j in seq_along(df)) {
  #   vals <- fmt_val(df[[j]], names(df)[j])
  #   tot  <- if (is.numeric(df[[j]]))
  #     fmt_val(sum(df[[j]], na.rm = TRUE), names(df)[j]) else ""
  #   max_char <- max(nchar(c(vals, tot), type = "width"), na.rm = TRUE)
  #   
  #   if (is.numeric(df[[j]])) {
  #     widths[j] <- max(ceiling(max_char * 1.1) + 2, 18)
  #   } else {
  #     widths[j] <- min(max_char + 2, 50)
  #   }
  # }
  widths <- numeric(ncol(df))
  for (j in seq_along(df)) {
    
    # vettore di stringhe formattate per tutti i valori della colonna
    # vals <- fmt_val(df[[j]])
    vals <- fmt_val(df[[j]], names(df)[j])
    
    # stringa formattata per il totale (se numerico), altrimenti vuota
    # tot  <- if (is.numeric(df[[j]]))
    #   fmt_val(sum(df[[j]], na.rm = TRUE)) else ""
    tot  <- if (is.numeric(df[[j]]))
      fmt_val(sum(df[[j]], na.rm = TRUE), names(df)[j]) else ""
    
    max_char <- max(nchar(c(vals, tot), type = "width"), na.rm = TRUE)
    
    if (is.numeric(df[[j]])) {
      widths[j] <- max(ceiling(max_char * 1.1) + 2, 18)
    } else {
      widths[j] <- min(max_char + 2, 50)
    }
  }
  
  # widths[1] <- max(widths[1], 15)
  widths <- pmax(widths, 10)               # ← larghezza MINIMA per tutte le colonne
  widths[1] <- max(widths[1], 15)          # prima colonna resta ≥ 15

  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = widths)
  
  # ── 8) altezze dinamiche titoli + header ---------------------------------
  lines_needed <- function(text, col_width) {
    ceiling(nchar(text, type = "width") / pmax(col_width, 1))
  }
  # titoli
  # openxlsx::setRowHeights(
  #   wb, sheet_name, rows = 1,
  #   heights = 18 + (lines_needed(title, widths[1]) - 1) * 15
  # )
  # openxlsx::setRowHeights(
  #   wb, sheet_name, rows = 2,
  #   heights = 18 + (lines_needed(subtitle, widths[1]) - 1) * 15
  # )
  openxlsx::setRowHeights(wb, sheet_name, rows = 1:2, heights = 18)
  # header blu
  max_h1 <- max(mapply(lines_needed, desc, widths))
  openxlsx::setRowHeights(
    wb, sheet_name, rows = hdr1,
    heights = 15 + (max_h1 - 1) * 15
  )
  # header grigio
  max_h2 <- max(mapply(lines_needed, names_uc, widths))
  openxlsx::setRowHeights(
    wb, sheet_name, rows = hdr2,
    heights = 15 + (max_h2 - 1) * 15
  )
  
  invisible(wb)
}


  
# utiities---------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b

is_int_col <- function(x) {
  inherits(x, "integer")   # oppure typeof(x) == "integer"
}

is_pct_col <- function(name, x) {
  startsWith(name, "p_") && is.numeric(x)
}



#' Scrive N tabelle verticali in un foglio Excel usando openxlsx
#'
#' @param wb          workbook openxlsx già creato
#' @param tables      lista di data.frame; ogni elemento è una tabella da incollare
#' @param title       character(1) – titolo generale del foglio (prima riga)
#' @param subtitles   character vector – titoli specifici per ciascuna tabella
#' @param sources     character vector o NULL – fonte per ciascuna tabella
#' @param note        character(1) o NULL – nota finale (dopo l’ultima tabella)
#' @param sheet_name  nome del foglio (se non esiste viene creato)
#' @param start_row   riga dell’header blu (default 4)
#' @param gap_rows    righe vuote fra una tabella e la successiva (default 2)
#' @param header_df   opzionale: data‑frame con colonne name / label
#' @param apply_labels se TRUE applica i label presenti in header_df
#' @return workbook invisibile (wb)
#' @export
# ── write_tables_to_wb ────────────────────────────────────────────────────
write_tables_to_wb <- function(
    wb,
    tables,
    title,
    subtitles,
    sources = NULL,
    note = NULL,
    sheet_name = "Foglio1",
    start_row  = 4,      # riga header blu della 1ª tabella
    gap_rows   = 1,      # righe bianche fra tabelle
    header_df  = NULL,
    apply_labels = TRUE
) {
  
  # ── 0) prerequisiti & helper ---------------------------------------------------
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Serve il pacchetto 'openxlsx'.")
  
  `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
  
  is_int_col <- function(nm, x) {
    if (!is.numeric(x)) return(FALSE)
    nm <- toupper(nm)
    ## 1) tipo integer puro
    is_integer_type <- typeof(x) == "integer"
    ## 2) nome che indica conteggio/identificativo
    name_hint <- grepl("^(N|ID|COUNT|TOT)$|_N$", nm)
    is_integer_type || name_hint
  }
  
  is_pct_col <- function(nm, x) {
    nm <- tolower(nm)
    if (grepl("%|perc|pct|p_", nm)) return(TRUE)
    if (!is.numeric(x)) return(FALSE)
    rng <- range(x, na.rm = TRUE)
    if (rng[1] < 0 || rng[2] > 1) return(FALSE)
    any(x != 0 & x != 1, na.rm = TRUE)
  }
  
  fmt_val <- function(x, name) {
    if (is_pct_col(name, x)) {
      paste0(round(x * 100), "%")
    } else if (is_int_col(name, x)) {
      formatC(x, format = "d", big.mark = ".")
    } else if (is.numeric(x)) {
      formatC(x, digits = 2, format = "f",
              big.mark = ".", decimal.mark = ",")
    } else {
      as.character(x)
    }
  }
  
  lines_needed <- function(text, col_width)
    ceiling(nchar(text, type = "width") / pmax(col_width, 1))
  
  # ── 1) input ---------------------------------------------------
  if (!is.list(tables) || length(tables) == 0)
    stop("`tables` deve essere una lista di data.frame.")
  if (!all(vapply(tables, is.data.frame, logical(1))))
    stop("Tutti gli elementi di `tables` devono essere data.frame.")
  
  n_tables  <- length(tables)
  subtitles <- rep_len(subtitles, n_tables)
  sources   <- rep_len(sources,   n_tables)
  
  # ── 2) label opzionali ---------------------------------------------------
  if (!is.null(header_df) && apply_labels) {
    if (!all(c("name", "label") %in% names(header_df)))
      stop("`header_df` deve avere colonne 'name' e 'label'.")
    for (tbl in tables) {
      hdr_sub <- header_df[header_df$name %in% names(tbl), , drop = FALSE]
      mapply(function(nm, lb) attr(tbl[[nm]], "label") <<- lb,
             hdr_sub$name, hdr_sub$label)
    }
  }
  
  # ── 3) foglio ------------------------------------------------------------
  if (!(sheet_name %in% openxlsx::sheets(wb)))
    openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::showGridLines(wb, sheet_name, showGridLines = FALSE)
  
  # ── 4) stili -------------------------------------------------------------
  titleStyle    <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold",
                                         halign = "left", wrapText = FALSE)
  subtitleStyle <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
                                         halign = "left", wrapText = FALSE)
  
  headBlue <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                    fgFill = "#4F81BD", fontColour = "#FFFFFF",
                                    halign = "center", valign = "center",
                                    wrapText = TRUE,
                                    border = c("top","bottom","left","right"))
  headGrey <- openxlsx::createStyle(fontSize = 10, textDecoration = "bold",
                                    fgFill = "#D9D9D9",
                                    halign = "center", valign = "center",
                                    wrapText = TRUE,
                                    border = c("top","bottom","left","right"))
  formulaStyle <- openxlsx::createStyle(fontSize = 10,
                                        textDecoration = "italic",
                                        fgFill = "#EEEEEE",
                                        halign = "center", valign = "center",
                                        border = c("top","bottom","left","right"))
  
  dataText <- openxlsx::createStyle(border = c("top","bottom","left","right"))
  dataInt  <- openxlsx::createStyle(numFmt = "#,##0",
                                    border = c("top","bottom","left","right"))
  dataNum  <- openxlsx::createStyle(numFmt = "#,##0.00",
                                    border = c("top","bottom","left","right"))
  dataPct  <- openxlsx::createStyle(numFmt = "0%",
                                    border = c("top","bottom","left","right"))
  
  totalBorder <- openxlsx::createStyle(border = c("top","bottom","left","right"))
  totalText   <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       halign = "left")
  totalInt    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       numFmt = "#,##0",   halign = "right")
  totalNum    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       numFmt = "#,##0.00", halign = "right")
  totalPct    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                       numFmt = "0%",       halign = "right")
  
  sourceStyle <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
                                       halign = "left", wrapText = FALSE)
  noteStyle   <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
                                       halign = "left", wrapText = FALSE)
  
  # ── 5) titolo globale ----------------------------------------------------
  openxlsx::writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, sheet_name, titleStyle, rows = 1, cols = 1)
  
  # ── 6) loop tabelle ------------------------------------------------------
  current_row <- start_row - 1                 # riga del 1° sottotitolo
  max_cols    <- max(vapply(tables, ncol, integer(1)))
  col_widths  <- numeric(max_cols)             # larghezze cumulative
  
  for (i in seq_len(n_tables)) {
    
    df       <- tables[[i]]
    ncol_df  <- ncol(df)
    nrow_df  <- nrow(df)
    
    subtitle_row <- current_row
    hdr1         <- subtitle_row + 1           # riga blu
    hdr2         <- subtitle_row + 2           # riga grigia
    formula_row  <- subtitle_row + 3           # riga “formule”
    data_start   <- subtitle_row + 4           # inizio dati
    data_end     <- data_start + nrow_df - 1   # fine dati
    total_row    <- data_end + 1               # riga totali
    source_row   <- total_row + 1              # riga fonte
    
    # 6a) sottotitolo
    openxlsx::writeData(wb, sheet_name, subtitles[i],
                        startRow = subtitle_row, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, subtitleStyle,
                       rows = subtitle_row, cols = 1)
    
    # 6b) header doppio
    desc <- vapply(names(df), function(nm) {
      if (!is.null(header_df) && nm %in% header_df$name) {
        lab <- header_df$label[match(nm, header_df$name)]
        if (!is.null(lab) && nzchar(lab)) return(lab)
      }
      lb <- attr(df[[nm]], "label", exact = TRUE)
      if (!is.null(lb) && nzchar(lb)) lb else toupper(nm)
    }, character(1))
    
    names_uc <- toupper(names(df))
    
    openxlsx::writeData(wb, sheet_name, as.list(desc),
                        startRow = hdr1, startCol = 1, colNames = FALSE)
    openxlsx::writeData(wb, sheet_name, as.list(names_uc),
                        startRow = hdr2, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, headBlue,
                       rows = hdr1, cols = 1:ncol_df, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet_name, headGrey,
                       rows = hdr2, cols = 1:ncol_df, gridExpand = TRUE)
    
    # 6c) riga “formule”
    eligible <- which(vapply(seq_along(df), function(j)
      is.numeric(df[[j]]) || inherits(df[[j]], "Date") ||
        is_pct_col(names(df)[j], df[[j]]), logical(1)))
    formula_labels <- rep("", ncol_df)
    formula_labels[eligible] <- LETTERS[seq_along(eligible)]
    
    openxlsx::writeData(wb, sheet_name, as.list(formula_labels),
                        startRow = formula_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, formulaStyle,
                       rows = formula_row, cols = 1:ncol_df, gridExpand = TRUE)
    
    # 6d) dati
    openxlsx::writeData(wb, sheet_name, df,
                        startRow = data_start, startCol = 1,
                        colNames = FALSE, rowNames = FALSE)
    
    for (j in seq_along(df)) {
      nm <- names(df)[j]
      x  <- df[[j]]
      st <- if (is_pct_col(nm, x)) {
        dataPct
      } else if (is.numeric(x)) {
        if (is_int_col(nm, x)) dataInt else dataNum
      } else {
        dataText
      }
      openxlsx::addStyle(wb, sheet_name, st,
                         rows = data_start:data_end, cols = j,
                         gridExpand = TRUE, stack = TRUE)
    }
    
    # 6e) totali
    for (j in seq_along(df)) {
      nm <- names(df)[j]
      if (is.numeric(df[[j]]) && !is_pct_col(nm, df[[j]])) {
        colL <- openxlsx::int2col(j)
        openxlsx::writeFormula(
          wb, sheet_name,
          sprintf("SUM(%s%d:%s%d)", colL, data_start, colL, data_end),
          startRow = total_row, startCol = j
        )
      }
    }
    
    openxlsx::writeData(wb, sheet_name, "Totale",
                        startRow = total_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, totalBorder,
                       rows = total_row, cols = 1:ncol_df, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet_name, totalText,
                       rows = total_row, cols = 1, stack = TRUE)
    
    for (j in 2:ncol_df) {
      nm <- names(df)[j]
      if (is.numeric(df[[j]])) {
        st_tot <- if (is_pct_col(nm, df[[j]])) {
          totalPct
        } else if (is_int_col(nm, df[[j]])) {
          totalInt
        } else {
          totalNum
        }
        openxlsx::addStyle(wb, sheet_name, st_tot,
                           rows = total_row, cols = j, stack = TRUE)
      }
    }
    
    # 6f) fonte
    if (!is.null(sources[i]) && nzchar(sources[i])) {
      openxlsx::writeData(wb, sheet_name, paste0("Fonte: ", sources[i]),
                          startRow = source_row, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, sourceStyle,
                         rows = source_row, cols = 1)
    } else {
      source_row <- total_row
    }
    
    # 6g) larghezze colonne (accumulate)
    for (j in seq_along(df)) {
      
      vals <- fmt_val(df[[j]], names(df)[j])
      if (is.numeric(df[[j]]))
        vals <- c(vals,
                  fmt_val(sum(df[[j]], na.rm = TRUE), names(df)[j]))
      
      vals <- vals[!is.na(vals) & vals != ""]
      
      # max_char <- if (length(vals))
      #   max(nchar(vals, type = "width")) else 0
      # 
      # width_tmp <- if (is.numeric(df[[j]]))
      #   max(ceiling(max_char * 1.1) + 2,
      #       if (is_int_col(names(df)[j], df[[j]])) 18 else 18)
      
      ## 1. lunghezza massima in caratteri (dati + eventuale totale)
      max_char <- if (length(vals)) max(nchar(vals, type = "width")) else 0
      
      ## 2. stima in punti: 10-15 pt ≃ 1 “carattere Excel”
      ##    moltiplichiamo per 1.1 per un piccolo margine e aggiungiamo 2 pt di padding
      width_tmp <- ceiling(max_char * 1.1) + 2

      ## 3. minimo e massimo ragionevoli
      if (is.numeric(df[[j]])) {
        width_tmp <- max(width_tmp, 12)      # numerici: minimo 12 pt (non 18!)
      } else {
        width_tmp <- max(width_tmp, 10)      # testuali: minimo 10 pt
      }
      width_tmp <- min(width_tmp, 50)          # opz.: tetto 50 pt
      
      col_widths[j] <- max(col_widths[j], width_tmp, na.rm = TRUE)
    }
    
    # 6h) altezze dinamiche header  ----------------------------------------
    ## 1. larghezze effettive da usare nel calcolo
    eff_w <- col_widths[seq_len(ncol_df)]     # copie
    eff_w[eff_w == 0] <- 10                   # fallback minimo
    eff_w[1] <- max(eff_w[1], 15)             # prima colonna ≥ 15 (come setColWidths)
    
    ## 2. quante righe servono per ciascuna intestazione
    extra <- 0.25    # frazione di riga (20 %)
    h_desc <- max(mapply(lines_needed, desc,     eff_w)) + extra
    h_desc <- ceiling(h_desc)
    h_name <- max(mapply(lines_needed, names_uc, eff_w)) + extra
    h_name <- ceiling(h_name)
    
    ## 3. imposta le altezze (15 pt per la prima riga + 15 pt per ogni riga extra)
    openxlsx::setRowHeights(wb, sheet_name, rows = hdr1,
                            heights = 15 + (h_desc - 1) * 15)
    openxlsx::setRowHeights(wb, sheet_name, rows = hdr2,
                            heights = 15 + (h_name - 1) * 15)
    
    ## riga “formule” fissa
    openxlsx::setRowHeights(wb, sheet_name, rows = formula_row, heights = 15)
    
    # 6i) riga successiva
    current_row <- source_row + 1 + gap_rows
  }
  
  # ── 7) nota finale -------------------------------------------------------
  if (!is.null(note) && nzchar(note)) {
    openxlsx::writeData(wb, sheet_name, note,
                        startRow = current_row, startCol = 1)
    openxlsx::addStyle(wb, sheet_name, noteStyle,
                       rows = current_row, cols = 1)
  }
  
  # ── 8) applico larghezze colonne ----------------------------------------
  col_widths[col_widths == 0] <- 10          # fallback
  col_widths[1] <- max(col_widths[1], 15)    # prima colonna ≥ 15
  openxlsx::setColWidths(wb, sheet_name,
                         cols = seq_along(col_widths),
                         widths = pmax(col_widths, 10))
  
  invisible(wb)
}















# stili

library(openxlsx)

# TODO:
# scrivere funzione init_styles con alcuni paramentri (fontColour, size, ecc.)

#' Stile per celle con bordo
#'
#' Stile per celle con bordo da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
# style_border <- createStyle(border = c("top", "bottom", "left", "right"), fontColour = "#000000")
style_border <- createStyle(border = c("top", "bottom", "left", "right"))

#' Stile per celle con bordo
#'
#' Stile per celle con bordo da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
# style_border <- createStyle(border = c("top", "bottom", "left", "right"), fontColour = "#000000")
style_border_blue <- createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#FFFFFF")


#' Stile per celle con numeri con due decimali
#'
#' Stile per celle con numeri con due decimali da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
style_number2 <- createStyle(numFmt = "#,##0.00", halign = "right", border = c("top", "bottom", "left", "right"), fontColour = "#000000")


#' Stile per celle con numeri senza decimali
#'
#' Stile per celle con numeri senza decimali da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
style_number <- createStyle(numFmt = "#,##0", halign = "right", border = c("top", "bottom", "left", "right"), fontColour = "#000000")



#' Stile per celle con percentuali
#'
#' Stile per celle con percentuali da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
style_percentage <- createStyle(numFmt = "PERCENTAGE", border = c("top", "bottom", "left", "right"), fontColour = "#000000")

#' Stile per celle con date
#'
#' Stile per celle con date da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
# style_date <- createStyle(numFmt = "DATE", border = c("top", "bottom", "left", "right"))
style_date <- createStyle(numFmt = "dd/mm/yyyy", border = c("top", "bottom", "left", "right"))


#' Stile per celle con date senza bordo
#'
#' Stile per celle con date senza bordo da applicare con openxlsx
#'
#' @note  Non prevede parametri editabili, sono stili standard per OC
#' @return Stile compatibile con addStyle() in openxlsx
style_date2 <- createStyle(numFmt = "DATE")


# format automatico per righe e colonne
# TODO: copia da 20240831 > DEV > PNRR > schede aacc
