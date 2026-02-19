# funzioni per reportistica excel standard


#' # ──────────────────────────────────────────────────────────────────────────
#' # Funzione: write_table_to_wb  – revisione 2025-07-05
#' # ──────────────────────────────────────────────────────────────────────────
#' #' Scrive e formatta una tabella in un workbook **openxlsx**
#' #'
#' #' Crea (o aggiorna) un foglio Excel con:
#' #' \itemize{
#' #'   \item Titolo e sottotitolo nelle prime due righe;
#' #'   \item Un doppio header (riga blu con descrizioni e riga grigia con nomi tecnici);
#' #'   \item Una terza riga “formule” con lettere \emph{A, B, C…} per
#' #'         le sole colonne numeric/Date/percentuale;
#' #'   \item La tabella dati, bordata, con formati:
#' #'         interi ``#,##0``, decimali ``#,##0.00``, percentuali ``0%``;
#' #'   \item Una riga “Totale” con somma automatica esclusa sulle percentuali;
#' #'   \item Fonte e nota finali, senza fusione celle;
#' #'   \item Larghezze calcolate sui dati (con minimi globali) e griglia disattivata.
#' #' }
#' #'
#' #' Etichette di variabile possono essere passate via \code{header_df} e, se
#' #' presenti, sovrascrivono i nomi tecnici nella riga blu.
#' #'
#' #' @param wb        \code{openxlsx::Workbook}. Oggetto workbook aperto o creato.
#' #' @param df        \code{data.frame}. Dataset da esportare.
#' #' @param title     \code{character(1)}. Titolo principale (riga 1).
#' #' @param subtitle  \code{character(1)}. Sottotitolo (riga 2).
#' #' @param source    \code{character(1)}. Testo “Fonte: …”.
#' #' @param note      \code{character(1)}. Nota esplicativa.
#' #' @param sheet_name \code{character(1)}. Nome del foglio (default ``"Foglio1"``).
#' #'                   Se non esiste viene creato.
#' #' @param start_row \code{integer(1)}. Riga da cui far partire l’header
#' #'                  (default 4).
#' #' @param header_df \code{data.frame} opzionale con colonne \code{name}/\code{label}
#' #'                  per applicare etichette descrittive.
#' #' @param apply_labels \code{logical(1)}. Se \code{TRUE} (default) applica i
#' #'                  label presenti in \code{header_df}.
#' #'
#' #' @details
#' #' \strong{Formati colonna}\cr
#' #' \itemize{
#' #'   \item Colonne \code{integer}: \verb{#,##0}
#' #'   \item Colonne \code{numeric}: \verb{#,##0.00}
#' #'   \item Colonne percentuale (prefisso ``"p_"``): \verb{0%}
#' #'   \item Colonne \code{Date}: formato \verb{DD/MM/YYYY}
#' #' }
#' #'
#' #' \strong{Totali}\cr
#' #' Per ogni colonna numerica diversa da percentuale viene scritta la formula
#' #' \code{SUM()} nella riga dei totali. Le percentuali restano vuote.
#' #'
#' #' @return Invisibilmente, lo stesso \code{wb} modificato.
#' #'
#' #' @seealso \code{\link[openxlsx]{createWorkbook}}, \code{is_pct_col},
#' #'          \code{is_int_col}
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(openxlsx)
#' #' wb <- createWorkbook()
#' #'
#' #' write_table_to_wb(
#' #'   wb        = wb,
#' #'   df        = iris,
#' #'   title     = "Analisi Iris",
#' #'   subtitle  = "Esempio di tabella formattata",
#' #'   source    = "Fonte: dataset iris",
#' #'   note      = "Nota: misure in centimetri.",
#' #'   sheet_name = "Iris",
#' #'   start_row  = 4
#' #' )
#' #'
#' #' saveWorkbook(wb, "iris_formattata.xlsx", overwrite = TRUE)
#' #' }
#' #'
#' #' @importFrom openxlsx createStyle addWorksheet writeData addStyle
#' #'   writeFormula setColWidths setRowHeights int2col sheets showGridLines
#' #' @keywords internal
#' write_table_to_wb <- function(
#'     wb,
#'     df,
#'     title,
#'     subtitle,
#'     source=NULL,
#'     note=NULL,
#'     sheet_name = "Foglio1",
#'     start_row  = 4,
#'     header_df  = NULL,
#'     apply_labels = TRUE
#' ) {
#'   if (!requireNamespace("openxlsx", quietly = TRUE))
#'     stop("Serve il pacchetto 'openxlsx'.")
#'   
#'   `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
#'   
#'   # ── 0) label opzionali ---------------------------------------------------
#'   if (!is.null(header_df) && apply_labels) {
#'     if (!all(c("name", "label") %in% names(header_df)))
#'       stop("`header_df` deve avere colonne 'name' e 'label'.")
#'     header_df <- header_df[header_df$name %in% names(df), , drop = FALSE]
#'     mapply(function(nm, lb) attr(df[[nm]], "label") <<- lb,
#'            header_df$name, header_df$label)
#'   }
#'   
#'   # ── 1) foglio ------------------------------------------------------------
#'   if (!(sheet_name %in% openxlsx::sheets(wb)))
#'     openxlsx::addWorksheet(wb, sheet_name)
#'   
#'   # elimina linee griglia
#'   openxlsx::showGridLines(
#'     wb, sheet = sheet_name,
#'     showGridLines = FALSE
#'   )
#'   
#'   # ── 2) stili -------------------------------------------------------------
#'   titleStyle    <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold",
#'                                          halign = "left", wrapText = FALSE)
#'   subtitleStyle <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
#'                                          halign = "left", wrapText = FALSE)
#'   
#'   headBlue <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
#'                                     fgFill = "#4F81BD", fontColour = "#FFFFFF",
#'                                     halign = "center", valign = "center",
#'                                     wrapText = TRUE,
#'                                     border = c("top","bottom","left","right"))
#'   headGrey <- openxlsx::createStyle(fontSize = 10, textDecoration = "bold",
#'                                     fgFill = "#D9D9D9",
#'                                     halign = "center", valign = "center",
#'                                     wrapText = TRUE,
#'                                     border = c("top","bottom","left","right"))
#'   formulaStyle <- openxlsx::createStyle(fontSize = 10,
#'                                     textDecoration = "italic",
#'                                     fgFill = "#EEEEEE",                   
#'                                     halign = "center",
#'                                     valign = "center",
#'                                     border = c("top", "bottom", "left", "right"))
#' 
#'   dataText <- openxlsx::createStyle(border = c("top","bottom","left","right"))
#'   dataInt  <- openxlsx::createStyle(numFmt = "#,##0",
#'                                     border = c("top","bottom","left","right"))
#'   dataNum  <- openxlsx::createStyle(numFmt = "#,##0.00",
#'                                     border = c("top","bottom","left","right"))
#'   
#'   totalBorder <- openxlsx::createStyle(border = c("top","bottom","left","right"))
#'   totalText   <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
#'                                        halign = "left")
#'   totalInt    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
#'                                        numFmt = "#,##0",   halign = "right")
#'   totalNum    <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
#'                                        numFmt = "#,##0.00", halign = "right")
#'   
#'   dataPct  <- openxlsx::createStyle(numFmt = "0%",
#'                                     border = c("top","bottom","left","right"))
#'   totalPct <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
#'                                     numFmt   = "0%",
#'                                     halign   = "right",
#'                                     border   = c("top","bottom","left","right"))
#' 
#'   sourceStyle <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
#'                                        halign = "left", wrapText = FALSE)
#'   noteStyle   <- openxlsx::createStyle(fontSize = 9, textDecoration = "italic",
#'                                        halign = "left", wrapText = FALSE)
#'   
#'   # ── 3) titoli ------------------------------------------------------------
#'   openxlsx::writeData(wb, sheet_name, title,    startRow = 1, startCol = 1)
#'   openxlsx::writeData(wb, sheet_name, subtitle, startRow = 2, startCol = 1)
#'   openxlsx::addStyle(wb, sheet_name, titleStyle,    rows = 1, cols = 1)
#'   openxlsx::addStyle(wb, sheet_name, subtitleStyle, rows = 2, cols = 1)
#'   
#'   # ── 4) header doppio -----------------------------------------------------
#'   hdr1        <- start_row          # riga blu
#'   hdr2        <- start_row + 1      # riga grigia
#'   formula_row <- start_row + 2      # nuova riga “formule”
#'   data_start  <- start_row + 3      # i dati iniziano una riga più sotto
#'   
#'   desc <- vapply(seq_along(df), function(j) {
#'     lb <- attr(df[[j]], "label", exact = TRUE)
#'     if (!is.null(lb) && nzchar(lb)) lb else toupper(names(df)[j])
#'   }, character(1))
#'   names_uc <- toupper(names(df))
#'   
#'   openxlsx::writeData(wb, sheet_name, as.list(desc),
#'                       startRow = hdr1, startCol = 1, colNames = FALSE)
#'   openxlsx::writeData(wb, sheet_name, as.list(names_uc),
#'                       startRow = hdr2, startCol = 1, colNames = FALSE)
#'   openxlsx::addStyle(wb, sheet_name, headBlue,
#'                      rows = hdr1, cols = 1:ncol(df), gridExpand = TRUE)
#'   openxlsx::addStyle(wb, sheet_name, headGrey,
#'                      rows = hdr2, cols = 1:ncol(df), gridExpand = TRUE)
#'   
#'   # ── 4b) riga “formule” ---------------------------------------------------
#'   # indice delle colonne candidabili
#'   # eligible <- which(vapply(df, function(x)
#'   #   is.numeric(x) || inherits(x, "Date"), logical(1)))
#'   eligible <- which(vapply(seq_along(df), function(j)
#'     is.numeric(df[[j]]) || inherits(df[[j]], "Date") || is_pct_col(names(df)[j], df[[j]]),
#'     logical(1)))
#'   
#'   # vettore lettere maiuscole per le sole colonne eleggibili
#'   letters_for_formulas <- LETTERS[seq_along(eligible)]   # "A", "B", …
#'   
#'   # inizializza con stringhe vuote
#'   formula_labels <- rep("", ncol(df))
#'   formula_labels[eligible] <- letters_for_formulas
#'   
#'   openxlsx::writeData(
#'     wb, sheet_name,
#'     as.list(formula_labels),
#'     startRow = formula_row,
#'     startCol = 1,
#'     colNames = FALSE
#'   )
#'   openxlsx::addStyle(
#'     wb, sheet_name,
#'     formulaStyle,
#'     rows = formula_row,
#'     cols = 1:ncol(df),
#'     gridExpand = TRUE
#'   )
#' 
#'   # ── 5) dati --------------------------------------------------------------
#'   openxlsx::writeData(wb, sheet_name, df,
#'                       startRow = data_start, startCol = 1,
#'                       colNames = FALSE, rowNames = FALSE)
#'   
#'   data_end  <- data_start + nrow(df) - 1
#'   total_row <- data_end + 1
#'   
#'   # for (j in seq_along(df)) {
#'   #   st <- if (is.numeric(df[[j]])) {
#'   #     # if (toupper(names(df)[j]) == "N") dataInt else dataNum
#'   #     if (is_int_col(df[[j]])) dataInt else dataNum
#'   #   } else dataText
#'   #   openxlsx::addStyle(wb, sheet_name, st,
#'   #                      rows = data_start:data_end, cols = j,
#'   #                      gridExpand = TRUE, stack = TRUE)
#'   # }
#'   
#'   for (j in seq_along(df)) {
#'     nm <- names(df)[j]
#'     x  <- df[[j]]
#'     
#'     st <- if (is_pct_col(nm, x)) {
#'       dataPct
#'     } else if (is.numeric(x)) {
#'       if (is_int_col(x)) dataInt else dataNum
#'     } else {
#'       dataText
#'     }
#'     
#'     openxlsx::addStyle(
#'       wb, sheet_name, st,
#'       rows = data_start:data_end, cols = j,
#'       gridExpand = TRUE, stack = TRUE
#'     )
#'   }
#'   
#'   # ── 5b) totali -----------------------------------------------------------
#'   # for (j in seq_along(df)) {
#'   #   if (is.numeric(df[[j]])) {
#'   #     colLtr <- openxlsx::int2col(j)
#'   #     openxlsx::writeFormula(
#'   #       wb, sheet_name,
#'   #       sprintf("SUM(%s%d:%s%d)", colLtr, data_start, colLtr, data_end),
#'   #       startRow = total_row, startCol = j
#'   #     )
#'   #   }
#'   # }
#'   # 
#'   for (j in seq_along(df)) {
#'     nm <- names(df)[j]
#'     x  <- df[[j]]
#'     
#'     if (is.numeric(x) && !is_pct_col(nm, x)) {   # ← ESCLUDE le percentuali
#'       colLtr <- openxlsx::int2col(j)
#'       openxlsx::writeFormula(
#'         wb, sheet_name,
#'         sprintf("SUM(%s%d:%s%d)", colLtr, data_start, colLtr, data_end),
#'         startRow = total_row, startCol = j
#'       )
#'     }
#'   }
#'   
#'   openxlsx::writeData(wb, sheet_name, "Totale",
#'                       startRow = total_row, startCol = 1, colNames = FALSE)
#'   openxlsx::addStyle(wb, sheet_name, totalBorder,
#'                      rows = total_row, cols = 1:ncol(df), gridExpand = TRUE)
#'   openxlsx::addStyle(wb, sheet_name, totalText,
#'                      rows = total_row, cols = 1, stack = TRUE)
#'   
#'   # for (j in 2:ncol(df)) {
#'   #   if (is.numeric(df[[j]])) {
#'   #     # st <- if (toupper(names(df)[j]) == "N") totalInt else totalNum
#'   #     st <- if (is_int_col(df[[j]])) totalInt else totalNum
#'   #     openxlsx::addStyle(wb, sheet_name, st,
#'   #                        rows = total_row, cols = j, stack = TRUE)
#'   #   }
#'   # }
#'   
#'   for (j in 2:ncol(df)) {
#'     nm <- names(df)[j]
#'     if (is.numeric(df[[j]])) {
#'       st <- if (is_pct_col(nm, df[[j]])) {
#'         totalPct
#'       } else if (is_int_col(df[[j]])) {
#'         totalInt
#'       } else {
#'         totalNum
#'       }
#'       openxlsx::addStyle(wb, sheet_name, st,
#'                          rows = total_row, cols = j, stack = TRUE)
#'     }
#'   }
#'   
#'   
#'   # ── 6) fonte & nota ------------------------------------------------------
#'   
#'   if (!is.null(source)) {
#'     src_row  <- total_row + 1
#'     openxlsx::writeData(wb, sheet_name, paste0("Fonte: ", source),
#'                         startRow = src_row,  startCol = 1)
#'     openxlsx::addStyle(wb, sheet_name, sourceStyle, rows = src_row,  cols = 1)
#'   }
#'   
#'   if (!is.null(note)) {
#'     note_row <- src_row + 2
#'     openxlsx::writeData(wb, sheet_name, note,
#'                         startRow = note_row, startCol = 1)
#'     openxlsx::addStyle(wb, sheet_name, noteStyle,   rows = note_row, cols = 1)
#'   }
#'   # openxlsx::mergeCells(wb, sheet_name, rows = src_row,  cols = 1:ncol(df))
#'   # openxlsx::mergeCells(wb, sheet_name, rows = note_row, cols = 1:ncol(df))
#'   
#'   # ── 7) larghezze colonne (solo dati) -------------------------------------
#'   # fmt_val <- function(x, colname) {
#'   #   if (is.numeric(x)) {
#'   #     if (toupper(colname) == "N") {
#'   #       formatC(x, digits = 0, format = "f",
#'   #               big.mark = ",", decimal.mark = ".")
#'   #     } else {
#'   #       formatC(x, digits = 2, format = "f",
#'   #               big.mark = ",", decimal.mark = ".")
#'   #     }
#'   #   } else as.character(x)
#'   # }
#'   # fmt_val <- function(x) {
#'   #   if (is_int_col(x)) {
#'   #     formatC(x, digits = 0, format = "f",
#'   #             big.mark = ".", decimal.mark = "")
#'   #   } else if (is.numeric(x)) {
#'   #     formatC(x, digits = 2, format = "f",
#'   #             big.mark = ".", decimal.mark = ",")
#'   #   } else {
#'   #     as.character(x)
#'   #   }
#'   # }
#'   
#'   fmt_val <- function(x, name) {
#'     if (is_pct_col(name, x)) {
#'       paste0(round(x * 100), "%")
#'     } else if (is_int_col(x)) {
#'       # formatC(x, digits = 0, format = "f",
#'       #         big.mark = ".", decimal.mark = "")
#'       formatC(x, format = "d", big.mark = ".", decimal.mark = "")
#'     } else if (is.numeric(x)) {
#'       formatC(x, digits = 2, format = "f",
#'               big.mark = ".", decimal.mark = ",")
#'     } else {
#'       as.character(x)
#'     }
#'   }
#'   
#'   
#'   # widths <- numeric(ncol(df))
#'   # for (j in seq_along(df)) {
#'   #   vals <- fmt_val(df[[j]], names(df)[j])
#'   #   tot  <- if (is.numeric(df[[j]]))
#'   #     fmt_val(sum(df[[j]], na.rm = TRUE), names(df)[j]) else ""
#'   #   max_char <- max(nchar(c(vals, tot), type = "width"), na.rm = TRUE)
#'   #   
#'   #   if (is.numeric(df[[j]])) {
#'   #     widths[j] <- max(ceiling(max_char * 1.1) + 2, 18)
#'   #   } else {
#'   #     widths[j] <- min(max_char + 2, 50)
#'   #   }
#'   # }
#'   widths <- numeric(ncol(df))
#'   for (j in seq_along(df)) {
#'     
#'     # vettore di stringhe formattate per tutti i valori della colonna
#'     # vals <- fmt_val(df[[j]])
#'     vals <- fmt_val(df[[j]], names(df)[j])
#'     
#'     # stringa formattata per il totale (se numerico), altrimenti vuota
#'     # tot  <- if (is.numeric(df[[j]]))
#'     #   fmt_val(sum(df[[j]], na.rm = TRUE)) else ""
#'     tot  <- if (is.numeric(df[[j]]))
#'       fmt_val(sum(df[[j]], na.rm = TRUE), names(df)[j]) else ""
#'     
#'     max_char <- max(nchar(c(vals, tot), type = "width"), na.rm = TRUE)
#'     
#'     if (is.numeric(df[[j]])) {
#'       widths[j] <- max(ceiling(max_char * 1.1) + 2, 18)
#'     } else {
#'       widths[j] <- min(max_char + 2, 50)
#'     }
#'   }
#'   
#'   # widths[1] <- max(widths[1], 15)
#'   widths <- pmax(widths, 10)               # ← larghezza MINIMA per tutte le colonne
#'   widths[1] <- max(widths[1], 15)          # prima colonna resta ≥ 15
#' 
#'   openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = widths)
#'   
#'   # ── 8) altezze dinamiche titoli + header ---------------------------------
#'   lines_needed <- function(text, col_width) {
#'     ceiling(nchar(text, type = "width") / pmax(col_width, 1))
#'   }
#'   # titoli
#'   # openxlsx::setRowHeights(
#'   #   wb, sheet_name, rows = 1,
#'   #   heights = 18 + (lines_needed(title, widths[1]) - 1) * 15
#'   # )
#'   # openxlsx::setRowHeights(
#'   #   wb, sheet_name, rows = 2,
#'   #   heights = 18 + (lines_needed(subtitle, widths[1]) - 1) * 15
#'   # )
#'   openxlsx::setRowHeights(wb, sheet_name, rows = 1:2, heights = 18)
#'   # header blu
#'   max_h1 <- max(mapply(lines_needed, desc, widths))
#'   openxlsx::setRowHeights(
#'     wb, sheet_name, rows = hdr1,
#'     heights = 15 + (max_h1 - 1) * 15
#'   )
#'   # header grigio
#'   max_h2 <- max(mapply(lines_needed, names_uc, widths))
#'   openxlsx::setRowHeights(
#'     wb, sheet_name, rows = hdr2,
#'     heights = 15 + (max_h2 - 1) * 15
#'   )
#'   
#'   invisible(wb)
#' }
#' 
#' 
#'   
# 
# `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
# 
# is_int_col <- function(x) {
#   inherits(x, "integer")   # oppure typeof(x) == "integer"
# }
# 
# is_pct_col <- function(name, x) {
#   startsWith(name, "p_") && is.numeric(x)
# }



















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







#' Scrive N tabelle verticali in un foglio Excel usando openxlsx
#'
#' @param file_name   path con file name con cui salvare
#' @param wb          workbook openxlsx già creato
#' @param tables      lista di data.frame; ogni elemento è una tabella da incollare
#' @param title       character(1) – titolo generale del foglio (prima riga)
#' @param subtitles   character vector – titoli specifici per ciascuna tabella
#' @param sources     character vector o NULL – fonte per ciascuna tabella
#' @param note        character(1) o NULL – nota finale (dopo l’ultima tabella)
#' @param sheet_name  nome del foglio (se non esiste viene creato)
#' @param start_row   riga dell’header blu (default 4)
#' @param gap_rows    righe vuote fra una tabella e la successiva (default 1)
#' @param apply_labels se TRUE applica i label presenti in header_df
#' @param total_regex regex per riconoscere Totali/Subtotali nella prima colonna
#'        (default: inizio riga con "totale" o "subtotale", case-insensitive)
#' @return workbook invisibile (wb)
#' @export
wrapper_write_excel_report <- function(
    file_path,
    tables,
    title,
    subtitles,
    sources = NULL,
    note = NULL,
    sheet_name = "Foglio1",
    start_row  = 4
) {
  
  header <- read_xlsx(file.path(INPUT, "header.xlsx"))
  
  wb <- createWorkbook()
  
  write_tables_to_wb(
    wb          = wb,
    tables      = tables,
    title       = title,
    subtitles   = subtitles,
    source      = sources,
    note        = note,
    sheet_name  = sheet_name,
    start_row   = start_row,
    header_df   = header
  )
  
  saveWorkbook(wb, file = file_path, overwrite = TRUE)

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
#' @param gap_rows    righe vuote fra una tabella e la successiva (default 1)
#' @param header_df   opzionale: data-frame con colonne name / label / (formula|formule)
#' @param apply_labels se TRUE applica i label presenti in header_df
#' @param total_regex regex per riconoscere Totali/Subtotali nella prima colonna
#'        (default: inizio riga con "totale" o "subtotale", case-insensitive)
#' @return workbook invisibile (wb)
#' @export
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
    apply_labels = TRUE,
    total_regex = "^(?i)\\s*(?:sub)?totale\\b"
) {
  # ── 0) prerequisiti & helper ---------------------------------------------------
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Serve il pacchetto 'openxlsx'.")
  
  `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
  
  is_int_col <- function(nm, x) {
    if (!is.numeric(x)) return(FALSE)
    nm <- toupper(nm)
    is_integer_type <- typeof(x) == "integer"
    name_hint <- grepl("^(N|ID|COUNT|TOT)$|_N$", nm)
    is_integer_type || name_hint
  }
  
  # is_pct_col <- function(nm, x) {
  #   nm <- tolower(nm)
  #   if (grepl("%|perc|pct|p_", nm)) return(TRUE)
  #   if (!is.numeric(x)) return(FALSE)
  #   rng <- range(x, na.rm = TRUE)
  #   if (rng[1] < 0 || rng[2] > 1) return(FALSE)
  #   any(x != 0 & x != 1, na.rm = TRUE)
  # }
  # 
  # fmt_val <- function(x, name) {
  #   if (is_pct_col(name, x)) {
  #     paste0(round(x * 100), "%")
  #   } else if (is_int_col(name, x)) {
  #     formatC(x, format = "d", big.mark = ".", decimal.mark = "")
  #   } else if (is.numeric(x)) {
  #     formatC(x, digits = 2, format = "f", big.mark = ".", decimal.mark = ",")
  #   } else {
  #     as.character(x)
  #   }
  # }
  
  is_pct_col <- function(nm, x) {
    # Escludi subito tipi data/tempo
    if (inherits(x, c("Date", "POSIXt"))) return(FALSE)
    # Deve essere numerico per essere percentuale
    if (!is.numeric(x)) return(FALSE)
    
    nm <- tolower(nm)
    # Solo prefisso "p_" all'inizio oppure indicatori espliciti nel nome
    if (grepl("^p_", nm, perl = TRUE)) return(TRUE)
    if (grepl("%|\\bperc\\b|\\bpct\\b", nm, perl = TRUE)) return(TRUE)
    
    # Heuristica sul range [0,1]
    rng <- range(x, na.rm = TRUE)
    if (!all(is.finite(rng))) return(FALSE)
    if (rng[1] < 0 || rng[2] > 1) return(FALSE)
    any(x != 0 & x != 1, na.rm = TRUE)
  }
  
  fmt_val <- function(x, name) {
    # Date come testo (evita qualsiasi x * 100)
    if (inherits(x, "Date"))   return(format(x, "%d/%m/%Y"))
    if (inherits(x, "POSIXt")) return(format(x, "%d/%m/%Y %H:%M"))
    
    if (is_pct_col(name, x)) {
      paste0(round(x * 100), "%")
    } else if (is_int_col(name, x)) {
      formatC(x, format = "d", big.mark = ".", decimal.mark = "")
    } else if (is.numeric(x)) {
      formatC(x, digits = 2, format = "f", big.mark = ".", decimal.mark = ",")
    } else {
      as.character(x)
    }
  }

  lines_needed <- function(text, col_width)
    ceiling(nchar(text, type = "width") / pmax(col_width, 1))
  
  # NUOVO: helper per righe Totale/Subtotale basato sulla prima colonna
  is_total_like <- function(x, rx) {
    x_chr <- trimws(as.character(x))
    nz <- nzchar(x_chr)
    out <- rep(FALSE, length(x_chr))
    out[nz] <- grepl(rx, x_chr[nz], perl = TRUE)
    out
  }
  
  # ── 1) input ---------------------------------------------------
  if (!is.list(tables) || length(tables) == 0)
    stop("`tables` deve essere una lista di data.frame.")
  if (!all(vapply(tables, is.data.frame, logical(1))))
    stop("Tutti gli elementi di `tables` devono essere data.frame.")
  
  n_tables  <- length(tables)
  subtitles <- rep_len(subtitles, n_tables)
  sources   <- rep_len(sources,   n_tables)
  
  # ── 2) label opzionali + lookup formule ---------------------------------
  formula_col <- NULL
  if (!is.null(header_df)) {
    if (!all(c("name", "label") %in% names(header_df)))
      stop("`header_df` deve avere colonne 'name' e 'label'.")
    # supporto sia 'formula' che 'formule'
    if ("formula" %in% names(header_df)) formula_col <- "formula"
    if (is.null(formula_col) && "formule" %in% names(header_df)) formula_col <- "formule"
    
    if (apply_labels) {
      for (tbl in tables) {
        hdr_sub <- header_df[header_df$name %in% names(tbl), , drop = FALSE]
        mapply(function(nm, lb) attr(tbl[[nm]], "label") <<- lb,
               hdr_sub$name, hdr_sub$label)
      }
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
  formulaStyle <- openxlsx::createStyle(fontSize = 8,
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
    formula_row  <- subtitle_row + 3           # riga “formule” (testo)
    data_start   <- subtitle_row + 4           # inizio dati
    data_end     <- data_start + nrow_df - 1   # fine dati (incluso eventuale Totale)
    source_row   <- data_end + 1               # riga fonte (subito dopo i dati)
    
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
    
    # 6c) riga “formule” (testo da header_df$formula/formule)
    formula_labels <- rep("", ncol_df)
    if (!is.null(header_df) && !is.null(formula_col)) {
      m <- match(names(df), header_df$name)
      has <- !is.na(m)
      txt <- rep("", length(m))
      txt[has] <- header_df[[formula_col]][m[has]]
      txt[is.na(txt)] <- ""
      formula_labels <- as.character(txt)
    }
    openxlsx::writeData(wb, sheet_name, as.list(formula_labels),
                        startRow = formula_row, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet_name, formulaStyle,
                       rows = formula_row, cols = 1:ncol_df, gridExpand = TRUE)
    
    # 6d) dati
    openxlsx::writeData(wb, sheet_name, df,
                        startRow = data_start, startCol = 1,
                        colNames = FALSE, rowNames = FALSE)
    
    # stili celle dati
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
    
    # 6e) totali + SUB-TOTALI: applica stili a tutte le righe che matchano total_regex
    if (nrow_df > 0) {
      idx_totlike <- which(is_total_like(df[[1]], total_regex))
      if (length(idx_totlike)) {
        for (k in idx_totlike) {
          total_row_idx <- data_start + k - 1
          
          # bordo su tutta la riga dei (sub)totali
          openxlsx::addStyle(wb, sheet_name, totalBorder,
                             rows = total_row_idx, cols = 1:ncol_df, gridExpand = TRUE)
          # prima colonna bold left
          openxlsx::addStyle(wb, sheet_name, totalText,
                             rows = total_row_idx, cols = 1, stack = TRUE)
          
          # altre colonne: stile numerico da totale
          for (j in 2:ncol_df) {
            if (is.numeric(df[[j]])) {
              nm <- names(df)[j]
              st_tot <- if (is_pct_col(nm, df[[j]])) {
                totalPct
              } else if (is_int_col(nm, df[[j]])) {
                totalInt
              } else {
                totalNum
              }
              openxlsx::addStyle(wb, sheet_name, st_tot,
                                 rows = total_row_idx, cols = j, stack = TRUE)
            }
          }
        }
      }
    }
    
    # 6f) fonte
    if (!is.null(sources[i]) && nzchar(sources[i])) {
      openxlsx::writeData(wb, sheet_name, paste0("Fonte: ", sources[i]),
                          startRow = source_row, startCol = 1)
      openxlsx::addStyle(wb, sheet_name, sourceStyle,
                         rows = source_row, cols = 1)
    } else {
      source_row <- data_end  # nessuna riga addizionale se non c'è la fonte
    }
    
    # 6g) larghezze colonne (accumulate) — includi ANCHE la riga "formule"
    for (j in seq_along(df)) {
      # dati formattati
      vals <- fmt_val(df[[j]], names(df)[j])
      vals <- vals[!is.na(vals) & vals != ""]
      data_chars <- if (length(vals)) max(nchar(vals, type = "width")) else 0L
      
      # testo formule per la colonna j (stringa singola)
      fl <- if (!is.null(formula_labels) && length(formula_labels) >= j) formula_labels[j] else ""
      if (is.na(fl)) fl <- ""
      formula_chars <- nchar(fl, type = "width")
      
      max_char <- max(data_chars, formula_chars, na.rm = TRUE)
      
      # margini e limiti come già facevi
      width_tmp <- ceiling(max_char * 1.10) + 2
      if (is.numeric(df[[j]])) {
        width_tmp <- max(width_tmp, 12)
      } else {
        width_tmp <- max(width_tmp, 10)
      }
      width_tmp <- min(width_tmp, 50)
      
      col_widths[j] <- max(col_widths[j], width_tmp, na.rm = TRUE)
    }
    
    # 6h) altezze dinamiche header
    eff_w <- col_widths[seq_len(ncol_df)]
    eff_w[eff_w == 0] <- 10
    eff_w[1] <- max(eff_w[1], 15)
    
    extra <- 0.25
    h_desc <- max(mapply(lines_needed, desc,     eff_w)); h_desc <- ceiling(h_desc + extra)
    h_name <- max(mapply(lines_needed, names_uc, eff_w)); h_name <- ceiling(h_name + extra)
    
    openxlsx::setRowHeights(wb, sheet_name, rows = hdr1,
                            heights = 15 + (h_desc - 1) * 15)
    openxlsx::setRowHeights(wb, sheet_name, rows = hdr2,
                            heights = 15 + (h_name - 1) * 15)
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
  col_widths[col_widths == 0] <- 10
  col_widths[1] <- max(col_widths[1], 15)
  openxlsx::setColWidths(wb, sheet_name,
                         cols = seq_along(col_widths),
                         widths = pmax(col_widths, 10))
  
  invisible(wb)
}

#' Riepilogo programmi con riga "Totale" (stile bind_rows)
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector con i nomi delle colonne di raggruppamento (>=1)
#' @param total_label_col  character(1) – una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG
#' @import dplyr rlang tidyselect
report_summarise_risorse <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  # --- controlli base ---
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  
  required_num <- c("RISORSE_COE", "COE", "COE_IMP", "COE_PAG")
  missing_req  <- setdiff(required_num, names(df))
  if (length(missing_req) > 0) {
    warning(sprintf("Colonne richieste assenti/non coerenti: %s. Restituisco NULL.",
                    paste(missing_req, collapse = ", ")))
    return(NULL)
  }
  
  # --- metto in char i gruppi per evitare problemi con factor/levels ---
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character))
  
  # --- riepilogo per gruppi ---
  by_grp <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(group_cols), RISORSE_COE)

  # --- riga totale costruita “alla tua maniera”: mutate + group_by + summarise ---
  blank_others <- setdiff(group_cols, total_label_col)
  
  total_row <- df2 %>%
    dplyr::mutate(
      # metto "" su tutte le altre colonne di gruppo
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      # e "Totale" (o label_total) solo su quella designata
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(group_cols), RISORSE_COE)
  
  # --- output finale ---
  appo <- dplyr::bind_rows(by_grp, total_row)
  
  # --- colonna con percentuali su colonna
  temp <- sum(by_grp$RISORSE_COE, na.rm = TRUE)
  appo <- appo %>% 
    mutate(TEMP = temp,
           p_RISORSE = round(RISORSE_COE/TEMP, 2)) %>% 
    select(-TEMP)
  
  return(appo)
}


#' Riepilogo programmi con riga "Totale" (stile bind_rows)
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector con i nomi delle colonne di raggruppamento (>=1)
#' @param total_label_col  character(1) – una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG
#' @import dplyr rlang tidyselect
report_summarise_coe <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  # --- controlli base ---
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  
  required_num <- c("RISORSE_COE", "COE", "COE_IMP", "COE_PAG")
  missing_req  <- setdiff(required_num, names(df))
  if (length(missing_req) > 0) {
    warning(sprintf("Colonne richieste assenti/non coerenti: %s. Restituisco NULL.",
                    paste(missing_req, collapse = ", ")))
    return(NULL)
  }
  
  # --- switch per N
  if (!"N" %in% names(df)) {
    df$N <- 1
  }

  # --- metto in char i gruppi per evitare problemi con factor/levels ---
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character))
  
  # --- riepilogo per gruppi ---
  by_grp <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      # N = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      COE         = sum(.data$COE,         na.rm = TRUE),
      COE_IMP     = sum(.data$COE_IMP,     na.rm = TRUE),
      COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_COE     = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE    / RISORSE_COE, 2)),
      p_COE_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_IMP/ RISORSE_COE, 2)),
      p_COE_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_PAG/ RISORSE_COE, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG)
  
  # --- riga totale costruita “alla tua maniera”: mutate + group_by + summarise ---
  blank_others <- setdiff(group_cols, total_label_col)
  
  total_row <- df2 %>%
    dplyr::mutate(
      # metto "" su tutte le altre colonne di gruppo
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      # e "Totale" (o label_total) solo su quella designata
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      # N = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      COE         = sum(.data$COE,         na.rm = TRUE),
      COE_IMP     = sum(.data$COE_IMP,     na.rm = TRUE),
      COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_COE     = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE    / RISORSE_COE, 2)),
      p_COE_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_IMP/ RISORSE_COE, 2)),
      p_COE_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_PAG/ RISORSE_COE, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG, N)
  
  # --- output finale ---
  dplyr::bind_rows(by_grp, total_row)
}


#' COE: riepilogo con N subtotali + totale (ordine intercalato)
#'
#' @param df           tibble/data.frame
#' @param key_col      character(1): colonna chiave (es. "AMM_TIT")
#' @param base_levels  character(): livelli/ordine base della chiave
#' @param subtotals    named list: list("Etichetta1" = c(val1, val2, ...), "Etichetta2" = c(...))
#' @param total_label  character(1): etichetta del totale (default "Totale")
#'
#' @return tibble con: key, N, RISORSE_COE, COE, p_COE, COE_IMP, p_COE_IMP, COE_PAG, p_COE_PAG
#' @import dplyr rlang tidyselect
#' @examples
#' # livelli base
#' lst_regioni <- c("ABRUZZO","BASILICATA","CALABRIA","CAMPANIA","MOLISE",
#'                  "PUGLIA","SARDEGNA","SICILIA",
#'                  "EMILIAROMAGNA","FRIULIVG","LAZIO","LIGURIA",
#'                  "LOMBARDIA","MARCHE","PABOLZANO","PATRENTO",
#'                  "PIEMONTE","TOSCANA","UMBRIA","VALLEDAOSTA","VENETO")
#'
#' # definizione subtotali
#' subtotals <- list(
#'   "Totale Sud" = c("ABRUZZO","BASILICATA","CALABRIA","CAMPANIA","MOLISE",
#'                    "PUGLIA","SARDEGNA","SICILIA"),
#'   "Totale CN"  = c("EMILIAROMAGNA","FRIULIVG","LAZIO","LIGURIA","LOMBARDIA",
#'                    "MARCHE","PABOLZANO","PATRENTO","PIEMONTE","TOSCANA",
#'                    "UMBRIA","VALLEDAOSTA","VENETO")
#' )
#'
#' # chiamata funzione
#' out <- report_summarise_cp_subtot(
#'   df = analisi,
#'   key_col = "AMM_TIT",
#'   base_levels = lst_regioni,
#'   subtotals = subtotals,
#'   total_label = "Totale"
#' )
#'
report_summarise_coe_subtot <- function(
    df,
    key_col,
    base_levels,
    subtotals = list(),
    total_label = "Totale"
) {
  stopifnot(is.data.frame(df))
  need <- c(key_col, "RISORSE_COE", "COE", "COE_IMP", "COE_PAG")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop(sprintf("Colonne mancanti: %s", paste(miss, collapse = ", ")))
  
  # --- costruzione final_levels intercalata ---
  covered <- character(0)
  final_levels <- character(0)
  
  if (length(subtotals)) {
    for (lab in names(subtotals)) {
      vals <- subtotals[[lab]]
      not_in_base <- setdiff(vals, base_levels)
      if (length(not_in_base))
        warning(sprintf("Valori non presenti in base_levels per '%s': %s", lab, paste(not_in_base, collapse = ", ")))
      blk <- intersect(base_levels, vals)   # mantiene l'ordine di base_levels
      final_levels <- c(final_levels, blk, lab)
      covered <- c(covered, blk)
    }
  }
  
  leftovers <- setdiff(base_levels, covered)
  if (length(leftovers))
    warning(sprintf("Alcuni livelli base non sono assegnati a nessun subtotale: %s. Li metto in coda.",
                    paste(leftovers, collapse = ", ")))
  
  final_levels <- c(final_levels, leftovers, total_label)
  
  dups <- unique(final_levels[duplicated(final_levels)])
  if (length(dups))
    warning(sprintf("final_levels contiene duplicati: %s", paste(dups, collapse = ", ")))
  
  key_sym <- rlang::sym(key_col)
  
  # --- switch per N
  if (!"N" %in% names(df)) {
    df$N <- 1
  }
  
  # porto la chiave a factor per stabilizzare i filtri; ordinamento finale lo imposto dopo
  df2 <- df %>%
    dplyr::mutate(!!key_sym := factor(.data[[key_col]], levels = base_levels))
  
  # --- dettaglio per ogni valore base presente ---
  by_key <- df2 %>%
    dplyr::group_by(.data[[key_col]]) %>%
    dplyr::summarise(
      # N           = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      COE         = sum(.data$COE,         na.rm = TRUE),
      COE_IMP     = sum(.data$COE_IMP,     na.rm = TRUE),
      COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_COE     = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE     / RISORSE_COE, 2)),
      p_COE_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_IMP / RISORSE_COE, 2)),
      p_COE_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_PAG / RISORSE_COE, 2))
    )
  
  # --- subtotali (uno per etichetta) ---
  subtables <- list()
  if (length(subtotals)) {
    for (lab in names(subtotals)) {
      blk <- intersect(base_levels, subtotals[[lab]])
      if (!length(blk)) next
      subtables[[lab]] <- df2 %>%
        dplyr::filter(.data[[key_col]] %in% blk) %>%
        dplyr::mutate(!!key_sym := lab) %>%
        dplyr::group_by(.data[[key_col]]) %>%
        dplyr::summarise(
          # N           = dplyr::n(),
          N = sum(.data$N, na.rm = TRUE),
          RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
          COE         = sum(.data$COE,         na.rm = TRUE),
          COE_IMP     = sum(.data$COE_IMP,     na.rm = TRUE),
          COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          p_COE     = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE     / RISORSE_COE, 2)),
          p_COE_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_IMP / RISORSE_COE, 2)),
          p_COE_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_PAG / RISORSE_COE, 2))
        )
    }
  }
  subtables_rows <- if (length(subtables)) dplyr::bind_rows(subtables) else NULL
  
  # --- totale complessivo ---
  tot_all <- df2 %>%
    dplyr::mutate(!!key_sym := total_label) %>%
    dplyr::group_by(.data[[key_col]]) %>%
    dplyr::summarise(
      # N           = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE),
      COE         = sum(.data$COE,         na.rm = TRUE),
      COE_IMP     = sum(.data$COE_IMP,     na.rm = TRUE),
      COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_COE     = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE     / RISORSE_COE, 2)),
      p_COE_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_IMP / RISORSE_COE, 2)),
      p_COE_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_COE == 0 | is.na(RISORSE_COE), 0, round(COE_PAG / RISORSE_COE, 2))
    )
  
  # --- bind + ordinamento finale intercalato ---
  out <- dplyr::bind_rows(by_key, subtables_rows, tot_all) %>%
    dplyr::mutate(!!key_sym := factor(.data[[key_col]], levels = final_levels)) %>%
    dplyr::arrange(.data[[key_col]])
  
  out
}



#' Riepilogo _TOT con riga "Totale" (stile bind_rows)
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1)
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, N, RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG
#' @import dplyr rlang tidyselect
report_summarise_cp <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  # --- controlli base ---
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  
  required_num <- c("RISORSE_TOT", "CP", "IMP", "PAG")
  missing_req  <- setdiff(required_num, names(df))
  if (length(missing_req) > 0) {
    warning(sprintf("Colonne richieste assenti/non coerenti: %s. Restituisco NULL.",
                    paste(missing_req, collapse = ", ")))
    return(NULL)
  }
  
  # --- switch per N
  if (!"N" %in% names(df)) {
    df$N <- 1
  }
  
  # --- evito problemi coi factor ---
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character))
  
  # --- riepilogo per gruppi ---
  by_grp <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      # N = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_TOT = sum(.data$RISORSE_TOT, na.rm = TRUE),
      CP          = sum(.data$CP,          na.rm = TRUE),
      IMP         = sum(.data$IMP,         na.rm = TRUE),
      PAG         = sum(.data$PAG,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_CP  = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(CP /  RISORSE_TOT, 2)),
      p_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(IMP / RISORSE_TOT, 2)),
      p_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(PAG / RISORSE_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG, N)
  
  # --- riga totale in stile tuo: mutate + summarise ---
  blank_others <- setdiff(group_cols, total_label_col)
  
  total_row <- df2 %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      # N = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_TOT = sum(.data$RISORSE_TOT, na.rm = TRUE),
      CP          = sum(.data$CP,          na.rm = TRUE),
      IMP         = sum(.data$IMP,         na.rm = TRUE),
      PAG         = sum(.data$PAG,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_CP  = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(CP /  RISORSE_TOT, 2)),
      p_IMP = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(IMP / RISORSE_TOT, 2)),
      p_PAG = dplyr::if_else(abs(RISORSE_COE) < 0.01 | RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(PAG / RISORSE_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG, N)
  
  dplyr::bind_rows(by_grp, total_row)
}


#' CP/IMP/PAG: riepilogo con N subtotali + totale (ordine intercalato)
#'
#' @param df           tibble/data.frame
#' @param key_col      character(1): colonna chiave
#' @param base_levels  character(): livelli/ordine base della chiave
#' @param subtotals    named list: list("Etichetta1" = c(val1, ...), "Etichetta2" = c(...))
#' @param total_label  character(1): etichetta del totale (default "Totale")
#'
#' @return tibble con: key, N, RISORSE_TOT, CP, p_CP, IMP, p_IMP, PAG, p_PAG
#' @import dplyr rlang tidyselect
#' @examples
#' # livelli base
#' lst_regioni <- c("ABRUZZO","BASILICATA","CALABRIA","CAMPANIA","MOLISE",
#'                  "PUGLIA","SARDEGNA","SICILIA",
#'                  "EMILIAROMAGNA","FRIULIVG","LAZIO","LIGURIA",
#'                  "LOMBARDIA","MARCHE","PABOLZANO","PATRENTO",
#'                  "PIEMONTE","TOSCANA","UMBRIA","VALLEDAOSTA","VENETO")
#'
#' # definizione subtotali
#' subtotals <- list(
#'   "Totale Sud" = c("ABRUZZO","BASILICATA","CALABRIA","CAMPANIA","MOLISE",
#'                    "PUGLIA","SARDEGNA","SICILIA"),
#'   "Totale CN"  = c("EMILIAROMAGNA","FRIULIVG","LAZIO","LIGURIA","LOMBARDIA",
#'                    "MARCHE","PABOLZANO","PATRENTO","PIEMONTE","TOSCANA",
#'                    "UMBRIA","VALLEDAOSTA","VENETO")
#' )
#'
#' # chiamata funzione
#' out <- report_summarise_cp_subtot(
#'   df = analisi,
#'   key_col = "AMM_TIT",
#'   base_levels = lst_regioni,
#'   subtotals = subtotals,
#'   total_label = "Totale"
#' )
#'
report_summarise_cp_subtot <- function(
    df,
    key_col,
    base_levels,
    subtotals = list(),
    total_label = "Totale"
) {
  stopifnot(is.data.frame(df))
  need <- c(key_col, "RISORSE_TOT", "CP", "IMP", "PAG")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop(sprintf("Colonne mancanti: %s", paste(miss, collapse = ", ")))
  
  covered <- character(0)
  final_levels <- character(0)
  
  if (length(subtotals)) {
    for (lab in names(subtotals)) {
      vals <- subtotals[[lab]]
      not_in_base <- setdiff(vals, base_levels)
      if (length(not_in_base))
        warning(sprintf("Valori non presenti in base_levels per '%s': %s", lab, paste(not_in_base, collapse = ", ")))
      blk <- intersect(base_levels, vals)
      final_levels <- c(final_levels, blk, lab)
      covered <- c(covered, blk)
    }
  }
  
  leftovers <- setdiff(base_levels, covered)
  if (length(leftovers))
    warning(sprintf("Alcuni livelli base non sono assegnati a nessun subtotale: %s. Li metto in coda.",
                    paste(leftovers, collapse = ", ")))
  
  final_levels <- c(final_levels, leftovers, total_label)
  dups <- unique(final_levels[duplicated(final_levels)])
  if (length(dups))
    warning(sprintf("final_levels contiene duplicati: %s", paste(dups, collapse = ", ")))
  
  key_sym <- rlang::sym(key_col)
  
  # --- switch per N
  if (!"N" %in% names(df)) {
    df$N <- 1
  }
  
  df2 <- df %>%
    dplyr::mutate(!!key_sym := factor(.data[[key_col]], levels = base_levels))
  
  by_key <- df2 %>%
    dplyr::group_by(.data[[key_col]]) %>%
    dplyr::summarise(
      # N           = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_TOT = sum(.data$RISORSE_TOT, na.rm = TRUE),
      CP          = sum(.data$CP,          na.rm = TRUE),
      IMP         = sum(.data$IMP,         na.rm = TRUE),
      PAG         = sum(.data$PAG,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_CP  = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(CP  / RISORSE_TOT, 2)),
      p_IMP = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(IMP / RISORSE_TOT, 2)),
      p_PAG = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(PAG / RISORSE_TOT, 2))
    )
  
  subtables <- list()
  if (length(subtotals)) {
    for (lab in names(subtotals)) {
      blk <- intersect(base_levels, subtotals[[lab]])
      if (!length(blk)) next
      subtables[[lab]] <- df2 %>%
        dplyr::filter(.data[[key_col]] %in% blk) %>%
        dplyr::mutate(!!key_sym := lab) %>%
        dplyr::group_by(.data[[key_col]]) %>%
        dplyr::summarise(
          # N           = dplyr::n(),
          N = sum(.data$N, na.rm = TRUE),
          RISORSE_TOT = sum(.data$RISORSE_TOT, na.rm = TRUE),
          CP          = sum(.data$CP,          na.rm = TRUE),
          IMP         = sum(.data$IMP,         na.rm = TRUE),
          PAG         = sum(.data$PAG,         na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          p_CP  = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(CP  / RISORSE_TOT, 2)),
          p_IMP = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(IMP / RISORSE_TOT, 2)),
          p_PAG = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(PAG / RISORSE_TOT, 2))
        )
    }
  }
  subtables_rows <- if (length(subtotals)) dplyr::bind_rows(subtables) else NULL
  
  tot_all <- df2 %>%
    dplyr::mutate(!!key_sym := total_label) %>%
    dplyr::group_by(.data[[key_col]]) %>%
    dplyr::summarise(
      # N           = dplyr::n(),
      N = sum(.data$N, na.rm = TRUE),
      RISORSE_TOT = sum(.data$RISORSE_TOT, na.rm = TRUE),
      CP          = sum(.data$CP,          na.rm = TRUE),
      IMP         = sum(.data$IMP,         na.rm = TRUE),
      PAG         = sum(.data$PAG,         na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_CP  = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(CP  / RISORSE_TOT, 2)),
      p_IMP = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(IMP / RISORSE_TOT, 2)),
      p_PAG = dplyr::if_else(RISORSE_TOT == 0 | is.na(RISORSE_TOT), 0, round(PAG / RISORSE_TOT, 2))
    )
  
  out <- dplyr::bind_rows(by_key, subtables_rows, tot_all) %>%
    dplyr::mutate(!!key_sym := factor(.data[[key_col]], levels = final_levels)) %>%
    dplyr::arrange(.data[[key_col]])
  
  out
}


#' Riepilogo ST_* già presenti (solo summary) + riga "Totale"
#' Nessuna pivot: le colonne ST_* devono essere già nel df.
#'
#' @param df               data.frame/tibble di input (con colonne ST_*)
#' @param group_cols       character vector: colonne di raggruppamento (>=1)
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, ST_NOMONIT, ST_NOSTART, ST_PROG, ST_GARA, ST_ESEC, ST_END, ST_TOT, p_ESEC
#' @import dplyr rlang tidyselect tidyr
report_summarise_stato <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  
  st_cols <- c("ST_NOMONIT","ST_NOSTART","ST_PROG","ST_GARA","ST_ESEC","ST_END")
  missing_st <- setdiff(st_cols, names(df))
  if (length(missing_st) > 0) {
    stop(sprintf("Mancano colonne ST_*: %s", paste(missing_st, collapse = ", ")))
  }
  
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character))
  
  # dettaglio per gruppi
  by_grp <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      ST_NOMONIT = sum(.data$ST_NOMONIT, na.rm = TRUE),
      ST_NOSTART = sum(.data$ST_NOSTART, na.rm = TRUE),
      ST_PROG    = sum(.data$ST_PROG,    na.rm = TRUE),
      ST_GARA    = sum(.data$ST_GARA,    na.rm = TRUE),
      ST_ESEC    = sum(.data$ST_ESEC,    na.rm = TRUE),
      ST_END     = sum(.data$ST_END,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  # riga "Totale" in stile tuo
  blank_others <- setdiff(group_cols, total_label_col)
  
  total_row <- df2 %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      ST_NOMONIT = sum(.data$ST_NOMONIT, na.rm = TRUE),
      ST_NOSTART = sum(.data$ST_NOSTART, na.rm = TRUE),
      ST_PROG    = sum(.data$ST_PROG,    na.rm = TRUE),
      ST_GARA    = sum(.data$ST_GARA,    na.rm = TRUE),
      ST_ESEC    = sum(.data$ST_ESEC,    na.rm = TRUE),
      ST_END     = sum(.data$ST_END,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  dplyr::bind_rows(by_grp, total_row)
}


#' Riepilogo stati ST_* da x_STATO_PROCEDURALE (pivot RISORSE_COE) + riga "Totale"
#' Stile bind_rows, con accorpamento a monte via case_when e livelli fissati prima della pivot.
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1)
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, ST_NOMONIT, ST_NOSTART, ST_PROG, ST_GARA, ST_ESEC, ST_END, ST_TOT, p_ESEC
#' @import dplyr tidyr rlang tidyselect
report_pivot_stato_old <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  if (!"x_STATO_PROCEDURALE" %in% names(df)) stop("Colonna 'x_STATO_PROCEDURALE' non trovata.")
  if (!"RISORSE_COE" %in% names(df))         stop("Colonna 'RISORSE_COE' non trovata.")
  
  # Livelli attesi (tutti i casi previsti)
  stato_levels <- c(
    "Non monitorato",
    "Non avviato",
    "In corso di progettazione",
    "In affidamento",
    "In esecuzione",
    "Eseguito"
  )
  
  # 1) Ricodifica a monte e forzo i livelli PRIMA della pivot
  df_prep <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character)) %>%
    dplyr::mutate(
      x_STATO_PROCEDURALE = as.character(.data$x_STATO_PROCEDURALE),
      x_STATO_PROCEDURALE = dplyr::case_when(
        .data$x_STATO_PROCEDURALE == "In avvio di progettazione" ~ "In corso di progettazione",
        TRUE ~ .data$x_STATO_PROCEDURALE
      ),
      # qui fisso i livelli: la pivot creerà tutte le colonne (names_expand=TRUE)
      x_STATO_PROCEDURALE = factor(.data$x_STATO_PROCEDURALE, levels = stato_levels)
    )
  
  # 2) group_by + sum + pivot + rename
  df2 <- df_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), .data$x_STATO_PROCEDURALE) %>%
    dplyr::summarise(RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols      = dplyr::all_of(group_cols),
      names_from   = x_STATO_PROCEDURALE,
      names_expand = TRUE,   # <-- grazie ai livelli fissati sopra, crea tutte le colonne
      values_from  = RISORSE_COE,
      values_fill  = 0
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::rename(
      ST_NOMONIT = `Non monitorato`,
      ST_NOSTART = `Non avviato`,
      ST_PROG    = `In corso di progettazione`,
      ST_GARA    = `In affidamento`,
      ST_ESEC    = `In esecuzione`,
      ST_END     = `Eseguito`
    )
  
  # garantisco le colonne ST_* (ultra-difensivo, in caso di input "sporco")
  st_cols <- c("ST_NOMONIT","ST_NOSTART","ST_PROG","ST_GARA","ST_ESEC","ST_END")
  for (cc in st_cols) if (!cc %in% names(df2)) df2[[cc]] <- 0
  
  # 3) dettaglio per gruppi (ST_TOT e p_ESEC inline)
  by_grp <- df2 %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  # 4) riga totale (somma colonne + etichette gruppi)
  totals_only <- by_grp %>%
    dplyr::summarise(
      ST_NOMONIT = sum(.data$ST_NOMONIT, na.rm = TRUE),
      ST_NOSTART = sum(.data$ST_NOSTART, na.rm = TRUE),
      ST_PROG    = sum(.data$ST_PROG,    na.rm = TRUE),
      ST_GARA    = sum(.data$ST_GARA,    na.rm = TRUE),
      ST_ESEC    = sum(.data$ST_ESEC,    na.rm = TRUE),
      ST_END     = sum(.data$ST_END,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    )
  
  blank_others <- setdiff(group_cols, total_label_col)
  total_row <- totals_only %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  dplyr::bind_rows(by_grp, total_row)
}



#' Riepilogo stati ST_* da x_STATO_PROCEDURALE (pivot RISORSE_COE) + riga "Totale"
#' Stile bind_rows, con accorpamento a monte via case_when e livelli fissati prima della pivot.
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1)
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, ST_NOMONIT, ST_NOSTART, ST_PROG, ST_GARA, ST_ESEC, ST_END, ST_TOT, p_ESEC
#' @import dplyr tidyr rlang tidyselect
report_pivot_stato <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  if (!"x_STATO" %in% names(df)) stop("Colonna 'x_STATO' non trovata.")
  if (!"RISORSE_COE" %in% names(df))         stop("Colonna 'RISORSE_COE' non trovata.")
  
  # Livelli attesi (tutti i casi previsti)
  stato_levels <- c(
    "NOMONIT",
    "NOSTART",
    "PROG",
    "GARA",
    "ESEC",
    "END"
  )
  
  # 1) Ricodifica a monte e forzo i livelli PRIMA della pivot
  df_prep <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character)) %>%
    dplyr::mutate(
      x_STATO = as.character(.data$x_STATO),
      x_STATO = dplyr::case_when(
        .data$x_STATO == "In avvio di progettazione" ~ "PROG",
        TRUE ~ .data$x_STATO
      ),
      # qui fisso i livelli: la pivot creerà tutte le colonne (names_expand=TRUE)
      x_STATO = factor(.data$x_STATO, levels = stato_levels)
    )
  
  # 2) group_by + sum + pivot + rename
  df2 <- df_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), .data$x_STATO) %>%
    dplyr::summarise(RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols      = dplyr::all_of(group_cols),
      names_from   = x_STATO,
      names_expand = TRUE,   # <-- grazie ai livelli fissati sopra, crea tutte le colonne
      values_from  = RISORSE_COE,
      values_fill  = 0
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::rename(
      ST_NOMONIT = `NOMONIT`,
      ST_NOSTART = `NOSTART`,
      ST_PROG    = `PROG`,
      ST_GARA    = `GARA`,
      ST_ESEC    = `ESEC`,
      ST_END     = `END`
    )
  
  # garantisco le colonne ST_* (ultra-difensivo, in caso di input "sporco")
  st_cols <- c("ST_NOMONIT","ST_NOSTART","ST_PROG","ST_GARA","ST_ESEC","ST_END")
  for (cc in st_cols) if (!cc %in% names(df2)) df2[[cc]] <- 0
  
  # 3) dettaglio per gruppi (ST_TOT e p_ESEC inline)
  by_grp <- df2 %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  # 4) riga totale (somma colonne + etichette gruppi)
  totals_only <- by_grp %>%
    dplyr::summarise(
      ST_NOMONIT = sum(.data$ST_NOMONIT, na.rm = TRUE),
      ST_NOSTART = sum(.data$ST_NOSTART, na.rm = TRUE),
      ST_PROG    = sum(.data$ST_PROG,    na.rm = TRUE),
      ST_GARA    = sum(.data$ST_GARA,    na.rm = TRUE),
      ST_ESEC    = sum(.data$ST_ESEC,    na.rm = TRUE),
      ST_END     = sum(.data$ST_END,     na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ST_TOT = ST_NOMONIT + ST_NOSTART + ST_PROG + ST_GARA + ST_ESEC + ST_END,
      p_ESEC = dplyr::if_else(ST_TOT == 0 | is.na(ST_TOT), 0,
                              round((ST_ESEC + ST_END) / ST_TOT, 2))
    )
  
  blank_others <- setdiff(group_cols, total_label_col)
  total_row <- totals_only %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(st_cols),
                  ST_TOT, p_ESEC)
  
  dplyr::bind_rows(by_grp, total_row)
}


#' Riepilogo RISORSE_COE per CHK_RITARDO (pivot) + riga "Totale" + RIT_TOT
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1), es. "CIS"
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, colonne CHK_RITARDO e RIT_TOT
#' @import dplyr tidyr rlang tidyselect
# report_pivot_ritardo <- function(
#     df,
#     group_cols,
#     total_label_col,
#     label_total = "Totale",
#     label_blank = ""
# ) {
#   stopifnot(is.data.frame(df))
#   if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
#   if (!all(group_cols %in% names(df))) {
#     stop(sprintf("Colonne di raggruppamento mancanti: %s",
#                  paste(setdiff(group_cols, names(df)), collapse = ", ")))
#   }
#   if (!total_label_col %in% group_cols) {
#     stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
#   }
#   if (!"CHK_RITARDO" %in% names(df)) stop("Colonna 'CHK_RITARDO' non trovata.")
#   if (!"RISORSE_COE" %in% names(df)) stop("Colonna 'RISORSE_COE' non trovata.")
#   
#   # livelli fissi per CHK_RITARDO
#   lst_chk <- c("non monitorato", "ritardo grave", "ritardo",
#                "scaduti", "in scadenza", "regolare", "concluso")
#   
#   # 1) factor con livelli ordinati
#   df_prep <- df %>%
#     dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character)) %>%
#     dplyr::mutate(
#       CHK_RITARDO = factor(.data$CHK_RITARDO, levels = lst_chk)
#     )
#   
#   # 2) group_by + sum + pivot
#   df2 <- df_prep %>%
#     dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), .data$CHK_RITARDO) %>%
#     dplyr::summarise(RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE), .groups = "drop") %>%
#     tidyr::pivot_wider(
#       id_cols      = dplyr::all_of(group_cols),
#       names_from   = CHK_RITARDO,
#       names_expand = TRUE,
#       values_from  = RISORSE_COE,
#       values_fill  = 0
#     ) %>%
#     dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))
#   
#   # 3) garantisco che tutte le colonne dei livelli siano presenti
#   for (cc in lst_chk) if (!cc %in% names(df2)) df2[[cc]] <- 0
#   
#   # 4) dettaglio: aggiungo RIT_TOT come somma delle colonne CHK_RITARDO
#   by_grp <- df2 %>%
#     dplyr::mutate(
#       RIT_TOT = rowSums(dplyr::across(dplyr::all_of(lst_chk)), na.rm = TRUE)
#     ) %>%
#     dplyr::select(dplyr::all_of(group_cols), dplyr::all_of(lst_chk), RIT_TOT)
#   
#   # 5) riga totale: somma delle colonne + RIT_TOT
#   totals_only <- by_grp %>%
#     dplyr::summarise(
#       dplyr::across(dplyr::all_of(lst_chk), ~ sum(.x, na.rm = TRUE)),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       RIT_TOT = rowSums(dplyr::across(dplyr::all_of(lst_chk)), na.rm = TRUE)
#     )
#   
#   blank_others <- setdiff(group_cols, total_label_col)
#   total_row <- totals_only %>%
#     dplyr::mutate(
#       !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
#       !!rlang::sym(total_label_col) := label_total
#     ) %>%
#     dplyr::select(dplyr::all_of(group_cols), dplyr::all_of(lst_chk), RIT_TOT)
#   
#   dplyr::bind_rows(by_grp, total_row)
# }

#' Riepilogo RISORSE_COE per CHK_RITARDO (pivot) + riga "Totale" + RIT_TOT
#' Stile bind_rows; livelli fissati a monte e mapping colonne RIT_* dopo la pivot.
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1), es. "CIS"
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, RIT_NOMONIT, RIT_NO_365, RIT_NO_060, RIT_NO_000, RIT_OK_000, RIT_OK_060, RIT_END, RIT_TOT
#' @import dplyr tidyr rlang tidyselect
report_pivot_ritardo <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  if (!"CHK_RITARDO" %in% names(df)) stop("Colonna 'CHK_RITARDO' non trovata.")
  if (!"RISORSE_COE" %in% names(df)) stop("Colonna 'RISORSE_COE' non trovata.")
  
  # livelli fissi per CHK_RITARDO (in ordine)
  rit_levels <- c("non monitorato", "ritardo grave", "ritardo",
                  "scaduti", "in scadenza", "regolare", "concluso")
  
  # 1) porto i gruppi a char e fisso i livelli PRIMA della pivot
  df_prep <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character)) %>%
    dplyr::mutate(
      CHK_RITARDO = factor(.data$CHK_RITARDO, levels = rit_levels)
    )
  
  # 2) group_by + sum + pivot
  df2 <- df_prep %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), .data$CHK_RITARDO) %>%
    dplyr::summarise(RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      id_cols      = dplyr::all_of(group_cols),
      names_from   = CHK_RITARDO,
      names_expand = TRUE,
      values_from  = RISORSE_COE,
      values_fill  = 0
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    # 3) mapping delle colonne della pivot verso le sigle RIT_*
    dplyr::rename(
      RIT_NOMONIT = `non monitorato`,
      RIT_NO_365  = `ritardo grave`,
      RIT_NO_060  = `ritardo`,
      RIT_NO_000  = `scaduti`,
      RIT_OK_000  = `in scadenza`,
      RIT_OK_060  = `regolare`,
      RIT_END     = `concluso`
    )
  
  # garantisco che tutte le RIT_* esistano (cintura e bretelle)
  rit_cols <- c("RIT_NOMONIT","RIT_NO_365","RIT_NO_060","RIT_NO_000","RIT_OK_000","RIT_OK_060","RIT_END")
  for (cc in rit_cols) if (!cc %in% names(df2)) df2[[cc]] <- 0
  
  # 4) dettaglio: totale riga
  by_grp <- df2 %>%
    dplyr::mutate(
      RIT_TOT = RIT_NOMONIT + RIT_NO_365 + RIT_NO_060 + RIT_NO_000 + RIT_OK_000 + RIT_OK_060 + RIT_END
    ) %>%
    dplyr::select(dplyr::all_of(group_cols), dplyr::all_of(rit_cols), RIT_TOT)
  
  # 5) riga totale
  totals_only <- by_grp %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(rit_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      RIT_TOT = RIT_NOMONIT + RIT_NO_365 + RIT_NO_060 + RIT_NO_000 + RIT_OK_000 + RIT_OK_060 + RIT_END
    )
  
  blank_others <- setdiff(group_cols, total_label_col)
  total_row <- totals_only %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::select(dplyr::all_of(group_cols), dplyr::all_of(rit_cols), RIT_TOT)
  
  dplyr::bind_rows(by_grp, total_row)
}



#' Riepilogo milestone risk (pivot su RISK_MILESTONE_SINTESI) + riga "Totale"
#' Stile bind_rows; lst_milestones fissato all'inizio della funzione.
#'
#' @param df               data.frame/tibble di input (già filtrato a monte se serve)
#' @param group_cols       character vector: colonne di raggruppamento (>=1), es. c("AMBITO","SEZIONE")
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, colonne per ciascun livello milestone, RISK_TOT
#' @import dplyr tidyr rlang tidyselect
report_summarise_milestones <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  # livelli milestone fissati qui
  lst_milestones <- c("RISK_NOMLS", "RISK_NOMNT", "RISK_RIT", "RISK_OK")
  
  # --- controlli base ---
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  if (!"RISK_MILESTONE_SINTESI" %in% names(df)) {
    stop("Colonna 'RISK_MILESTONE_SINTESI' non trovata.")
  }
  if (!"RISORSE_COE" %in% names(df)) {
    stop("Colonna 'RISORSE_COE' non trovata.")
  }
  
  # --- pipeline principale ---
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character)) %>%
    dplyr::mutate(
      RISK_MILESTONE_SINTESI = factor(.data$RISK_MILESTONE_SINTESI,
                                      levels = lst_milestones)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)),
                    .data$RISK_MILESTONE_SINTESI) %>%
    dplyr::summarise(RISORSE_COE = sum(.data$RISORSE_COE, na.rm = TRUE), .groups = "drop") %>%
    # integra colonne per avere tutti i casi
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(group_cols)),
      RISK_MILESTONE_SINTESI = lst_milestones,
      fill = list(RISORSE_COE = 0)
    ) %>%
    # pivot
    dplyr::arrange(.data$RISK_MILESTONE_SINTESI) %>%
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(group_cols),
      names_from  = RISK_MILESTONE_SINTESI,
      values_from = RISORSE_COE,
      values_fill = 0
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))
  
  # assicuro presenza di tutte le colonne milestone
  for (cc in lst_milestones) if (!cc %in% names(df2)) df2[[cc]] <- 0
  
  # calcolo totale
  by_grp <- df2 %>%
    dplyr::mutate(
      RISK_TOT = rowSums(dplyr::across(dplyr::all_of(lst_milestones)), na.rm = TRUE)
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(lst_milestones),
                  RISK_TOT)
  
  # --- riga "Totale"
  totals_only <- by_grp %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(lst_milestones), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(RISK_TOT = rowSums(dplyr::across(dplyr::all_of(lst_milestones)), na.rm = TRUE))
  
  blank_others <- setdiff(group_cols, total_label_col)
  total_row <- totals_only %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  dplyr::all_of(lst_milestones),
                  RISK_TOT)
  
  dplyr::bind_rows(by_grp, total_row)
}


#' Riepilogo COE_PAG vs TARGET_NEXT con p_TARGET_NEXT + riga "Totale"
#' Stile bind_rows; nessun filtro interno (gestiscilo a monte).
#'
#' @param df               data.frame/tibble di input
#' @param group_cols       character vector: colonne di raggruppamento (>=1), es. c("AMBITO","SEZIONE")
#' @param total_label_col  character(1): una delle group_cols su cui scrivere "Totale"
#' @param label_total      etichetta per il totale (default "Totale")
#' @param label_blank      etichetta per gli altri gruppi nella riga totale (default "")
#'
#' @return tibble con: group_cols, COE_PAG, TARGET_NEXT, p_TARGET_NEXT
#' @import dplyr rlang tidyselect
report_summarise_target <- function(
    df,
    group_cols,
    total_label_col,
    label_total = "Totale",
    label_blank = ""
) {
  # --- controlli base ---
  stopifnot(is.data.frame(df))
  if (length(group_cols) < 1) stop("Passa almeno una colonna in 'group_cols'.")
  if (!all(group_cols %in% names(df))) {
    stop(sprintf("Colonne di raggruppamento mancanti: %s",
                 paste(setdiff(group_cols, names(df)), collapse = ", ")))
  }
  if (!total_label_col %in% group_cols) {
    stop("'total_label_col' deve essere uno degli elementi di 'group_cols'.")
  }
  required_cols <- c("COE_PAG", "TARGET_NEXT")
  missing_req   <- setdiff(required_cols, names(df))
  if (length(missing_req) > 0) {
    stop(sprintf("Colonne richieste assenti/non coerenti: %s",
                 paste(missing_req, collapse = ", ")))
  }
  
  # --- difensivo: evito problemi con factor sui gruppi ---
  df2 <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(group_cols), as.character))
  
  # --- dettaglio per gruppi ---
  by_grp <- df2 %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      COE_PAG    = sum(.data$COE_PAG,    na.rm = TRUE),
      TARGET_NEXT = sum(.data$TARGET_NEXT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_TARGET_NEXT = dplyr::if_else(TARGET_NEXT == 0 | is.na(TARGET_NEXT),
                                     1,  # come nel tuo script
                                     round(COE_PAG / TARGET_NEXT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  COE_PAG, TARGET_NEXT, p_TARGET_NEXT)
  
  # --- riga "Totale" in stile tuo: mutate + summarise ---
  blank_others <- setdiff(group_cols, total_label_col)
  
  total_row <- df2 %>%
    dplyr::mutate(
      !!!setNames(as.list(rep(label_blank, length(blank_others))), blank_others),
      !!rlang::sym(total_label_col) := label_total
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      COE_PAG     = sum(.data$COE_PAG,     na.rm = TRUE),
      TARGET_NEXT = sum(.data$TARGET_NEXT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_TARGET_NEXT = dplyr::if_else(TARGET_NEXT == 0 | is.na(TARGET_NEXT),
                                     1,
                                     round(COE_PAG / TARGET_NEXT, 2))
    ) %>%
    dplyr::select(dplyr::all_of(group_cols),
                  COE_PAG, TARGET_NEXT, p_TARGET_NEXT)
  
  dplyr::bind_rows(by_grp, total_row)
}

