# funzioni per reportistica excel standard

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
style_border <- createStyle(border = c("top", "bottom", "left", "right"), fontColour = "#000000")

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
style_date <- createStyle(numFmt = "DATE", border = c("top", "bottom", "left", "right"))




# format automatico per righe e colonne
# TODO: copia da 20240831 > DEV > PNRR > schede aacc
