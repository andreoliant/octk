
appo <- po_riclass %>%
  mutate(FLT = "")

regioni <- c(
  "PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
  "PA BOLZANO", "PA TRENTO", "FRIULI VENEZIA GIULIA", "EMILIA ROMAGNA", "P.A. TRENTO", "P.A. BOLZANO",
  
  "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO",
  "ABRUZZO", "MOLISE", "SARDEGNA",
  "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA"

  )

# patti <- c("PATTO GENOVA", "PATTO BOLOGNA",  "PATTO VENEZIA", 
#            "PATTO CAGLIARI", "PATTO FIRENZE", "PATTO MILANO",
#            "PATTO BARI",  "PATTO MESSINA", "PATTO NAPOLI", "PATTO CATANIA", "PATTO PALERMO", "PATTO REGGIO CALABRIA")
# 
# COMPLETAMENTO SALERNO-REGGIO CALABRIA
# PALAZZO DI GIUSTIZIA DI REGGIO CALABRIA


for (reg in regioni) {
  print(reg)
  appo <- appo %>%
    mutate(FLT = case_when(FLT %in% regioni ~ FLT,
                           grepl(reg, x_PROGRAMMA) ~ reg,
                           TRUE ~ "NAZ"))  
}

# MANCA IL BLOCCO DI CORREZIONI SU PATTI E ALTRO CON CALABRIA + FEASR


write_delim(appo, file.path(getwd(), "..", "oc", "data-raw", "po_riclass_ext.csv"), delim = ";", na = "")

