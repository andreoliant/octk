# Aggiorna dati generali in data/

# setup
# library("usethis")
# usethis::use_data_raw()

matrix_comuni <- read_csv2("data-raw/matrix_comuni.csv")
usethis::use_data(matrix_comuni, overwrite = TRUE)

po_riclass <- read_csv2("data-raw/po_riclass.csv")
usethis::use_data(po_riclass, overwrite = TRUE)

monithon_clp <- read_csv2("data-raw/monithon_clp.csv")
usethis::use_data(monithon_clp, overwrite = TRUE)

# progetti <- load_progetti(bimestre = bimestre, visualizzati=TRUE)
# usethis::use_data(progetti, overwrite = TRUE)
