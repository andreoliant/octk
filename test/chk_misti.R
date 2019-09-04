# chk programmi misti con ":::"

# libs
devtools::load_all(path = ".")

# setup
oc_init(
  bimestre = "20190228",
  db_ver = "NIGHTLY",
  data_path = "/Users/aa/dati/oc",
  use_drive=TRUE,
  DEV_MODE=TRUE
)

# load
progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, debug = TRUE, light = TRUE)

# chk
appo <- progetti %>%
  filter(grepl(":::", OC_CODICE_PROGRAMMA))

misti_freq <- appo %>%
  group_by(OC_CODICE_PROGRAMMA, x_PROGRAMMA) %>%
  summarise(N = n(),
            CP = sum(OC_FINANZ_TOT_PUB_NETTO, ra.rm = TRUE)) %>%
  arrange(desc(CP))

chk <- misti_freq %>%
  anti_join(po_riclass, by = "OC_CODICE_PROGRAMMA")


