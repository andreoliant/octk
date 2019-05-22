

temp <- paste0("finanziamenti_esteso_", bimestre, ".csv")
delibere <- read_csv2(file.path(DATA, temp), guess_max = 5000)

# merge
progetti %>%
  filter(OC_CODICE_PROGRAMMA == "2007SI002FAPA1") %>%
  select(COD_LOCALE_PROGETTO) %>%
  inner_join(delibere %>%
               select(COD_LOCALE_PROGETTO, NUMERO_DEL_CIPE, ANNO_DEL_CIPE),
             by = "COD_LOCALE_PROGETTO") %>%
  count(NUMERO_DEL_CIPE, ANNO_DEL_CIPE)



progetti %>%
  filter(OC_CODICE_PROGRAMMA == "2007PI004MA013") %>%
  summarise(FTP = sum(OC_FINANZ_TOT_PUB_NETTO),
            COE = sum(OC_FINANZ_STATO_FSC_NETTO))

progetti %>%
  filter(x_CICLO == "2007-2013", x_AMBITO == "POC") %>%
  count(x_MACROAREA)

progetti %>%
  filter(x_CICLO == "2007-2013", x_AMBITO == "POC", x_MACROAREA == "Centro-Nord") %>%
  group_by(x_PROGRAMMA) %>%
  summarise(N = n(),
            CP =sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE))

temp <- c("PAC FRIULI VENEZIA GIULIA", "PAC PIEMONTE", "PAC UMBRIA", "PAC VALLE D'AOSTA")
chk <- progetti %>%
  filter(x_CICLO == "2007-2013", x_AMBITO == "POC", x_MACROAREA == "Centro-Nord") %>%
  filter(!(x_PROGRAMMA %in% temp)) %>%
  select(COD_LOCALE_PROGETTO, OC_TITOLO_PROGETTO, x_PROGRAMMA, OC_FINANZ_TOT_PUB_NETTO)

progetti %>%
  # filter(x_CICLO == "2007-2013", x_AMBITO == "FSC") %>%
  filter(OC_COD_CICLO == 1, x_AMBITO == "FSC") %>%
  # group_by(x_GRUPPO) %>%
  summarise(N = n(),
            CP =sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
            FSC =sum(FINANZ_STATO_FSC, na.rm = TRUE))



