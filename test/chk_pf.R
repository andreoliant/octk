

# 2014IT16RFOP022

chk <- progetti %>%
  filter(OC_CODICE_PROGRAMMA == "2014IT16RFOP022")

chk2 <- chk %>%
  filter(COD_INDICATORE_1 == 101) %>%
  select(DATA_INIZIO_EFF_ESECUZIONE, REALIZZATO_INDICATORE_1)



names(chk)
chk %>% count(COD_INDICATORE_1)
