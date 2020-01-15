# OC > Explorer > Perimetri
# Report





# ----------------------------------------------------------------------------------- #
# sintesi

report_sintesi <- function(perimetro, livelli_classe, debug=FALSE) {

  # totali
  n_tot <- perimetro %>% count() %>% .$n
  cp_tot <- perimetro %>% summarise(CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE)) %>% .$CP
  # pag_tot <- perimetro %>% summarise(PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>% .$PAG

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # sintesi
  sintesi <- perimetro %>%
    group_by(x_CICLO, CLASSE) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", CLASSE = "Totale") %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    bind_rows(perimetro %>%
                group_by(x_CICLO, CLASSE = "Totale") %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", CLASSE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    mutate(N_QUOTA = N / n_tot,
           CP_QUOTA = CP / cp_tot) %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    arrange(x_CICLO, CLASSE) %>%
    select(x_CICLO, CLASSE, N, N_QUOTA, CP, CP_QUOTA, PAG)

  if (debug == TRUE) {
    sintesi %>%
      write.csv2(file.path(TEMP, "report", "sintesi.csv"), na = "", row.names = FALSE)
  }
  return(sintesi)
}





# ----------------------------------------------------------------------------------- #
# macroaree

report_macroaree <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # macroaree
  macroaree <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_MACROAREA) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/macroarea
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/macroarea
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/macroarea
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per macroarea
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", x_MACROAREA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels=c("Sud", "Centro-Nord", "Nazionale", "Trasversale", "Estero"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_MACROAREA)

  if (debug == TRUE) {
    sintesi %>%
      write.csv2(file.path(TEMP, "report", "macroaree.csv"), na = "", row.names = FALSE)
  }



  return(macroaree)

}


#
# # macroaree_short <- macroaree %>%
# #   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
# #   unite("KEY", CLASSE, KEY, sep = "_") %>%
# #   spread(key = "KEY", value = "VALUE", fill = 0) %>%
# #   arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_MACROAREA)
#








# ----------------------------------------------------------------------------------- #
# regioni

report_regioni <- function(perimetro, livelli_classe, debug=FALSE) {


  # OLD:
  # # defactor
  # perimetro <- perimetro %>%
  #   mutate(CLASSE = as.character(CLASSE))
  # # MEMO: evita warning quando aggiungo "Totale"
  #
  # # semplifica non-regioni
  reg_cn <- c("001", "002", "003", "004", "005", "006",
              "007", "008", "009", "010", "011", "012")
  names(reg_cn) <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "VENETO", "FRIULI-VENEZIA GIULIA",
                     "LIGURIA",  "EMILIA-ROMAGNA", "TOSCANA", "UMBRIA", "MARCHE", "LAZIO")
  #
  reg_sud <- c("013", "014", "015", "016", "017", "018", "019", "020")
  names(reg_sud) <- c("ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA")

  # regioni
  appo <- perimetro %>%
    mutate(DEN_REGIONE = ifelse(COD_REGIONE %in% c(reg_cn, reg_sud), DEN_REGIONE, "ALTRO TERRITORIO"))
  # MEMO: semplifica DEN_REGIONE diversi da vera Regione in "ALTRO TERRITORIO"

  regioni <- appo %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_MACROAREA, x_REGIONE) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/macroarea/regione
    bind_rows(appo %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA, x_REGIONE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/macroarea/regione
    bind_rows(appo %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA, x_REGIONE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/macroarea/regione
    bind_rows(appo %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, x_MACROAREA, x_REGIONE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per macroarea/regione
    bind_rows(appo %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", x_MACROAREA, x_REGIONE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(x_MACROAREA = factor(x_MACROAREA, levels=c("Sud", "Centro-Nord", "Nazionale", "Trasversale", "Estero"))) %>%
    mutate(x_REGIONE = factor(x_REGIONE, levels = c(names(reg_cn), names(reg_sud), "ALTRO TERRITORIO"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_MACROAREA, x_REGIONE)


  if (debug == TRUE) {
    regioni %>%
      write.csv2(file.path(TEMP, "report", "regioni.csv"), na = "", row.names = FALSE)
  }

  return(regioni)

}





# regioni %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "regioni_sintesi.csv"), na = "", row.names = FALSE)
#


# ----------------------------------------------------------------------------------- #
# programmi


report_programmi <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # programmi
  programmi <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_PROGRAMMA) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per programma (ciclo/fondo/gruppo sono pari a programma per definizione)
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE = "Totale", x_PROGRAMMA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, x_PROGRAMMA)

  if (debug == TRUE) {
    programmi %>%
      write.csv2(file.path(TEMP, "report", "programmi.csv"), na = "", row.names = FALSE)
  }

  return(programmi)

}


# programmi %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "programmi_sintesi.csv"), na = "", row.names = FALSE)



# ----------------------------------------------------------------------------------- #
# nature

report_nature <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # nature
  nature <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/natura
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/natura
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/natura
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per natura
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", CUP_DESCR_NATURA) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(CUP_DESCR_NATURA = factor(CUP_DESCR_NATURA,
                                     levels=c("REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)",
                                              "ACQUISTO DI BENI",
                                              "ACQUISTO O REALIZZAZIONE DI SERVIZI",
                                              "CONCESSIONE DI INCENTIVI AD UNITA' PRODUTTIVE",
                                              "CONCESSIONE DI CONTRIBUTI AD ALTRI SOGGETTI (DIVERSI DA UNITA' PRODUTTIVE)",
                                              "NON CLASSIFICATO"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA)

  if (debug == TRUE) {
    nature %>%
      write.csv2(file.path(TEMP, "report", "nature.csv"), na = "", row.names = FALSE)
  }

  return(nature)


  # nature %>%
  #   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
  #   unite("KEY", CLASSE, KEY, sep = "_") %>%
  #   spread(key = "KEY", value = "VALUE", fill = 0) %>%
  #   write.csv2(file.path(tmp_path, "nature_sintesi.csv"), na = "", row.names = FALSE)

}







# ----------------------------------------------------------------------------------- #
# dimensioni

report_dimensioni <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # dimensioni
  dimensioni <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CLASSE_FIN) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(CLASSE_FIN = factor(CLASSE_FIN, levels = c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CLASSE_FIN)

  if (debug == TRUE) {
    dimensioni %>%
      write.csv2(file.path(TEMP, "report", "dimensioni.csv"), na = "", row.names = FALSE)
  }

  return(dimensioni)


}


# dimensioni %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "dimensioni_sintesi.csv"), na = "", row.names = FALSE)





# ----------------------------------------------------------------------------------- #
# dimensione per natura

report_dimensioni_nature <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # dimensione per natura
  dimensioni_nature <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA, CLASSE_FIN) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/natura/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/natura/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/natura/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, CUP_DESCR_NATURA, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per natura/dimensione
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", CUP_DESCR_NATURA, CLASSE_FIN) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(CUP_DESCR_NATURA = factor(CUP_DESCR_NATURA,
                                     levels=c("REALIZZAZIONE DI LAVORI PUBBLICI (OPERE ED IMPIANTISTICA)",
                                              "ACQUISTO DI BENI",
                                              "ACQUISTO O REALIZZAZIONE DI SERVIZI",
                                              "CONCESSIONE DI INCENTIVI AD UNITA' PRODUTTIVE",
                                              "CONCESSIONE DI CONTRIBUTI AD ALTRI SOGGETTI (DIVERSI DA UNITA' PRODUTTIVE)",
                                              "NON CLASSIFICATO"))) %>%
    mutate(CLASSE_FIN = factor(CLASSE_FIN, levels = c("0-100k", "100k-500k", "500k-1M", "1M-2M", "2M-5M", "5M-10M", "10M-infty"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA, CLASSE_FIN)


  if (debug == TRUE) {
    dimensioni_nature %>%
      write.csv2(file.path(TEMP, "report", "dimensioni_nature.csv"), na = "", row.names = FALSE)
  }

  return(dimensioni_nature)

}





# dimensioni_nature %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "dimensioni_nature_sintesi.csv"), na = "", row.names = FALSE)


# ----------------------------------------------------------------------------------- #
# stati
# MEMO: variabile OC

report_stati <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # stato per CP
  stati <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, OC_STATO_PROGETTO) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROGETTO) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROGETTO) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROGETTO) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per stato
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", OC_STATO_PROGETTO) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(OC_STATO_PROGETTO = factor(OC_STATO_PROGETTO, levels = c("Concluso", "Liquidato", "In corso", "Non avviato",  "Non determinabile"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, OC_STATO_PROGETTO)

  if (debug == TRUE) {
    stati %>%
      write.csv2(file.path(TEMP, "report", "stati.csv"), na = "", row.names = FALSE)
  }

  return(stati)

}



# stati %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "stati.csv"), na = "", row.names = FALSE)






# ----------------------------------------------------------------------------------- #
# stati procedurali
# MEMO: nuova variabile usata per IDRICO e DISSESTO


report_statiproc <- function(perimetro, livelli_classe, debug=FALSE) {

  # defactor
  perimetro <- perimetro %>%
    mutate(CLASSE = as.character(CLASSE))
  # MEMO: evita warning quando aggiungo "Totale"

  # stato per CP
  statiproc <- perimetro %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, OC_STATO_PROCEDURALE) %>%
    summarise(N = n(),
              CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
              PAG = sum(TOT_PAGAMENTI, na.rm = TRUE)) %>%
    # totali per ciclo/fondo + classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO, x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROCEDURALE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per ciclo + classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO, x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROCEDURALE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per classe/stato
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE, OC_STATO_PROCEDURALE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    # totali per stato
    bind_rows(perimetro %>%
                group_by(x_CICLO = "TOTALE", x_AMBITO = "TOTALE", x_GRUPPO = "TOTALE", CLASSE = "Totale", OC_STATO_PROCEDURALE) %>%
                summarise(N = n(),
                          CP = sum(OC_FINANZ_TOT_PUB_NETTO, na.rm = TRUE),
                          PAG = sum(TOT_PAGAMENTI, na.rm = TRUE))) %>%
    as.data.frame() %>%
    mutate(CLASSE = factor(CLASSE, levels = c(livelli_classe, "Totale"))) %>%
    mutate(OC_STATO_PROCEDURALE = factor(OC_STATO_PROCEDURALE,
                                                 levels =  c("Non avviato",
                                                             "In avvio di progettazione",
                                                             "In corso di progettazione",
                                                             "In affidamento",
                                                             "In esecuzione",
                                                             "Eseguito",
                                                             "Non determinabile"))) %>%
    arrange(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, OC_STATO_PROCEDURALE)


  if (debug == TRUE) {
    statiproc %>%
      write.csv2(file.path(TEMP, "report", "statiproc.csv"), na = "", row.names = FALSE)
  }

  return(statiproc)



}


# statiproc %>%
#   gather(key = "KEY", value = "VALUE", N, CP, PAG) %>%
#   unite("KEY", CLASSE, KEY, sep = "_") %>%
#   spread(key = "KEY", value = "VALUE", fill = 0) %>%
#   write.csv2(file.path(tmp_path, "statiproc.csv"), na = "", row.names = FALSE)








# ----------------------------------------------------------------------------------- #
# esporta tutto

export_report <- function(perimetro, livelli_classe, use_template=TRUE) {

  # load
  # if (exists("perimetro")) {
  #   print("perimetro... is ready!")
  # } else {
  #   perimetro <- read_csv2(file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), ".csv")))
  # }

  # libs
  library("openxlsx")

  # sintesi
  sintesi <- report_sintesi(perimetro, livelli_classe, debug=FALSE)
  macroaree <- report_macroaree(perimetro, livelli_classe, debug=FALSE)
  regioni <- report_regioni(perimetro, livelli_classe, debug=FALSE)
  programmi <- report_programmi(perimetro, livelli_classe, debug=FALSE)
  nature <- report_nature(perimetro, livelli_classe, debug=FALSE)
  dimensioni <- report_dimensioni(perimetro, livelli_classe, debug=FALSE)
  dimensioni_nature <- report_dimensioni_nature(perimetro, livelli_classe, debug=FALSE)
  stati <- report_stati(perimetro, livelli_classe, debug=FALSE)
  statiproc <- report_statiproc(perimetro, livelli_classe, debug=FALSE) # MEMO: opzione sospesa e da allineare a nuova variabile

  # spread
  # simply by CP
  macroaree_cp <- macroaree %>%
    select(-N, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = x_MACROAREA, value = CP, fill = 0) %>%
    arrange(CLASSE)

  # simply by N
  macroaree_n <- macroaree %>%
    select(-CP, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = x_MACROAREA, value = N, fill = 0) %>%
    arrange(CLASSE)

  # simply by CP
  nature_cp <- nature %>%
    select(-N, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    mutate(CUP_DESCR_NATURA = if_else(is.na(CUP_DESCR_NATURA), "ND", as.character(CUP_DESCR_NATURA))) %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA) %>%
    summarise(CP = sum(CP, na.rm = TRUE)) %>%
    spread(key = CUP_DESCR_NATURA, value = CP, fill = 0) %>%
    arrange(CLASSE)

  # simply by N
  nature_n <- nature %>%
    select(-CP, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    mutate(CUP_DESCR_NATURA = if_else(is.na(CUP_DESCR_NATURA), "ND", as.character(CUP_DESCR_NATURA))) %>%
    group_by(x_CICLO, x_AMBITO, x_GRUPPO, CLASSE, CUP_DESCR_NATURA) %>%
    summarise(N = sum(N, na.rm = TRUE)) %>%
    spread(key = CUP_DESCR_NATURA, value = N, fill = 0) %>%
    arrange(CLASSE)

  # simply by CP
  dimensioni_cp <- dimensioni %>%
    select(-N, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = CLASSE_FIN, value = CP, fill = 0) %>%
    arrange(CLASSE)

  # simply by N
  dimensioni_n <- dimensioni %>%
    select(-CP, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = CLASSE_FIN, value = N, fill = 0) %>%
    arrange(CLASSE)

  # simply by CP
  stati_cp <- stati %>%
    select(-N, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = OC_STATO_PROGETTO, value = CP, fill = 0) %>%
    arrange(CLASSE)

  # simply by N
  stati_n <- stati %>%
    select(-CP, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = OC_STATO_PROGETTO, value = N, fill = 0) %>%
    arrange(CLASSE)

  # simply by CP
  statiproc_cp <- statiproc %>%
    select(-N, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = OC_STATO_PROCEDURALE, value = CP, fill = 0) %>%
    arrange(CLASSE)

  # simply by N
  statiproc_n <- statiproc %>%
    select(-CP, -PAG) %>%
    filter(!(x_GRUPPO == "TOTALE" & x_AMBITO != "TOTALE")) %>%
    spread(key = OC_STATO_PROCEDURALE, value = N, fill = 0) %>%
    arrange(CLASSE)


  # ----------------------------------------------------------------------------------------- #
  # export

  # install.packages("openxlsx")
  # https://rdrr.io/cran/openxlsx/man/write.xlsx.html
  # https://rdrr.io/cran/openxlsx/f/inst/doc/formatting.pdf
  library("openxlsx")

  tab_ls <- list(sintesi = sintesi,
                 macroaree = macroaree,
                 macroaree_cp = macroaree_cp,
                 macroaree_n = macroaree_n,
                 regioni = regioni,
                 programmi = programmi,
                 nature = nature,
                 nature_cp = nature_cp,
                 nature_n = nature_n,
                 dimensioni = dimensioni,
                 dimensioni_cp = dimensioni_cp,
                 dimensioni_n = dimensioni_n,
                 dimensioni_nature = dimensioni_nature,
                 stati = stati,
                 stati_cp = stati_cp,
                 stati_n = stati_n,
                 statiproc = statiproc,
                 statiproc_cp = statiproc_cp,
                 statiproc_n = statiproc_n)

  if (use_template == FALSE) {
    # write all tables
    # MEMO: usato al primo giro per creare template (poi integrato a mano)
    write.xlsx(tab_ls, file = file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), "_elab0.xlsx")), asTable = TRUE, firstRow = TRUE, overwrite = TRUE)
    # CHK: verificare numero righe con formato...
  } else {

    # edit template
    # wb <- loadWorkbook(file.path(src_path, "elab_template.xlsx"))
    wb <- loadWorkbook(system.file("extdata", "elab_template.xlsx", package = "oc", mustWork = TRUE))
    for (i in seq_along(tab_ls)) {
      print(names(tab_ls)[i])
      removeTable(wb = wb, sheet = names(tab_ls)[i], table = getTables(wb, sheet = names(tab_ls)[i]))
      writeDataTable(wb, sheet = names(tab_ls)[i], x = tab_ls[[i]], stack = TRUE)
    }
    saveWorkbook(wb, file = file.path(OUTPUT, paste0(paste(focus, bimestre, sep = "_"), "_elab.xlsx")), overwrite = TRUE)

  }

  # new from scratch
  # wb <- createWorkbook()
  # addWorksheet(wb, "sintesi")
  # writeDataTable(wb, sheet = "sintesi", x = sintesi)
  # saveWorkbook(wb, file = file.path(tmp_path, "prova.xlsx"), overwrite = TRUE)

  # ALTRO:
  # install.packages("writexl")
  # library("writexl")
  # tab_ls <- list(sintesi = sintesi, programmi = programmi)
  # write_xlsx(tab_ls, path = file.path(tmp_path, "prova.xlsx"), col_names = TRUE)
  # MEMO: poche opzioni e passo ad altro...

}






# ----------------------------------------------------------------------------------------- #

