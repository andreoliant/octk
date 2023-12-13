# OC > Toolkit
# Utility per aggiornare gli oc_asset in data/

# WARNING: richiede oc_init



# ----------------------------------------------------------------------------------- #
# setup nel package

# po_riclass
po_riclass <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_riclass.csv")) %>%
  # MEMO: raw contiene NA per i programmi POC 2014-2020 > fanno casino perché join con progetti è sempre su OC_COD_PROGRAMMA
  filter(!is.na(OC_CODICE_PROGRAMMA))
usethis::use_data(po_riclass, overwrite = TRUE)

# po_riclass
# po_riclass_ext <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_riclass_ext.csv"))
# usethis::use_data(po_riclass_ext, overwrite = TRUE)

# matrix_comuni
comuni <- read_csv2(file.path(getwd(), "setup", "data-raw", "matrix_comuni.csv"))
usethis::use_data(comuni, overwrite = TRUE)
# MEMO: questo viene workflow sas di Luca

# matrix_op
matrix_op <- read_csv2(file.path(getwd(), "setup", "data-raw", "matrix_op.csv"))
usethis::use_data(matrix_op, overwrite = TRUE)

# matrix_opos
matrix_opos <- read_csv2(file.path(getwd(), "setup", "data-raw", "matrix_opos.csv"))
usethis::use_data(matrix_opos, overwrite = TRUE)

# matrix_ra_opos
matrix_ra_opos <- read_csv2(file.path(getwd(), "setup", "data-raw", "matrix_ra_opos.csv"))
usethis::use_data(matrix_ra_opos, overwrite = TRUE)

# matrix_ra_temi_fsc
matrix_ra_temi_fsc <- read_csv2(file.path(getwd(), "setup", "data-raw", "matrix_ra_temi_fsc.csv"))
usethis::use_data(matrix_ra_temi_fsc, overwrite = TRUE)

# categorie_cup
categorie_cup <- read_csv2(file.path(getwd(), "setup", "data-raw", "categorie_cup.csv"))
usethis::use_data(categorie_cup, overwrite = TRUE)

# po_linee_azioni
po_linee_azioni <- read_csv2(file.path(getwd(), "setup", "data-raw", "po_linee_azioni.csv"))
usethis::use_data(po_linee_azioni, overwrite = TRUE)

# categorie_ue
# make_matrix_ue()
# MEMO: va creato staticamente sulla base delle tabelle di contesto
categorie_ue <- read_csv2(file.path(getwd(), "setup", "data-raw", "categorie_ue.csv"))
usethis::use_data(categorie_ue, overwrite = TRUE)

# ra
ra <- read_csv2(file.path(getwd(), "setup", "data-raw", "ra.csv"))
usethis::use_data(ra, overwrite = TRUE)

# aree_temi_fsc
aree_temi_fsc <- read_csv2(file.path(getwd(), "setup", "data-raw", "aree_temi_fsc.csv"))
usethis::use_data(aree_temi_fsc, overwrite = TRUE)

# aree_temi_psc
aree_temi_psc <- read_csv2(file.path(getwd(), "setup", "data-raw", "aree_temi_psc.csv"))
usethis::use_data(aree_temi_psc, overwrite = TRUE)


# strum_att
strum_att <- read_csv2(file.path(getwd(), "setup", "data-raw", "strum_att.csv"))
usethis::use_data(strum_att, overwrite = TRUE)

# prog_comp
prog_comp <- read_csv2(file.path(getwd(), "setup", "data-raw", "prog_comp.csv"))
usethis::use_data(prog_comp, overwrite = TRUE)

# delib_cipe
delib_cipe <- read_csv2(file.path(getwd(), "setup", "data-raw", "delib_cipe.csv"))
usethis::use_data(delib_cipe, overwrite = TRUE)

# patt
patt <- read_csv2(file.path(getwd(), "setup", "data-raw", "patt.csv"))
usethis::use_data(patt, overwrite = TRUE)

# qsn
qsn <- read_csv2(file.path(getwd(), "setup", "data-raw", "qsn.csv"), col_types = "ccic")
usethis::use_data(qsn, overwrite = TRUE)

# tipologie_cup
tipologie_cup <- read_csv2(file.path(getwd(), "setup", "data-raw", "tipologie_cup.csv"))
usethis::use_data(tipologie_cup, overwrite = TRUE)

# flag_beniconf
flag_beniconf <- read_csv2(file.path(getwd(), "setup", "data-raw", "flag_beniconf.csv"))
usethis::use_data(flag_beniconf, overwrite = TRUE)

# stoplist
stoplist <- read_csv2(file.path(getwd(), "setup", "data-raw", "template_stoplist.csv"))
usethis::use_data(stoplist, overwrite = TRUE)

# safelist
safelist <- read_csv2(file.path(getwd(), "setup", "data-raw", "template_safelist.csv"))
usethis::use_data(safelist, overwrite = TRUE)

# fixlist
fixlist <- read_csv2(file.path(getwd(), "setup", "data-raw", "template_fixlist.csv"))
usethis::use_data(fixlist, overwrite = TRUE)

# moniton_clp
monithon_clp <- read_csv2(file.path(getwd(), "setup", "data-raw", "monithon_clp.csv"))
usethis::use_data(monithon_clp, overwrite = TRUE)

# fixlist
keyword <- read_csv2(file.path(getwd(), "setup", "data-raw", "template_query_keyword.csv"))
usethis::use_data(keyword, overwrite = TRUE)

# forma_giuridica_soggetti
forma_giuridica_soggetti <- read_csv2(file.path(getwd(), "setup", "data-raw", "forma_giuridica_soggetti.csv"), col_types = "ccccl")
usethis::use_data(forma_giuridica_soggetti, overwrite = TRUE)
