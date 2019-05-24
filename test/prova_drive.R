library("haven")

ROOT <- "/Volumes/GoogleDrive/Drive del team"

path <- file.path(ROOT, "/DATI/20190228/DASAS/DATABASE/oc_programmi.sas7bdat")

appo <- read_sas(path)
