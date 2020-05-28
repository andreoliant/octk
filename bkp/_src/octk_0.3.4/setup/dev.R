# OC > Toolkit
# development platform

# versione
oc_ver <- "0.3.4"

# rm(list=ls())
library("devtools")


# ----------------------------------------------------------------------------------- #
# workflow generale
# RStudio + GitHub

# inizializzazione progetto
# $:
# git commit -m "first commit"
# git remote add origin https://github.com/andreoliant/oc.git
# git push -u origin master

# gestione modifiche
# $:
# git pull origin master
# ...
# git add .
# git commit -m "Something"
# git push -u origin master
# MEMO: su può fare direttamente da RStudio

# clona in altro folder fino a un tag
# git clone /Users/aa/coding/oc --branch v0.1.0 new_folder
# git clone <orign_path> --branch <tag_name> <dest_path>


# ----------------------------------------------------------------------------------- #
# init

# libs
devtools::load_all(path = ".")

# setup
# oc_init(
#   bimestre = "20191231",
#   db_ver = "NIGHTLY",
#   data_path = "/home/antonio/dati/oc",
#   use_drive=TRUE,
#   DEV_MODE=TRUE
#   )


# setup
oc_init(
  bimestre = "20200228",
  elab = "SETUP",
  focus = "setup",
  ver = "V.01",
  data_path = "/home/antonio/dati/oc",
  db_ver = "NIGHTLY",
  use_drive = TRUE,
  drive_root = "/home/antonio/ExpanDrive/OC/Team Drives"
)

# MEMO: con DEV_MODE la workarea è in locale octk/test


# ----------------------------------------------------------------------------------- #
# setup bimestre

# download
# copy data from GoogleDrive to local
# oc_init_data(
#   bimestre = "20190228",
#   data_path = "/Users/aa/dati/oc"
# )
# TODO: serve versione con package "googledrive"

# HAND: eseguire i blocchi in "chk_bimestre.R"

# HAND: eseguire i blocchi in "setup_bimestre.R"


# ----------------------------------------------------------------------------------- #
# documents

# usethis::use_build_ignore(c("test", "bkp", ".git"))
# usethis::use_vignette("oc")
devtools::document()
devtools::load_all(path = ".")


# ----------------------------------------------------------------------------------- #
# add packages

# usethis::use_package("dplyr") # 'tidyverse' is a meta-package and it is rarely a good idea to depend on it
# usethis::use_package("haven")
# usethis::use_package("readxl")


# ----------------------------------------------------------------------------------- #
# analisi peso variabili

# progetti <- load_progetti(bimestre = bimestre, visualizzati = TRUE, light = FALSE)
# for (var in names(progetti)) {
#   appo <- progetti %>% select(var)
#   print(paste0(var, ": ", object.size(appo)))
#   write.csv2(appo, file.path(TEMP, "prova_peso", paste0(var, ".csv")), row.names = FALSE)
# }


# ----------------------------------------------------------------------------------- #
# build as bundle


# build
devtools::load_all(path = ".")
# devtools::check(path = "/Users/aa/coding/oc")
devtools::build(pkg = ".", path = "/home/antonio/coding/octk/bkp")
# MEMO: build to boundle "oc_X.X.X.tar.gz"

# install
temp <- paste0("/home/antonio/coding/octk/bkp/octk_", oc_ver, ".tar.gz")
install.packages(temp, repos = NULL, type="source")

# build as binary
# devtools::build(path = "/Users/aa/coding/oc", binary = TRUE)
# MEMO: build to binary "oc_0.1.0.tgz"
# WARNING: it is platform specific!


# ----------------------------------------------------------------------------------- #
# backup source

system(
  paste0('VERS="octk_', oc_ver, '";',
         "mkdir bkp/_src/$VERS;",
         # "cp oc.Rproj bkp/_src/$VERS/;",
         "cp README.md bkp/_src/$VERS/;",
         "cp DESCRIPTION bkp/_src/$VERS/;",
         "cp NAMESPACE bkp/_src/$VERS/;",
         "cp -r setup bkp/_src/$VERS/;",
         "cp -r R bkp/_src/$VERS/;",
         "cp -r data bkp/_src/$VERS/;",
         "cp -r vignettes bkp/_src/$VERS/;",
         "cp -r man bkp/_src/$VERS/;",
         "cp -r inst bkp/_src/$VERS/"
         )
  )


# $:
# VERS="octk_0.2.1"
# cp oc.Rproj bkp/_src/$VERS/
# cp README.md bkp/_src/$VERS/
# cp DESCRIPTION bkp/_src/$VERS/
# cp NAMESPACE bkp/_src/$VERS/
# cp -r setup bkp/_src/$VERS/
# cp -r R bkp/_src/$VERS/
# cp -r data bkp/_src/$VERS/
# cp -r vignettes bkp/_src/$VERS/
# cp -r man bkp/_src/$VERS/
# cp -r inst bkp/_src/$VERS/


# ----------------------------------------------------------------------------------- #
# google drive sync

# TODO: sostituire con 2 copy (uno per folder in _src   e uno tar.gz)

# system(
#   paste0("DEV_BKP='/home/antonio/coding/octk/bkp/';",
#          "GOOGLE='/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK';",
#          'rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"'
#   )
# )


system(
  paste0("DEV_BKP='/home/antonio/coding/octk/bkp/';",
         "GOOGLE='/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK';",
         'VERS="octk_', oc_ver, '";',
         "cp -r $DEV_BKP/_src/$VERS/ $GOOGLE/_src/$VERS/;",
         "cp $DEV_BKP/$VERS.tgz $GOOGLE/$VERS.tgz;",
  )
)


# $:
# DEV_BKP='/Users/aa/coding/oc/bkp/'
# GOOGLE='/Volumes/GoogleDrive/Drive condivisi/TOOLS/OCTK'
# rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"


# ----------------------------------------------------------------------------------- #
# tag

# HAND: fare commit prima di inserire tag

system(
  paste0("git tag v", oc_ver)
  # paste0("git tag v", oc_ver, "-REV.01")
  )



