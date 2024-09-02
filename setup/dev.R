# OC > Toolkit
# development platform

# versione
oc_ver <- "0.7.1"
# HAND: 
# verificare ultimo invio in mail
# poi da modificare nel file DESCRIPTION

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
# library("tidyverse")
devtools::load_all(path = ".")
packageVersion("octk")

# setup
# oc_init(
#   bimestre = "20221031",
#   elab = "SETUP",
#   focus = "setup",
#   ver = "V.01",
#   # user = "Antonio",
#   data_path = "/media/antonio/Volume/dati/oc",
#   db_ver = "20221031.00",
#   use_drive = TRUE,
#   # drive_root = "/home/antonio/ExpanDrive/OC/Team Drives"
#   drive_root = "/home/antonio/ExpanDrive/OC/Shared Drives"
# )

oc_init(
  bimestre = "20240630",
  elab = "SETUP",
  focus = "setup",
  ver = "V.01",
  # user = "Antonio",
  data_path = "/media/aa/22F70AE20061C7C8/dati/oc",
  db_ver = "20240630.00",
  use_drive = TRUE,
  # drive_root = "/home/antonio/ExpanDrive/OC/Team Drives"
  drive_root = "/home/aa/oc_drive"
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
# usethis::use_vignette("perimetri")
# usethis::use_vignette("coesione")
# usethis::use_vignette("cis")
# usethis::use_vignette("psc")  
devtools::document()
devtools::load_all(path = ".")
options(rmarkdown.html_vignette.check_title = FALSE)
# install.packages(c("vroom", "tzdb"))
devtools::build_vignettes()

# dir.create("manuali")
# dir("doc", full.names=TRUE)
# file.copy("doc/oc.html", "manuali", overwrite=TRUE)
# knitr::knit("doc/oc.html", "manuali/oc.md")


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

packageVersion("octk")

# devtools::check(path = "~/coding/octk")
# devtools::build(pkg = ".", path = "/home/antonio/coding/octk/bkp")
devtools::build(pkg = ".", path = "/home/aa/coding/octk/bkp")
# MEMO: build to boundle "oc_X.X.X.tar.gz"

# install
# remove.packages("octk")
message(oc_ver)
# temp <- paste0("/home/antonio/coding/octk/bkp/octk_", oc_ver, ".tar.gz")
temp <- paste0("/home/aa/coding/octk/bkp/octk_", oc_ver, ".tar.gz")
install.packages(temp, repos = NULL, type="source")

# build as binary
# devtools::build(path = "/Users/aa/coding/oc", binary = TRUE)
# MEMO: build to binary "oc_0.1.0.tgz"
# WARNING: it is platform specific!

# CHK:
# ** byte-compile and prepare package for lazy loading
# Note: possible error in 'init_programmazione(usa_temi = FALSE, ': unused arguments (usa_temi = FALSE, add_713 = add_713, export = FALSE) 
# Note: possible error in 'init_programmazione(usa_temi = FALSE, ': unused arguments (usa_temi = FALSE, add_713 = add_713, export = FALSE) 
# Note: possible error in 'init_programmazione(usa_temi = FALSE, ': unused arguments (usa_temi = FALSE, export = FALSE) 


# ----------------------------------------------------------------------------------- #
# NIGHTLY to Drive

# local
system(
  paste0("cp README.md bkp/_src/_NIGHTLY/;",
         "cp DESCRIPTION bkp/_src/_NIGHTLY/;",
         "cp NAMESPACE bkp/_src/_NIGHTLY/;",
         "cp -r setup bkp/_src/_NIGHTLY/;",
         "cp -r R bkp/_src/_NIGHTLY/;",
         "cp -r data bkp/_src/_NIGHTLY/;",
         "cp -r vignettes bkp/_src/_NIGHTLY/;",
         "cp -r man bkp/_src/_NIGHTLY/;",
         "cp -r inst bkp/_src/_NIGHTLY/"
  )
)

# drive
# system(
#   paste0(# "DEV_BKP='/home/antonio/coding/octk/bkp/_src/_NIGHTLY/';",
#          "DEV_BKP='/home/aa/coding/octk/bkp/_src/_NIGHTLY/';",
#          # "GOOGLE='/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK/_src/_NIGHTLY/';",
#          # "GOOGLE='/home/antonio/ExpanDrive/OC/Shared Drives/TOOLS/OCTK/_src/_NIGHTLY/';",
#          "GOOGLE='/home/aa/oc_drive/TOOLS/OCTK/_src/_NIGHTLY/';",
#          'rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"'
#   )
# )

# TODO: da sostituire con cp perché impiega troppo

system(
  paste0(
    "DEV_BKP='/home/aa/coding/octk/bkp/_src/_NIGHTLY/.';",
    "GOOGLE='/home/aa/oc_drive/TOOLS/OCTK/_src/_NIGHTLY/';",
    'cp -r "$DEV_BKP" "$GOOGLE";'
    # 'rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"'
  )
)

# MEMO:
# devtools::load_all(path = "/home/antonio/ExpanDrive/OC/Shared Drives/TOOLS/OCTK/_src/_NIGHTLY")


# ----------------------------------------------------------------------------------- #
# backup source

system(
  paste0('VERS="octk_', oc_ver, '";',
         "mkdir bkp/_src/$VERS;", # MEMO: qesto va commentato quando si esegue di nuovo
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


# ----------------------------------------------------------------------------------- #
# google drive sync

# OLD
# system(
#   paste0("DEV_BKP='/home/antonio/coding/octk/bkp/';",
#          "GOOGLE='/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK';",
#          'rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE"'
#   )
# )

# src
system(
  paste0(# "DEV_BKP='/home/antonio/coding/octk/bkp/_src/octk_", oc_ver, "/';",
         "DEV_BKP='/home/aa/coding/octk/bkp/_src/octk_", oc_ver, "/';",
         # "GOOGLE='/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK/_src/octk_", oc_ver, "';",
         # "GOOGLE='/home/antonio/ExpanDrive/OC/Shared Drives/TOOLS/OCTK/_src/octk_", oc_ver, "';",
         "GOOGLE='/home/aa/oc_drive/TOOLS/OCTK/_src/octk_", oc_ver, "';",
         'rsync -rca --progress --delete "$DEV_BKP" "$GOOGLE";'
  )
)

# file tar.gz 
system(
  paste0(# "DEV_BKP='/home/antonio/coding/octk/bkp/octk_", oc_ver,".tar.gz';",
         "DEV_BKP='/home/aa/coding/octk/bkp/octk_", oc_ver,".tar.gz';",
         # 'GOOGLE="/home/antonio/ExpanDrive/OC/Shared Drives/TOOLS/OCTK/octk_', oc_ver, '.tar.gz";',
         # 'GOOGLE="/home/antonio/ExpanDrive/OC/Team Drives/TOOLS/OCTK/octk_', oc_ver, '.tar.gz";',
         'GOOGLE="/home/aa/oc_drive/TOOLS/OCTK/octk_', oc_ver, '.tar.gz";',
         'cp -r "$DEV_BKP" "$GOOGLE";'
  )
)


# ----------------------------------------------------------------------------------- #
# tag

# HAND: fare commit prima di inserire tag

system(
  paste0("git tag v", oc_ver)
  # paste0("git tag v", oc_ver, "_REV.01")
  )

# library(gitcreds)
# gitcreds_set() #.github_token
# gitcreds_get()
# VEDI NUOVA SOLUZIONE IN DRIVE SETUP

usethis::git_sitrep()


# HAND: push

# git push origin --tags
# https://stackoverflow.com/questions/18216991/create-a-tag-in-a-github-repository

# WARNING: ASPETTA CHE EXPANDRIVE FINISCA RSYNC PRIMA DI CHIUDERE E INVIARE MAIL!!!!!!!!!!!
