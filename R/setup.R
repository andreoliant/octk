# OC > Explorer > Main console

# rm(list=ls())

# libs
# MEMO: questo dovrebbe venire da DESCRIPTION...?
library(tidyverse)
# MEMO: questi sono in /R
# source("tools.R")
# source("get_categorie_UE.R")
# source("get_macroarea.R")
# source("get_stato_attuazione.R")
# source("get_dimensione_fin.R")

# versione oc
# MEMO: proviene da singolo tests/main.R
# oc_ver <- "20180430"
# oc_ver <- "20180630"

# paths
# setwd("/Users/aa/coding/oc_explorer/")
# src_path <- file.path(getwd(), this_path, "src")
# dat_path <- file.path(getwd(), this_path, "dat")

# switch(menu(c("List letters", "List LETTERS")) + 1,
#        cat("Nothing done\n"), letters, LETTERS)
#
# menu(c("Yes", "No"), title="Do you want this?")
# x <- readline("What is the value of x?")
#
# menu(c("Yes", "No"), title="Do you want this?")
# x <- readline("What is the value of x?")

INPUT <- file.path(getwd(), "tests", "input")
TEMP <- file.path(getwd(), "tests", "temp")
OUTPUT <- file.path(getwd(), "tests", "output")
DATA <- file.path("/Users/aa/dati", paste0("oc_", bimestre))

# loads
# MEMO: questo è caricato con il package
# po_riclass <- read_csv2("po_riclass.csv")


