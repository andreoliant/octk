# workflow demo per GoogleDrive

rm(list=ls())

# libs
devtools::load_all(path = ".")

# setup
bimestre <- "20190228"
db_ver <- "20190620"
# drive_root <- "/Volumes/GoogleDrive/Drive condivisi"

# setup
oc_init(bimestre, db_ver,
        use_drive=TRUE)
