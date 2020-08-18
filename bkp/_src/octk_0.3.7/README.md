# OpenCoesione Toolkit


Il toolkit contiene funzioni e processi per l'analisi dei dati pubblicati nella sezione "open data" del portale OpenCoesione.


# Installazione
Il package non è pubblicato su CRAN, va installato da sorgente:

```r
# download e installazione direttamente da GitHub
# install.packages("devtools")
devtools::install_github("andreoliant/oc")
library("octk")

# installazione direttamente da archivio disponible in locale
install.packages("path/to/local/octk_x.y.z.tar.gz", repos = NULL, type="source")
library("octk")

# caricamento sorgente da folder di sviluppo (vedi sotto)
devtools::load_all(path = "path/to/local/octk")
# non è necessario invocare library("octk")
```


# Setup
E' necessario avere i dati di OpenCoesione in folder locale con questa organizzazione:

```r
/20191231
  /progetti_light_20191231.csv
  /operazioni_light_20191231.csv
  /PROGETTI_PREESTESO.csv
  /finanziamenti_preesteso.sas7bdat

/20200228
  /progetti_light_20200228.csv
  /...
```

Il package può salvare i risultati in locale o direttamente nel Drive del team. Definire i parametri di configurazione nel setup di ogni nuova elaborazione:

```r

?oc_init # vedi manuale per informazioni

# setup standard per Drive
oc_init(
  bimestre = "20191231",
  data_path = "/path/to/data/folder",
  db_ver = "20200331",
  use_drive = TRUE,
  drive_root = "/path/to/filestream/volume",
  elab = "lev1",
  focus = "lev2")

# setup standard in locale
oc_init(
  bimestre = "20191231",
  data_path = "/path/to/data/folder",
  db_ver = "/path/to/db/folder",
  workarea = "/path/to/working/folder")

```


# Connessione a GitHub per sviluppo
Per contribuire allo sviluppo del package, creare in locale un progetto RStudio di sviluppo, File > New Project > Version Control > Git:
* URL repository: https://github.com/andreoliant/oc
* project directory name: octk

Alla creazione, RStudio sincronizza la cartella del progetto con il master su GitHub. Poi identificarsi nel terminale:

```bash
git config --global user.email "you@mail.com"
git config --global user.name "your-github-username"

# memorizza password (opzionale)
git config --global credential.helper wincred # windows
git config --global credential.helper osxkeychain # mac
git config --global credential.helper 'cache --timeout=10000000' # linux

```

Al primo push RStudio richiederà di inserire le proprie credenziali di GitHub.
I comandi sopra possono essere dati anche senza il parametro "--global" per un setup specifico.

