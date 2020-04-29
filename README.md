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
Definire i parametri di configurazione prima di lanciare.

```r
# configurazione
bimestre <- "20181231"         # Bimestre OC di riferimento
focus <- "turismo"             # Prefisso per i file da esportare
workarea <- "/path/to/project" # Path della workarea (es. progetto RStudio)
data_path <- "/path/to/data"   # Path dei dati

library("oc")
```

Oppure con devtools:

```r
devtools::build(path = "/path/to/library/oc", binary = FALSE)

```

# Connessione a GitHub per sviluppo
Per contribuire allo sviluppo del package, creare in locale un progetto RStudio di sviluppo, File > New Project > Version Control > Git.
URL repository: https://github.com/andreoliant/oc
Alla creazione, RStudio sincronizza la cartella del progetto con il master su GitHub.

Poi identificarsi nel terminale:

```bash
git config --global user.email "you@mail.com"
git config --global user.name "your-github-username"

# memorizza password (opzionale)
git config --global credential.helper wincred # windows
git config --global credential.helper osxkeychain # mac
git config --global credential.helper 'cache --timeout=10000000' # linux

```



Al primo push RStudio richiederà di inserire le proprie credenziali di GitHub,

