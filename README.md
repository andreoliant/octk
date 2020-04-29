# OpenCoesione Toolkit


Il toolkit contiene funzioni e processi per l'analisi dei dati pubblicati nella sezione "open data" del portale OpenCoesione.


# Installazione
Il package non è pubblicato su CRAN, va installato da sorgente:

```{r, echo = TRUE, eval = FALSE}
# download e installazione direttamente da GitHub
# install.packages("devtools")
devtools::install_github("andreoliant/oc")
library("octk")

# installazione direttamente da archivio disponible in locale
install.packages("path/to/local/octk_x.y.z.tar.gz", repos = NULL, type="source")
library("octk")

# caricamento sorgente da da folder di sviluppo
devtools::load_all(path = "path/to/local/octk")
# non è necessario invocare library("octk")
```


# Setup
Definire i parametri di configurazione prima di lanciare.

```{r, echo = TRUE, eval = FALSE}
# configurazione
bimestre <- "20181231"         # Bimestre OC di riferimento
focus <- "turismo"             # Prefisso per i file da esportare
workarea <- "/path/to/project" # Path della workarea (es. progetto RStudio)
data_path <- "/path/to/data"   # Path dei dati

library("oc")
```

Oppure con devtools:

```{r, echo = TRUE, eval = FALSE}
devtools::build(path = "/path/to/library/oc", binary = FALSE)

```

# Connessione a GitHub per sviluppo
Per contribuire allo sviluppo del package, creare in locale un progetto RStudio di sviluppo, File > New Project > Version Control > Git.
URL repository: https://github.com/andreoliant/oc
Alla creazione, RStudio sincronizza la cartella del progetto con il master su GitHub.

Poi in Terminale identificarsi con:

```bash
git config user.email "you@mail.com"
git config user.name "your-github-username"
```

Al primo push RStdio richiederà di inserire le proprie credenziali di GitHub,

