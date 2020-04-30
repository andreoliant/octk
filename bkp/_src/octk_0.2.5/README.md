# OpenCoesione Toolkit


Il toolkit contiene funzioni e processi per l'analisi dei dati pubblicati nella sezione "open data" del portale OpenCoesione.

# Setup
Installa R.
Installa RStudio (opzionale).

# Installazione
Il package non è pubblicato su CRAN. Fare download da GitHub.

```bash
cd /path/to/library/oc
git pull origin master

```

Poi in R:

```{r, echo = TRUE, eval = FALSE}
install.packages("oc_X.X.X.tar.gz", repos = NULL, type="source")
```

# Esecuzione
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
