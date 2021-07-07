# OpenCoesione Toolkit


Il toolkit contiene funzioni e processi per l'analisi dei dati pubblicati nella sezione "open data" del portale OpenCoesione.


# Installazione
Vedi wiki in https://github.com/andreoliant/octk/wiki/Setup


# Connessione a GitHub per sviluppo
Per contribuire allo sviluppo del package, creare in locale un progetto RStudio di sviluppo, File > New Project > Version Control > Git:
* URL repository: https://github.com/andreoliant/octk
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

