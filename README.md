# ShinySOM

Installation:

```r
devtools::install_github('exaexa/EmbedSOM')
devtools::install_github('exaexa/DiffSOM')
devtools::install_github('exaexa/ShinySOM')
```

Before running, you need to create some space for data:

```r
dir.create('data')      # scratch space for the user files
dir.create('datasets')  # dataset storage
```

(the locations are configurable, these are the default directories)

Running is then easy:

```r
library(ShinySOM)
ShinySOM()
```

Rigorous documentation does not exist yet. Spam me with mail/issues with any questions.

Some functions are simply not implemented yet; if you want something available quickly, please open a feature request.
