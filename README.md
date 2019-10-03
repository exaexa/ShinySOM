# ShinySOM

FlowSOM-style workflow&tools packaged in a Shiny application.

## Installation

ShinySOM requires quite a bit of supporting packages, many of which are not yet on CRAN. First, you need `flowCore` and the related packages; these can be installed from Bioconductor (see https://www.bioconductor.org/packages/release/bioc/html/flowCore.html). After installing flowCore, you may get the rest of dependencies and ShinySOM via `git`, using `devtools`:

```r
devtools::install_github('tsieger/mhca')
devtools::install_github('exaexa/EmbedSOM')
devtools::install_github('exaexa/scattermore')
devtools::install_github('exaexa/shinyDendro')
devtools::install_github('exaexa/ShinySOM')
```

## Running ShinySOM

ShinySOM requires some scratch space for its own data (user data uploads,
temporary files, datasets, ...). Switch to the directory and create the
directories, if they do not exist:

```r
setwd('/some/directory/you/want/for/shinysom/')
dir.create('data')      # scratch space for the user files
dir.create('datasets')  # dataset storage
```

(The locations are in fact configurable, the directory names here match the
defaults.)

```r
library(ShinySOM)
ShinySOM()
```

This should open a browser window with ShinySOM GUI running. If not, follow the link that the command should have written to your terminal.

## Documentation

Rigorous documentation does not exist yet. Spam me with mail/issues with any questions.

Several non-central functions are simply not implemented yet -- if you want something available quickly, please open a feature request.
