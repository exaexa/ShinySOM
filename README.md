# ShinySOM

FlowSOM-style workflow&tools packaged in a Shiny application.

## Installation

ShinySOM requires quite a bit of supporting packages, many of which are not yet on CRAN. First, you need `flowCore` and the related packages; these can be installed from Bioconductor (see https://www.bioconductor.org/packages/release/bioc/html/flowCore.html):

```r
install.packages("BiocManager")
BiocManager::install("flowCore")
```

After installing `flowCore`, you may get the rest of dependencies and ShinySOM via `git`, using `devtools`:

```r
devtools::install_github('tsieger/mhca')
devtools::install_github('exaexa/EmbedSOM')
devtools::install_github('exaexa/scattermore')
devtools::install_github('exaexa/shinyDendro')
devtools::install_github('exaexa/ShinySOM')
```

You may want to check the versions of `shiny` and `htmlwidgets`, as the older versions may contain issues that prevent e.g. the clustering interface from working correctly. Make sure the versions are reasonably up-to-date. Specifically, _avoid_ combining `shiny-1.3.2` with `htmlwidgets-1.5`.

```
> packageVersion('shiny')
[1] ‘1.4.0’
> packageVersion('htmlwidgets')
[1] ‘1.5’
```

## Running ShinySOM

ShinySOM requires some scratch space for its own data (user data uploads,
temporary files, datasets, ...). For simplicity, we assume that the code is run
on a Unix version of R (e.g. Linux or BSD), but the code should be transferable
to non-Unix operating systems as well with only minor changes to file naming.

Basically, you need to switch to the scratch-space directory and create the
directories, if they do not exist:

```r
setwd('/some/directory/you/want/for/shinysom/')
dir.create('data')      # scratch space for the user files
dir.create('datasets')  # dataset storage
```

(The locations are configurable, as a parameters of `ShinySOM()` function below.
The displayed directory names match the defaults.)

After that, you may start ShinySOM:

```r
library(ShinySOM)
ShinySOM()
```

That should open a browser window with ShinySOM GUI running. If not, follow the link that the command should have printed out into your console.

## Documentation

Rigorous documentation does not exist yet. Spam me with mail/issues with any questions.

Several non-central functions are simply not implemented yet -- if you want something available quickly, please open a feature request.
