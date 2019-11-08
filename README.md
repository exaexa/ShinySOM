# ShinySOM

FlowSOM-style workflow&tools packaged in a Shiny application.

## Fast installation (Docker)

The docker image with ShinySOM can be downloaded and started as such:

```sh
docker pull exaexa/shinysom:latest
docker run -d -p 8080:8080 -t exaexa/shinysom
```

After that, type [localhost:8080](http://localhost:8080/) into your web browser. If everything went right, you should be able to see and use a working ShinySOM installation.

For quick start, continue to the [tutorial](TUTORIAL.md).

## Manual installation

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
devtools::install_gitlab('exaexa/ShinySOM')
```

You may want to check the versions of `shiny` and `htmlwidgets`, as the older versions may contain issues that prevent e.g. the clustering interface from working correctly. Make sure the versions are reasonably up-to-date. Specifically, _avoid_ combining `shiny-1.3.2` with `htmlwidgets-1.5`.

```
> packageVersion('shiny')
[1] ‘1.4.0’
> packageVersion('htmlwidgets')
[1] ‘1.5’
```

### Starting the manually installed ShinySOM

ShinySOM requires some scratch space for its own data (user data uploads,
temporary files, datasets, ...). For simplicity, we assume that the code is run
on a Unix version of R (e.g. Linux or BSD), but the code should be transferable
to non-Unix operating systems as well with only minor changes to file naming.

Basically, you need to switch to the scratch-space directory and create the
directories, if they do not exist:

```r
setwd('/some/directory/you/want/for/shinysom/')   # fill this in
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

## Using ShinySOM

Generally, ShinySOM attempts to mimic most of the methodology used with FlowSOM; you may want to read the [FlowSOM article](https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.22625) to get an overview of the data processing.

There is also a [separate tutorial](TUTORIAL.md) that shows the most common features on an example dataset.

## FAQs and common issues

### Will ShinySOM help me with compensating the acquired samples?

No, ShinySOM focuses on multidimensional analysis of the data, compensation and quality-control of the samples is out of its scope. Please use the software supplied by your cytometer manufacturer for compensating the data. You may also try [FlowAI](https://bioconductor.org/packages/release/bioc/html/flowAI.html), [CytoRSuite](https://github.com/DillonHammill/CytoRSuite) and [Catalyst](https://github.com/HelenaLC/CATALYST) that contain viable tools for cleaning and re-compensating the samples.

### I want to export the analysis results, but clicking the buttons does not start any download. Why?

Analysis results are exported to the server side of the application. You may download it from the data transfer interface, accessible by clicking the "Upload/download data" button in the top bar.

### I found a bug, what to do now?

If ShinySOM does something unexpected or crashes (Shiny application crashes are presented by sudden graying of the screen), feel free to use the [GitLab issue tracker](https://gitlab.com/exaexa/ShinySOM/issues) to report what went wrong. Please include a reproducible description of how to trigger the problem in the report, including the OS used to run ShinySOM and the browser used for viewing the interface. Properly reported bugs may be fixed in several hours.

### Is ShinySOM suitable for producing graphics for publication?

Not directly -- all visualisations in ShinySOM are optimized for on-screen viewing only. You can get high-quality prints of any produced data using the standard R tools (e.g. `ggplot`); an example of that is available in [the tutorial](TUTORIAL.md).

### Where is the FlowSOM metaclustering and Consensus clustering?

FlowSOM-style metaclustering is perhaps the most noticeable part of FlowSOM workflow that we have modified. There has been a lot of discussion (most recently by [Pedersen&Olsen in Cytometry A](https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.23917)) about how the computational clustering output does not really match many biologically relevant expectations. ShinySOM tries to avoid this "algorithmic bias" by using the [idendro-style](https://github.com/tsieger/idendro) clustering tool ([shinyDendro](https://github.com/exaexa/shinyDendro)), that:

- gives the scientist a very fast way to select the well-separated clusters detected by FlowSOM by a simple mouse click,
- at the same time, allows him to precisely observe and quickly correct deficiencies in the detected data structure (again using just a few mouse clicks),
- avoids many anti-intuitive properties of the clustering algorithms, such as the selection of cluster number, and appearance of clusters of "uncategorizable" data.

### Is there any support for the more common embedding algorithms, like tSNE, UMAP or trimap?

In the interactive environment, the used visualization&embedding method is required to be very fast in order to be useful. We currently use EmbedSOM, because it can deliver a good embedding in several seconds (moreover, the SOM computation is shared with clustering). On the contrary, tSNE and UMAP usually take several minutes even on relatively small datasets.

Outside of ShinySOM, you may apply any dimensionality reduction algorithm to the data in an exported dataset. You can use e.g. the `ExportDF` function from the API to get the raw cell expression data in datasets, which can be used as input of tSNE or UMAP. (See the tutorial for more information on API use.)

### How to turn on the SIMD support?

You need EmbedSOM installation compiled with the matching compiler flags; ShinySOM will automatically use the result. See [EmbedSOM documentation](https://github.com/exaexa/EmbedSOM) for details. The expected speedup of SOM building and embedding ranges between 3x and 20x, usually around 5x.
