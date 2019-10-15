
# ShinySOM Tutorial

## Introduction

ShinySOM is a Shiny application for quick, highly interactive exploration of multidimensional flow cytometry data. The analysis model is derived from FlowSOM: The user inputs prepared samples in FCS files, uses SOMs and related analysis algorithms to dissect the data, and runs various analyses and clustering algorithms on the result.

This tutorial assumes that you have ShinySOM and all dependencies installed and running (as described in the [README](./README.md)). If not, please make sure ShinySOM can be run, or ask your local technical staff for help.

General ShinySOM workflow is as such:

1. The user inputs a set of compensated and quality-controlled samples, that are aggregated to form a **dataset**. In ShinySOM, the dataset abstracts a "batch" of analyzed data, usually from the the same experiment, with same parameters and using the same panel.
2. The data in the dataset are transformed to space viable for analysis (e.g. using the usual logicle or arcsinh transforms)
3. As in FlowSOM, a SOM is created to provide a dimensionality-reduced view of the whole dataset
4. The dataset is dissected into named subpopulations, which allows
  - analyzing and comparing the contents of population in original FCS files
  - exporting the populations as annotated FCS files or new subset datasets
5. The identified populations may be analyzed separately, repeating the whole workflow from point 3.

In this tutorial, we demonstrate this workflow on a [well-explored dataset used for monitoring immune cells in spleen (Sayes et al., 2016)](https://www.nature.com/articles/nri.2016.56). The dataset can be obtained from FlowRepository under [accession number ZZQY](https://flowrepository.org/id/FR-FCM-ZZQY).

## Loading data & creating a dataset

ShinySOM is a server application and operates its own storage of intermediate files and datasets. Data and dataset management is controlled from the top bar:

![top bar](media/tutorial-topbar.png?raw=true)

The buttons from left to right, allow:

- **Upload/download data**: moving the data to the server (or, eventually, downloading the results and exported files from the server)
- **Manage datasets**: aggregating and converting the uploaded FCS files into datasets (also cloning and deleting the existing datasets)
- **Load**: opening the dataset selected in the dropdown list
- **Save current**: force-saving the currently open dataset

After downloading the [data from FlowRepository](https://flowrepository.org/id/FR-FCM-ZZQY) online, we first want to upload them to the server. After clicking the **Upload/download data** button, we can choose the 4 FCS files from the dataset to upload using the **Browser** button:

![browse button](media/tutorial-upload.png?raw=true)

When the upload is complete, we continue by aggregating the files into a dataset. After clicking the **Manage datasets** button, an interface for creating the datasets appear:

![dataset create](media/tutorial-dscreate.png?raw=true)

In the interface, you will be able to

1. choose the 4 uploaded files on the server using the **Select dataset files** button,
2. pick several data columns from the FCS files relevant for the analysis (For the demonstration, we chose all columns except `Time`)
3. reduce the dataset slightly by subsampling (This step is unnecessary, but smaller datasets are processed and plotted much quicker, which helps to maintain the workflow interactivity. As a general rule, dataset exploration requires highly interactive environment and should not take more than around 1 million of cells.)
4. Choose the final name of the dataset (here we call it just "Spleen") and click the **Create dataset** button, which starts the aggregation process.

After a short while, the newly created dataset should appear in the list in the top bar, from where we can opened it using the **Load** button:

![dataset choose](media/tutorial-dschoose.png?raw=true)

## Viewing the dataset contents

Open datasets can be explored in the usual 2-dimensional scatterplots; these are available in the **Data overview** tab. We can choose a set of parameters for horizontal and vertical axes, and a color scheme for the points different from the default density:

![overview](media/tutorial-overview.png?raw=true)

The screenshot shows a combination of forward and side scatter plots, colored by side-scatter signal width. (This clearly identifies e.g. the doublet clusters.)

We note that all expression colors in ShinySOM are plotted using the relatively standard ColorBrewer's **RdYlBu** palette, where blue shades mean "negative", gray-yellow "mean" and red shades "positive" expression. The color scaling is based on the virtual normal distribution, which produces a good result in a wide spectrum of use-cases. See [EmbedSOM source code](https://github.com/exaexa/EmbedSOM/blob/master/R/utils.R) for details.

Among other, the plot with CD11b marker shows that the data still need to be transformed.

## Transforming the data

## Identifying the populations

### Clustering and embedding

### iDendro-style dissection

## Simple analysis

## Subset datasets

## Data export
