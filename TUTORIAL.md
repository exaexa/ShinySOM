
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

Opened datasets can be explored in the usual 2-dimensional scatterplots; these are available in the **Data overview** tab. We can choose a set of parameters for horizontal and vertical axes, and a color scheme for the points different from the default density:

![overview](media/tutorial-overview.png?raw=true)

The screenshot shows a combination of forward and side scatter plots, colored by side-scatter signal width. (This clearly identifies e.g. the doublet clusters.)

We note that all expression colors in ShinySOM are plotted using the relatively standard ColorBrewer's **RdYlBu** palette, where blue shades represent "negative", gray-yellow "average" and red shades "positive" expression of the marker. The color scaling is based on the virtual normal distribution, which produces good results in a wide spectrum of use-cases. See [EmbedSOM source code](https://github.com/exaexa/EmbedSOM/blob/master/R/utils.R) for details.

Among other, the plot with CD11b marker clearly shows that the data still need to be transformed, which is the next step in the workflow.

## Transforming the data

Transformation editor is available under the tab **Transform&Scale**. Individual data dimensions (ie. cell parameters) can be selected and subjected to some of the common transformation methods:

![transform](media/tutorial-transform.png?raw=true)

In the screenshot, we have

1. selected all fluorescent markers for transformation,
2. chosen the logicle transformation (which is a good default for fluorescence-based flow cytometry)
3. selected two markers from the dataset for verifying the result of the transformation (here we use CD3 vs. CD19, which should produce relatively well separated clusters of T and B-cells)
4. increased the W parameter to avoid spreading of the negative population.

Finally, the **Apply** button is pressed (see 5 in the screenshot) to save the transformation in the dataset.

Other transforms can be applied as well, e.g. the ArcSinh transform is viable for mass-cytometry samles.

## Identifying the populations

Population identification (or "gating") is carried out in two steps, using FlowSOM to train a self-organizing map of the data, and then clustering on the resulting map. Additionally, ShinySOM uses the EmbedSOM view to show a quickly apprehensible 2-dimensional "guiding" picture for the dataset.

Here, we will first focus on "cleaning", i.e. removing the doublets, cell debris, dead cells and various other events, in order to create a dataset with only live singlets. (We will further dissect the individual cell types later.)

### Clustering and embedding

SOM training and embedding is done in the **Embedding** tab, as shown on the screenshot:

![embedding](media/tutorial-embed.png?raw=true)

The performed actions are as follows:

1. We have chosen a subset of the cell parameters that will be used in this step of dissection. To improve the cleaning, we have selected only the forward and side scatters, and the L/D marker (total 7 markers). Other parameters of the SOM may be selected -- e.g. size can be slightly increased to improve clustering resolution, various cell parameters can be assigned additional importance, and SOM training space metrics may be changed. Default values should work well in most usecases.
2. SOM training is executed by clicking the **Compute SOM** button. On the default SOM size, the training takes around 10 seconds; after that the resulting SOM is displayed in the plot on the right. SOM training process is randomized and the SOM generated on another computer will probably differ, but the count and relative positions of the identified clusters should stay roughly same.
3. The contents of the SOM plot can be colored by choosing different cell parameters. For example, choosing the L/D marker allows seeing a clearly defined cluster of dead cells in the upper part of the plot.
4. A better EmbedSOM-based view is obtained by clicking the **Compute embedding** button.
5. The resulting embedded cells can be observed in the lower right. Again, different markers may be highlighted in the picture -- for example, highlighted FSC-W allows seeing the embedded doublet population.

(Alternatively, we could have chosen to cluster the populations using all available markers at once. Although that would work correctly (and the detailed cell populations would be identifiable right away), for the purpose of this tutorial we split the dissection into 2 separate steps, in order to demonstrate the functionality of creating cell subsets.)

### iDendro-style dissection

Cell population are selected in a dendrogram that is built upon the FlowSOM clustering. This allows quick identification of large datasets of interest, and provides a relatively effortless way for separating the data in multiple dimensions at once. Additionally, gating bias is reduced, since the dendrogram is precomputed to separate the populations well in the multidimensional space, and diverging from it requires additional user effort.

The populations are selected in a [shinyDendro](https://github.com/exaexa/shinyDendro)-based interface:
![creating a subset dataset](media/tutorial-cluster.png?raw=true)

1. First, in the **Clustering** tab, we create a dendrogram structure using a selected hierarchical clustering algorithm.
2. The populations are selected in the dendrogram as such: After clicking (and focusing) the shinyDendro interface, keyboard is used to choose a single-letter cluster "key" (any of `a` to `z` and `0` to `9`); cluster key represents an unique assigned classification, which is, put more simply, the "cluster color". Clicking in the dendrogram causes the clicked branch of the tree to be painted by this chosen color. Resulting classification is immediately visible in the embedding on the right. Additionally, the embedding plot can be used for brushing -- drawing a rectangle around cells in the view highlights the selected cells in the dendrogram.
3. Cell parameters may be displayed next to the dendrogram as a heatmap, in order to aid cluster identification.
4. The embedding view can be quickly colored by any marker expression or converted to display the usual 2-dimensional dotplot (with the same possibility of brushing).
5. After the desired populations are selected, we can assign them interpretable names and save them. That makes the named populations available to the final parts of the workflow (mainly analysis and subset dissection).
6. The clustered cells may be immediately observed in a more complicated scatterplot structure, using the same interface as in Overview.

## Create a dataset subset

For demonstration, we will extract the identified live singlet cells and save them in a separate dataset using the **Dissection** tab:

![creating a subset dataset](media/tutorial-dissect.png?raw=true)

The "Spleen-clean" dataset will appear in the top bar, next to the original "Spleen" dataset.

## Working with the extracted subset

After reducing the dataset, we have embedded and dissected it again, to get a complete view of the subpopulations:

![creating a subset dataset](media/tutorial-cluster2.png?raw=true)

Compared to the processing of the original dataset, there are three main changes:
- it was _not_ necessary to transform the dataset again (the data stays transformed from the previous step)
- we have avoided the use of the scatter and L/D cell parameters for SOM and embedding, and focused only on the lineage markers
- we used 24x24 SOM size for increased detail of the small populations

The screenshot shows dissection of the populations into B cells, T cells, and several other cell types. (Clusters of macrophages, dendritic cells and neutrophils were selected by brushing in the embedded cells, which highlighted their corresponding data in the dendrogram interface.)

## Simple analyses

ShinySOM offers several useful analyses for getting a good overview of the contents of the selected populations and the differences between individual files. These are available as sub-tabs in the **Analysis** tab:

- Tab **Cluster expressions** allows quick visual comparison of expression of markers in files and clusters. This is useful e.g. for monitoring various activation-related markers in samples with different stimulation. Unfortunately, the dataset we chose does not contain a viable marker for this analysis; but the view can still be used at least for verifying marker expression strength is roughly equal in all files:
![Cluster expressions](media/tutorial-clustexpr.png?raw=true)
- Tab **Cluster size heatmap** provides a visual representation of relative cluster cell count in different files (the R-originated heatmap additionally attempts to group the clusters and files by relative similarity and draws a dendrogram to express it). The data is normalized by columns to show changes in cluster contents well. Precise cell counts for each cluster and file can be exported in a CSV file using the **Export data** tab.
![Clusters vs. files heatmap](media/tutorial-heatmap.png?raw=true)
- Tab **Compare files** allows seeing the difference between two different file groups in the embedding, giving a quick visual comparison of presence of various cell populations.
- Tab **Significance plots** improves this view by precisely expressing the significance of the cluster size difference by coloring based on statistical testing results. P-values from testing the cluster sizes from "control" and "experiment" group for one-sided inequality are used as a basis the coloring. Significance plots are designed for detection of small statistically significant differences in size of the populations. In our example, the significance plot confirms the findings from the heatmap. Because the statistical significance of the difference is relatively low (p-value is around 0.15 for both B and T cell clusters), the p-value slider needs to be adjusted in order to see the coloring:
![Significance plots](media/tutorial-sig.png?raw=true)

## Data export

Tab **Export data** provides a way to export the generated data for external programs:

- The whole dataset can be exported as FCS or CSV file, which allows post-processing in various other cytometry-related tools.
- SOM, clustering and other values can be exported for R in a RDS file, in order to be used with FlowSOM, EmbedSOM, or various pretty-plotting packages such as ggplot.
- Cell count in populations (which is the usual outcome of many analyses) can be exported directly as a CSV file.
