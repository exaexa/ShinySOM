
# ShinySOM Tutorial

## Introduction

ShinySOM is a Shiny application for quick, highly interactive exploration and dissection of multidimensional flow cytometry data. The analysis model is derived from FlowSOM: The user inputs prepared samples in FCS files, uses SOMs and related analysis algorithms to dissect the data, and runs various analyses and clustering algorithms on the result.

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

<img src="media/tutorial-topbar.png?raw=true" alt="Top bar" width="50%"/>

The buttons from left to right, allow:

- **Upload/download data**: moving the data to the server (or, eventually, downloading the results and exported files from the server)
- **Manage datasets**: aggregating and converting the uploaded FCS files into datasets (also cloning and deleting the existing datasets)
- **Load**: opening the dataset selected in the drop-down list
- **Save current**: force-saving the currently open dataset

After downloading the [data from FlowRepository](https://flowrepository.org/id/FR-FCM-ZZQY) online, we first want to upload them to the server. After clicking the **Upload/download data** button, we can choose the 4 FCS files from the dataset to upload using the **Browser** button:

<img src="media/tutorial-upload.png?raw=true" alt="browse button" width="50%">

When the upload is complete, we continue by aggregating the files into a dataset. After clicking the **Manage datasets** button, an interface for creating the datasets appear:

<img src="media/tutorial-dscreate.png?raw=true" alt="dataset create" width="50%">

In the interface, you will be able to

1. choose the 4 uploaded files on the server using the **Select dataset files** button,
2. pick several data columns from the FCS files relevant for the analysis (For the demonstration, we chose all columns except `Time`)
3. reduce the dataset slightly by sub-sampling (This step is unnecessary, but smaller datasets are processed and plotted much quicker, which helps to maintain the workflow interactivity. As a general rule, dataset exploration requires highly interactive environment and should not take more than around 1 million of cells.)
4. Choose the final name of the dataset (here we call it just "Spleen") and click the **Create dataset** button, which starts the aggregation process.

After a short while, the newly created dataset should appear in the list in the top bar, from where we can opened it using the **Load** button:

<img src="media/tutorial-dschoose.png?raw=true" alt="dataset choose" width="50%">

## Viewing the dataset contents

Opened datasets can be explored in the usual 2-dimensional scatterplots; these are available in the **Data overview** tab. We can choose a set of parameters for horizontal and vertical axes, and a color scheme for the points different from the default density:

<img src="media/tutorial-overview.png?raw=true" alt="overview" width="50%">

The screenshot shows a combination of forward and side scatter plots, colored by side-scatter signal width. (This clearly identifies e.g. the doublet clusters.)

We note that all expression colors in ShinySOM are plotted using the relatively standard ColorBrewer's **RdYlBu** palette, where blue shades represent "negative", gray-yellow "average" and red shades "positive" expression of the marker. The color scaling is based on the virtual normal distribution, which produces good results in a wide spectrum of use-cases. See [EmbedSOM source code](https://github.com/exaexa/EmbedSOM/blob/master/R/utils.R) for details.

Among other, the plot with CD11b marker clearly shows that the data still need to be transformed, which is the next step in the workflow.

## Transforming the data

Transformation editor is available under the tab **Transform&Scale**. Individual data dimensions (ie. cell parameters) can be selected and subjected to some of the common transformation methods:

<img src="media/tutorial-transform.png?raw=true" alt="transform" width="50%">

In the screenshot, we have

1. selected all fluorescent markers for transformation,
2. chosen the logicle transformation (which is a good default for fluorescence-based flow cytometry)
3. selected two markers from the dataset for verifying the result of the transformation (here we use CD3 vs. CD19, which should produce relatively well separated clusters of T and B-cells)
4. increased the W parameter to avoid spreading of the negative population.

Finally, the **Apply** button is pressed (see 5 in the screenshot) to save the transformation in the dataset.

Other transforms can be applied as well, e.g. the ArcSinh transform is viable for mass-cytometry samples.

## Identifying the populations

Population identification (or "gating") is carried out in two steps, using FlowSOM to train a self-organizing map of the data, and then clustering on the resulting map. Additionally, ShinySOM uses the EmbedSOM view to show a quickly apprehensible 2-dimensional "guiding" picture for the dataset.

Here, we will first focus on "cleaning", i.e. removing the doublets, cell debris, dead cells and various other events, in order to create a dataset with only live singlets. (We will further dissect the individual cell types later.)

### Clustering and embedding

SOM training and embedding is done in the **Embedding** tab, as shown on the screenshot:

<img src="media/tutorial-embed.png?raw=true" alt="embedding" width="50%">

The performed actions are as follows:

1. We have chosen a subset of the cell parameters that will be used in this step of dissection. To improve the cleaning, we have selected only the forward and side scatters, and the L/D marker (total 7 markers). Other parameters of the SOM may be selected -- e.g. size can be slightly increased to improve clustering resolution, various cell parameters can be assigned additional importance, and SOM training space metrics may be changed. Default values should work well in most use cases.
2. SOM training is executed by clicking the **Compute SOM** button. On the default SOM size, the training takes around 10 seconds; after that the resulting SOM is displayed in the plot on the right. SOM training process is randomized and the SOM generated on another computer will probably differ, but the count and relative positions of the identified clusters should stay roughly same.
3. The contents of the SOM plot can be colored by choosing different cell parameters. For example, choosing the L/D marker allows seeing a clearly defined cluster of dead cells in the upper part of the plot.
4. A better EmbedSOM-based view is obtained by clicking the **Compute embedding** button.
5. The resulting embedded cells can be observed in the lower right. Again, different markers may be highlighted in the picture -- for example, highlighted FSC-W allows seeing the embedded doublet population.

(Alternatively, we could have chosen to cluster the populations using all available markers at once. Although that would work correctly (and the detailed cell populations would be identifiable right away), for the purpose of this tutorial we split the dissection into 2 separate steps, in order to demonstrate the functionality of creating cell subsets.)

### iDendro-style dissection

Cell population are selected in a dendrogram that is built upon the FlowSOM clustering. This allows quick identification of large datasets of interest, and provides a relatively effortless way for separating the data in multiple dimensions at once. Additionally, gating bias is reduced, since the dendrogram is precomputed to separate the populations well in the multidimensional space, and diverging from it requires additional user effort.

The populations are selected in a [shinyDendro](https://github.com/exaexa/shinyDendro)-based interface:

<img src="media/tutorial-cluster.png?raw=true" alt="creating a subset dataset" width="50%">

1. First, in the **Clustering** tab, we create a dendrogram structure using a selected hierarchical clustering algorithm.
2. The populations are selected in the dendrogram as such: After clicking (and focusing) the shinyDendro interface, keyboard is used to choose a single-letter cluster "key" (any of `a` to `z` and `0` to `9`); cluster key represents an unique assigned classification, which is, put more simply, the "cluster color". Clicking in the dendrogram causes the clicked branch of the tree to be painted by this chosen color. Resulting classification is immediately visible in the embedding on the right. Additionally, the embedding plot can be used for brushing -- drawing a rectangle around cells in the view highlights the selected cells in the dendrogram.
3. Cell parameters may be displayed next to the dendrogram as a heatmap, in order to aid cluster identification.
4. The embedding view can be quickly colored by any marker expression or converted to display the usual 2-dimensional scatter plot (with the same possibility of brushing).
5. After the desired populations are selected, we can assign them interpretable names and save them. That makes the named populations available to the final parts of the workflow (mainly analysis and subset dissection).
6. The clustered cells may be immediately observed in a more complicated scatter plot structure, using the same interface as in Overview.

## Create a dataset subset

For demonstration, we will extract the identified live singlet cells and save them in a separate dataset using the **Dissection** tab:

<img src="media/tutorial-dissect.png?raw=true" alt="creating a subset dataset" width="50%">

The "Spleen-clean" dataset will appear in the top bar, next to the original "Spleen" dataset.

## Working with the extracted subset

After reducing the dataset, we have embedded and dissected it again, to get a complete view of the subpopulations:

<img src="media/tutorial-cluster2.png?raw=true" alt="creating a subset dataset" width="50%">

Compared to the processing of the original dataset, there are three main changes:

- it was _not_ necessary to transform the dataset again (the data stays transformed from the previous step)
- we have avoided the use of the scatter and L/D cell parameters for SOM and embedding, and focused only on the lineage markers
- we used 24x24 SOM size for increased detail of the small populations

The screenshot shows dissection of the populations into B cells, T cells, and several other cell types. (Clusters of macrophages, dendritic cells and neutrophils were selected by brushing in the embedded cells, which highlighted their corresponding data in the dendrogram interface.)

## Simple analyses

ShinySOM offers several useful analyses for getting a good overview of the contents of the selected populations and the differences between individual files. These are available as sub-tabs in the **Analysis** tab:

- Tab **Cluster expressions** allows quick visual comparison of expression of markers in files and clusters. This is useful e.g. for monitoring various activation-related markers in samples with different stimulation. Unfortunately, the dataset we chose does not contain a viable marker for this analysis; but the view can still be used at least for verifying marker expression strength is roughly equal in all files:

<img src="media/tutorial-clustexpr.png?raw=true" alt="Cluster expressions" width="50%">

- Tab **Cluster size heatmap** provides a visual representation of relative cluster cell count in different files (the R-originated heatmap additionally attempts to group the clusters and files by relative similarity and draws a dendrogram to express it). The data is normalized by columns to show changes in cluster contents well. Precise cell counts for each cluster and file can be exported in a CSV file using the **Export data** tab.

<img src="media/tutorial-heatmap.png?raw=true" alt="Clusters vs. files heatmap" width="50%">

- Tab **Compare files** allows seeing the difference between two different file groups in the embedding, giving a quick visual comparison of presence of various cell populations.

- Tab **Significance plots** improves this view by precisely expressing the significance of the cluster size difference by coloring based on statistical testing results. P-values from testing the cluster sizes from "control" and "experiment" group for one-sided inequality are used as a basis the coloring. Significance plots are designed for detection of small statistically significant differences in size of the populations. In our example, the significance plot confirms the findings from the heatmap. Because the statistical significance of the difference is relatively low (p-value is around 0.15 for both B and T cell clusters), the p-value slider needs to be adjusted in order to see the coloring:

<img src="media/tutorial-sig.png?raw=true" alt="Significance plots" width="50%">

## Data export

Tab **Export data** provides a way to export the generated data for external programs:

- Cell counts of individual populations (which are the usual output of many experiments) can be exported directly as a CSV file.
- The whole dataset can be exported as FCS or CSV file, which allows post-processing in various other cytometry-related tools.
- While datasets can be exported in RDS format. That allows interoperability with other packages that use the same data structures (most notably any packages that work with FlowSOM). Exported dataset objects may additionally be used for batch-processing using the ShinySOM batch API (see below).

# Using the batch API

## Why batch processing?

There are various limitations that prevent efficient interactive work with large datasets. Those consist mostly of necessary delays while processing large data -- although ShinySOM tries to do the best with large datasets, timing and resource usage of the involved algorithms will always cross the "bearable" limit for interactive usage if the datasets grow enough.

One possible alleviation of this problem is to downscale the datasets. Because ShinySOM is designed to cope well with a few millions of loaded cells, the downsampling is not even required for many datasets (at least not for common experiments); and creates only a minor statistical loss even in larger experiments. Despite of that, downsampled datasets should not be used for obtaining any final results.

Batch API of ShinySOM is designed for alleviating this problem: You can prepare the analysis (create a "gating scheme") in the interactive interface using a slightly reduced cell sample, export the analysis, and automatically apply it to any number of incoming FCS files.

## Exporting and loading the analysis

The dataset objects exported from the ShinySOM **Export data** tab are useful both as sources of actual cell data, and as sources of metadata about analysis. The analysis can be reduced to metadata, but still carries full information necessary for reproducing the workflow -- the reduced datasets are obtained using **Export analysis RDS** button. Main advantage of exporting just the metadata is the size of resulting data, which is reduced to several kilobytes.

For demonstration, we have exported the analysis files from both datasets we have created before, using the **Export analysis RDS button** to create files `step1.shinysom` and `step2.shinysom`. After that, we have transferred these files to the batch processing environment.

The dataset files are formatted as standard RDS and can be directly loaded in R:

```r
step1 <- readRDS('step1.shinysom')
step2 <- readRDS('step2.shinysom')
```

The loaded structures contain some interesting data about the analysis, e.g. the list of all transformations applied to the data is available in `step1$transforms` and the FlowSOM-compatible map object for the final data can be obtained as `step2$map`. While this may be already useful, ShinySOM provides own functions that simplify the batch processing.

## Available functions

- Function `LoadCells` creates dataset objects from FCS files, in a manner
  similar to `AggregateFlowFrames` from FlowSOM.
- Function `Process` applies the stored analysis to a data file.
- Functions `ExportDF`, `ExportFlowFrame` and `PopulationSizes` can be used to
  export data and statistics from the processed datasets (in a similar manner
  as in **Export data** tab),
- Function `Dissect` can be used to reduce the datasets to annotated subsets,
  just like in **Dissection** tab. To help that purpose, function
  `PopulationKeys` returns all annotated subsets available in the dataset.

## Batch-processing the tutorial dataset

Finally, batch processing the original, non-subsampled datasets is done by reading the whole FCS files using `LoadCells` functions, applying the analysis and dissection steps using the `Process` and `Dissect` functions.

First, we read the analysis objects and full 2 million cells of FCS contents:

```r
library(ShinySOM)

step1 <- readRDS('step1.shinysom')
step2 <- readRDS('step2.shinysom')
dataset <- LoadCells(
 c('21-10-15_Tube_028.fcs',
   '21-10-15_Tube_030.fcs',
   '21-10-15_Tube_031.fcs',
   '21-10-15_Tube_032.fcs'))
```

After the cells are loaded, we can apply the analysis and look at the result:

```r
dataset <- Process(dataset, step1)
PopulationKeys(dataset)
```

After the processing (mapping and embedding) is done, the later command should print out the keys for the available populations:
```
             2              b              d              l 
    "Doublets"       "Debris"   "Dead cells" "Single cells" 
```

From that, we want to reduce the dataset to the subset marker with `l` key, which contains the live singlets:

```r
dataset <- Dissect(dataset, c('l'))
```

(Note that more keys can be specified.)

The reduced dataset is ready for being processed by the second step:

```r
dataset <- Process(dataset, step2)
```

## Using the results

### Obtaining statistics
After the results are ready, we can obtain the population statistics using e.g. `PopulationSizes(dataset)`:

```
                   Annotation
File                B cells Dendritic cells Macrophages Neutrophils NK cells NK T cells T cells   <NA>
  21-10-15_Tube_028  207493           11006       10279        3225     7177       1425   78099   7778
  21-10-15_Tube_030  173869            8783       11310        3368     3706       1246   92268   6548
  21-10-15_Tube_031  198724            9642        7469        3025     6366       1472  107295   6879
  21-10-15_Tube_032  218421            9904       11307        3017     7789       1673   75288   5373
```

The column marked `<NA>` represents the cells that were not assigned any annotation, ie. those that were left out as "gray" in the clustering interface.

### Processed single-cell data
For various purposes, it may also be beneficial to export the data. `ExportDF(dataset)` exports a large data frame that contains a lot of information about the dataset:

```
> ExportDF(dataset)[1:5,]
      FSC-A FSC-H    FSC-W     SSC-A  SSC-H    SSC-W    FITC-A
1 104919.57 91624 75045.94  45042.46  41000 71997.62  47.96475
2 104539.68 72977 93880.44 129815.17 104981 81039.11 145.31468
3  92914.92 82279 74007.61  23377.52  20195 75863.80 -49.92402
4  21442.05 20042 70114.07  36617.53  34942 68678.57 174.06121
5  79960.23 74381 70451.77  34708.58  33134 68650.38  34.66024
  MHCII#PerCp-Cy5-5 (PerCP-Cy5-5-A) CD49b#eFluor450 (Pacific Blue-A) AmCyan-A
1                          2.879356                        1.1410280 1.204577
2                          3.092070                        1.3442259 1.484064
3                          1.249903                        1.1925033 1.100682
4                          2.155605                        0.5509763 1.563597
5                          1.115314                        1.1847186 1.047064
  CD11b#BV605 (BV605-A) CD64#BV711 (BV711-A) FcERI#BV786 (BV786-A)
1             2.4137815            1.3216869             0.9004143
2             1.5304053            1.1014624             1.1421245
3             0.7284499            1.0002436             0.8207423
4             1.7426230            2.0427317             2.2640543
5             1.2014615            0.9959151             0.9562646
  CD161#APC (APC-A) Ly-6G#AF700 (Alexa Fluor 700-A) L/D#eFluor780 (APC-Cy7-A)
1         1.0150891                       1.1384442                 0.9560442
2         1.0533812                       0.9488114                 1.9926031
3         0.8836237                       0.8330915                 1.1454782
4         0.9893947                       0.9126866                 0.9558701
5         0.8401625                       0.7957797                 1.0021014
  CD3#PE (PE-A) CD19#PE-Cy5 (PE-Cy5-A) CD11c#PE-Cy7 (PE-Cy7-A)   Time CellFile
1     1.6601674              2.0630345                3.456712 1254.5        1
2     1.7487420              1.5789861                3.840369 5520.4        1
3     0.7130027              2.7802491                1.011603 1846.4        1
4     1.2357820              0.7009728                1.572579 2042.0        1
5     2.8881256              0.6032841                1.165158  287.9        1
  EmbedSOM1 EmbedSOM2 SOM1 SOM2 ClusterKey      Population
1  8.383144 22.866970    9   24          2 Dendritic cells
2  8.215800 21.121189    8   21          2 Dendritic cells
3 19.121624  9.981588   20    9          1         B cells
4  3.686083 22.917351    4   24          5     Macrophages
5 19.597309 18.423843   20   19          7         T cells
```

### Exporting a FCS file

Similarly, one can save this data to an FCS file and analyze it in different software:
```r
flowCore::write.FCS(ExportFlowFrame(dataset), "exported_data.fcs")
```

### Plotting the data

Raw data available in the dataset can be used for plotting high-quality graphics suitable for publishing. Importantly, the data frame obtained from `ExportDF` can be directly used in `ggplot2`:

```r
library(ggplot2)
ggsave('plot.png', units='in', width=4, height=4,
ggplot(ExportDF(dataset)) +
  geom_point(size=.1, shape=16, alpha=.1,
             aes(EmbedSOM1, EmbedSOM2, color=`MHCII#PerCp-Cy5-5 (PerCP-Cy5-5-A)`)) +
  EmbedSOM::ExpressionGradient(guide=F) +
  xlim(-3,26) + ylim(-3,26) +
  ggtitle("MHCII expression") +
  theme_classic()
)
```

Above code may produce the following plot:

<img src="media/tutorial-plot.png?raw=true" alt="Result from ggplot" width="50%">
