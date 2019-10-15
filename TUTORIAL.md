
# ShinySOM Tutorial

## Introduction

ShinySOM is a Shiny application for quick, highly interactive exploration of multidimensional flow cytometry data. The analysis model is derived from FlowSOM: The user inputs prepared samples in FCS files, uses SOMs and related analysis algorithms to dissect the data, and runs various analyses and clustering algorithms on the result.

General ShinySOM workflow is as such:

1. The user inputs a set of compensated and quality-controlled samples, that are aggregated to form a **dataset**. In ShinySOM, the dataset abstracts a "batch" of analyzed data, usually from the the same experiment, with same parameters and using the same panel.
2. The data in the dataset are transformed to space viable for analysis (e.g. using the usual logicle or arcsinh transforms)
3. As in FlowSOM, a SOM is created to provide a dimensionality-reduced view of the whole dataset
4. The dataset is dissected into named subpopulations, which allows
  - analyzing and comparing the contents of population in original FCS files
  - exporting the populations as annotated FCS files or new subset datasets
5. The identified populations may be analyzed separately, repeating the whole workflow from point 3.

In this tutorial, we demonstrate this workflow on a [well-explored dataset used for monitoring immune cells in spleen (Sayes et al., 2016)](https://www.nature.com/articles/nri.2016.56). The dataset can be obtained from FlowRepository under [accession number ZZQY](https://flowrepository.org/id/FR-FCM-ZZQY).

## Loading data, creating datasets

## Transforming the data

## Identifying the populations

### Clustering and embedding

### iDendro-style dissection

## Simple analysis

## Subset datasets

## Data export
