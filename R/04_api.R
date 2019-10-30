
#' LoadCells
#' 
#' Opens several files and concatenates their contents into a dataset object.
#'
#' @param files Character vector of file names
#' @param nCells Maximum integer number of cells to load, or NULL (default) to load all available cells
#' @param noComp Avoid using the compensation matrices stored in files (default FALSE)
#' @param colsToLoad Limit loading to specified character vector of columns
#' @return The newly created dataset
#'
#' @export
LoadCells <- function(files, nCells=NULL, noComp=F, colsToLoad=NULL) {
  dsCreateDoLoadInternal(files, !is.null(nCells), noComp, nCells, colsToLoad)
}

#' Process
#'
#' Batch-process the cells according to the analysis saved in ds
#'
#' @param nds Loaded cell object, e.g. as received from 'LoadCells'
#' @param a Loaded analysis object, e.g. as exported from ShinySOM GUI and loaded using 'readRDS'
#' @return The processed dataset
#'
#' @export
Process <- function(nds, a) {
  # apply the transformations
  ds <- a
  ds$data <- nds$data
  ds$files <- nds$files
  ds$cellFile <- nds$cellFile
  ds$prettyColnames <- nds$prettyColnames

  for(desc in ds$transforms)
    ds$data <- transformedDsDataByDesc(ds, desc)$data

  # run the mapping
  if(is.null(ds$colsToUse)||is.null(ds$map)) return(ds)
  if(!all(ds$colsToUse %in% ds$prettyColnames)) {
    cat("missing columns: ", ds$colsToUse[!(ds$colsToUse %in% ds$prettyColnames)] ,'\n')
    stop("dataset is missing required columns")
  }
  ds$map$mapping <- EmbedSOM::MapDataToCodes(
    codes=ds$map$codes,
    data=diffsomPrepareData(ds$data, ds$prettyColnames, ds$colsToUse, ds$importance),
    distf=ds$map$distf)

  # run the embedding, if present
  if(!is.null(ds$smooth) &&
     !is.null(ds$adjust) &&
     !is.null(ds$k) &&
     !is.null(ds$emcoords))
    ds$e <- EmbedSOM::EmbedSOM(
      data=diffsomPrepareData(ds$data, ds$prettyColnames, ds$colsToUse, ds$importance),
      map=ds$map,
      smooth=ds$smooth,
      adjust=ds$adjust,
      k=ds$k,
      emcoords=ds$emcoords)

  ds
}

#' ExportDF
#'
#' Convert a dataset object to a data frame
#'
#' @param ds The dataset to be converted
#' @return A data frame
#'
#' @export
ExportDF <- function(ds) {
  dsExportDF(ds)
}

#' ExportFlowFrame
#'
#' Save the available dataset information in a flowFrame.
#' You can save the resulting flowFrame to FCS file using e.g.
#' 'flowCore::write.FCS'.
#'
#' @param ds The dataset to be exported
#' @return A new flowFrame object
#'
#' @export
ExportFlowFrame <- function(ds) {
  dsExportFlowFrame(ds)
}

#' Dissect
#'
#' Reduce the dataset to subsets, as specified by the selected populations
#'
#' @param ds The original dataset to be reduced
#' @param pops Population keys (as character vector) to be dissected out of the
#'             original dataset (the resulting dataset will only
#'             contain populations specified here)
#' @return The reduced dataset object
#'
#' @export
Dissect <- function(ds, pops) {
  if(is.null(ds$clust) || is.null(ds$map))
    stop("The dataset must be processed first! (it does not contain clustering yet)")

  filt <- ds$clust[ds$map$mapping[,1]] %in% pops
  list(
    files=ds$files,
    data=ds$data[filt,,drop=F],
    cellFile=ds$cellFile[filt],
    prettyColnames=ds$prettyColnames
  )
}

#' PopulationKeys
#'
#' Get available population names from a dataset
#'
#' @param ds The dataset to be processed
#' @return Named character vector with population names and keys
#'
#' @export
PopulationKeys <- function(ds) {
  if(is.null(ds$annotation)) character(0)
  else ds$annotation
}

#' PopulationSizes
#'
#' Get cell counts in annotated populations in a dataset
#'
#' @param ds The dataset to be processed
#' @return Table with all annotated population sizes
#'
#' @export
PopulationSizes <- function(ds) {
  dsExportPopSizes(ds)
}

# TODO: Generally, it would be nice to have an API like this for all operations
# available in ShinySOM.
