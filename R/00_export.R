
dsExportDF <- function(ds) {
  df <- data.frame(ds$data, CellFile=ds$cellFile)
  colnames(df)[seq_len(length(ds$prettyColnames))] <- ds$prettyColnames

  if(!is.null(ds$embed))
    df <- cbind(df,
      EmbedSOM1=ds$embed[,1],
      EmbedSOM2=ds$embed[,2]
    )

  if(!is.null(ds$map))
    df <- cbind(df,
      SOM1=1+((ds$map$mapping[,1]-1) %% ds$xdim),
      SOM2=1+as.integer((ds$map$mapping[,1]-1) / ds$xdim)
    )

  if(!is.null(ds$clust)) {
    cl <- as.numeric(factor(ds$clust[ds$map$mapping[,1]]))
    df <- cbind(df, ClusterKey=cl)
    if(!is.null(ds$annotation))
      df <- cbind(df, Population=ds$annotation[cl])
  }

  df
}

dsExportFlowFrame <- function(ds) {
  df <- data.frame(ds$data, CellFile=ds$cellFile)
  descs <- c(ds$prettyColnames, "File ID")

  if(!is.null(ds$embed)) {
    df <- cbind(df,
      EmbedSOM1=ds$embed[,1],
      EmbedSOM2=ds$embed[,2]
    )
    descs <- c(descs, 'EmbedSOM 1', 'EmbedSOM 2')
  }

  if(!is.null(ds$map)) {
    df <- cbind(df,
      SOM1=1+((ds$map$mapping[,1]-1) %% ds$xdim),
      SOM2=1+as.integer((ds$map$mapping[,1]-1) / ds$xdim)
    )
    descs <- c(descs, 'SOM vertex 1', 'SOM vertex 2')
  }

  if(!is.null(ds$clust)) {
    cl <- as.numeric(factor(ds$clust[ds$map$mapping[,1]]))
    cl[is.na(cl)] <- 0
    df <- cbind(df, Cluster=cl)
    descs <- c(descs, 'Population ID')
  }

  ff <- new('flowFrame', exprs=as.matrix(df))
  ff@parameters@data[,'desc'] <- descs #magic!!
  ff
}

dsExportPopSizes <- function(ds) {
  table(useNA='ifany', data.frame(
    File=ds$files[ds$cellFile],
    Annotation=ds$annotation[ds$clust[ds$map$mapping[,1]]]))
}
