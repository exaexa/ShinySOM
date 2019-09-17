
loadFCSColnames <- function(file) {
  ff <- flowCore::read.FCS(file, which.lines=1)
  list(
    labels = unname(ff@parameters@data$desc),
    cols = unname(ff@parameters@data$name)
  )
}

loadFCSTryCompensate <- function(ff, progress) {
  # FlowSOM-like finding of any possible compensation matrix
  # (base code originated in FlowSOM, (C) 2015-2019 Sofie Van Gassen et al.)
  if(!is.null(ff@description$SPILL)){
    setProgress('Applying stored compensation...', value=progress)
    return(flowCore::compensate(ff, ff@description$SPILL))
  }
  
  if (!is.null(ff@description$`$SPILLOVER`)){
    setProgress('Applying stored compensation...', value=progress)
    if(class(ff@description$`$SPILLOVER`)=="matrix"){
      spillover <- ff@description$`$SPILLOVER`
      ff@description$SPILL <- spillover
    } else {
      spilloverStr <- strsplit(ff@description$`$SPILLOVER`, ",")[[1]]
      n <- as.numeric(spilloverStr[1])
      spillover <- t(matrix(
        as.numeric(spilloverStr[(n+2):length(spilloverStr)]),
        ncol=n))
      colnames(spillover) <- spilloverStr[2:(n+1)]
      ff@description$SPILL <- spillover
    }
    return(flowCore::compensate(ff, ff@description$SPILL))
  }
  
  ff # didn't find anything
}

loadFCSAggregate <- function(fileNames, cells, transformColNums) {
  nf <- length(fileNames)
  cf <- ceiling(cells/nf)

  ffs <- c()
  files <- c()
  
  #TODO: what about turning off truncate_max_range?

    for(i in seq_len(nf)) {
      setProgress('Loading FCS file...', value=i)
      ff <- flowCore::read.FCS(fileNames[i])

      ns <- min(nrow(ff), cf)
      cs <- sample(nrow(ff), ns)
      flowCore::exprs(ff) <- flowCore::exprs(ff)[cs,]
      files <- c(files, rep(i, ns))
      
      ff <- loadFCSTryCompensate(ff, i)

      if(!is.null(transformColNums)) {
        #TODO: remove this and use a separate transform editor later
        setProgress('Transforming...', value=i)

        ff <- flowCore::transform(ff,
          flowCore::transformList(ff@parameters@data$name[transformColNums],
            tfun=flowCore::logicleTransform()))
      }

      setProgress('Aggregating...', value=i)
      if(is.null(ffs))
        ffs <- ff
      else
        flowCore::exprs(ffs) <- rbind(
          flowCore::exprs(ffs),
          flowCore::exprs(ff)
        )
    }

  list(
    exprs = flowCore::exprs(ffs),
    cellFile = files,
    labels = unname(ffs@parameters@data$desc),
    cols = unname(ffs@parameters@data$name)
  )
}
