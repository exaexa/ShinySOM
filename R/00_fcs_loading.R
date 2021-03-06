
loadFCSColnames <- function(file) {
  ff <- flowCore::read.FCS(file, which.lines=1)
  list(
    labels = unname(ff@parameters@data$desc),
    cols = unname(ff@parameters@data$name)
  )
}

loadFCSTryCompensate <- function(ff, progressVal, fn, progress=F) {
  # FlowSOM-like finding of any possible compensation matrix
  # (base code originated in FlowSOM, (C) 2015-2019 Sofie Van Gassen et al.)
  if(!is.null(ff@description$SPILL)){
    if(progress) setProgress('Applying stored compensation...', value=progressVal)
    return(flowCore::compensate(ff, ff@description$SPILL))
  }

  if (!is.null(ff@description$`$SPILLOVER`)){
    if(progress) setProgress('Applying stored compensation...', value=progressVal)
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
    to_remove <- c()
    for(i in seq_len(dim(ff@description$SPILL)[2])) {
      cn <- colnames(ff@description$SPILL)[i]
      if(!cn %in% ff@parameters$name) {
        #ok the column is missing, at least try whether the label is numeric and we can assign it to something
        cnn <- as.numeric(cn)
        if(is.na(cnn) || cnn > length(ff@parameters$name)) {
          to_remove <- c(to_remove, -i)
        } else {
          colnames(ff@description$SPILL)[i] <- ff@parameters$name[cnn]
        }
      }
    }
    if(!is.null(to_remove)) #OMG R, dude, come on, there can't be special cases for everything!
      ff@description$SPILL <- ff@description$SPILL [to_remove, to_remove]

    tryCatch(
      return(flowCore::compensate(ff, ff@description$SPILL)),
    error=function(e){
      showNotification(type='warning', paste("Compensation on file", fn, "failed:",e))
      return(ff)
    })
  }

  ff # didn't find anything
}

loadFCSAggregate <- function(fileNames, cells, noComp, progress=F) {
  nf <- length(fileNames)
  cf <- ceiling(cells/nf)

  ffs <- c()
  files <- c()

  for(i in seq_len(nf)) {
    if(progress) setProgress('Loading FCS file...', value=i)
    ff <- flowCore::read.FCS(fileNames[i])

    ns <- min(nrow(ff), cf)
    cs <- sample(nrow(ff), ns)
    flowCore::exprs(ff) <- flowCore::exprs(ff)[cs,]
    files <- c(files, rep(i, ns))

    if(!noComp)
      ff <- loadFCSTryCompensate(ff, i, fileNames[i], progress)

    if(progress) setProgress('Aggregating...', value=i)
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
