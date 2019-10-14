
getClustEmbedData <- function(e, data, prettyColnames, str) {
    if(str %in% prettyColnames) data[,findColIds(str, prettyColnames)]
    else if(str=='(Embedding X)') e[,1]
    else e[,2]
}

plotClustEmbed <- function(e, clust, annotation, data, prettyColnames, color, dimx, dimy, cex, alpha) {

  cl <- factor(clust)
  par(mar=c(0,0,0,0))

  xy <- cbind(
    getClustEmbedData(e, data, prettyColnames, dimx),
    getClustEmbedData(e, data, prettyColnames, dimy)
  )

  if(color=='(cluster)')
    EmbedSOM::PlotEmbed(xy, clust=as.numeric(cl), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)
  else
    EmbedSOM::PlotEmbed(xy, data=data[,findColIds(color, prettyColnames),drop=F], value=1, cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  plotClusterTextAnnotation(xy, cl, annotation)
}
