
plotClustEmbed <- function(e, clust, annotation, data, prettyColnames, color, cex, alpha) {

  cl <- factor(clust)
  par(mar=c(0,0,0,0))

  if(color=='(cluster)')
    EmbedSOM::PlotEmbed(e, clust=as.numeric(cl), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)
  else
    EmbedSOM::PlotEmbed(e, data=data[,findColIds(color, prettyColnames),drop=F], value=1, cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  plotClusterTextAnnotation(e, cl, annotation)
}
