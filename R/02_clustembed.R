
plotClustEmbed <- function(e, clust, annotation, cex, alpha) {

  cl <- factor(clust)
  par(mar=c(0,0,0,0))

  EmbedSOM::PlotEmbed(e, clust=as.numeric(cl), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  plotClusterTextAnnotation(e, cl, annotation)
}
