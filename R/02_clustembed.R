
plotClustEmbed <- function(e, clust, annotation, cex, alpha) {

  print("clust")
  print(clust)
  cl <- factor(clust)
  par(mar=c(0,0,0,0))

  print(cl)
  EmbedSOM::PlotEmbed(e, clust=as.numeric(cl), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  print("done")
  plotClusterTextAnnotation(e, cl, annotation)
}
