
plotClustEmbed <- function(e, data, annotation, cex, alpha) {
  annotation <- factor(annotation)
  anns <- levels(annotation)
  centers <- t(sapply(anns,
    function(ann) apply(e[annotation==ann,], 2, median)
  ))

  par(mar=c(0,0,0,0))

  if(is.null(data))
    EmbedSOM::PlotEmbed(e, clust=as.numeric(annotation), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)
  else
    EmbedSOM::PlotEmbed(e, data=matrix(data, ncol=1), 1, cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  text(centers, anns)
}
