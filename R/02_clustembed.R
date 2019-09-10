
plotClustEmbed <- function(e, annotation, cex, alpha) {
  annotation <- factor(annotation)
  anns <- levels(annotation)
  if(length(anns)>0) centers <- t(sapply(anns,
    function(ann) apply(e[annotation==ann,], 2, median)
  ))

  par(mar=c(0,0,0,0))

  EmbedSOM::PlotEmbed(e, clust=as.numeric(annotation), cex=cex, alpha=alpha, plotf=scattermoreplot, frame.plot=F)

  if(length(anns)>0) text(centers, anns)
}
