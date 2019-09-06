
plotDsADiff <- function(fns, color, files, data, cellFile, e, prettyColnames, annotation, alpha, cex) {

  filter <- files[cellFile]%in%fns

  par(mar=c(0,0,0,0))

  if(color=='(density)') {
    EmbedSOM::PlotEmbed(
      e[filter,],
      cex=cex,
      alpha=alpha,
      plotf=scattermoreplot,
      frame.plot=F)
  }
  else if (color=='(cluster)') {
    annotation <- factor(annotation)
    anns <- levels(annotation)
    centers <- t(sapply(anns, function(ann)
      apply(e[annotation==ann,], 2, median)
    ))

    EmbedSOM::PlotEmbed(
      e[filter,],
      clust=as.numeric(annotation)[filter],
      nclust=length(anns),
      plotf=scattermoreplot,
      cex=cex,
      alpha=alpha,
      frame.plot=F)

    text(centers, anns)
  }
  else if (color=='(file)') {
    file <- factor(cellFile)
    EmbedSOM::PlotEmbed(
      e[filter,],
      clust=as.numeric(file)[filter],
      nclust=length(levels(file)),
      plotf=scattermoreplot,
      cex=cex,
      alpha=alpha,
      frame.plot=F)
  }
  else
    EmbedSOM::PlotEmbed(
      e[filter,],
      data=matrix(ncol=1, data[filter, findColIds(color, prettyColnames)]),
      value=1,
      plotf=scattermoreplot,
      cex=cex,
      alpha=alpha,
      frame.plot=F)
}
