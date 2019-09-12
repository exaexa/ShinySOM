
plotDsADiff <- function(fns, color, files, data, cellFile, e, prettyColnames, clust, annotation, alpha, cex) {

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

    cl <- factor(clust)

    EmbedSOM::PlotEmbed(
      e[filter,],
      clust=as.numeric(cl)[filter],
      nclust=nlevels(cl),
      plotf=scattermoreplot,
      cex=cex,
      alpha=alpha,
      frame.plot=F)

    plotClusterTextAnnotation(e, cl, annotation)
  }
  else if (color=='(file)') {
    file <- factor(cellFile)
    EmbedSOM::PlotEmbed(
      e[filter,],
      clust=as.numeric(file)[filter],
      nclust=nlevels(file),
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
