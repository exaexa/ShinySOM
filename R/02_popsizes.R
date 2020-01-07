
plotDsAPopSizes <- function(clust, annotation, cellFile, files, showClusters, doReorder) {
  cl <- factor(clust)
  probs <- matrix(0, nrow=nlevels(cl), ncol=length(files))


  for(i in seq_len(length(files))) {
    probs[,i] <- pad.zero(tabulate(cl[cellFile==i]), nlevels(cl))
  }

  rownames(probs) <- annotation[levels(cl)]
  colnames(probs) <- files

  showIds <- findColIds(showClusters, levels(cl))
  probs <- probs[showIds,,drop=F]

  probs <- apply(probs, 2, function(v) {s <- sum(v); if(s>0) v/s else v;})

  if(doReorder && nrow(probs)>1)
    probs <- probs[,hclust(dist(t(probs), method='manhattan'))$order,drop=F]

  barplot(probs,
    col=EmbedSOM::ClusterPalette(nlevels(cl))[showIds],
    border='#808080', ylab='Relative size', xlab='File', xaxt='n',
    width=1, space=.2)
  text(.15+1.2*(seq_len(ncol(probs))-1), 0, colnames(probs), srt=90, adj=0)
}

plotDsAPopLegend <- function(clust, annotation, showClusters) {
  if(is.null(showClusters) || length(showClusters)<1) return();
  cl <- factor(clust)
  par(mar=c(0,1,1,0))
  plot(NULL, xaxt='n', yaxt='n', bty='n', xlab='', ylab='', xlim=0:1, ylim=0:1)
  legend("topleft",
    legend=annotation[showClusters],
    col=EmbedSOM::ClusterPalette(nlevels(cl))
      [findColIds(showClusters, levels(cl))],
    pch=15,
    pt.cex=2)
  mtext("Population color", adj=0)
}
