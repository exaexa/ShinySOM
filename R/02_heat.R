
plotDsAHeat <- function(clust, annotation, cellFile, files) {
  cl <- factor(clust)
  probs <- matrix(0, ncol=nlevels(cl), nrow=length(files))


  for(i in seq_len(length(files))) {
    probs[i,] <- pad.zero(tabulate(cl[cellFile==i]), nlevels(cl))/sum(cellFile==i)
  }

  colnames(probs) <- annotation[levels(cl)]
  rownames(probs) <- files

  #TODO: guess margin length from colnames/rownames length
  heatmap(scale(probs), margins=c(10,10), col=EmbedSOM::ExpressionPalette(100))
}
