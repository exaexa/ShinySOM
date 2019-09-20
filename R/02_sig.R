
plotDsASig <- function(control, experiment, files, cellFile, e,
  gran, mapping, clust, annotation,
  pow, cex, alpha)
{
  cl <- 1
  if(gran=='SOM') cl <- mapping
  else if(gran=='Clusters') cl <- clust[mapping]

  cl <- factor(cl)
  ncl <- nlevels(cl)
  cl <- as.integer(cl)

  conID <- findColIds(control, files)
  expID <- findColIds(experiment, files)

  probsC <- matrix(0, ncol=ncl, nrow=length(conID))
  probsE <- matrix(0, ncol=ncl, nrow=length(expID))
  for(i in seq_len(length(conID)))
    probsC[i,] <- pad.zero(tabulate(cl[cellFile==conID[i]]),ncl)/sum(cellFile==conID[i])
  for(i in seq_len(length(expID)))
    probsE[i,] <- pad.zero(tabulate(cl[cellFile==expID[i]]),ncl)/sum(cellFile==expID[i])


  p_less <- sapply(1:ncl, function(i)
    (1-wilcox.test(
			probsE[,i],
			probsC[,i],
			alternative='less',
			paired=F)$p.value)^pow)
  p_greater <- sapply(1:ncl, function(i)
   (1-wilcox.test(
			probsE[,i],
			probsC[,i],
			alternative='greater',
			paired=F)$p.value)^pow)

	col.inconclusive <- rgb(.75, .75, .75, alpha/2)
	col.greater <- rgb(1, .5, 0, alpha)
	col.less <- rgb(0, .5, 1, alpha)

	colv.greater <- col2rgb(col.greater, alpha=T)
	colv.less <- col2rgb(col.less, alpha=T)
	colv.inconclusive <- col2rgb(col.inconclusive, alpha=T)
	colv <- outer(p_greater,colv.greater)+
		outer(p_less,colv.less)+
		outer(1-p_greater-p_less,colv.inconclusive)
	colv=colv[,,1]/255 #TODO explain the 3rd dimension... :]
	colv[colv>1] <- 1 # fixup rounding errors that sometimes kill rgb()
	colv[colv<0] <- 0
  acl <- cl[!is.na(cl)]
	colors <- rgb(colv[acl,1], colv[acl,2], colv[acl,3], colv[acl,4])

  par(mar=c(0,0,0,0))
  EmbedSOM::PlotEmbed(e[!is.na(cl),], col=colors, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
  plotClusterTextAnnotation(e[!is.na(cl),], cl[!is.na(cl)], annotation)
}
