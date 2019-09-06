
#TODO: plot population annotations as well
plotDsASig <- function(control, experiment, files, cellFile, e,
  gran, mapping, clust, annotation,
  pow, cex, alpha)
{
  cl <- 1
  if(gran=='SOM') cl <- mapping
  else if(gran=='Metaclusters') cl <- clust[mapping]
  else if(gran=='Annotated clusters') cl <- annotation[clust[mapping]]

  cl <- factor(cl)
  ncl <- nlevels(cl)
  cl <- as.integer(cl)
  
  conID <- findColIds(control, files)
  expID <- findColIds(experiment, files)

  pad.zero <- function(v, l) {
		if(length(v)==l) {
			v
		} else if (length(v)>l) {
			v[1:l]
		}  else {
			c(v, rep(0, times=l-length(v)))
		}
	}

  probsC <- matrix(0, ncol=ncl, nrow=length(conID))
  probsE <- matrix(0, ncol=ncl, nrow=length(expID))
  for(i in 1:length(conID))
    probsC[i,] <- pad.zero(tabulate(cl[cellFile==conID[i]]),ncl)/sum(cellFile==conID[i])
  for(i in 1:length(expID))
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
	colors <- rgb(colv[cl,1], colv[cl,2], colv[cl,3], colv[cl,4])

  EmbedSOM::PlotEmbed(e, col=colors, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
}
