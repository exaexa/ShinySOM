
formatPVal <- function(pv)
  format(signif(pv, digits=2), scientific=F, drop0trailing=T)

plotDsASig <- function(control, experiment, files, cellFile, e,
  gran, mapping, clust, annotation,
  pval, cex, alpha)
{
  cl <- 1

  pow <- log(.5,1-pval)

  if(gran=='SOM') cl <- mapping
  else if(gran=='Clusters') cl <- clust[mapping]

  cl <- factor(cl)
  cllevs <- levels(cl)
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


  suppressWarnings(po_less <- sapply(1:ncl, function(i)
    wilcox.test(
			probsE[,i],
			probsC[,i],
			alternative='less',
			paired=F)$p.value))
  suppressWarnings(po_greater <- sapply(1:ncl, function(i)
   wilcox.test(
			probsE[,i],
			probsC[,i],
			alternative='greater',
			paired=F)$p.value))

  p_less <- (1-po_less)^pow
  p_greater <- (1-po_greater)^pow

	col.inconclusive <- rgb(.75, .75, .75, alpha/2)
	col.greater <- rgb(1, .5, 0, alpha)
	col.less <- rgb(0, .5, 1, alpha)

	colv.greater <- col2rgb(col.greater, alpha=T)
	colv.less <- col2rgb(col.less, alpha=T)
	colv.inconclusive <- col2rgb(col.inconclusive, alpha=T)
	colv <- outer(p_greater,colv.greater)+
		outer(p_less,colv.less)+
		outer(1-p_greater-p_less,colv.inconclusive)
	colv=colv[,,1]/255
	colv[colv>1] <- 1 # fixup rounding errors that sometimes kill rgb()
	colv[colv<0] <- 0
  acl <- cl[!is.na(cl)]
	colors <- rgb(colv[acl,1], colv[acl,2], colv[acl,3], colv[acl,4])

  par(mar=c(0,0,0,0))
  EmbedSOM::PlotEmbed(e[!is.na(cl),], col=colors, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
  for(i in 1:ncl) {
    ai <- cllevs[i]
    if(ai %in% names(annotation)) {
      if(po_greater[i]<sqrt(pval))
        annotation[ai] <- paste0(annotation[ai], '\nincrease p=', formatPVal(po_greater[i]))
      if(po_less[i]<sqrt(pval))
        annotation[ai] <- paste0(annotation[ai], '\ndecrease p=', formatPVal(po_less[i]))
    }
  }
  plotClusterTextAnnotation(e[!is.na(cl),], clust[mapping][!is.na(cl)], annotation)
}

plotDsASigLegend <- function(pval) {
  pvalMod <- c(.5,.8,1,1.2,1.5)
  pvalsG <- 1-c(rep(1,6), pval^(pvalMod))
  pvalsL <- 1-c(pval^rev(pvalMod), rep(1,6))

  pvss <- formatPVal(c(pval^rev(pvalMod),1,pval^pvalMod))
  pvss <- paste0(c(rep("decrease, p=",5), "both p=", rep("increase, p=",5)), pvss)

  pow <- log(.5,1-pval)
	colv.inconclusive <- col2rgb(rgb(.75, .75, .75))
	colv.greater <- col2rgb(rgb(1, .5, 0))
	colv.less <- col2rgb(rgb(0, .5, 1))
  colv <- outer(pvalsG^pow, colv.greater) +
    outer(pvalsL^pow, colv.less) +
    outer(1-pvalsG^pow-pvalsL^pow, colv.inconclusive)
	colv=colv[,,1]/255
	colv[colv>1] <- 1 # fixup rounding errors that sometimes kill rgb()
	colv[colv<0] <- 0
	cs <- rgb(colv[,1], colv[,2], colv[,3])

  par(mar=c(0,0,1,0))
  plot(NULL, xaxt='n', yaxt='n', bty='n', xlab='', ylab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend=pvss, col=cs, pch=19)
  mtext("Wilcoxon signed-rank test outcome", adj=0)
}
