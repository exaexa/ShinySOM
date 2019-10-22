
plotEmbedExpr <- function(e, exprs, cex, alpha) {
  if(is.null(e)) return()
  par(mar=c(0,0,0,0))
  if(is.null(exprs))
    EmbedSOM::PlotEmbed(e, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
  else
    EmbedSOM::PlotEmbed(e, data=matrix(exprs, ncol=1), value=1, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
}
