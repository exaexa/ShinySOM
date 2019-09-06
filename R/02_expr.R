
plotEmbedExpr <- function(e, exprs, cex, alpha) {
  par(mar=c(0,0,0,0))
  if(is.null(exprs))
    EmbedSOM::PlotEmbed(e, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
  else
    EmbedSOM::PlotEmbed(e, data=as.matrix(exprs), value=1, alpha=alpha, cex=cex, plotf=scattermoreplot, frame.plot=F)
}
