
plotEmbedExpr <- function(e, exprs, cex, alpha) {
  ggplot2::ggplot(data.frame(EmbedSOM1=e[,1], EmbedSOM2=e[,2], Expression=exprs)) +
  ggplot2::geom_point(ggplot2::aes(x=EmbedSOM1, y=EmbedSOM2, color=Expression), alpha=alpha, size=cex, shape=16) +
  ggplot2::scale_color_gradientn(colors=EmbedSOM::ExpressionPalette(32))
}
