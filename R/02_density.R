
plotEmbedDensity <- function(e) {
  ggplot2::ggplot(data.frame(EmbedSOM1=e[,1], EmbedSOM2=e[,2])) +
  ggplot2::geom_bin2d(ggplot2::aes(x=EmbedSOM1, y=EmbedSOM2), bins=100) +
  ggplot2::scale_fill_gradientn(colors=EmbedSOM::ExpressionPalette(32)) +
  cowplot::theme_cowplot()
}
