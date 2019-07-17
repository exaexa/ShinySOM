
plotSOMOverview <- function(xdim, ydim, codevals, datavals, mapping) {
  n <- xdim * ydim
  size <- sqrt(tabulate(mapping))
  stroke <- sapply(1:n, function(cl)sd(datavals[mapping==cl]))
  stroke[is.na(stroke)]<-0
  m <- max(stroke)
  if(m<=0) m <- 1
  stroke <- 0.2+5*stroke/max(stroke)

  ggplot2::ggplot(
    data.frame(
      expand.grid(x=1:xdim,y=1:ydim),
      stroke=stroke,
      size=sqrt(tabulate(mapping)),
      color=codevals)) +
  ggplot2::geom_point(
    ggplot2::aes(
      fill=color,
      x=x, y=y,
      size=size,
      stroke=stroke),
    shape=21,
    color='gray') +
  ggplot2::scale_size_continuous(guide=F) +
  ggplot2::scale_fill_gradientn(colors=EmbedSOM::ExpressionPalette(32), guide=F) +
  cowplot::theme_cowplot()
}
