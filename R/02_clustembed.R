
plotClustEmbed <- function(e, data, annotation, cex, alpha) {
  annotation <- factor(annotation)
  centers <- sapply(levels(annotation), function(ann) {
    ce <- e[annotation==ann,]
    apply(ce, 2, median)
  })
  apos <- data.frame(
    name=levels(annotation),
    x=centers[1,],
    y=centers[2,])

  if(is.null(data))
    ggplot2::ggplot(data.frame(EmbedSOM1=e[,1], EmbedSOM2=e[,2])) +
    ggplot2::geom_point(
      alpha=.5, shape=16, size=.5,
      ggplot2::aes(
        x=EmbedSOM1,
        y=EmbedSOM2,
        color=annotation)) +
    ggplot2::annotate("text", x=apos$x, y=apos$y, label=apos$name) +
    ggplot2::guides(color=ggplot2::guide_legend(override.aes=list(size=4, alpha=1))) +
    cowplot::theme_cowplot()
  else 
    ggplot2::ggplot(data.frame(EmbedSOM1=e[,1], EmbedSOM2=e[,2], Value=data)) +
    ggplot2::geom_point(
      alpha=alpha, shape=16, size=cex,
      ggplot2::aes(
        x=EmbedSOM1,
        y=EmbedSOM2,
        color=Value)) +
    EmbedSOM::ExpressionGradient() +
    ggplot2::annotate("text", x=apos$x, y=apos$y, label=apos$name) +
    cowplot::theme_cowplot()
}
