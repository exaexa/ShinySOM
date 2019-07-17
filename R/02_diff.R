
plotDsADiff <- function(fns, color, files, data, cellFile, e, prettyColnames, annotation, alpha, cex) {

  filter <- files[cellFile]%in%fns

  if(color=='(density)') {
    ggplot2::ggplot(data.frame(EmbedSOM1=e[filter,1], EmbedSOM2=e[filter,2])) +
    ggplot2::geom_bin2d(ggplot2::aes(x=EmbedSOM1, y=EmbedSOM2), bins=100) +
    ggplot2::scale_fill_gradientn(colors=EmbedSOM::ExpressionPalette(32), guide=F) +
    ggplot2::xlim(min(e[,1]), max(e[,1])) + ggplot2::xlab('') +
    ggplot2::ylim(min(e[,2]), max(e[,2])) + ggplot2::ylab('') +
    cowplot::theme_cowplot()
  } else if (color=='(cluster)') {
    annotation <- factor(annotation)
    centers <- sapply(levels(annotation), function(ann) {
      ce <- e[annotation==ann,]
      apply(ce, 2, median)
    })
    apos <- data.frame(
      name=levels(annotation),
      x=centers[1,],
      y=centers[2,])

    ggplot2::ggplot(data.frame(EmbedSOM1=e[filter,1], EmbedSOM2=e[filter,2])) +
    ggplot2::geom_point(
      alpha=alpha, shape=16, size=cex,
      ggplot2::aes(
        x=EmbedSOM1,
        y=EmbedSOM2,
        color=annotation[filter])) +
    ggplot2::scale_color_discrete(guide=F) +
    ggplot2::annotate("text", x=apos$x, y=apos$y, label=apos$name) +
    cowplot::theme_cowplot()
  } else if (color=='(file)') {
    file <- factor(files[cellFile])
    ggplot2::ggplot(data.frame(EmbedSOM1=e[filter,1], EmbedSOM2=e[filter,2])) +
    ggplot2::geom_point(
      alpha=alpha, shape=16, size=cex,
      ggplot2::aes(
        x=EmbedSOM1,
        y=EmbedSOM2,
        color=file[filter])) +
    ggplot2::scale_color_discrete(guide=F) +
    cowplot::theme_cowplot()
  } else {
    ggplot2::ggplot(data.frame(
      EmbedSOM1=e[filter,1],
      EmbedSOM2=e[filter,2],
      Value=data[filter,findColIds(color, prettyColnames)])) +
    ggplot2::geom_point(
      alpha=alpha, shape=16, size=cex,
      ggplot2::aes(
        x=EmbedSOM1,
        y=EmbedSOM2,
        color=Value)) +
    EmbedSOM::ExpressionGradient(guide=F) +
    cowplot::theme_cowplot()
  }
}
