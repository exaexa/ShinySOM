
plotClustExpr <- function(cols, data, annotation) {
  x <- data.frame(Annotation=factor(annotation), data)
  colnames(x)<-c("Annotation", cols)
  x <- tidyr::gather(x, Column, Value, cols)

  if(F) { #they said popcount doesn't really matter here
  ggplot2::ggplot(x) +
  ggplot2::geom_density(
    stat='bin',
    binwidth=sd(x$Value)/50,
    size=2,
    ggplot2::aes(
      x=Value,
      color=Annotation)) +
  ggplot2::facet_grid(Column ~ ., scales='free')
  }

  ggplot2::ggplot(x) +
  ggplot2::geom_density(
    bw=sd(x$Value)/50,
    size=2,
    ggplot2::aes(
      x=Value,
      color=Annotation)) +
  ggplot2::facet_grid(Column ~ ., scales='free')
}
