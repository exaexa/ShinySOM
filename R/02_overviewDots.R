
#TODO: this needs to be completely rethinked. Better perhaps have a selector for showing files separately? Certainly use PlotEmbed for better density.

plotOverviewDots <- function(data, files, cellFile, prettyColnames, sepFiles, alpha, count, horiz, vert) {
  x <- findColIds(horiz, prettyColnames)
  y <- findColIds(vert, prettyColnames)

  df <- data.frame(
    X=as.vector(data[,x]),
    Y=as.vector(data[,y]))

  if(sepFiles) df <- cbind(df, data.frame(File=basename(files)[cellFile]))

  n <- dim(df)[1]
  if(count>n) count<-n
  set.seed(1) #TODO: ugly
  df <- df[sample(n,count),]

  p <- ggplot2::ggplot(df, ggplot2::aes(x=X, y=Y)) +
  cowplot::theme_cowplot() +
  ggplot2::xlab(prettyColnames[x]) +
  ggplot2::ylab(prettyColnames[y])

  if(sepFiles)
    p <- p + ggplot2::geom_point(alpha=alpha, shape='.', ggplot2::aes(color=File)) +
      ggplot2::scale_color_discrete(guide=ggplot2::guide_legend(override.aes=list(shape=19, size=5, alpha=1)))
  else p <- p + ggplot2::geom_point(alpha=alpha, shape='.')

  p
}
