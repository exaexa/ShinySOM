
plotOverviewExprs <- function(data, files, cellFile, prettyColnames, selectedColnames) {
  x <- findColIds(selectedColnames, prettyColnames)

  if(length(x) == 0) return(NULL)

  nc <- dim(data)[1]
  labs <- selectedColnames
  names(labs)=x

  df <- data.frame(
    Value=as.vector(data[,x]),
    Colid=rep(x, each=nc))
  if(length(files)>1) df <- data.frame(df,
    File=as.factor(rep(basename(files)[cellFile], times=length(x))))

  ggplot2::ggplot(df,
    if(length(files)>1)
      ggplot2::aes(
        x=Value,
        color=File,
        fill=File)
      else
      ggplot2::aes(x=Value)
      ) +
  cowplot::theme_cowplot() +
  ggplot2::geom_density(alpha=.2) +
  ggplot2::facet_grid(Colid ~ ., scales='free',
    labeller=ggplot2::labeller(Colid = labs))
}
