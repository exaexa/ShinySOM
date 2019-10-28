
plotSOMOverview <- function(xdim, ydim, codevals, datavals, mapping) {
  n <- xdim * ydim
  stroke <- sapply(1:n, function(cl)var(datavals[mapping==cl]))
  stroke[is.na(stroke)]<-0
  m <- max(stroke)
  if(m<=0) m <- 1
  stroke <- 0.1+4*stroke/max(stroke)
  sizet <- sqrt(tabulate(mapping))
  size <- rep(0, n)
  size[1:length(sizet)] <- sizet
  size <- 2*size/max(size)
  points <- expand.grid(1:xdim, 1:ydim)

  par(mar=c(0,0,0,0))
  plot(points[,1], points[,2],
    col=rgb(.75,.75,.75),
    pch=19,
    cex=size+stroke, xaxt='n', yaxt='n', frame.plot=F)
  points(points[,1], points[,2],
    col=EmbedSOM::ExpressionPalette(32)[1+31*EmbedSOM::NormalizeColor(codevals)],
    pch=19,
    cex=size)
}
