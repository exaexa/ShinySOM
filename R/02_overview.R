
overviewPlotHistMargin <- 0.3

gatherOverviewPlotDim <- function(ds, data, id) {

  addScatter <- function(x) (x + rnorm(length(x), sd=0.1))

  if(id=='(File)')
    addScatter(ds$cellFile)
  else if (id=='(Cluster)')
    addScatter(as.numeric(factor(ds$clust)[ds$map$mapping[,1]]))
  else if (id=='(SOM X)')
    addScatter((ds$map$mapping[,1]-1)%%ds$map$xdim)
  else if (id=='(SOM Y)')
    addScatter(as.integer((ds$map$mapping[,1]-1) / ds$map$xdim))
  else if (id=='(Embedding X)')
    ds$e[,1]
  else if (id=='(Embedding Y)')
    ds$e[,2]
  else if (id %in% ds$prettyColnames)
    data[,findColIds(id, ds$prettyColnames)]
  else {
    warning(paste("overview dim gathering unknown column:",id))
    NULL
  }
}

gatherOverviewColorDim <- function(ds, data, id) {
  if(id=='(File)')
    factor(ds$cellFile)
  else if (id=='(Cluster)')
    factor(ds$clust[ds$map$mapping[,1]])
  else if (id %in% c('(Black)', '(Density)'))
    id
  else if (id %in% ds$prettyColnames)
    data[,findColIds(id, ds$prettyColnames)]
  else {
    warning(paste("overview color gathering unknown column:",id))
    NULL
  }
}

plotOverviewDimsWithColor <- function(d, clr, cex, alpha) {
  flt <- !is.na(d[,1]) & !is.na(d[,2])
  if(is.factor(clr))
    EmbedSOM::PlotEmbed(d[flt,], clust=clr[flt], cex=cex, alpha=alpha, plotf=scattermoreplot)
  else if(is.numeric(clr))
    EmbedSOM::PlotEmbed(d[flt,], data=cbind(Val=clr[flt]), value='Val', cex=cex, alpha=alpha, plotf=scattermoreplot)
  else if(is.character(clr) && length(clr)==1 && clr == '(Black)')
    EmbedSOM::PlotEmbed(d[flt,], col=rgb(0,0,0,alpha), cex=cex, plotf=scattermoreplot)
  else if(is.character(clr) && length(clr)==1 && clr == '(Density)')
    EmbedSOM::PlotEmbed(d[flt,], cex=cex, alpha=alpha, plotf=scattermoreplot)
  else
    warning("missing overview color plot implementation")
}

plotOverviewHist <- function(d, vertical) {
    d <- d[!is.na(d)]
    dlim <- c(min(d),max(d))
    bw <- 0.05 * (dlim[2]-dlim[1])
    dens <- density(d, from=dlim[1], to=dlim[2], width=bw)
    plot(
      if(vertical) dens$y else dens$x,
      if(vertical) dens$x else dens$y,
      type='n', frame.plot=F, xlab='', ylab='',
      xlim=if(vertical) c(max(dens$y), 0) else dlim,
      ylim=if(vertical) dlim else c(0, max(dens$y)))

    px <- c(dlim[1],dens$x,dlim[2])
    py <- c(0, dens$y, 0)
    polygon(
      if(vertical) py else px,
      if(vertical) px else py,
      col=rgb(.9,.9,.9),
      border=rgb(.75,.75,.75), lwd=2)
}

plotOverview <- function(ds, data, markersH, markersV, markerColor, cex, alpha) {
  set.seed(42) #this is ugly, but avoids jitter on irrelevant changes (e.g. plot alpha)

  par(xaxt='n', yaxt='n')
  nh <- length(markersH)
  sh <- nh+overviewPlotHistMargin
  nv <- length(markersV)
  sv <- nv+overviewPlotHistMargin
  colc <- gatherOverviewColorDim(ds, data, markerColor)

  for(i in seq_len(nh)) {
    par(mar=c(.1, .1, 1, .1), new=T,
      fig=c(
        c(overviewPlotHistMargin+i-1, overviewPlotHistMargin+i)/sh,
        c(nv,nv+overviewPlotHistMargin)/sv
      )
    )

    plotOverviewHist(gatherOverviewPlotDim(ds, data, markersH[i]), vertical=F)
    mtext(markersH[i], side=3)
  }
  for(i in seq_len(nv)) {
    par(mar=c(.1, 1, .1, .1), new=T,
      fig=c(
        c(0, overviewPlotHistMargin)/sh,
        c(nv-i, nv-i+1)/sv
      )
    )
    plotOverviewHist(gatherOverviewPlotDim(ds, data, markersV[i]), vertical=T)
    mtext(markersV[i], side=2)
  }

  for(i in seq_len(nh)) for(j in seq_len(nv)) {
    par(mar=c(.1, .1, .1, .1), new=T,
      fig=c(
        c(overviewPlotHistMargin+i-1, overviewPlotHistMargin+i)/sh,
        c(nv-j, nv-j+1)/sv
      )
    )
    plotOverviewDimsWithColor(
      cbind(
        gatherOverviewPlotDim(ds, data, markersH[i]),
        gatherOverviewPlotDim(ds, data, markersV[j])
      ),
      colc,
      cex=cex,
      alpha=alpha
    )
  }
}
