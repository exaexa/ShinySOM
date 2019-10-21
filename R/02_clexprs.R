
plotDsAExprs <- function(files, hlFiles, cellFile,
                         clusters, clust, annotation,
                         markers, d) {

  if(length(clusters)==0 || length(markers)==0) return()

  par(mar=c(0,1.2,1.5,0), mfrow=c(length(clusters), length(markers)))

  fclust <- factor(clust)
  ccols <- EmbedSOM::ClusterPalette(nlevels(fclust))

  for(cl in clusters) for(m in seq_len(length(markers))) {

    if(m > dim(d)[2]) next
    
    clid <- findColIds(cl, levels(fclust))
    
    if(length(d[,m])>0)
      dlim <- c(min(d[,m]), max(d[,m]))
    else dlim <- c(0,1)
    if(dlim[1]==dlim[2])
      dlim <- dlim+c(-1,+1)

    bw <- 0.05*(dlim[2]-dlim[1])

    plot(0,0,
      xlim=dlim,
      ylim=c(0,length(files)),
      frame.plot=F, type='n', xaxt='n', yaxt='n')
    mtext(side=3, markers[m])
    if(cl %in% names(annotation)) mtext(side=2, annotation[cl])

    for(i in seq_len(length(files))) {
      flt <- (cellFile==i)&(clust==cl)
      flt[is.na(flt)] <- F
      dens <- d[flt,m]
      if(length(dens)>=1) {
        dens <- density(dens, from=dlim[1], to=dlim[2], width=bw)
        px<-c(dlim[1], dens$x, dlim[2])
        py<-c(0, dens$y, 0)
        if(max(dens$y)>0) py <- .9*py/max(dens$y)
      } else {
        px<-c(dlim[1], dlim[2])
        py<-c(0,0)
      }
      polygon(px, py+length(files)-i,
        col=adjustcolor(ccols[clid], alpha=if(files[i]%in%hlFiles) .75 else .1),
        border=ccols[clid],
        lwd=2)
    }
  }
}
