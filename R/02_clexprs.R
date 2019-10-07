
plotDsAExprs <- function(files, hlFiles, cellFile,
                         clusters, clust, annotation,
                         markers, d) {

  par(mar=c(0,1.2,1.5,0), mfrow=c(length(clusters), length(markers)))

  fclust <- factor(clust)
  ccols <- EmbedSOM::ClusterPalette(nlevels(fclust))

  for(cl in clusters) for(m in seq_len(length(markers))) {
    
    clid <- findColIds(cl, levels(fclust))
    
    dlim <- c(min(d[,m]), max(d[,m]))
    bw <- 0.05*(dlim[2]-dlim[1])

    plot(0,0,
      xlim=dlim,
      ylim=c(0,length(files)),
      frame.plot=F, type='n', xaxt='n', yaxt='n')
    mtext(side=3, markers[m])
    mtext(side=2, annotation[cl])

    for(i in seq_len(length(files))) {
      flt <- (cellFile==i)&(clust==cl)
      flt[is.na(flt)] <- F
      dens <- density(d[flt,m], from=dlim[1], to=dlim[2], width=bw)
      px<-c(dlim[1], dens$x, dlim[2])
      py<-c(0, dens$y, 0)
      if(max(dens$y)>0) py <- .9*py/max(dens$y)
      polygon(px, py+length(files)-i,
        col=adjustcolor(ccols[clid], alpha=if(files[i]%in%hlFiles) .75 else .1),
        border=ccols[clid],
        lwd=2)
    }
  }
}