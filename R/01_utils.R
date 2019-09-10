
findColIds <- function(cols, all) {
  seq_len(length(all))[all %in% cols]
}

ilDiv <- function(..., style='') div(
  style=paste0('display: inline-block;', style), ...)

sliderPointSize <- function(id, value=1) sliderInput(
  id, "Point size", value=value, min=0, max=3, step=.1)

sliderAlpha <- function(id, value=0.3) sliderInput(
  id, "Alpha", value=value, min=0.01, max=1, step=.01)

setClustNAs <- function(x) {
  x[x==' ']<-NA
  x
}

unsetClustNAs <- function(x) {
  x[is.na(x)] <- ' '
  x
}

getHeatmapColors <- function(ds, colors) {
  if(is.null(colors)) return(NULL)
  ids <- findColIds(colors, ds$colsToUse)
  if(length(ids)==0) return(NULL)

  pal <- EmbedSOM::ExpressionPalette(100)
  d <- apply(ds$map$codes[,ids,drop=F], 2, function(col) {
    rng <- max(col)-min(col)
    if(rng==0) rng <- 1
    pal[1+as.integer(99*(col-min(col))/rng)]
  })
  colnames(d) <- ds$colsToUse[ids]
  d
}
