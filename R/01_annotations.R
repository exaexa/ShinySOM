
annotationRender <- function(ds) {
  if(is.null(ds$clust)) return(div())

  res <- tagList()
  res <- tagAppendChild(res, h4("Population names"))
  anns <- isolate(ds$annotation)
  for(l in levels(factor(ds$clust)))
    res <- tagAppendChild(res, 
      div(
        span(l),
        ilDiv(
          textInput(
            paste0('dsClustAnnotation_',l),
            NULL,
            value=if(is.null(anns) || is.na(anns[l])) l else anns[l]
          )
        )
      )
    )
  res
}

annotationGather <- function(ds, input) {
  res <- list()
  for(l in levels(factor(ds$clust))) {
    res[[l]] <- input[[paste0('dsClustAnnotation_',l)]]
  }
  unlist(res)
}

annotationServe <- function(elem, ds, input, output) {
  output[[elem]] <- renderUI(annotationRender(ds))

  observe({
    tmp <- annotationGather(ds, input)
    if(!is.null(tmp)) ds$annotation <- tmp
    if(!is.null(ds$clust) && all(is.na(ds$clust))) ds$annotation <- character(0)
  })
}
