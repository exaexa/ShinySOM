
annotationRender <- function(ds) {
  if(is.null(ds$clust)) return(div())
  cl <- factor(ds$clust)
  if(length(levels(cl))==0) return(div())

  res <- tags$table(class="table")
  res <- tagAppendChild(res, tags$tr(tags$th("Key", align='center'), tags$th("Population name"), tags$th("Cells", align='right')))
  anns <- isolate(ds$annotation)
  for(l in levels(cl))
    res <- tagAppendChild(res,
      tags$tr(
        tags$td(strong(l), align='center'),
        tags$td(
          textInput(
            paste0('dsClustAnnotation_',l),
            NULL,
            value=if(is.null(anns) || is.na(anns[l])) l else anns[l]
          )
        ),
        tags$td(align='right', paste0(format(
            100*sum(cl[ds$map$mapping[,1]]==l,na.rm=T)
              /dim(ds$map$mapping)[1],
            digits=3),
          "%")
        )
      )
    )
  div(
    res,
    actionButton('dsClustAnnotationDoSave', "Save annotations")
  )
}

annotationGather <- function(ds, input) {
  res <- list()
  for(l in levels(factor(ds$clust))) {
    res[[l]] <- input[[paste0('dsClustAnnotation_',l)]]
  }
  res <- unlist(res)
  if(is.null(res)) character(0) else res
}

annotationServe <- function(elem, ds, input, output) {
  output[[elem]] <- renderUI(annotationRender(ds))

  observeEvent(input$dsClustAnnotationDoSave, {
    ds$annotation <- annotationGather(ds, input)
  })
}
