
renderMenu <- function(ws) {
  choices <- as.list(c(ws$datasets, '__foreign__', '__dsCreate__'))
  names(choices) <- c(ws$datasets, 'Upload/Download', 'Create datasets')

  div(
    selectInput("pageId", label="Select a page",
      selected=isolate(ws$page),
      choices=choices),
    actionButton("pageSave", "Save current dataset")
  )
}

serveMenu <- function(ws, ds, input, output) {
  observeEvent(input$pageId, {
    if(input$pageId!=ws$page && input$pageId!="") {
    if(ws$page %in% ws$datasets) {
      print("saving dataset...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
    }
    ws$page <- input$pageId
    if(ws$page %in% ws$datasets) {
      print("loading dataset...")
      dsInitFromDataset(ds, loadDataset(ws$page))
    }
  }})

  observeEvent(input$pageSave, {
    if(input$pageId == ws$page && ws$page %in% ws$datasets) {
      print("saving dataset explicitly...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
      showNotification(type='message', "Dataset file saved.")
    }
  })
}
