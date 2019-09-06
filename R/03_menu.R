
renderMenu <- function(ws) {
  choices <- as.list(ws$datasets)
  names(choices) <- ws$datasets

  fluidRow(
    div("Choices:"), #TODO: replace this with some better sep
    ilDiv(actionButton("menuCreate", "New dataset")),
    ilDiv(actionButton("menuForeign", "Upload/Download data")),
    ilDiv("Dataset: "),
    ilDiv(style='vertical-align: top',
      selectInput("pageId", label=NULL,
        selected=isolate(ws$page),
        choices=choices)),
    ilDiv(actionButton("menuOpen", "Open")),
    ilDiv(actionButton("menuSave", "Save current"))
  )
}

setMenuDataset <- function(dsid, ws,ds) {
    if(dsid!=ws$page && dsid!="") {
    if(ws$page %in% ws$datasets) {
      print("saving dataset...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
    }
    ws$page <- dsid
    if(ws$page %in% ws$datasets) {
      print("loading dataset...")
      dsInitFromDataset(ds, loadDataset(ws$page))
    }
  }
}

serveMenu <- function(ws, ds, input, output) {
  observeEvent(input$menuOpen, setMenuDataset(input$pageId, ws, ds))
  observeEvent(input$menuCreate, setMenuDataset('__dsCreate__', ws, ds))
  observeEvent(input$menuForeign, setMenuDataset('__foreign__', ws, ds))

  observeEvent(input$menuSave, {
    if(input$pageId == ws$page && ws$page %in% ws$datasets) {
      print("saving dataset explicitly...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
      showNotification(type='message', "Dataset file saved.")
    }
  })
}
