
renderMenu <- function(ws) {
  choices <- as.list(ws$datasets)
  names(choices) <- ws$datasets

  fluidRow(
    div(style="margin-top:1em;"),
    ilDiv(actionButton("menuCreate", "\u2730 Manage datasets")),
    ilDiv(actionButton("menuForeign", "\u2b83 Upload/Download data")),
    ilDiv("Dataset: "),
    ilDiv(style='vertical-align: top',
      selectInput("pageId", label=NULL,
        selected=isolate(ws$page),
        choices=choices)),
    ilDiv(actionButton("menuOpen", "\u2935 Open")),
    ilDiv(actionButton("menuSave", "\ud83d\udcbe Save current"))
  )
}

setMenuDataset <- function(dsid, ws,ds) {
    if(dsid!=ws$page && dsid!="") {
    if(ws$page %in% ws$datasets) {
      print("saving dataset...")
      showNotification("Saving...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
    }
    ws$page <- dsid
    if(ws$page %in% ws$datasets) {
      print("loading dataset...")
      showNotification("Loading...")
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
      showNotification("Saving...")
      saveDataset(ws, ws$page, dsGetDataset(ds))
      showNotification(type='message', "Dataset saved.")
    }
  })
}
