
ui <- fluidPage(
  fluidRow(
    column(3, div(
      img(src=paste0('data:image/png;base64,', logoImgSrc),
        style="width:3em; height:3em; vertical-align:baseline;"),
      h1("ShinySOM", style="display: inline-block;")
    )),
    column(9, uiOutput("selectPage"))
  ),
  uiOutput("mainPage")
)

datasetNameValid <- function(name) {
  ((regexpr(text=name, pattern='^[a-zA-Z0-9 ,._@#%^&()-]*$')==1) && (nchar(name)>2))
}

datasetExists <- function(workspace, name) {
  name %in% workspace$datasets
}

getDatasetPath <- function() {
  options('ShinySOM.Datasets')$ShinySOM.Datasets
}

listDatasets <- function() {
  tools::file_path_sans_ext(dir(path=getDatasetPath(), pattern='.*\\.shinysom'))
}

saveDataset <- function(workspace, name, data) {
  if(!(name %in% workspace$datasets))
    workspace$datasets <- sort(c(workspace$datasets, name))
  saveRDS(data, paste0(getDatasetPath(),'/',name,'.shinysom'))
}

loadDataset <- function(name) {
  readRDS(paste0(getDatasetPath(),'/',name,'.shinysom'))
}

removeDataset <- function(workspace, name) {
  unlink(paste0(getDatasetPath(),'/',name,'.shinysom'))
  workspace$datasets <- workspace$datasets[workspace$datasets != name]
}

server <- function(input, output, session) {
  workspace <- reactiveValues(datasets=listDatasets(), page='__foreign__')

  foreign <- reactiveValsForeign()
  diffsom <- reactiveValsDiffsom()

  output$selectPage <- renderUI({
    renderMenu(workspace)
  })

  output$mainPage <- renderUI({
    if(workspace$page=='__dsCreate__') uiOutput("dsCreate")
    # else if(workspace$page=='__save__') uiOutput("saving")
    else if(workspace$page=='__foreign__') uiOutput("foreign")
    else uiOutput("diffsom")
  })

  #output$saving <- renderUI({renderSaving()})
  output$foreign <- renderUI({renderForeign(foreign)})
  output$diffsom <- renderUI({renderDiffsom()})

  serveMenu(workspace, diffsom, input, output)
  serveForeign(foreign, input, output)
  serveDsCreate(workspace, input, output, session)
  serveDiffsom(workspace, diffsom, input, output, session)
}

#' Run ShinySOM in browser
#' @export
ShinySOM <- function(
    dataset.dir='datasets',
    roots=c(`Session storage`='data'),
    ...) {
  if(!dir.exists(dataset.dir)) stop("Dataset directory does not exist!")
  if(!all(dir.exists(roots))) stop("Some of the storage root directories do not exist!")
  options(ShinySOM.Datasets=paste0(dataset.dir, '/'))
  options(ShinySOM.foreignRoots=roots)
  options(shiny.maxRequestSize=100*2^20)
  shinyApp(ui=ui, server=server, ...)
}

# how to run this:
#ShinySOM(options=list(port=8087))
