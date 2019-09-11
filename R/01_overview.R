
#TODO: isolate this from ds$clust, ds$map and ds$e updates
overviewRender <- function(ds, id='', savedSel, title="Overview", defaultColor="(Density)") {
  my <- function(x) paste0(x,"_",id)

  choices <- ds$prettyColnames
  names(choices) <- choices

  #TODO: Filter saved choices!

  # Keep this in sync with 02_overview.R!

  extraDims <- c(
    '(File)',
    if(is.null(ds$clust)) NULL else '(Cluster)',
    if(is.null(ds$map)) NULL else c('(SOM X)', '(SOM Y)'),
    if(is.null(ds$e)) NULL else c('(Embedding X)', '(Embedding Y)')
  )
  names(extraDims) <- extraDims

  extraColors <- c(
    '(Black)',
    '(Density)',
    '(File)',
    if(is.null(ds$clust)) NULL else '(Cluster)'
  )
  names(extraColors) <- extraColors
  
  div(h3(title),
  fluidRow(
    column(3,
      selectInput(my('dsOverviewMarkersH'),
        "Horizontal axis",
        choices=c(extraDims, choices),
        multiple=T,
        selected=isolate(savedSel$horiz)
      ),
      selectInput(my('dsOverviewMarkersV'),
        "Vertical axis",
        choices=c(extraDims, choices),
        multiple=T,
        selected=isolate(savedSel$vert)
      ),
      selectInput(my('dsOverviewColor'),
        "Point colors",
        choices=c(extraColors, choices),
        multiple=F,
        selected=isolate(if(is.null(savedSel$color)) defaultColor else savedSel$color)
      ),
      if(is.null(isolate(savedSel$pointsize)))
        sliderPointSize(my('dsOverviewCex'))
      else sliderPointSize(my('dsOverviewCex'), value=isolate(savedSel$pointsize)),
      if(is.null(isolate(savedSel$alpha)))
        sliderAlpha(my('dsOverviewAlpha'))
      else sliderAlpha(my('dsOverviewAlpha'), value=isolate(savedSel$alpha)),
      if(is.null(isolate(savedSel$size)))
        sliderInput(my('dsOverviewSize'), "Plot size", value=15, min=10, max=50, step=1)
      else
        sliderInput(my('dsOverviewSize'), "Plot size", value=isolate(savedSel$size), min=10, max=50, step=1)
    ),
    column(9,
      uiOutput(my('uiDsOverviewPlot'))
    )
  ))
}

overviewRenderPlot <- function(id, ds, size, h, v) {
  my <- function(x) paste0(x,"_",id)
  if(h==0 && v==0) "Select markers first."
  else plotOutput(my('plotDsOverview'),
    width=paste0(size*(h+overviewPlotHistMargin), 'em'),
    height=paste0(size*(v+overviewPlotHistMargin),'em'))
}

overviewServe <- function(element, id='', ds, input, output, ...) {
  my <- function(x) paste0(x,"_",id)

  savedSel <- reactiveValues(
    horiz=NULL,
    vert=NULL,
    color=NULL,
    pointsize=NULL,
    alpha=NULL,
    size=NULL)

  observeEvent(input[[my('dsOverviewMarkersH')]],
    {savedSel$horiz <- input[[my('dsOverviewMarkersH')]]})
  observeEvent(input[[my('dsOverviewMarkersV')]],
    {savedSel$vert <- input[[my('dsOverviewMarkersV')]]})
  observeEvent(input[[my('dsOverviewColor')]],
    {savedSel$color <- input[[my('dsOverviewColor')]]})
  observeEvent(input[[my('dsOverviewCex')]],
    {savedSel$pointsize <- input[[my('dsOverviewCex')]]})
  observeEvent(input[[my('dsOverviewAlpha')]],
    {savedSel$alpha <- input[[my('dsOverviewAlpha')]]})
  observeEvent(input[[my('dsOverviewSize')]],
    {savedSel$size <- input[[my('dsOverviewSize')]]})

  output[[element]] <- renderUI({
    overviewRender(ds, id, savedSel, ...)
  })

  output[[my('plotDsOverview')]] <- renderPlot({
    plotOverview(ds, 
      input[[my('dsOverviewMarkersH')]],
      input[[my('dsOverviewMarkersV')]],
      input[[my('dsOverviewColor')]],
      input[[my('dsOverviewCex')]],
      input[[my('dsOverviewAlpha')]])
  })

  output[[my('uiDsOverviewPlot')]] <- renderUI({
    overviewRenderPlot(
      ds,
      id=id,
      input[[my('dsOverviewSize')]],
      length(input[[my('dsOverviewMarkersH')]]),
      length(input[[my('dsOverviewMarkersV')]]))
  })
}
