
#
# DiffSOM core values, dataset loading/saving
#

reactiveValsDiffsom <- function()
  reactiveValues(
    files=NULL,
    cellFile=NULL,
    data=NULL,
    prettyColnames=NULL,
    seed=NULL,
    colsToUse=NULL,
    xdim=NULL,
    ydim=NULL,
    rlen=NULL,
    map=NULL,
    smooth=NULL,
    adjust=NULL,
    k=NULL,
    emcoords=NULL,
    e=NULL,
    #TODO: importance
    nclust=NULL,
    clust=NULL,
    annotation=NULL
  )

# TODO: erase this when it gets exported from DiffSOM
ds_hclust <- function(codes, data, importance=NULL, mapping, k=7){
    # This code was proudly adapted from FlowSOM
    d <- stats::dist(codes, method = "euclidean")
    fit <- stats::hclust(d, method = "ward.D2")
    stats::cutree(fit, k=k)
    #end of FlowSOM copy
}

ds_kmeans <- function(codes, data, importance=NULL, mapping, k=7){
    kmeans(x=codes, centers=k)$cluster
}

ds_consensus <- function(codes, data, importance=NULL, mapping, k=7){
    seed <- sample(1000000000,1) #force "normal" behavior
    # another FlowSOM copy begins
    results <- suppressMessages(ConsensusClusterPlus::ConsensusClusterPlus(
                                t(codes),
                                maxK=k, reps=100, pItem=0.9, pFeature=1, 
                                title=tempdir(), plot="pdf", verbose=FALSE,
                                clusterAlg="hc",
                                distance="euclidean",
                                seed=seed
    ))
    results[[k]]$consensusClass
    # FlowSOM copy ends
}
#TODO: erase ends here

#
# constants
#

CLUSTER_METHODS=list(
  HCA=ds_hclust,
  kMeans=ds_kmeans,
  Consensus=ds_consensus
)

#
# dataset initialization
#

dsInitFromDataset <- function(ds, dataset) {
  print("getting dataset from workspace")
  print(names(dataset))

  # reset everything first so that stuff gets properly reloaded
  ds$files <- NULL
  ds$cellFile <- NULL
  ds$data <- NULL
  ds$prettyColnames <- NULL
  ds$seed <- NULL
  ds$colsToUse <- NULL
  ds$xdim <- NULL
  ds$ydim <- NULL
  ds$rlen <- NULL
  ds$map <- NULL
  ds$smooth <- NULL
  ds$adjust <- NULL
  ds$k <- NULL
  ds$emcoords <- NULL
  ds$e <- NULL
  ds$nclust <- NULL
  ds$clust <- NULL
  ds$annotation <- NULL

  # and load now
  ds$files <- dataset$files
  ds$cellFile <- dataset$cellFile
  ds$data <- dataset$data
  ds$prettyColnames <- dataset$prettyColnames
  ds$seed <- dataset$seed
  ds$colsToUse <- dataset$colsToUse
  ds$xdim <- dataset$xdim
  ds$ydim <- dataset$ydim
  ds$rlen <- dataset$rlen
  ds$map <- dataset$map
  ds$smooth <- dataset$smooth
  ds$adjust <- dataset$adjust
  ds$k <- dataset$k
  ds$emcoords <- dataset$emcoords
  ds$e <- dataset$e
  ds$nclust <- dataset$nclust
  ds$clust <- dataset$clust
  ds$annotation <- dataset$annotation
}

dsGetDataset <- function(ds) {
  print("returning dataset to workspace")
  print("saving annotation:")
  print(ds$annotation)
  list(
  files=ds$files,
  cellFile=ds$cellFile,
  data=ds$data,
  prettyColnames=ds$prettyColnames,
  seed=ds$seed,
  colsToUse=ds$colsToUse,
  xdim=ds$xdim,
  ydim=ds$ydim,
  rlen=ds$rlen,
  map=ds$map,
  smooth=ds$smooth,
  adjust=ds$adjust,
  k=ds$k,
  emcoords=ds$emcoords,
  e=ds$e,
  nclust=ds$nclust,
  clust=ds$clust,
  annotation=ds$annotation)
}

#
# Tab renderers
#

renderDiffsom <- function() {
  tabsetPanel(type='tabs',
    tabPanel('Data overview', uiOutput('diffsomOverview')),
    tabPanel('Embedding', uiOutput('diffsomEmbedding')),
    tabPanel('Clustering', uiOutput('diffsomClustering')),
    tabPanel('Analysis', uiOutput('diffsomAnalysis')),
    tabPanel('Gating', uiOutput('diffsomGating')),
    tabPanel('Export data', uiOutput('diffsomExport'))
  )
}

diffsomRenderOverview <- function(ds) {
  tabsetPanel(type='tabs',
    tabPanel('Expressions in files', uiOutput('diffsomOverviewExprs')),
    tabPanel('Dot plots', uiOutput('diffsomOverviewDots'))
  )
}

#
# Overview / Expressions
#

diffsomRenderOverviewExprs <- function(ds) {
  choices <- ds$prettyColnames
  names(choices) <- choices
  
  fluidRow(
    column(3,
      pickerInput('dsOverviewExprsMarkers',
        "Markers",
        choices=choices,
        options=list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = 'count > 2'
        ),
        multiple=T
      )
    ),
    column(7,
      uiOutput("plotDsOverviewExprsUi")
    )
  )
}

#
# Overview / Dots
#

diffsomRenderOverviewDots <- function(ds) {
  
  choices <- ds$prettyColnames
  names(choices) <- choices
  
  fluidRow(
    column(3,
      checkboxInput("dsOverviewDotFiles",
        "Separate colors for files",
        value=F
      ),
      numericInput("dsOverviewDotAlpha",
        "Dot alpha",
        value=.5,
        min=0.02,
        max=1,
        step=0.02
      ),
      numericInput("dsOverviewDotCount",
        "Dot count",
        value=20000,
        min=1,
        max=1000000,
        step=1
      ),
      pickerInput('dsOverviewDotV',
        "Vertical axis",
        choices=choices,
        options=list(size=10),
        multiple=F
      ),
      pickerInput('dsOverviewDotH',
        "Horizontal axis",
        choices=choices,
        options=list(size=10),
        multiple=F
      )
    ),
    column(7,
      uiOutput("plotDsOverviewDotsUi")
    )
  )
}

#
# Embedding
#

diffsomRenderEmbedding <- function(ds) {
  choices <- ds$prettyColnames
  names(choices) <- choices

  fluidRow(
    column(4,
      h3("SOM"),
      pickerInput('dsEmbedColsToUse', "Columns to use",
        choices=choices,
        options=list(
          size=10,
          `actions-box` = TRUE, 
          `selected-text-format` = 'count > 3'
        ),
        multiple=T,
        selected=isolate(if(is.null(ds$colsToUse)) ds$prettyColnames else ds$colsToUse)),
      sliderInput('dsEmbedXdim', "SOM nodes X", value=isolate(if(is.null(ds$xdim)) 10 else ds$xdim), min=2, max=50, step=1),
      sliderInput('dsEmbedYdim', "SOM nodes Y", value=isolate(if(is.null(ds$ydim)) 10 else ds$ydim), min=2, max=50, step=1),
      sliderInput('dsEmbedRlen', "SOM repeats", value=isolate(if(is.null(ds$rlen)) 10 else ds$rlen), min=1, max=100, step=1),
      numericInput('dsEmbedSeed', "Random seed", value=isolate(if(is.null(ds$seed)) 1 else ds$seed), min=0, max=999999999, step=1),
      actionButton('dsEmbedDoSOM', "Compute SOM"),
      uiOutput('uiDsEmbedParams')
    ),
    column(6,
      h3("SOM view"),
      uiOutput('uiDsEmbedSOMView'),
      h3("Embedding view"),
      uiOutput('uiDsEmbedEView')
    )
  )
}

diffsomRenderEmbeddingParams <- function(ds) {
  emcoords <- c('flat','som','mst','tsne','umap')
  names(emcoords) <- c('Flat', 'U-matrix', 'MST-based', 'tSNE meta-embedding', 'UMAP meta-embedding')
  div(
    h3("Embedding"),
    sliderInput('dsEmbedSmooth', "Smoothing", value=if(is.null(ds$smooth)) 0 else ds$smooth, min=-5, max=5, step=.1),
    sliderInput('dsEmbedAdjust', "Adjust", value=if(is.null(ds$adjust)) 1 else ds$adjust, min=0, max=5, step=.1),
    sliderInput('dsEmbedK', "NNs (k)", value=if(is.null(ds$k)) 20 else ds$k, min=3, max=ds$xdim*ds$ydim, step=1),
    selectInput('dsEmbedCoords', "Embedding coords manipulation", choices=emcoords, selected=ds$emcoords),
    actionButton('dsEmbedDoEmbed', "Compute Embedding")
  )
}

diffsomRenderEmbedSOMView <- function(ds) {
  if(is.null(ds$map)) p("Compute the map first")
  else div(
    pickerInput('dsEmbedSOMViewCol', "Column",
      choices=isolate(ds$colsToUse),
      options=list(size=10),
      multiple=F,
      selected=isolate(ds$colsToUse[1])
      ),
    plotOutput("plotDsEmbedSOMView", width=paste0(2*(1+ds$map$xdim),'em'), height=paste0(2*(1+ds$map$ydim),'em'))
  )
}

diffsomRenderEmbedEView <- function(ds) {
  if(is.null(ds$e)) p("Compute the embedding first")
  else div(
    h3("Density"),
    plotOutput('plotDsEmbedEDensity', width='30em', height='25em'),
    h3("Expressions"),
    pickerInput('dsEmbedEViewCol', "Column",
      choices=unname(ds$prettyColnames),
      options=list(size=10),
      multiple=F,
      selected=ds$prettyColnames[1]
      ),
    sliderInput('dsEmbedEExprCex', "Point size", value=0.5, min=0.1, max=2),
    sliderInput('dsEmbedEExprAlpha', "Alpha", value=0.3, min=0.01, max=1),
    plotOutput("plotDsEmbedEExpr", width='64em', height='56em')
  )
}

#
# Clustering
#

diffsomRenderClustering <- function(ds) {
  if(is.null(ds$map)) p("Compute the SOM first")
  else fluidRow(
    column(4,
      h3("Clustering"),
      selectInput("dsClusterMethod", label="Clustering algorithm", choices=names(CLUSTER_METHODS), multiple=F),
      numericInput("dsClusterNClust", label="Number of clusters", value=10, min=2, max=30, step=1),
      #TODO: seed?
      actionButton("dsClusterDoCluster", label="Run clustering"),
      h3("Annotations"),
      uiOutput("uiDsClusterAnnotate"),
      uiOutput("uiDsClusterAnnotateSummary")
    ),
    column(6,
      h3("Expressions in clusters"),
      pickerInput("dsClusterExpressionCols",
        "Columns to display",
        choices=unname(ds$prettyColnames),
        options=list(size=10),
        multiple=T),
      uiOutput("uiDsClusterExpressions"),
      h3("Embedded clusters"),
      pickerInput("dsClusterEmbedCol",
        "Columns to display",
        choices=c("(show clusters)", unname(ds$prettyColnames)),
        options=list(size=10),
        selected="(show clusters)",
        multiple=F),
      uiOutput("uiDsClusterEmbedding"),
      sliderInput('dsClustEmbedCex', "Point size", value=0.5, min=0.1, max=2),
      sliderInput('dsClustEmbedAlpha', "Alpha", value=0.3, min=0.01, max=1)
    )
  )
}

diffsomRenderClusterAnnotation <- function(ds) {
  if(is.null(ds$clust) || is.null(ds$nclust)) p("Compute the clustering first")
  else {
    res <- tagList()
    for(i in 1:(ds$nclust))
      res <- tagAppendChild(res,
        #TODO: this doesn't reload properly, fix later.
        textInput(paste0("dsClusterAnnotation",i), NULL, value=isolate(if(is.null(ds$annotation)||is.na(ds$annotation[i])) paste("Cluster",i) else ds$annotation[i])))
    res
  }
}

gatherAnnotation <- function(ds, input) {
  sapply(1:ds$nclust, function(x) input[[paste0("dsClusterAnnotation",x)]])
}

diffsomRenderClusterAnnotationSummary <- function(ds,input) {
  #this transparently automagically gathers all annotations to ds$annotation
  if(is.null(ds$clust) || is.null(ds$nclust)) div()
  else {
    anns <- gatherAnnotation(ds, input)
    ds$annotation <- anns
    p(paste("(total ", length(table(anns)), "annotated populations)"))
  }
}

diffsomRenderClusterExpressions <- function(ds, colsSelected) {
  if(is.null(ds$clust) || is.null(ds$nclust))
    p("Compute the clustering first")
  else if (colsSelected==0)
    p("Select at least one column")
  else 
    plotOutput('plotDsClustExprs',
      width='50em', height=paste0(10*(1+colsSelected),'em'))
}

diffsomRenderClusterEmbedding <- function(ds) {
  if(is.null(ds$e))
    p("Compute the embedding first")
  else if(is.null(ds$clust))
    p("Compute the clustering first")
  else plotOutput('plotDsClustEmbed',
    width='40em', height='40em')
}

#
# Analysis
#

diffsomRenderAnalysis <- function(ds) {
  tabsetPanel(type='tabs',
    tabPanel('Compare files', uiOutput('diffsomAnalysisDiff')),
    tabPanel('Significance plots', uiOutput('diffsomAnalysisSignificance'))
  )
}

diffsomRenderADiff <- function(ds) {
  if(is.null(ds$e))
    p("Embed the population first")
  else fluidRow(
    column(2,
      pickerInput('dsADiffFilesLeft',
        'Left',
        choices=ds$files,
        multiple=T),
      pickerInput('dsADiffFilesRight',
        'Right',
        choices=ds$files,
        multiple=T),
      pickerInput('dsADiffColor',
        'Color',
        choices=c('(density)',
                  if(is.null(ds$annotation)) NULL else '(cluster)',
                  '(file)',
                  unname(ds$prettyColnames)),
        multiple=F,
        selected='(density)'),
      sliderInput('dsADiffCex', "Point size", value=0.5, min=0.1, max=2),
      sliderInput('dsADiffAlpha', "Alpha", value=0.3, min=0.01, max=1)
    ),
    column(4,
      plotOutput('plotDsADiffL', width='30em', height='30em')
    ),
    column(4,
      plotOutput('plotDsADiffR', width='30em', height='30em')
    )
  )
}

#TODO: paired possibility (any reasonable way to assign the pairs in Shiny?)
diffsomRenderASignificance <- function(ds) {
  if(is.null(ds$e))
    p("Embed the population first")
  else fluidRow(
    column(3,
      pickerInput('dsASigControl',
        'Control sample group',
        choices=ds$files,
        multiple=T),
      pickerInput('dsASigExperiment',
        'Experiment sample group',
        choices=ds$files,
        multiple=T),
      selectInput('dsASigGran', label='Granularity',
        choices=c(
          'SOM',
          if(is.null(ds$clust)) NULL else 'Metaclusters',
          if(is.null(ds$annotation)) NULL else 'Annotated clusters'),
        selected='SOM',
        multiple=F),
      sliderInput('dsASigPow', "p-value transform", value=10, min=0.1, max=30),
      sliderInput('dsASigCex', "Point size", value=0.5, min=0.1, max=2),
      sliderInput('dsASigAlpha', "Alpha", value=0.3, min=0.01, max=1)
    ),
    column(7,
      plotOutput('plotDsASig', width='50em', height='50em'),
      p('Blue = significantly less cells in experiment group.'),
      p('Orange = significantly more cells in experiment group.'),
      p('Gray = no significant result.')
    )
  )
}

#
# Gating
#

diffsomRenderGating <- function(ds) {
  if(is.null(ds$annotation) || is.null(ds$clust))
    p("Cluster and annotate the populations first.")
  else div(
    h3("Gate populations"),
    pickerInput("dsGatePops",
      "Populations",
      choices=levels(factor(ds$annotation)),
      multiple=T,
      options=list(size=10)),
    textInput("dsGateNewName", "New dataset name", placeholder="Enter name"),
    actionButton("dsGateDoGate", "Create dataset")
  )
}

#
# Export
#

diffsomRenderExport <- function(ds) {
  div(
    h3("Export population statistics"),
    if(is.null(ds$annotation) || is.null(ds$clust)) p("Cluster and annotate the populations first.")
    else shinySaveButton("dsExportPops", "Population cell counts CSV", "Save a the population cell counts", filename='populations.csv'),
    h3("Export DiffSOM objects"),
    if(is.null(ds$map)) p("Compute SOM for exporting it first")
    else shinySaveButton("dsExportDSMap", "DiffSOM map RDS", "Save the DiffSOM MAP RDS", filename='map.RDS'),
    shinySaveButton("dsExportDSObj", "DiffSOM object RDS", "Save DiffSOM Object RDS", filename='dataset.RDS'),
    h3("Different export formats"),
    shinySaveButton("dsExportFCS", "Export FCS", "Save Flow Cytometry Standard export file", filename='export.fcs'),
    shinySaveButton("dsExportCSV", "Export CSV", "Save CSV export", filename='export.csv')
  )
}

#
# Server
#

serveDiffsom <- function(ws, ds, input, output) {

  #
  # Overview tab
  #

  output$diffsomOverview <- renderUI({
    diffsomRenderOverview(ds)
  })

  # Overview / Single-marker Expression in files

  output$diffsomOverviewExprs <- renderUI({
    diffsomRenderOverviewExprs(ds)
  })

  debouncedOverviewExprsMarkers <- debounce(input$dsOverviewExprsMarkers, 100)

  output$plotDsOverviewExprsUi <- renderUI({
    plotOutput("plotDsOverviewExprs",
      width='100%',
      height=paste0(5+10*length(debouncedOverviewExprsMarkers()),'em'))
  })

  output$plotDsOverviewExprs <- renderPlot({
    plotOverviewExprs(
      ds$data,
      ds$files,
      ds$cellFile,
      ds$prettyColnames,
      debouncedOverviewExprsMarkers()
    )
  })

  # Overview / Dot plots

  debouncedOverviewDotParams <- debounce(
    list(
      files=input$dsOverviewDotFiles,
      alpha=input$dsOverviewDotAlpha,
      count=input$dsOverviewDotCount,
      h=input$dsOverviewDotH,
      v=input$dsOverviewDotV),
    100)

  output$diffsomOverviewDots <- renderUI({
    diffsomRenderOverviewDots(ds)
  })
  
  output$plotDsOverviewDotsUi <- renderUI({
    plotOutput("plotDsOverviewDots",
      width=if(debouncedOverviewDotParams()$files) '80em' else '50em',
      height='50em')
  })

  output$plotDsOverviewDots <- renderPlot({
    plotOverviewDots(
      ds$data,
      ds$files,
      ds$cellFile,
      ds$prettyColnames,
      debouncedOverviewDotParams()$files,
      debouncedOverviewDotParams()$alpha,
      debouncedOverviewDotParams()$count,
      debouncedOverviewDotParams()$h,
      debouncedOverviewDotParams()$v)
  })

  #
  # Embedding tab
  #

  output$diffsomEmbedding <- renderUI({
    diffsomRenderEmbedding(ds)
  })

  observeEvent(input$dsEmbedColsToUse, {ds$colsToUse <- input$dsEmbedColsToUse})
  observeEvent(input$dsEmbedXdim, {ds$xdim <- input$dsEmbedXdim})
  observeEvent(input$dsEmbedYdim, {ds$ydim <- input$dsEmbedYdim})
  observeEvent(input$dsEmbedRlen, {ds$rlen <- input$dsEmbedRlen})
  observeEvent(input$dsEmbedSeed, {ds$seed <- input$dsEmbedSeed})

  observeEvent(input$dsEmbedDoSOM, {
    set.seed(ds$seed)
    ds$map <- EmbedSOM::SOM(
      data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)],
      xdim=ds$xdim,
      ydim=ds$ydim,
      rlen=ds$rlen,
      negAlpha=0,
      negRadius=1)
    ds$e <- NULL
  })

  output$uiDsEmbedParams <- renderUI({
    diffsomRenderEmbeddingParams(ds)
  })

  output$uiDsEmbedSOMView <- renderUI({
    diffsomRenderEmbedSOMView(ds)
  })

  output$plotDsEmbedSOMView <- renderPlot({
    plotSOMOverview(
      ds$map$xdim,
      ds$map$ydim,
      ds$map$codes[,findColIds(input$dsEmbedSOMViewCol, ds$colsToUse)],
      ds$data[,findColIds(input$dsEmbedSOMViewCol, ds$prettyColnames)],
      ds$map$mapping[,1])
  })
  
  observeEvent(input$dsEmbedSmooth, {ds$smooth <- input$dsEmbedSmooth})
  observeEvent(input$dsEmbedAdjust, {ds$adjust <- input$dsEmbedAdjust})
  observeEvent(input$dsEmbedK, {ds$k <- input$dsEmbedK})
  observeEvent(input$dsEmbedCoords, {ds$emcoords <- input$dsEmbedCoords})

  observeEvent(input$dsEmbedDoEmbed, {
    if(!is.null(ds$map)) {
      ds$e <- EmbedSOM::EmbedSOM(
        data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)], #TODO: perhaps use ds$map$colsUsed?
        map=ds$map,
        smooth=ds$smooth,
        adjust=ds$adjust,
        k=ds$k,
        emcoords=ds$emcoords)
      }
  })

  output$uiDsEmbedEView <- renderUI({
    diffsomRenderEmbedEView(ds)
  })

  output$plotDsEmbedEDensity <- renderPlot({
    plotEmbedDensity(ds$e)
  })

  output$plotDsEmbedEExpr <- renderPlot({
    plotEmbedExpr(
      ds$e,
      ds$data[,findColIds(input$dsEmbedEViewCol, ds$prettyColnames)],
      input$dsEmbedEExprCex,
      input$dsEmbedEExprAlpha
    )
  })
  
  #
  # Clustering tab
  #

  output$diffsomClustering <- renderUI({
    diffsomRenderClustering(ds)
  })

  observeEvent(input$dsClusterDoCluster, {
    if(!is.null(ds$map)) {
      ds$nclust <- input$dsClusterNClust
      ds$clust <- CLUSTER_METHODS[[input$dsClusterMethod]](
        codes=ds$map$codes,
        data=ds$fs$data[,ds$map$colsUsed], #TODO: perhaps use findColIds?
        importance=NULL, #TODO: add support
        mapping=ds$map$mapping[,1],
        k=ds$nclust)
    }
  })

  output$uiDsClusterAnnotate <- renderUI({
    diffsomRenderClusterAnnotation(ds)
  })

  output$uiDsClusterAnnotateSummary <- renderUI({
    diffsomRenderClusterAnnotationSummary(ds, input)
  })

  output$uiDsClusterExpressions <- renderUI({
    diffsomRenderClusterExpressions(ds, length(input$dsClusterExpressionCols))
  })

  output$uiDsClusterEmbedding <- renderUI({
    diffsomRenderClusterEmbedding(ds)
  })

  output$plotDsClustExprs <- renderPlot({
    if(length(input$dsClusterExpressionCols)>0)
      plotClustExpr(
        input$dsClusterExpressionCols,
        ds$data[,findColIds(input$dsClusterExpressionCols, ds$prettyColnames)],
        ds$annotation[ds$clust[ds$map$mapping[,1]]])
  })

  output$plotDsClustEmbed <- renderPlot({
    plotClustEmbed(
      ds$e,
      if(input$dsClusterEmbedCol %in% ds$prettyColnames)
        ds$data[,findColIds(input$dsClusterEmbedCol, ds$prettyColnames)]
      else NULL,
      ds$annotation[ds$clust[ds$map$mapping[,1]]],
      input$dsClustEmbedCex,
      input$dsClustEmbedAlpha)
  })

  #
  # Analysis tab
  #

  output$diffsomAnalysis <- renderUI({
    diffsomRenderAnalysis(ds)
  })

  output$diffsomAnalysisDiff <- renderUI({
    diffsomRenderADiff(ds)
  })

  output$diffsomAnalysisSignificance <- renderUI({
    diffsomRenderASignificance(ds)
  })

  output$plotDsADiffL <- renderPlot({
    if(length(input$dsADiffFilesLeft)>0)
      plotDsADiff(
        input$dsADiffFilesLeft,
        input$dsADiffColor,
        ds$files, ds$data, ds$cellFile, ds$e,
        ds$prettyColnames,
        ds$annotation[ds$clust[ds$map$mapping[,1]]],
        input$dsADiffAlpha,
        input$dsADiffCex)
  })

  output$plotDsADiffR <- renderPlot({
    if(length(input$dsADiffFilesRight)>0)
      plotDsADiff(
        input$dsADiffFilesRight,
        input$dsADiffColor,
        ds$files, ds$data, ds$cellFile, ds$e,
        ds$prettyColnames,
        ds$annotation[ds$clust[ds$map$mapping[,1]]],
        input$dsADiffAlpha,
        input$dsADiffCex)
  })

  output$plotDsASig <- renderPlot({
    if(length(input$dsASigControl)>0 && length(input$dsASigExperiment)>0)
      plotDsASig(
        input$dsASigControl,
        input$dsASigExperiment,
        ds$files, ds$cellFile, ds$e,
        input$dsASigGran, ds$map$mapping[,1], ds$clust, ds$annotation,
        input$dsASigPow,
        input$dsASigCex,
        input$dsASigAlpha)
  })

  #
  # Gating tab
  #

  output$diffsomGating <- renderUI({
    diffsomRenderGating(ds)
  })

  observeEvent(input$dsGateDoGate, {
    if(datasetExists(ws,input$dsGateNewName)) {
      showNotification(type='error', "Dataset of same name already exists.")
    } else if (!datasetNameValid(input$dsGateNewName)) {
      showNotification(type='error', "Dataset name is invalid.")
    }else if(length(input$dsGatePops)==0) {
      showNotification(type='error', "No populations selected")
    } else {
      filt <- ds$annotation[ds$clust[ds$map$mapping[,1]]] %in% input$dsGatePops
      nds <- list(
        files=ds$files,
        data=ds$data[filt,],
        cellFile=ds$cellFile[filt],
        prettyColnames=ds$prettyColnames,
        colsToUse=ds$colsToUse)

      saveDataset(ws, input$dsGateNewName, nds)
      showNotification(type='message', "New dataset created.")
    }
  })

  #
  # Export tab
  #

  output$diffsomExport <- renderUI({
    diffsomRenderExport(ds)
  })

  shinyFileSave(input, "dsExportPops", roots=getForeignRoots())
  shinyFileSave(input, "dsExportDSMap", roots=getForeignRoots())
  shinyFileSave(input, "dsExportDSObj", roots=getForeignRoots())
  shinyFileSave(input, "dsExportFCS", roots=getForeignRoots())
  shinyFileSave(input, "dsExportCSV", roots=getForeignRoots())

  observeEvent(input$dsExportPops, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportPops)
    if(dim(outpath)[1]==1) {
      d <- table(data.frame(File=ds$files[ds$cellFile], Annotation=ds$annotation[ds$clust[ds$map$mapping[,1]]]))
      write.csv(d, outpath$datapath)
      showNotification(type='message', "Population export saved.")
    }
  })

  observeEvent(input$dsExportDSMap, { #TODO: this.
  })
}
