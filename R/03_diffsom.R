
#
# DiffSOM-style core values, dataset loading/saving
#

reactiveValsDiffsom <- function()
  #TODO: I guess we can create this and 3 extra lists from a single list of values right?
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
    hclust=NULL,
    clust=NULL,
    annotation=NULL
  )

#
# dataset initialization
#

dsInitFromDataset <- function(ds, dataset) {
  print("getting dataset from workspace")

  # reset everything first so that reactive stuff gets properly reloaded
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
  ds$hclust <- NULL
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
  ds$clust <- dataset$clust #switch order because of reloading shinyDendro
  ds$hclust <- dataset$hclust
  ds$annotation <- dataset$annotation
}

dsGetDataset <- function(ds) {
  print("returning dataset to workspace")
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
  hclust=ds$hclust,
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
    tabPanel('Dissection', uiOutput('diffsomGating')),
    tabPanel('Export data', uiOutput('diffsomExport'))
  )
}

#
# Embedding
#

diffsomRenderEmbedding <- function(ds) {
  choices <- ds$prettyColnames
  names(choices) <- choices
  
  #TODO: this deserves an accordion as in here: https://getbootstrap.com/docs/4.0/components/collapse/#accordion-example

  fluidRow(
    column(3,
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
    column(9,
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
    pickerInput('dsEmbedEViewCol', "Column",
      choices=c('(show density)', unname(ds$prettyColnames)),
      options=list(size=10),
      multiple=F,
      selected='(show density)'
      ),
    sliderInput('dsEmbedEExprCex', "Point size", value=1, min=0, max=5, step=.1),
    sliderInput('dsEmbedEExprAlpha', "Alpha", value=0.3, min=0.01, max=1),
    plotOutput("plotDsEmbedEExpr", width='56em', height='56em')
  )
}

#
# Clustering
#

diffsomRenderClustering <- function(ds) {
  if(is.null(ds$map)) p("Compute the SOM first")
  else div(
  fluidRow(
    column(3,
      h3("Clustering"),
      selectInput("dsClusterMethod", label="Hierarchical clustering method", choices=names(CLUSTER_METHODS), multiple=F),
      actionButton("dsClusterDoCluster", label="Create hierarchy"),
      uiOutput("uiDsClusterHeat"),
      uiOutput("uiDsClusterAnnotation")
    ),
    column(4,
      h4("Cluster assignment"),
      shinyDendroOutput("dsClustDendro", width='100%', height='50em')
    ),
    column(4,
      h4("Clusters overview"),
      uiOutput("uiDsClusterEmbedding"),
      sliderPointSize('dsClustEmbedCex'),
      sliderAlpha('dsClustEmbedAlpha')
    )
  ),
  uiOutput('diffsomClustOverview')
  )
}

diffsomRenderClusterHeat <- function(ds) {
  if(is.null(ds$hclust)) div()
  else selectInput("dsClusterHeat", "Heatmap columns",
    choices=unname(ds$colsToUse),
    multiple=T,
    selected=c())
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
    column(5,
      plotOutput('plotDsADiffL', width='30em', height='30em')
    ),
    column(5,
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
      sliderInput('dsASigPow', "p-value threshold", value=10, min=0.1, max=30),
      sliderPointSize('dsASigCex'),
      sliderAlpha('dsASigAlpha')
    ),
    column(9,
      plotOutput('plotDsASig', width='50em', height='50em'),
      p('Blue = significantly less cells in experiment group.'),
      p('Orange = significantly more cells in experiment group.'),
      p('Gray = no significant result.')
    )
  )
}

#
# Gating a.k.a. dissection
#

diffsomRenderGating <- function(ds) {
  if(is.null(ds$annotation) || is.null(ds$clust))
    p("Cluster and annotate the populations first.")
  else div(
    h3("Create population subsets"),
    pickerInput("dsGatePops",
      "Populations in the new dataset",
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

  overviewServe('diffsomOverview', 'Standalone', ds, input, output)

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
        data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)],
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

  output$plotDsEmbedEExpr <- renderPlot({
    plotEmbedExpr(
      ds$e,
      if(input$dsEmbedEViewCol=='(show density)') NULL
      else ds$data[,findColIds(input$dsEmbedEViewCol, ds$prettyColnames)],
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
      ds$hclust <- CLUSTER_METHODS[[input$dsClusterMethod]](
        codes=ds$map$codes,
        data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)],
        importance=NULL, #TODO: add support
        mapping=ds$map$mapping[,1])
    }
  })

  output$uiDsClusterHeat <- renderUI(diffsomRenderClusterHeat(ds))

  output$dsClustDendro <- renderShinyDendro({
    if(!is.null(ds$hclust)) {
      colors <- getHeatmapColors(ds, input$dsClusterHeat)
      shinyDendro('dsClustDendroOutput', 
        ds$hclust$height,
        ds$hclust$merge,
        ds$hclust$order,
        heatmap=colors,
        assignment=if(is.null(isolate(ds$clust))) NULL else unsetClustNAs(isolate(ds$clust))
      )
    } else NULL
  })

  observeEvent(input$dsClustDendroOutput, {
    ds$clust <- setClustNAs(input$dsClustDendroOutput)
  })

  output$uiDsClusterEmbedding <- renderUI({
    diffsomRenderClusterEmbedding(ds)
  })

  output$plotDsClustEmbed <- renderPlot({
    plotClustEmbed(
      ds$e,
      ds$clust[ds$map$mapping[,1]],
      input$dsClustEmbedCex,
      input$dsClustEmbedAlpha)
  })

  overviewServe('diffsomClustOverview', 'Clust', ds, input, output)

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
