
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
  #switch order here because of reloading shinyDendro&annotations
  ds$annotation <- dataset$annotation
  ds$clust <- dataset$clust
  ds$hclust <- dataset$hclust
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
    tabPanel('Transform&Scale', uiOutput('diffsomTransform')),
    tabPanel('Embedding', uiOutput('diffsomEmbedding')),
    tabPanel('Clustering', uiOutput('diffsomClustering')),
    tabPanel('Analysis', uiOutput('diffsomAnalysis')),
    tabPanel('Dissection', uiOutput('diffsomGating')),
    tabPanel('Export data', uiOutput('diffsomExport'))
  )
}

#
# Transformations
#

diffsomRenderTransform <- function(ds) {
  choices <- ds$prettyColnames
  names(choices) <- choices

  trs1 <- unlist(lapply(TRANSFORM_LIST, function(x) x$name))
  trs <- names(trs1)
  names(trs) <- unname(trs1)

  div(
    h3("Data transformation editor"),
    fluidRow(
      column(3,
        pickerInput('dsTransCols', "Columns to modify",
          choices=choices,options=list(
            `actions-box`=TRUE,
            `selected-text-format` = 'count > 3'
          ),
          multiple=T
        )
      ),
      column(3,
        h4("Transformation parameters"),
        radioButtons('dsTransTrType', "Transformation type",
          choices=trs,
          selected='none' #check this in 00_transforms.R!
        ),
        uiOutput('diffsomTransformParams')
      ),
      column(3,
        h4("Scaling"),
        checkboxInput('dsTransNormalize', "Normalize the column to zero mean and unit variance"),
        checkboxInput('dsTransDoImportance', "Multiply the values by a constant"),
        numericInput('dsTransImportance', "Column importance constant", value=1, min=0.01, max=100, step=0.01)
      ),
      column(3,
        h4("Apply transformation"),
        p("This action modifies the current dataset. If you want to preserve the original data, clone the dataset before applying the transformation. Applying the same transformation twice is usually not recommended."),
        checkboxInput('dsTransConfirm', "Confirm the modification", value=FALSE),
        actionButton('dsTransApply', "Apply!")
      )
    ),
    uiOutput('diffsomTransformOverview')
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
      tooltip("You may choose parameters for constructing the self-organizing map (SOM) here. The SOM will be later used for describing and clustering the cell space.", h3("SOM")),
      tooltip("Select markers and data columns that carry information useful for this step of analysis and dissection.",
      pickerInput('dsEmbedColsToUse', "Columns to use",
        choices=choices,
        options=list(
          size=10,
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 3'
        ),
        multiple=T,
        selected=isolate(if(is.null(ds$colsToUse)) ds$prettyColnames else ds$colsToUse))),
      tooltip("Choose the X and Y sizes of SOM. The SOM will be able to approximate roughly X*Y different clusters. Larger SOMs capture more details, but take longer to compute.",
      sliderInput('dsEmbedXdim', "SOM nodes X", value=isolate(if(is.null(ds$xdim)) 16 else ds$xdim), min=2, max=50, step=1)),
      sliderInput('dsEmbedYdim', "SOM nodes Y", value=isolate(if(is.null(ds$ydim)) 16 else ds$ydim), min=2, max=50, step=1),
      tooltip("How many times the data will be presented to SOM, larger values may produce a better fitting SOM for the cost of computation time. Increase the value slightly if working with less than 10.000 cells. For datasets over 100.000 cells, the value can be safely decreased. Generally, epochs*cells should be at least 100.000.",
      sliderInput('dsEmbedRlen', "SOM epochs", value=isolate(if(is.null(ds$rlen)) 10 else ds$rlen), min=1, max=100, step=1)),
      tooltip("Random seed for SOM training. Choose a different value to train a different SOM.",
      numericInput('dsEmbedSeed', "Random seed", value=isolate(if(is.null(ds$seed)) 1 else ds$seed), min=0, max=999999999, step=1)),
      tooltip("SOM training may take several seconds to several minutes (on datasets larger than around one million cells).",
      actionButton('dsEmbedDoSOM', "Compute SOM")),
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
    tooltip("Embedding serves for visualizing the entire dataset in 2D; by itself it has no direct impact on the analysis.",
    h3("Embedding")),
    tooltip("Increase to produce rounder embedding; decrease to see (possibly uninteresting) details.",
    sliderInput('dsEmbedSmooth', "Smoothing", value=if(is.null(ds$smooth)) 0 else ds$smooth, min=-5, max=5, step=.1)),
    tooltip("Adjust parameter for EmbedSOM. Larger values remove non-local information from approximation (e.g. pathways or noise that is not captured by SOM).",
    sliderInput('dsEmbedAdjust', "Adjust", value=if(is.null(ds$adjust)) 1 else ds$adjust, min=0, max=5, step=.1)),
    tooltip("K parameter for EmbedSOM. Large values impact computation time, and may produce too smooth embedding. Small values cause approximation failures.",
    sliderInput('dsEmbedK', "NNs (k)", value=if(is.null(ds$k)) 20 else ds$k, min=3, max=ds$xdim*ds$ydim, step=1)),
    tooltip("Meta-embedding. The selected algorithm will embed only the SOM nodes, atop of which actual cells will be quickly fitted by EmbedSOM. Some choices require lowering Adjust.",
    selectInput('dsEmbedCoords', "Embedding coords manipulation", choices=emcoords, selected=ds$emcoords)),
    tooltip("Embedding computation generally takes several seconds for datasets smaller than million cells",
    actionButton('dsEmbedDoEmbed', "Compute Embedding"))
  )
}

diffsomRenderEmbedSOMView <- function(ds) {
  if(is.null(ds$map)) p("Compute the SOM first")
  else div(
    tooltip("The expression of the selected marker or data column will be shown in the plot below.",
    selectInput('dsEmbedSOMViewCol', "Column",
      choices=ds$colsToUse,
      multiple=F,
      selected=ds$colsToUse[1]
      )),
    tooltip("Each point represents a trained SOM node (and the corresponding cell cluster around it). Color = expression value of the selected marker. Size of the gray border grows with growing data variance in the cluster.",
    plotOutput("plotDsEmbedSOMView", width=paste0(2*(1+ds$map$xdim),'em'), height=paste0(2*(1+ds$map$ydim),'em')))
  )
}

diffsomRenderEmbedEView <- function(ds) {
  if(is.null(ds$e)) p("Compute the embedding first")
  else div(
    tooltip("The expression of the selected marker or data column will be shown in the embedding below.",
    pickerInput('dsEmbedEViewCol', "Column",
      choices=c('(show density)', unname(ds$prettyColnames)),
      options=list(size=10),
      multiple=F,
      selected='(show density)'
      )),
    plotOutput("plotDsEmbedEExpr", width='56em', height='56em'),
    sliderPointSize('dsEmbedEExprCex'),
    sliderAlpha('dsEmbedEExprAlpha')
  )
}

#
# Clustering
#

diffsomRenderClustering <- function(ds) {
  if(is.null(ds$map)) tooltip("For performance and precision reasons, the clustering is ran atop the space pre-partitioned by the traned SOM.",
    p("Compute the SOM first"))
  else div(
  fluidRow(
    column(3,
      tooltip("Clustering allows classification of the cells into named categories, usually corresponding to biologically relevant populations.", h3("Clustering")),
      tooltip("The method will be used to create a hierarchical dendrogram that will serve as a basis for classification.",
      selectInput("dsClusterMethod", label="Hierarchical clustering method", choices=names(CLUSTER_METHODS), multiple=F)),
      tooltip("Dendrograms are usually created instantly; except for Mahalanobis-based clustering which takes several seconds.",
      actionButton("dsClusterDoCluster", label="Create hierarchy")),
      uiOutput("uiDsClusterHeat"),
      uiOutput("uiDsClusterAnnotation")
    ),
    column(4,
      tooltip("Use the shinyDendro interface for assigning labels to dendrogram branches. Click the interface to focus it; then use keyboard keys (a-z, 1-9) to choose a cluster mark; then click a branch to assign the mark. Use Space to erase the marks.",
      h4("Cluster assignment")),
      shinyDendroOutput("dsClustDendro", width='100%', height=paste0(max(500, 2*ds$map$xdim*ds$map$ydim),'px'))
    ),
    column(4,
      tooltip("This is a rough preview of how the cell classification looks in the embedding. Use the overview scatterplots below to obtain more precise views.",
      h4("Clusters overview")),
      uiOutput("uiDsClusterEmbedding")
    )
  ),
  uiOutput('diffsomClustOverview')
  )
}

diffsomRenderClusterHeat <- function(ds) {
  if(is.null(ds$hclust)) div()
  else tooltip("Choose markers/data columns that will appear in the dendrogram. The view helps with precisely classifying the related cell populations.",
    pickerInput("dsClusterHeat",
      "Heatmap columns",
      choices=unname(ds$colsToUse),
      multiple=T,
      selected=c()
    ))
}

diffsomRenderClusterEmbedding <- function(ds) {
  if(is.null(ds$e))
    p("Compute the embedding first")
  else if(is.null(ds$clust))
    p("Create the clustering first")
  else div(
    plotOutput('plotDsClustEmbed',
      width='40em', height='40em',
      brush=brushOpts(id='dsBrushClustEmbed')),
    selectInput('dsClustEmbedColor',
      'Choose a marker',
      choices=c('(cluster)', unname(ds$prettyColnames)),
      multiple=F,
      selected='(cluster)'),
    sliderPointSize('dsClustEmbedCex'),
    sliderAlpha('dsClustEmbedAlpha')
  )
}

#
# Analysis
#

diffsomRenderAnalysis <- function(ds) {
  tabsetPanel(type='tabs',
    tabPanel('Files heatmap', uiOutput('diffsomAnalysisHeatmap')),
    tabPanel('Compare files', uiOutput('diffsomAnalysisDiff')),
    tabPanel('Significance plots', uiOutput('diffsomAnalysisSignificance'))
  )
}

diffsomRenderAHeat <- function(ds) {
  div(
    tooltip("Heatmap shows relative amount of population cells in the files. The data is converted to percentages for each file, then normalized again for populations to allow comparison.",
    h2("Relative cell count heatmap")),
    if(is.null(ds$clust) || nlevels(factor(ds$clust))<1 || nlevels(factor(ds$files)) <= 1)
      p("Heatmap requires at least two files and one cluster.")
    else
      plotOutput('plotDsAHeat', width='100%', height='65em')
  )
}

diffsomRenderADiff <- function(ds) {
  if(is.null(ds$e))
    p("Embed the population first")
  else fluidRow(
    column(3,
      tooltip("This display allows quick visual comparison of cell population presence and properties between file groups.",
      h2("Compare file groups")),
      tooltip("Only cells from selected files will be shown on the left/right plots.",
      pickerInput('dsADiffFilesLeft',
        'Left',
        choices=ds$files,
        multiple=T)),
      pickerInput('dsADiffFilesRight',
        'Right',
        choices=ds$files,
        multiple=T),
      tooltip("Points will be colored accordingly to the chosen marker/column.",
      pickerInput('dsADiffColor',
        'Color',
        choices=c('(density)',
                  if(is.null(ds$clust)) NULL else '(cluster)',
                  '(file)',
                  unname(ds$prettyColnames)),
        multiple=F,
        selected='(density)')),
      sliderPointSize('dsADiffCex'),
      sliderAlpha('dsADiffAlpha')
    ),
    column(4,
      plotOutput('plotDsADiffL', width='30em', height='30em')
    ),
    column(4,
      plotOutput('plotDsADiffR', width='30em', height='30em')
    )
  )
}

#TODO: paired possibility (any reasonable way to assign the pair permutations in Shiny?)
diffsomRenderASignificance <- function(ds) {
  if(is.null(ds$e))
    p("Embed the population first")
  else fluidRow(
    column(3,
      tooltip("Significance plots give quick informative overview of statistically relevant changes in relative cell abundance in files for each population. The clusters of selected granularity are painted blue (if the selected experiment file group has significantly less cells than the control group) or orange (if it has significantly more cells).",
      h2("Test population size difference")),
      tooltip("Samples that will be used as a baseline for testing.",
      pickerInput('dsASigControl',
        'Control sample group',
        choices=ds$files,
        multiple=T)),
      tooltip("Experiment-group samples will be tested for differences against the baseline group.",
      pickerInput('dsASigExperiment',
        'Experiment sample group',
        choices=ds$files,
        multiple=T)),
      tooltip("SOM-based granularity tests abundance of cells in neighborhood of each single SOM node, thus giving better view of small (possibly irrelevant) changes. Cluster-based granularity tests whole manually annotated populations.",
      selectInput('dsASigGran', label='Granularity',
        choices=c(
          'SOM',
          if(is.null(ds$clust)) NULL else 'Clusters'),
        selected='SOM',
        multiple=F)),
      tooltip("P-values obtained from testing are converted to color gradient position as (1-p)^transform. Default value 10 puts the reasonable p-value 0.05 to around half of the color scale. Low values exaggerate the differences, producing colors for even relatively insignificant changes.",
      sliderInput('dsASigPow', "Significance transform", value=10, min=0.1, max=30)),
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
    tooltip("This step is usually called 'gating'. Selected populations will be collected to form a new dataset.",
    h3("Create population subsets")),
    pickerInput("dsGateClusters",
      "Populations in the new dataset",
      choices=namesInvert(ds$annotation),
      multiple=T,
      options=list(size=10)),
    textInput("dsGateNewName", "New dataset name", placeholder="Enter name"),
    tooltip("Exporting the data should take several seconds. The new dataset will become available in the selection above.",
    actionButton("dsGateDoGate", "Create dataset"))
  )
}

#
# Export
#

diffsomRenderExport <- function(ds) {
  div(
    h3("Export population statistics"),
    if(is.null(ds$annotation) || is.null(ds$clust)) p("Cluster and annotate the populations first.")
    else tooltip("This exports a table where the cell count is computed for each gated population (in columns) in each file (in rows).",
      shinySaveButton("dsExportPops", "Population cell counts CSV", "Save a the population cell counts", filename='populations.csv')),

    h3("Export DiffSOM objects"),
    if(is.null(ds$map)) p("Compute SOM for exporting it first")
    else tooltip("The exported file can be imported to DiffSOM using readRDS, and used as map= argument of function Embed().",
      shinySaveButton("dsExportDSMap", "DiffSOM map RDS", "Save the DiffSOM MAP RDS", filename='map.RDS')),

    h3("Different export formats"),
    tooltip("Export FCS file with the embedding and population assignment.",
      shinySaveButton("dsExportFCS", "Export FCS", "Save Flow Cytometry Standard (FCS) file", filename='export.fcs')),
    tooltip("Same as FCS export, but the data are saved in CSV. File format is trivially readable, but may be several times larger than the corresponding FCS.",
    shinySaveButton("dsExportCSV", "Export CSV", "Save CSV export", filename='export.csv'))
  )
}

#
# Server
#

serveDiffsom <- function(ws, ds, input, output, session) {

  #
  # Overview tab
  #

  overviewServe('diffsomOverview', 'Standalone', ds, input, output)

  #
  # Transformations tab
  #

  output$diffsomTransform <- renderUI({
    diffsomRenderTransform(ds)
  })

  overviewServe('diffsomTransformOverview', 'Transform', ds, input, output,
    title="Transformed data (preview)", previewTransform=T)

  output$diffsomTransformParams <- renderUI(
    renderTransformParams(input$dsTransTrType)
  )

  observeEvent(input$dsTransApply, {
    if(!is.null(input$dsTransConfirm) && input$dsTransConfirm) {
      updateCheckboxInput(session=session, inputId='dsTransConfirm', value=F)
      ds$data <- transformedDsData(ds, input)
      updatePickerInput(session=session, inputId='dsTransCols', selected=character(0))
      showNotification(type='message', "Transformation applied.")
    } else showNotification(type='warning', "Confirmation required!")
  })

  #
  # Embedding tab
  #

  output$diffsomEmbedding <- renderUI({
    diffsomRenderEmbedding(ds)
  })

  observeEvent(input$dsEmbedDoSOM, {
    ds$colsToUse <- input$dsEmbedColsToUse
    ds$xdim <- input$dsEmbedXdim
    ds$ydim <- input$dsEmbedYdim
    ds$rlen <- input$dsEmbedRlen
    ds$seed <- input$dsEmbedSeed

    set.seed(ds$seed)
    showNotification("Computing SOM. This may take a while...")
    ds$map <- EmbedSOM::SOM(
      data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)],
      xdim=ds$xdim,
      ydim=ds$ydim,
      rlen=ds$rlen,
      negAlpha=0,
      negRadius=1)
    ds$e <- NULL
    ds$clust <- NULL
    ds$hclust <- NULL
    ds$annotation <- NULL
    showNotification(type='message', "SOM ready.")
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

  observeEvent(input$dsEmbedDoEmbed, {
    if(!is.null(ds$map)) {
      showNotification("Embedding the dataset. This should generally be around 5x faster than SOM.")
      ds$smooth <- input$dsEmbedSmooth
      ds$adjust <- input$dsEmbedAdjust
      ds$k <- input$dsEmbedK
      ds$emcoords <- input$dsEmbedCoords

      ds$e <- EmbedSOM::EmbedSOM(
        data=ds$data[,findColIds(ds$colsToUse, ds$prettyColnames)],
        map=ds$map,
        smooth=ds$smooth,
        adjust=ds$adjust,
        k=ds$k,
        emcoords=ds$emcoords)
      showNotification(type='message', "Embedding ready.")
    } else
      showNotification(type='warning', "SOM is not computed yet!")
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
      if(!is.null(input$dsBrushClustEmbed)) {
        b <- input$dsBrushClustEmbed
        st <- table(data.frame(
          som=ds$map$mapping[,1],
          tf=ds$e[,1]>=b$xmin & ds$e[,1]<=b$xmax &
             ds$e[,2]>=b$ymin & ds$e[,2]<=b$ymax
        ))
        br <- rep(0, length(ds$clust))
        br[as.numeric(rownames(st))] <- st[,'TRUE']/apply(st,1,function(v) max(sum(v), 1))
        colors <- cbind(colors, Brush=EmbedSOM::ExpressionPalette(100)[1+as.integer(99*br)])
      }
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
    if(!is.null(ds$clust))
    plotClustEmbed(
      ds$e,
      ds$clust[ds$map$mapping[,1]],
      ds$annotation,
      ds$data,
      ds$prettyColnames,
      input$dsClustEmbedColor,
      input$dsClustEmbedCex,
      input$dsClustEmbedAlpha)
  })

  overviewServe('diffsomClustOverview', 'Clust', ds, input, output)
  annotationServe('uiDsClusterAnnotation', ds, input, output)

  #
  # Analysis tab
  #

  output$diffsomAnalysis <- renderUI({
    diffsomRenderAnalysis(ds)
  })

  output$diffsomAnalysisHeatmap <- renderUI({
    diffsomRenderAHeat(ds)
  })

  output$diffsomAnalysisDiff <- renderUI({
    diffsomRenderADiff(ds)
  })

  output$diffsomAnalysisSignificance <- renderUI({
    diffsomRenderASignificance(ds)
  })

  output$plotDsAHeat <- renderPlot(
    if(!is.null(ds$clust) && length(levels(factor(ds$clust)))>0) {
      plotDsAHeat(
        ds$clust[ds$map$mapping[,1]],
        ds$annotation,
        ds$cellFile,
        ds$files
      )
    }
  )

  output$plotDsADiffL <- renderPlot({
    if(length(input$dsADiffFilesLeft)>0)
      plotDsADiff(
        input$dsADiffFilesLeft,
        input$dsADiffColor,
        ds$files, ds$data, ds$cellFile, ds$e,
        ds$prettyColnames,
        ds$clust[ds$map$mapping[,1]],
        ds$annotation,
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
        ds$clust[ds$map$mapping[,1]],
        ds$annotation,
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
    }else if(length(input$dsGateClusters)==0) {
      showNotification(type='error', "No populations selected")
    } else {
      filt <- ds$clust[ds$map$mapping[,1]] %in% input$dsGateClusters
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
  shinyFileSave(input, "dsExportFCS", roots=getForeignRoots())
  shinyFileSave(input, "dsExportCSV", roots=getForeignRoots())

  observeEvent(input$dsExportPops, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportPops)
    if(dim(outpath)[1]==1) {
      d <- table(data.frame(File=ds$files[ds$cellFile], Annotation=ds$annotation[ds$clust[ds$map$mapping[,1]]]))
      write.csv(d, outpath$datapath)
      showNotification(type='message', "Population export CSV saved.")
    }
  })

  observeEvent(input$dsExportDSMap, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportDSMap)
    if(dim(outpath)[1]==1) {
      map <- ds$map

      map$importance=rep(1,length(ds$colsToUse))
      map$colsUsed <- findColIds(ds$colsToUse, ds$prettyColnames)
      map$nclust <- nlevels(factor(ds$clust))
      map$clust <- as.numeric(factor(ds$clust))
      map$clust[is.na(map$clust)] <- 0

      saveRDS(map, outpath$datapath)
      showNotification(type='message', "DiffSOM map saved.")
    }
  })

  observeEvent(input$dsExportFCS, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportFCS)
    if(dim(outpath)[1]==1) {
      df <- data.frame(ds$data, CellFile=ds$cellFile)
      descs <- c(ds$prettyColnames, "File ID")

      if(!is.null(ds$embed)) {
        df <- cbind(df,
          EmbedSOM1=ds$embed[,1],
          EmbedSOM2=ds$embed[,2]
        )
        descs <- c(descs, 'EmbedSOM 1', 'EmbedSOM 2')
      }

      if(!is.null(ds$map)) {
        df <- cbind(df,
          SOM1=1+((ds$map$mapping[,1]-1) %% ds$xdim),
          SOM2=1+as.integer((ds$map$mapping[,1]-1) / ds$xdim)
        )
        descs <- c(descs, 'SOM vertex 1', 'SOM vertex 2')
      }

      if(!is.null(ds$clust)) {
        cl <- as.numeric(factor(ds$clust[ds$map$mapping[,1]]))
        cl[is.na(cl)] <- 0
        df <- cbind(df, Cluster=cl)
        descs <- c(descs, 'Population ID')
      }

      ff <- new('flowFrame', exprs=as.matrix(df))
      ff@parameters@data[,'desc'] <- descs #magic!!
      flowCore::write.FCS(ff, outpath$datapath)
      showNotification(type='message', "FCS file exported")
    }
  })

  observeEvent(input$dsExportCSV, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportCSV)
    if(dim(outpath)[1]==1) {
      df <- data.frame(ds$data, CellFile=ds$cellFile)
      colnames(df)[seq_len(length(ds$prettyColnames))] <- ds$prettyColnames

      if(!is.null(ds$embed))
        df <- cbind(df,
          EmbedSOM1=ds$embed[,1],
          EmbedSOM2=ds$embed[,2]
        )

      if(!is.null(ds$map))
        df <- cbind(df,
          SOM1=1+((ds$map$mapping[,1]-1) %% ds$xdim),
          SOM2=1+as.integer((ds$map$mapping[,1]-1) / ds$xdim)
        )

      if(!is.null(ds$clust)) {
        cl <- as.numeric(factor(ds$clust[ds$map$mapping[,1]]))
        df <- cbind(df, ClusterKey=cl)
        if(!is.null(ds$annotation))
          df <- cbind(df, Population=ds$annotation[cl])
      }

      write.csv(df, outpath$datapath)
      showNotification(type='message', "CSV file exported")
    }
  })
}
