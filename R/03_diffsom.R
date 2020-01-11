
#
# DiffSOM-style core values, dataset loading/saving
#

dsReactiveValNames <- c(
  'files',
  'cellFile',
  'data',
  'prettyColnames',
  'transforms',
  'seed',
  'colsToUse',
  'importance',
  'dataMetric',
  'somMetric',
  'xdim',
  'rlen',
  'map',
  'smooth',
  'adjust',
  'k',
  'emcoords',
  'e',
  'hclust',
  'clust',
  'annotation'
)


reactiveValsDiffsom <- function() {
  rvs <- list()
  for(n in dsReactiveValNames) rvs[n] <- list(NULL) # R!

  do.call(reactiveValues, rvs)
}

#
# dataset initialization
#

dsInitFromDataset <- function(ds, dataset) {
  print("getting dataset from workspace")

  # and load now
  for(n in dsReactiveValNames) ds[[n]] <- dataset[[n]]
}

dsClearDataset <- function(ds) {
  print("clearing the workspace")
  # reset everything first so that reactive stuff gets properly reloaded
  for(n in dsReactiveValNames) ds[[n]] <- NULL

}

dsGetDataset <- function(ds) {
  print("collecting the dataset")
  dataset <- list()
  for(n in dsReactiveValNames) dataset[n] <- list(ds[[n]])
  dataset
}

#
# Tab renderers
#

renderDiffsom <- function(ws) {
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
  metrics <- c(
    `Manhattan (sum)`='manhattan',
    `Euclidean (spherical)`='euclidean',
    `Maximum (Chebyshev)`='maximum'
  )

  fluidRow(
    column(3,
      tooltip("You may choose parameters for constructing the self-organizing map (SOM) here. The SOM will be later used for describing and clustering the cell space.", h3("SOM")),
      bs_accordion(id='dsSomSettingsAccordion') %>%
      bs_append(title="Basic settings", content=div(
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
        tooltip("Choose the size of the SOM square side. The SOM will be able to approximate roughly side^2 different clusters. Larger SOMs capture more details, but take longer to compute.",
        sliderInput('dsEmbedXdim', "SOM size (nodes on square side)", value=isolate(if(is.null(ds$xdim)) 16 else ds$xdim), min=2, max=50, step=1)),
      )) %>%
      bs_append(title="Column importance", content=uiOutput('uiDsEmbedImportance')) %>%
      bs_append(title="Training parameters", content=div(
        tooltip("How many times the data will be presented to SOM, larger values may produce a better fitting SOM for the cost of computation time. Increase the value slightly if working with less than 10.000 cells. For datasets over 100.000 cells, the value can be safely decreased. Generally, epochs*cells should be at least 100.000.",
        sliderInput('dsEmbedRlen', "SOM epochs", value=isolate(if(is.null(ds$rlen)) 10 else ds$rlen), min=1, max=100, step=1)),
        tooltip("Random seed for SOM training. Choose a different value to train a different SOM from the same data.",
        numericInput('dsEmbedSeed', "Random seed", value=isolate(if(is.null(ds$seed)) 1 else ds$seed), min=0, max=999999999, step=1)),
        tooltip("This metric will be used to measure the similarity of single cell parameter expressions, affecting the effective distances of the clusters.",
        selectInput('dsEmbedDataMetric', "Cell space metric", choices=metrics, selected=isolate(if(is.null(ds$dataMetric)) 'euclidean' else ds$dataMetric))),
        tooltip("This metric will be used in the output embedded (i.e. SOM) space, affecting the distribution and shape of the identified clusters in the map.",
        selectInput('dsEmbedSOMMetric', "Embedding space metric", choices=metrics, selected=isolate(if(is.null(ds$somMetric)) 'maximum' else ds$somMetric)))
      )),
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

diffsomRenderImportances <- function(colsToUse, orig) {
  res <- tagList()
  for(i in seq_len(length(colsToUse)))
    res <- tagAppendChild(res,
      sliderInput(
        paste0('dsImportance',i),
        colsToUse[i],
        value=if(is.null(orig) || is.na(orig[colsToUse[i]])) 1 else orig[colsToUse[i]],
        min=0.1,
        max=10,
        step=.1
      )
    )
  res
}

diffsomGatherImportances <- function(input, colsToUse) {
  n <- length(colsToUse)
  res <- rep(1, n)
  for(i in seq_len(n)) {
    tmp <- input[[paste0('dsImportance', i)]]
    if(is.null(tmp)) tmp<-1
    res[i] <- tmp
  }
  names(res) <- colsToUse
  res
}

diffsomPrepareData <- function(data, colnames, colsToUse, importance) {
  d <- apply(data[,findColIds(colsToUse, colnames), drop=F], 2, transformDoScale)
  im <- importance[colsToUse]
  im[is.na(im)] <- 1
  t(t(d)*im)
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
    sliderInput('dsEmbedK', "NNs (k)", value=if(is.null(ds$k)) 20 else ds$k, min=3, max=ds$xdim^2, step=1)),
    tooltip("Meta-embedding. The selected algorithm will embed only the SOM nodes, atop of which actual cells will be quickly fitted by EmbedSOM. Some choices require lowering Adjust.",
    selectInput('dsEmbedCoords', "Embedding coords manipulation", choices=emcoords, selected=ds$emcoords)),
    tooltip("Embedding computation generally takes several seconds for datasets smaller than million cells",
    actionButton('dsEmbedDoEmbed', "Compute Embedding"))
  )
}

diffsomRenderEmbedSOMView <- function(ds) {
  if(is.null(ds$map)) p("The SOM was not computed yet.")
  else div(
    tooltip("The expression of the selected marker or data column will be shown in the plot below.",
    selectInput('dsEmbedSOMViewCol', "Column",
      choices=ds$colsToUse,
      multiple=F,
      selected=ds$colsToUse[1]
      )),
    tooltip("Each point represents a trained SOM node (and the corresponding cell cluster around it). Color = expression value of the selected marker. Size of the gray border grows with growing data variance in the cluster.",
    plotOutput("plotDsEmbedSOMView", width=paste0(2*(1+ds$map$xdim),'em'), height=paste0(2*(1+ds$map$xdim),'em')))
  )
}

diffsomRenderEmbedEView <- function(ds) {
  if(is.null(ds$e)) p("The embedding was not computed yet.")
  else div(
    fluidRow(
      column(6,
        tooltip("The expression of the selected marker or data column will be shown in the embedding below.",
        pickerInput('dsEmbedEViewCol', "Column",
          choices=c('(show density)', unname(ds$prettyColnames)),
          options=list(size=10),
          multiple=F,
          selected='(show density)'
          )
        )
      ),
      column(6,
        sliderPointSize('dsEmbedEExprCex'),
        sliderAlpha('dsEmbedEExprAlpha')
      )
    ),
    plotOutput("plotDsEmbedEExpr", width='56em', height='56em')
  )
}

#
# Clustering
#

diffsomRenderClustering <- function(ds) {
  if(is.null(ds$map))
    tooltip("For performance and precision reasons, the clustering is ran atop the space pre-partitioned by the traned SOM.",
    p("Clustering is not available because SOM was not computed yet."))
  else div(
  fluidRow(
    column(3,
      tooltip("Clustering allows classification of the cells into named categories, usually corresponding to biologically relevant populations.",
      h3("Clustering")),
      tooltip("The method will be used to create a hierarchical dendrogram that will serve as a basis for classification.",
      selectInput("dsClusterMethod", label="Hierarchical clustering method", choices=names(CLUSTER_METHODS), multiple=F)),
      tooltip("Dendrograms are usually created instantly; except for Mahalanobis-based clustering which takes several seconds.",
      actionButton("dsClusterDoCluster", label="Create hierarchy")),
      uiOutput("uiDsClusterHeat"),
      tooltip("Assign human-readable names for each cluster you create. These will be used in later analysis and dissection.", h3('Cluster names')),
      uiOutput("uiDsClusterAnnotation")
    ),
    column(4,
      tooltip("Use the shinyDendro interface for assigning labels to dendrogram branches. Click the interface to focus it; then use keyboard keys (a-z, 1-9) to choose a cluster mark; then click a branch to assign the mark. Use Space to erase the marks.",
      h4("Cluster assignment")),
      uiOutput('uiDsClustDendroWrap')
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

diffsomRenderClustDendroWrap <- function(ds) {
  if(is.null(ds$hclust)) div("The clustering tool requires the cluster hierarchy, which was not computed yet.")
  else shinyDendroOutput('dsClustShinyDendro', width='100%', height='40em')
}

diffsomRenderClusterHeat <- function(ds) {
  tooltip("Choose markers/data columns that will appear in the dendrogram. The view helps with precisely classifying the related cell populations.",
  pickerInput("dsClusterHeat",
    "Heatmap columns",
    choices=unname(ds$colsToUse),
    multiple=T,
    selected=c()
  ))
}

diffsomRenderClusterEmbedding <- function(ds) {
  scatterChoices <- unname(ds$prettyColnames)
  if(!is.null(ds$e)) scatterChoices <- c('(Embedding X)','(Embedding Y)', scatterChoices)
  defaultX <- scatterChoices[1]
  defaultY <- scatterChoices[if(length(scatterChoices>1)) 2 else 1]

  if(is.null(ds$hclust))
    div()
  else div(
    plotOutput('plotDsClustEmbed',
      width='40em', height='40em',
      brush=brushOpts(id='dsBrushClustEmbed')),
    fluidRow(
      column(6,
        selectInput('dsClustEmbedColor',
            'Plot coloring',
            choices=c('(cluster)', unname(ds$prettyColnames)),
            multiple=F,
            selected='(cluster)'),
        uiOutput("uiDsClustReorder"),
        selectInput('dsClustEmbedX',
          'Horizontal axis',
          choices=scatterChoices,
          multiple=F,
          selected=defaultX),
        selectInput('dsClustEmbedY',
          'Vertical axis',
          choices=scatterChoices,
          multiple=F,
          selected=defaultY)
      ),
      column(6,
        sliderPointSize('dsClustEmbedCex'),
        sliderAlpha('dsClustEmbedAlpha')
      )
    )
  )
}

#
# Analysis
#

diffsomRenderAnalysis <- function(ds) {
  tabsetPanel(type='tabs',
    tabPanel('Marker expressions', uiOutput('diffsomAnalysisExprs')),
    tabPanel('Population sizes', uiOutput('diffsomAnalysisPopSizes')),
    tabPanel('Side-by-side comparison', uiOutput('diffsomAnalysisDiff')),
    tabPanel('Difference testing', uiOutput('diffsomAnalysisSignificance'))
  )
}

diffsomRenderAExprs <- function(ds) {
  if(is.null(ds$clust)||is.null(ds$annotation)) return(div("Displaying statistic about cell populations requires that you first create and annotate the population in the Clustering interface."))
  clust <- levels(factor(ds$clust))
  names(clust) <- ds$annotation[clust]
  clust <- clust[!is.na(clust)]
  fluidRow(
    column(3,
      tooltip("This displays expression profiles of selected populations in different files. That may be used e.g. to examine various cell activation states -- cluster the cells by lineage markers first, and view the activation markers using this interface.",
      h2("Expressions in clusters")),
      tooltip("Selected files will be visually separated from the rest",
      pickerInput('dsAExprsFiles',
        "Highlight files",
        choices=ds$files,
        multiple=T
      )),
      tooltip("Marker expressions to examine",
      pickerInput('dsAExprsMarkers',
        "Markers",
        choices=ds$prettyColnames,
        multiple=T
      )),
      tooltip("Annotated populations whose content will be used to create the plot",
      pickerInput('dsAExprsClusters',
        "Populations",
        choices=clust,
        multiple=T
      ))
    ),
    column(9,
      uiOutput('uiDsAExprs')
    )
  )
}

diffsomRenderAExprsPlot <- function(ds, input) {
  if(is.null(input$dsAExprsClusters) ||
    is.null(input$dsAExprsMarkers) ||
    length(input$dsAExprsClusters)==0 ||
    length(input$dsAExprsMarkers)==0)
    div("No data to plot. Select at least one marker and one annotated cell population in the interface on the left.")
  else plotOutput('plotDsAExprs',
    width=paste0(20*length(input$dsAExprsMarkers), 'em'),
    height=paste0(max(15, 2*length(ds$files))*length(input$dsAExprsClusters), 'em')
  )
}

diffsomRenderAPopSizes <- function(ds) {
  fluidRow(
    column(3,
      tooltip("This view shows relative amount of population cells in the files. The data is converted to compositions to allow comparison.",
      h2("Relative cell abundances")),
      pickerInput('dsAPSClusters',
        'Populations to compare',
        choices=namesInvert(ds$annotation),
        multiple=T,
        options=list(`actions-box`=T, size=10)),
      checkboxInput('dsAPSReorder', "Reorder by similarity", value = FALSE),
      plotOutput('plotDsAPopLegend', width='100%', height=paste0(4+length(ds$annotation)*2, 'em'))
    ),
    column(9,
      plotOutput('plotDsAPopSizes', width='100%', height='40em')
    )
  )
}

diffsomRenderADiff <- function(ds) {
  if(is.null(ds$e))
    div("No data to plot. Embedding must be computed before displaying the differences.")
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

# TODO: there is the possibility to work with paired data in testing, but is
# there any reasonable way to assign the pair permutations in Shiny?
diffsomRenderASignificance <- function(ds) {
  if(is.null(ds$e))
    div("No data to plot. Embedding must be computed before displaying the differences.")
  else fluidRow(
    column(3,
      tooltip("This plot gives quick informative overview of statistically relevant changes in relative cell abundance in files for each population. The clusters of selected granularity are painted blue (if the selected experiment file group has significantly less cells than the control group) or orange (if it has significantly more cells).",
      h2("Test population abundance differences")),
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
          if(is.null(ds$annotation) || length(ds$annotation)==0) NULL else 'Clusters'),
        selected='SOM',
        multiple=F)),
      tooltip("P-values obtained from testing are converted to color gradient; the selected value is put to around a half of the color scale. High selected p-values exaggerate any difference, producing colors for even relatively insignificant changes.",
      sliderTextInput('dsASigPval', "P-value at 50% color", selected=0.05,
                      choices=c(0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5),
                      grid=T)),
      sliderPointSize('dsASigCex'),
      sliderAlpha('dsASigAlpha')
    ),
    column(6,
      plotOutput('plotDsASig', width='50em', height='50em')
    ),
    column(3,
      plotOutput('plotDsASigLegend', width='100%')
    )
  )
}

#
# Gating a.k.a. dissection
#

diffsomRenderGating <- function(ds) {
  div(
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
    if(is.null(ds$annotation) || is.null(ds$clust)) p("Statistic export about populations requires that you create and annotate the population in the Clustering interface.")
    else tooltip("This exports a table where the cell count is computed for each gated population (in columns) in each file (in rows).",
      shinySaveButton("dsExportPops", "Population cell counts CSV", "Save a the population cell counts", filename='populations.csv')),

    h3("Export datasets"),
    p(
    tooltip("This exports the whole dataset, together with all cell data and analysis metadata.",
      shinySaveButton("dsExportDSFull", "Export full dataset RDS", "Save the dataset RDS", filename='ds.shinysom')),
    tooltip("This exports all dataset metadata without the individual cell data. The file is significantly smaller than the full dataset, and can still be used to process other files using the Batch API.",
      shinySaveButton("dsExportDSNoData", "Export analysis RDS", "Save the analysis RDS", filename='ds.shinysom'))
    ),

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
    renderTransformParams(input, input$dsTransTrType)
  )

  observeEvent(input$dsTransApply, {
    if(!is.null(input$dsTransConfirm) && input$dsTransConfirm) {
      updateCheckboxInput(session=session, inputId='dsTransConfirm', value=F)
      transform <- transformedDsData(ds, input)
      ds$data <- transform$data
      if(is.null(ds$transforms)) ds$transforms <- list()
      ds$transforms[[length(ds$transforms)+1]] <- transform$desc
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
    ds$rlen <- input$dsEmbedRlen
    ds$seed <- input$dsEmbedSeed
    ds$importance <- diffsomGatherImportances(input, ds$colsToUse)
    ds$somMetric <- input$dsEmbedSOMMetric
    ds$dataMetric <- input$dsEmbedDataMetric

    showNotification("Computing SOM. This may take a while...")

    distf <- 2
    if(ds$dataMetric=='manhattan') distf<-1
    if(ds$dataMetric=='maximum') distf<-3
    nhbr.method <- 'maximum'
    if(ds$somMetric %in% c('manhattan', 'euclidean'))
      nhbr.method <- ds$somMetric

    set.seed(ds$seed)
    ds$map <- EmbedSOM::SOM(
      data=diffsomPrepareData(ds$data, ds$prettyColnames, ds$colsToUse, ds$importance),
      xdim=ds$xdim,
      ydim=ds$xdim,
      rlen=ds$rlen,
      negAlpha=0, #is any parametrization of this viable?
      negRadius=1,
      distf=distf,
      nhbr.method=nhbr.method)
    ds$e <- NULL
    ds$clust <- rep(NA, dim(ds$map$codes)[1])
    ds$hclust <- NULL
    ds$annotation <- NULL
    showNotification(type='message', "SOM ready.")
  })

  output$uiDsEmbedImportance <- renderUI(
    diffsomRenderImportances(input$dsEmbedColsToUse, ds$importance)
  )

  output$uiDsEmbedParams <- renderUI(
    diffsomRenderEmbeddingParams(ds)
  )

  output$uiDsEmbedSOMView <- renderUI(
    diffsomRenderEmbedSOMView(ds)
  )

  output$plotDsEmbedSOMView <- renderPlot({
    plotSOMOverview(
      ds$map$xdim,
      ds$map$ydim,
      ds$map$codes[,findColIds(input$dsEmbedSOMViewCol, ds$colsToUse), drop=F],
      ds$data[,findColIds(input$dsEmbedSOMViewCol, ds$prettyColnames), drop=F],
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
        data=diffsomPrepareData(ds$data, ds$prettyColnames, ds$colsToUse, ds$importance),
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
    if(is.null(input$dsEmbedEViewCol)) NULL
    else plotEmbedExpr(
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
        data=diffsomPrepareData(ds$data, ds$prettyColnames, ds$colsToUse, ds$importance),
        mapping=ds$map$mapping[,1])
    }
  })

  output$uiDsClusterHeat <- renderUI(diffsomRenderClusterHeat(ds))

  output$uiDsClustDendroWrap <- renderUI(diffsomRenderClustDendroWrap(ds))

  output$dsClustShinyDendro <- renderShinyDendro(
    if(!is.null(ds$hclust)) {
    colors <- getHeatmapColors(ds, input$dsClusterHeat)
    if(!is.null(input$dsBrushClustEmbed)) {
      b <- input$dsBrushClustEmbed
      dx <- getClustEmbedData(ds$e, ds$data, ds$prettyColnames, input$dsClustEmbedX)
      dy <- getClustEmbedData(ds$e, ds$data, ds$prettyColnames, input$dsClustEmbedY)
      st <- table(data.frame(
        som=ds$map$mapping[,1],
        tf=factor(levels=c(T,F),
            dx>=b$xmin & dx<=b$xmax &
            dy>=b$ymin & dy<=b$ymax
          )
      ))
      br <- rep(0, length(isolate(ds$clust)))
      br[as.numeric(rownames(st))] <- st[,'TRUE']/apply(st,1,function(v) max(sum(v), 1))
      colors <- cbind(colors, Brush=EmbedSOM::ExpressionPalette(100)[1+as.integer(99*br)])
    }
    shinyDendro('dsClustDendroOutput',
      ds$hclust$height,
      ds$hclust$merge,
      ds$hclust$order,
      heatmap=colors,
      assignment=unsetClustNAs(isolate(ds$clust)),
      fontFg='black',
      fontShadow='white',
      fontScale=0.6,
      key=ws$page
    )
  })

  observeEvent(input$dsClustDendroOutput,
    if(getShinyDendroKey(input$dsClustDendroOutput) == ws$page)
      ds$clust <- setClustNAs(getShinyDendroAssignment(input$dsClustDendroOutput))
  )

  output$uiDsClusterEmbedding <- renderUI(
    diffsomRenderClusterEmbedding(ds)
  )

  output$plotDsClustEmbed <- renderPlot({
    if(!is.null(ds$clust))
    plotClustEmbed(
      ds$e,
      ds$clust[ds$map$mapping[,1]],
      ds$annotation,
      ds$data,
      ds$prettyColnames,
      input$dsClustEmbedColor,
      input$dsClustEmbedX,
      input$dsClustEmbedY,
      input$dsClustEmbedCex,
      input$dsClustEmbedAlpha)
  })

  output$uiDsClustReorder <- renderUI( {
    if(input$dsClustEmbedColor %in% ds$colsToUse)
      actionButton('dsClustDoReorder', "Reorder dendrogram")
    else
      span()
  })

  observeEvent(input$dsClustDoReorder, {
    if(!is.null(ds$hclust) && (input$dsClustEmbedColor %in% ds$colsToUse)) {
      cl <- ds$hclust
      class(cl) <- 'hclust'
      ds$hclust <- as.hclust(stats::reorder(
        as.dendrogram(cl),
        -ds$map$codes[,findColIds(input$dsClustEmbedColor, ds$colsToUse)],
        mean
      ))
    }
  })

  overviewServe('diffsomClustOverview', 'Clust', ds, input, output)
  annotationServe('uiDsClusterAnnotation', ds, input, output)

  #
  # Analysis tab
  #

  output$diffsomAnalysis <- renderUI(
    diffsomRenderAnalysis(ds)
  )

  output$diffsomAnalysisExprs <- renderUI(
    diffsomRenderAExprs(ds)
  )

  output$diffsomAnalysisPopSizes <- renderUI(
    diffsomRenderAPopSizes(ds)
  )

  output$diffsomAnalysisDiff <- renderUI(
    diffsomRenderADiff(ds)
  )

  output$diffsomAnalysisSignificance <- renderUI(
    diffsomRenderASignificance(ds)
  )

  output$uiDsAExprs <- renderUI(
    diffsomRenderAExprsPlot(ds, input)
  )

  output$plotDsAExprs <- renderPlot(
    plotDsAExprs(
      ds$files,
      input$dsAExprsFiles,
      ds$cellFile,
      input$dsAExprsClusters,
      ds$clust[ds$map$mapping[,1]],
      ds$annotation,
      input$dsAExprsMarkers,
      ds$data[,findColIds(input$dsAExprsMarkers, ds$prettyColnames),drop=F]
    )
  )

  output$plotDsAPopSizes <- renderPlot(
    if(!is.null(ds$clust) && length(levels(factor(ds$clust)))>0 && length(input$dsAPSClusters)>0)
      plotDsAPopSizes(
        ds$clust[ds$map$mapping[,1]],
        ds$annotation,
        ds$cellFile,
        ds$files,
        input$dsAPSClusters,
        input$dsAPSReorder
      )
  )

  output$plotDsAPopLegend <- renderPlot(
    if(!is.null(ds$clust))
      plotDsAPopLegend(ds$clust, ds$annotation, input$dsAPSClusters)
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
        input$dsASigPval,
        input$dsASigCex,
        input$dsASigAlpha)
  })

  output$plotDsASigLegend <- renderPlot({
    plotDsASigLegend(input$dsASigPval)
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
    } else if(is.null(input$dsGateClusters) || length(input$dsGateClusters)==0) {
      showNotification(type='error', "No populations selected")
    } else {
      filt <- ds$clust[ds$map$mapping[,1]] %in% input$dsGateClusters
      nds <- list(
        files=ds$files,
        data=ds$data[filt,,drop=F],
        cellFile=ds$cellFile[filt],
        prettyColnames=ds$prettyColnames
      )

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
  shinyFileSave(input, "dsExportDSFull", roots=getForeignRoots())
  shinyFileSave(input, "dsExportDSNoData", roots=getForeignRoots())
  shinyFileSave(input, "dsExportFCS", roots=getForeignRoots())
  shinyFileSave(input, "dsExportCSV", roots=getForeignRoots())

  observeEvent(input$dsExportPops, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportPops)
    if(dim(outpath)[1]==1) {
      d <- dsExportPopSizes(ds)
      write.csv(d, outpath$datapath)
      showNotification(type='message', "Population export CSV saved.")
    }
  })

  observeEvent(input$dsExportDSFull, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportDSFull)
    if(dim(outpath)[1]==1) {
      dso <- dsGetDataset(ds)

      saveRDS(dso, outpath$datapath)
      showNotification(type='message', "Dataset exported.")
    }
  })

  observeEvent(input$dsExportDSNoData, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportDSNoData)
    if(dim(outpath)[1]==1) {
      dso <- dsGetDataset(ds)

      dso$files <- NULL
      dso$prettyColnames <- NULL
      dso$data <- NULL
      dso$cellFile <- NULL

      dso$map$mapping <- NULL
      dso$e <- NULL

      saveRDS(dso, outpath$datapath)
      showNotification(type='message', "Dataset exported.")
    }
  })

  observeEvent(input$dsExportFCS, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportFCS)
    if(dim(outpath)[1]==1) {
      flowCore::write.FCS(dsExportFlowFrame(ds), outpath$datapath)
      showNotification(type='message', "FCS file exported")
    }
  })

  observeEvent(input$dsExportCSV, {
    outpath <- parseSavePath(getForeignRoots(), input$dsExportCSV)
    if(dim(outpath)[1]==1) {
      df <- dsExportDF(ds)
      write.csv(df, outpath$datapath)
      showNotification(type='message', "CSV file exported")
    }
  })
}
