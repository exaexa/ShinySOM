
#
# Dataset Create, renderers
#

renderDsCreate <- function(dsCreate) {
  div(
    tooltip("A ShinySOM dataset is similar to FlowSOM `fsom` data object: it contains the data, and some analysis-related information (e.g. the SOM and clustering data)",
    h1('Create datasets')),

    h3('1. Select files'),

    tooltip("The selected FCS files will be aggregated and used for the analysis",
    p(
      shinyFilesButton('dsCreateFiles',
      'Select dataset files',
      'Choose FCS files',
      multiple=T),
      uiOutput('dsCreateFileNumber')
    )),

    h3('2. Scale, Transform, Sample'),
    uiOutput('dsCreateNormalize'),

    h3('3. Create dataset'),
    uiOutput('dsCreateFinalize')
  )
}

renderDsCreateFileNum <- function(fs) {
  if(is.list(fs)) {
    nf <- length(fs$files)
    p(length(fs$files), if(nf==1) 'file' else 'files', 'selected.')
  }
  else 'No files selected.'
}

renderDsCreateNormalize <- function(fs) {
  if(is.list(fs)) {
    res <- tagList()

    res <- tagAppendChild(res,
      fluidRow(column(4,
          tooltip("You may choose data columns that are to be imported to the dataset. This is useful for dumping irrelevant information (e.g. redundant scatter information from multiple lasers, Time, and remains from barcoding).",
          uiOutput('dsCreateLoadColsUi'))
        ),
        column(4,
          tooltip("The parameters correspond to FlowSOM scaling, transformation and compensation parameters.",
          checkboxGroupInput('dsCreateParams', 'Loading parameters', 
            c(
              'Scale'='scale',
              'Transform'='transform',
              'Apply compensation'='compensate',
              'Sample cells'='subsample'
            )
          )),
          tooltip("You may want to downsample the dataset to under 1 million cells, in order to improve the interface responsiveness and reduce memory usage. The same analysis can later be applied to full datasets.",
          numericInput('dsCreateSubsample', 'Number of cells to sample', min=1, step=1, value=200000))
        ),
        column(4,
          tooltip("Loading and aggregating the data may take several tens of seconds, depending on the dataset size.",
          uiOutput('dsCreateToTransformUi'))
        )
      )
    )
    res
  }
  else 'No files selected.'
}

renderDsCreateFinalize <- function(fs, cols, dsCreate) {
  if(dsCreate$processing) return('Processing, please wait...')
  if(!is.list(fs)) return('No files selected.')
  if(! (length(cols)>0)) return('No data columns selected')

  path1 <- fs$files[[names(fs$files)[1]]]
  file1 <- path1[[length(path1)]]
  div(
    tooltip("Choose a simple desriptive name of the dataset, e.g. 'Spleen - all cells'",
    textInput('dsCreateName', 'Dataset name', value=file1)),
    tooltip("Loading and aggregating the data may take several tens of seconds, depending on the dataset size.",
    actionButton('dsCreateDoIt', 'Create dataset'))
  )
}

#
# Convert a list of files from shinyFiles to filenames
#

dsCreateFiles <- function(fs) {
  froot <- getRoot(fs$root)
  as.vector(unlist(lapply(fs$files, function(l) {
    l[['sep']] <- '/'
    paste(sep='/', froot, do.call('paste', l))
  })))
}

#
# Actual loading function that reads the files and puts the dataset into the workspace
#

dsCreateDoLoad <- function(name, fs, params, subsample, colsToLoad, colsToTransform, prettyCols, workspace) {
  if(!datasetNameValid(name)) stop('Invalid dataset name')
  if(datasetExists(workspace, name)) stop('Dataset of same name already exists')

  fns <- dsCreateFiles(fs)

  fs <- FlowSOM::ReadInput(
    FlowSOM::AggregateFlowFrames(
      fns,
      cTotal=if('subsample' %in% params) subsample else NULL,),
    scale='scale' %in% params,
    transform='transform' %in% params,
    compensate='compensate' %in% params,
    toTransform=findColIds(colsToTransform, prettyCols))

  nds <- list()

  nds$files <- fns #TODO: normalize the names a bit

  nds$cellFile <- as.numeric(factor(fs$data[,'File']))

  colSel <- findColIds(colsToLoad, prettyCols)

  nds$data <- fs$data[,colSel]

  nds$prettyColnames <- fs$prettyColnames[colSel]

  saveDataset(workspace, name, nds)
}

#
# Read column names for overview
#

dsCreateLoadPrettyCols <- function(fs) {
  fn <- dsCreateFiles(fs)[1]
  FlowSOM::ReadInput(fn)$prettyColnames
}

#
# Serving function
#

serveDsCreate <- function(workspace, input, output) {

  # Processing flag
  dsCreate <- reactiveValues(
    processing=F
  )

  # Preview of column names
  prettyCols <- reactive({dsCreateLoadPrettyCols(input$dsCreateFiles)})

  # Main UI
  output$dsCreate <- renderUI({renderDsCreate(dsCreate)})

  shinyFileChoose(input, 'dsCreateFiles', roots=getForeignRoots())

  output$dsCreateFileNumber <- renderUI({
    renderDsCreateFileNum(input$dsCreateFiles)
  })

  output$dsCreateNormalize <- renderUI({
    renderDsCreateNormalize(input$dsCreateFiles)
  })

  output$dsCreateFinalize <- renderUI({
    renderDsCreateFinalize(input$dsCreateFiles, input$dsCreateLoadCols, dsCreate)
  })

  output$dsCreateLoadColsUi <- renderUI({
    ch <- prettyCols()
    choices=as.list(ch)
    names(choices)<-ch
    pickerInput(
      inputId = 'dsCreateLoadCols', 
      label = 'Columns to load', 
      choices = choices,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = 'count > 2'
      ), 
      multiple = TRUE
    )
  })

  output$dsCreateToTransformUi <- renderUI({
    ch <- input$dsCreateLoadCols
    choices <- as.list(ch)
    names(choices) <- ch
    pickerInput(
      inputId = 'dsCreateToTransform', 
      label = 'Columns to transform', 
      choices = ch,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = 'count > 2'
      ), 
      multiple = TRUE
    )
  })

  #
  # Loading button + debounce
  #
  debounced.processing <- debounce(dsCreate$processing, 100)

  observeEvent(input$dsCreateDoIt, {
    dsCreate$processing <- T
  })

  observeEvent(debounced.processing(), {
    if(dsCreate$processing) {
      tryCatch(
        {
          dsCreateDoLoad(
            input$dsCreateName,
            input$dsCreateFiles,
            input$dsCreateParams,
            input$dsCreateSubsample,
            input$dsCreateLoadCols,
            input$dsCreateToTransform,
            prettyCols(),
            workspace)
          showNotification(type='message',
            'Dataset created.'
          )
        },
        error = function(e) {
          showNotification(type='error',
            paste('Dataset creation failed:', e)
          )
        }
      )
      dsCreate$processing <- F
    }
  })
}
