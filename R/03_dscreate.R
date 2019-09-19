
#
# Dataset Create, renderers
#

renderDsCreate <- function(dsCreate) {
  fluidRow(
    column(6,
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

      h3('2. Select data and sample cells'),
      uiOutput('dsCreateNormalize'),

      h3('3. Create dataset'),
      uiOutput('dsCreateFinalize')
    ),
    column(6,
      h1("Manage datasets"),
      tooltip("Cloning a dataset creates a complete copy of the original data, which may serve as a backup, or for trying different analysis approaches without losing the original data.",
      h3("Clone a dataset")),
      uiOutput('dsCreateClone'),
      tooltip("Deleting a dataset erases all its data, but not the original FCS files nor the exported results.",
      h3("Delete a dataset")),
      uiOutput('dsCreateDelete')
    )
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
  if(is.list(fs))
    div(
      tooltip("You may choose data columns that are to be imported to the dataset. This is useful for dumping irrelevant information (e.g. redundant scatter information from multiple lasers, Time, and remains from barcoding).",
      uiOutput('dsCreateLoadColsUi')),
      tooltip("Subsampling the cells makes the dataset smaller and all computations (and plotting) faster.",
      checkboxInput('dsCreateParams', 'reduce the dataset by subsampling', value=T)), 
      tooltip("You may want to downsample the dataset to under 1 million cells, in order to improve the interface responsiveness and reduce memory usage. This causes only negligible impact on analysis results, and the same analysis can later be applied to full datasets using batch processing.",
      numericInput('dsCreateSubsample', 'Number of cells to sample', min=1, step=1, value=250000))
    )
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

renderDsCreateClone <- function(ws) {
  div(
    pickerInput('dsCreateCloneOrig', "Original dataset",
      choices=ws$datasets,
      multiple=F,
      selected=ws$datasets[1]),
    textInput('dsCreateCloneName', 'Clone name', value=''),
    actionButton('dsCreateDoClone', 'Clone the dataset')
  )
}

renderDsCreateDelete <- function(ws) {
  div(
    pickerInput('dsCreateDeleteOrig', "Original dataset",
      choices=ws$datasets,
      multiple=F,
      selected=ws$datasets[1]),
    checkboxInput('dsCreateDeleteConfirm', 'Confirm deleting the dataset', value=F),
    actionButton('dsCreateDoDelete', 'Delete the dataset')
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
# Convert labels from loader functions to pretty column names, resolve duplicates
#

dsCreatePrettyColnames <- function(loaded) {
  res <- loaded$cols
  res[!is.na(loaded$labels)] <- paste0(loaded$labels, ' (', loaded$cols, ')')[!is.na(loaded$labels)]
  nums <- table(res)
  flt <- nums[res]>1
  flt[is.na(flt)] <- FALSE
  res[flt] <- paste0(res, '/', seq_len(length(res)))[flt]
  res
}

#
# Actual loading function that reads the files and puts the dataset into the workspace
#

dsCreateDoLoad <- function(name, fs, params, subsample, colsToLoad, prettyCols, workspace) {
  if(!datasetNameValid(name)) stop('Invalid dataset name')
  if(datasetExists(workspace, name)) stop('Dataset of same name already exists')

  fns <- dsCreateFiles(fs)

  withProgress(message='Aggregating FCS files', value=1, min=1, max=length(fns)+1, {
    data <- loadFCSAggregate(fns,
      if('subsample' %in% params) subsample else NULL)


    ds <- list()

    ds$files <- gsub('\\.fcs$','', gsub('.*/','',fns)) #TODO: perhaps leave in some directory info?
    ds$cellFile <- data$cellFile
    ds$prettyColnames <- dsCreatePrettyColnames(data)

    colSel <- findColIds(colsToLoad, ds$prettyColnames)
    ds$data <- data$exprs[,colSel]
    ds$prettyColnames <- ds$prettyColnames[colSel]

    # remove possible NaNs and NAs
    setProgress('Cleaning...', value=length(fns)+1)
    ds$data[is.nan(ds$data)] <- NA
    ds$data <- ds$data[apply(!is.na(ds$data), 1, all),]

    if('scale' %in% params) {
      setProgress('Scaling...', value=length(fns)+1)
      sds <- apply(ds$data,2,sd)
      sds[sds==0]<-1 #avoid ugly NaNs from constant columns
      ds$data <- scale(ds$data, scale=sds)
    }

    colnames(ds$data) <- ds$prettyColnames
    
    setProgress('Creating dataset', value=length(fns)+1)
    saveDataset(workspace, name, ds)
  })
}

#
# Clone&Delete handlers
#

dsCreateDoClone <- function(ws, orig, name) {
  if(!datasetNameValid(name)) {
    showNotification(type='error', "Invalid dataset name")
    return()
  }

  if(datasetExists(ws, name)) {
    showNotification(type='error', "Dataset already exists")
    return()
  }
  
  if(!datasetExists(ws, orig)) {
    showNotification(type='error', "Original dataset does not exist.")
    return()
  }

  tryCatch( {
      withProgress(message="Cloning the dataset...", value=1, min=1, max=3, {
        ds <- loadDataset(orig)
        setProgress(value=2)
        saveDataset(ws, name, ds)
        setProgress(value=3)
      })
      showNotification(type='message', "Dataset cloned.")
    },
    error=function(e) {
      showNotification(type='error',
        paste('Dataset cloning failed:', e)
      )
    }
  )
}

dsCreateDoDelete <- function(ws, name, confirm) {
  if(!datasetExists(ws, name)) {
    showNotification(type='warning', "The dataset does not exist already.")
    return()
  }

  if(!confirm) {
    showNotification(type='error', "Confirmation required!")
    return()
  }
    
  tryCatch({
      removeDataset(ws, name)
      showNotification(type='message', "Dataset removed.")
    },
    error=function(e) {
      showNotification(type='error',
        paste('Dataset cloning failed:', e)
      )
    }
  )
}

#
# Read column names for overview
#

dsCreateLoadPrettyCols <- function(fs) {
  fn <- dsCreateFiles(fs)[1]
  meta <- loadFCSColnames(fn)

  dsCreatePrettyColnames(meta)
}

#
# Serving function
#

serveDsCreate <- function(workspace, input, output, session) {

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

  #
  # Clone/Delete
  #

  output$dsCreateClone <- renderUI(
    renderDsCreateClone(workspace)
  )

  output$dsCreateDelete <- renderUI(
    renderDsCreateDelete(workspace)
  )

  observeEvent(input$dsCreateDoClone,
    dsCreateDoClone(workspace,
      input$dsCreateCloneOrig,
      input$dsCreateCloneName
    )
  )

  observeEvent(input$dsCreateDoDelete, {
    dsCreateDoDelete(workspace,
      input$dsCreateDeleteOrig,
      input$dsCreateDeleteConfirm
    )

    updateCheckboxInput(session, 'dsCreateDeleteConfirm', value=FALSE)
  })
}
