
getForeignRoots <- function() options('ShinySOM.foreignRoots')$ShinySOM.foreignRoots

getRoot <- function(root) {
  if(root %in% names(getForeignRoots())) getForeignRoots()[root]
  else stop("Unknown root!")
}

reactiveValsForeign <- function()
  reactiveValues(
    up.dir='/',
    up.root=names(options()$ShinySOM.foreignRoots)[0],
    down.files=c()
  )

renderForeign <- function(foreign) {
  div(
    tooltip("Send your local files to ShinySOM storage.",
      h1("Upload data")),

    p("Currently uploading to ",
      strong(foreign$up.root),
      " subdirectory: `",
      strong(foreign$up.dir),
      "' ",
      shinyDirButton('upfolder',
        'Change folder',
        'Select a folder for uploading files',
        FALSE)
    ),

    fileInput("fileUpload",
      "Choose files to upload",
      multiple=T),

    tooltip("Get files, results, datasets and tables from ShinySOM storage.",
      h1("Download results")),

    p(
      shinyFilesButton('downfiles',
        'Select files',
        "Choose a file to download",
        multiple=F),
      uiOutput("foreignDownload")
    )
  )
}

render_foreignDownload <- function(fs) {
  if(is.list(fs)) {
    n.files <- length(fs$files)
    downloadButton("fileDownload", paste("Download",n.files,if(n.files==1)"file" else "files"))
  }
  else "(no file selected)"
}

serveForeign <- function(foreign, input, output) {
  output$foreignDownload <- renderUI({render_foreignDownload(input$downfiles)})

  observeEvent(input$fileUpload, {
    fs <- as.vector(input$fileUpload[,'datapath'])
    tgt <- as.vector(paste0(getRoot(foreign$up.root), foreign$up.dir, '/', input$fileUpload[,'name']))
    ok <- 0
    failed <- 0
    apply(cbind(fs,tgt), 1, function(mv) {
      from <- mv['fs']
      to <- mv['tgt']
      if(file.exists(to)) failed <<- failed + 1
      else if(file.copy(from, to)) ok <<- ok + 1
      else failed <<- failed + 1
    })
    showNotification(type="message",
      paste(ok,"files uploaded successfully,",failed,"failed."))
  })

  observeEvent(input$upfolder, {
    if(is.list(input$upfolder)) {
      foreign$up.root <- input$upfolder$root
      path <- input$upfolder$path
      path[["sep"]] <- '/'
      foreign$up.dir <- do.call("paste", path)
    }
  })

  output$fileDownload <- downloadHandler(
    filename=function() {
      fpath <- input$downfiles$files[[ names(input$downfiles$files)[1] ]]
      fpath[[length(fpath)]]
    },
    content=function(tgt) {
      froot <- getRoot(input$downfiles$root)
      path <- input$downfiles$files[[ names(input$downfiles$files)[1] ]]
      path[["sep"]] <- '/'
      fn <- paste(sep='/', froot, do.call("paste", path))
      file.copy(fn, tgt)
    }
  )

  shinyDirChoose(input, 'upfolder', roots=getForeignRoots())
  shinyFileChoose(input, 'downfiles', roots=getForeignRoots())
}
