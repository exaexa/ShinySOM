
TRANSFORM_LIST <- list(
  none=list(
    name="No transformation",
    renderParams=function() div(),
    check=function(d, input) TRUE,
    trans=function(d, input) d
  ),
  log=list(
    name="Logarithm+P",
    renderParams=function()
      numericInput('dsTransPLogP', "P", value=0, step=0.01),
    check=function(d, input)
      if(any(d <= -input$dsTransPLogP))
        stop("P is too low to remove negative values"),
    trans=function(d, input) log(d+input$dsTransPLogP)
  ),
  logicle=list(
    name="Simplified logicle",
    renderParams=function() div(
        numericInput('dsTransPLogicleQR', "qR (linearization quantile)", value=.05, step=0.01, min=0, max=1),
        numericInput('dsTransPLogicleN', "Linear root", value=2, step=0.1, min=0.1, max=30),
        numericInput('dsTransPLogicleP', "Logarithm scale (inverse)", value=15, step=0.1, min=0.001)
    ),
    check=function(d, input) TRUE,
    trans=function(d, input) {
      z <- quantile(d, input$dsTransPLogicleQR)
      d <- d-z
      pw <- 1/input$dsTransPLogicleN
      pp <- input$dsTransPLogicleP
      d[d<0] <- (1-((1-d[d<0])^pw))/pw
      d[d>0] <- log(1+d[d>0]/pp)*pp
      d
    }
  ),
  asinh=list(
    name="Hyperbolic arcSin",
    renderParams=function()
      div(
        numericInput('dsTransPAsinhCq', "Center (quantile)", value=0.05, step=0.01, min=0, max=1),
        numericInput('dsTransPAsinhS', "Scale (inverse)", value=15, step=0.1, min=0.001)
      ),
    check=function(d, input) TRUE,
    trans=function(d, input) asinh((d-quantile(d, input$dsTransPAsinhCq))/input$dsTransPAsinhS)
  ),
  #TODO:
  #biexp=list(
  #  name="Biexponential",
  #  #TODO: f(x) = a*exp(b*(x-w))-c*exp(-d*(x-w))+f  (invert this! :D)
  #  renderParams=function() div(),
  #  check=function(d, input) TRUE,
  #  trans=function(d, input) d
  #),
  `2log`=list(
    name="2-sided logarithm",
    renderParams=function()
      div(
        numericInput('dsTransP2LogCq', "Center (quantile)", value=0.05, step=0.01, min=0, max=1),
        numericInput('dsTransP2LogS', "Scale (inverse)", value=15, step=0.1, min=0.001)
      ),
    check=function(d, input) TRUE,
    trans=function(d, input) {
      center <- quantile(d, input$dsTransP2LogCq)
      log(1+
        abs(d-center)*input$dsTransP2LogS
      )*sign(d-center)
    }
  ),
  quantile=list(
    name="Quantile",
    renderParams=function()
      radioButtons('dsTransPQuT', "Output distriution",
        choices=list(`Uniform`='unif', `Normal`='norm', `Exponential`='exp', `Logit`='logit'),
        selected='unif'
      ),
    check=function(d, input) TRUE,
    trans=function(d, input) {
      r <- rank(d, ties.method='average')
      r <- r/(length(r)+1)
      if(input$dsTransPQuT=='norm')
        r <- qnorm(r)
      else if(input$dsTransPQuT=='exp')
        r <- qexp(r)
      else if(input$dsTransPQuT=='logit')
        r <- -log(1/r - 1)
      r
    }
  )
)

renderTransformParams <- function(transType) {
  if(is.null(TRANSFORM_LIST[[transType]])) p("Could not find the transform")
  else TRANSFORM_LIST[[transType]]$renderParams()
}

transformDoScale <- function(d) {
  x = sd(d)
  if (x>0) (d-mean(d))/x
  else d*0
}

transformedDsData <- function(ds, input) {
  cols <- findColIds(input$dsTransCols, ds$prettyColnames)
  transType <- input$dsTransTrType
  if(is.null(TRANSFORM_LIST[[transType]])) stop("Could not find the transform")
  trans <- TRANSFORM_LIST[[transType]]

  #check the cols to provide a reasonable error message on math errors
  for(i in cols)
    trans$check(ds$data[,i], input)

  ndata <- ds$data

  for(i in cols)
    ndata[,i] <- trans$trans(ndata[,i], input)

  if(input$dsTransNormalize)
    for(i in cols)
      ndata[,i] <- transformDoScale(ndata[,i])

  if(input$dsTransDoImportance)
    for(i in cols)
      ndata[,i] <- input$dsTransImportance*ndata[,i]

  ndata
}
