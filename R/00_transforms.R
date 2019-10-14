
TRANSFORM_LIST <- list(
  none=list(
    name="No transformation",
    renderParams=function(input) div(),
    check=function(d, input) TRUE,
    trans=function(d, input) d
  ),
  logicle=list(
    name="Logicle",
    renderParams=function(input) div(
      sliderInput('dsTransLogicleW', "W (linearization width)", value=persistentP(input, 'dsTransLogicleW', .5), step=0.05, min=0, max=1),
      sliderInput('dsTransLogicleT', "log(t) (data bit width)", value=persistentP(input, 'dsTransLogicleT', 18), step=0.5, min=0.5, max=32),
      sliderInput('dsTransLogicleM', "M (output range)", value=persistentP(input, 'dsTransLogicleM', 4.5), step=0.1, min=0.1, max=10),
      sliderInput('dsTransLogicleA', "A (additional negative range)", value=persistentP(input, 'dsTransLogicleA', 0), step=0.1, min=0, max=1.5)
    ),
    check=function(d, input) TRUE,
    trans=function(d, input)
      flowCore::logicleTransform(
        w=input$dsTransLogicleW,
        t=as.integer(2^input$dsTransLogicleT),
        m=input$dsTransLogicleM,
        a=input$dsTransLogicleA)(d)
  ),
  asinh=list(
    name="Hyperbolic arcSin",
    renderParams=function(input) div(
      sliderInput('dsTransPAsinhCq', "Center (quantile)", value=persistentP(input, 'dsTransPAsinhCq', 0.05), step=0.01, min=0, max=1),
      sliderInput('dsTransPAsinhS', "log-scale", value=persistentP(input, 'dsTransPAsinhS', 0), step=.5, min=-20, max=20)
    ),
    check=function(d, input) TRUE,
    trans=function(d, input) asinh((d-quantile(d, input$dsTransPAsinhCq))*exp(input$dsTransPAsinhS))
  ),
  biexp=list(
    name="Biexponential",
    renderParams=function(input) div(
      numericInput('dsTransBiexW', 'Center (W)', value=persistentP(input, 'dsTransBiexW', 0), step=1),
      sliderInput('dsTransBiexA', 'Positive scale (A)', value=persistentP(input, 'dsTransBiexA', 0.5), step=0.1, min=0, max=20),
      sliderInput('dsTransBiexB', 'Positive compresion (B)', value=persistentP(input, 'dsTransBiexB', 1), step=0.1, min=0, max=20),
      sliderInput('dsTransBiexC', 'Negative scale (C)', value=persistentP(input, 'dsTransBiexC', 0.5), step=0.1, min=0, max=20),
      sliderInput('dsTransBiexD', 'Negative compresion (D)', value=persistentP(input, 'dsTransBiexD', 1), step=0.1, min=0, max=20)
    ),
    check=function(d,input) TRUE,
    trans=function(d,input)
      flowCore::biexponentialTransform(
        w=input$dsTransBiexW,
        a=input$dsTransBiexA,
        b=input$dsTransBiexB,
        c=input$dsTransBiexC,
        d=input$dsTransBiexD)(d)
  ),
  log=list(
    name="Logarithm+P",
    renderParams=function(input) div(
      numericInput('dsTransPLogP', "P", value=persistentP(input, 'dsTransPLogP', 0), step=0.01),
      numericInput('dsTransPLogC', "Negative clamp (logscale)", value=persistentP(input, 'dsTransPLogC', 1), step=0.1)
    ),
    check=function(d, input) TRUE,
    trans=function(d, input) {
      v <- d+input$dsTransPLogP
      threshold <- exp(-input$dsTransPLogC)
      v[v<threshold] <- threshold
      log(v)
    }
  ),
  `2log`=list(
    name="2-sided logarithm",
    renderParams=function(input) div(
      sliderInput('dsTransP2LogCq', "Center (quantile)", value=persistentP(input, 'dsTransP2LogCq', 0.05), step=0.01, min=0, max=1),
      sliderInput('dsTransP2LogS', "Log-scale", value=persistentP(input, 'dsTransP2LogS', 0), step=1, min=-20, max=20)
    ),
    check=function(d, input) TRUE,
    trans=function(d, input) {
      center <- quantile(d, input$dsTransP2LogCq)
      log(1+
        abs(d-center)*exp(input$dsTransP2LogS)
      )*sign(d-center)
    }
  ),
  quantile=list(
    name="Quantile",
    renderParams=function(input)
      radioButtons('dsTransPQuT', "Output distriution",
        choices=list(`Uniform`='unif', `Normal`='norm', `Exponential`='exp', `Logit`='logit'),
        selected=persistentP(input, 'dsTransPQuT', 'unif')
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

renderTransformParams <- function(input, transType) {
  if(is.null(TRANSFORM_LIST[[transType]])) p("Could not find the transform")
  else TRANSFORM_LIST[[transType]]$renderParams(input)
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
