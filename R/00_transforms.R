
TRANSFORM_LIST <- list(
  none=list(
    name="No transformation",
    renderParams=function(input) div(),
    gatherParams=function(input) list(),
    trans=function(d, p) d
  ),
  logicle=list(
    name="Logicle",
    renderParams=function(input) div(
      sliderInput('dsTransLogicleW', "W (linearization width)", value=persistentP(input, 'dsTransLogicleW', .5), step=0.05, min=0, max=1),
      sliderInput('dsTransLogicleT', "log(t) (data bit width)", value=persistentP(input, 'dsTransLogicleT', 18), step=0.5, min=0.5, max=32),
      sliderInput('dsTransLogicleM', "M (output range)", value=persistentP(input, 'dsTransLogicleM', 4.5), step=0.1, min=0.1, max=10),
      sliderInput('dsTransLogicleA', "A (additional negative range)", value=persistentP(input, 'dsTransLogicleA', 0), step=0.1, min=0, max=1.5)
    ),
    gatherParams=function(input) list(
      w=input$dsTransLogicleW,
      t=input$dsTransLogicleT,
      m=input$dsTransLogicleM,
      a=input$dsTransLogicleA),
    trans=function(d, p)
      flowCore::logicleTransform(w=p$w, t=as.integer(2^p$t), m=p$m, a=p$a)(d)
  ),
  asinh=list(
    name="Hyperbolic arcSin",
    renderParams=function(input) div(
      sliderInput('dsTransPAsinhCq', "Center (quantile)", value=persistentP(input, 'dsTransPAsinhCq', 0.05), step=0.01, min=0, max=1),
      sliderInput('dsTransPAsinhS', "log-scale", value=persistentP(input, 'dsTransPAsinhS', 0), step=.5, min=-20, max=20)
    ),
    gatherParams=function(input) list(
      cq=input$dsTransPAsinhCq,
      s=input$dsTransPAsinhS),
    trans=function(d, p) asinh((d-quantile(d, p$cq))*exp(p$s))
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
    gatherParams=function(input) list(
      w=input$dsTransBiexW,
      a=input$dsTransBiexA,
      b=input$dsTransBiexB,
      c=input$dsTransBiexC,
      d=input$dsTransBiexD),
    trans=function(d,p)
      flowCore::biexponentialTransform(
        w=p$w,
        a=p$a,
        b=p$b,
        c=p$c,
        d=p$d)(d)
  ),
  log=list(
    name="Logarithm+P",
    renderParams=function(input) div(
      numericInput('dsTransPLogP', "P", value=persistentP(input, 'dsTransPLogP', 0), step=0.01),
      numericInput('dsTransPLogC', "Negative clamp (logscale)", value=persistentP(input, 'dsTransPLogC', 1), step=0.1)
    ),
    gatherParams=function(input) list(
      c=input$dsTransPLogC,
      p=input$dsTransPLogP),
    trans=function(d, p) {
      v <- d+p$p
      threshold <- exp(-p$c)
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
    gatherParams=function(input) list(
      cq=input$dsTransP2LogCq,
      s=input$dsTransP2LogS),
    trans=function(d, p) {
      center <- quantile(d, p$cq)
      log(1+
        abs(d-center)*exp(p$s)
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
    gatherParams=function(input) list(type=input$dsTransPQuT),
    trans=function(d, p) {
      r <- rank(d, ties.method='average')
      r <- r/(length(r)+1)
      if(p$type=='norm')
        r <- qnorm(r)
      else if(p$type=='exp')
        r <- qexp(r)
      else if(p$type=='logit')
        r <- -log(1/r - 1)
      r
    }
  )
)

renderTransformParams <- function(input, transType) {
  if(is.null(TRANSFORM_LIST[[transType]])) p("N/A")
  else TRANSFORM_LIST[[transType]]$renderParams(input)
}

transformDoScale <- function(d) {
  x = sd(d)
  if (x>0) (d-mean(d))/x
  else d*0
}

transformFind <- function(n) {
  if(is.null(TRANSFORM_LIST[[n]]))
    stop("Non-existing transform function specified")
  TRANSFORM_LIST[[n]]
}
  
transformedDsDataByDesc <- function(ds, desc) {
  cols <- findColIds(desc$cols, ds$prettyColnames)
  trans <- transformFind(desc$transform)

  ndata <- ds$data
  for (i in cols)
    ndata[,i] <- trans$trans(ndata[,i], desc$params)

  if(desc$normalize)
    for(i in cols)
      ndata[,i] <- transformDoScale(ndata[,i])

  if(!is.null(desc$importance))
    for(i in cols)
      ndata[,i] <- desc$importance*ndata[,i]

  list(data=ndata, desc=desc)
}

transformedDsData <- function(ds, input) {
  desc <- list(
    cols=input$dsTransCols,
    transform=input$dsTransTrType,
    params=transformFind(input$dsTransTrType)$gatherParams(input),
    normalize=input$dsTransNormalize,
    importance=if(input$dsTransDoImportance) input$dsTransImportance else NULL)

  transformedDsDataByDesc(ds, desc)
}
