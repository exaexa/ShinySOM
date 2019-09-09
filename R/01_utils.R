
findColIds <- function(cols, all) {
  (1:length(all))[all %in% cols]
}

ilDiv <- function(..., style='') div(
  style=paste0('display: inline-block;', style), ...)

sliderPointSize <- function(id) sliderInput(
  id, "Point size", value=1, min=0, max=5, step=.1)

sliderAlpha <- function(id) sliderInput(
  id, "Alpha", value=0.3, min=0, max=1, step=.01)
