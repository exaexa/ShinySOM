
findColIds <- function(cols, all) {
  (1:length(all))[all %in% cols]
}

ilDiv <- function(..., style='') div(
  style=paste0('display: inline-block;', style), ...)
