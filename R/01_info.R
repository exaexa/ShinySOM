
infoSign <- '💭'

tooltip <- function(text, elem, pos='bottom') {
  div(
    ilDiv(elem),
    span(infoSign, style='cursor:help; vertical-align:80%;', `data-toggle`='tooltip', `data-placement`=pos, title=text)
  )
}
