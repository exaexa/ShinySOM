
findColIds <- function(cols, all) {
  (1:length(all))[all %in% cols]
}
