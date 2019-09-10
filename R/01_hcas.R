
wrapHClustFunc <- function(distances, method) (function(codes, data, importance=NULL, mapping) {
  cl <- stats::hclust(stats::dist(codes, method=distances), method=method)
  list(height=cl$height, merge=cl$merge, order=cl$order)
})
#
# constants
#

CLUSTER_METHODS=list(
  #will people want median/centroid linkages?
  `Euclidean/Ward.D2`=wrapHClustFunc('euclidean', 'ward.D2'),
  `Manhattan/Ward.D2`=wrapHClustFunc('manhattan', 'ward.D2'),
  `Euclidean/Complete`=wrapHClustFunc('euclidean', 'complete'),
  `Manhattan/Complete`=wrapHClustFunc('manhattan', 'complete'),
  `Euclidean/Average`=wrapHClustFunc('euclidean', 'average'),
  `Manhattan/Average`=wrapHClustFunc('manhattan', 'average'),
  `Mahalanobis Euclidean/Average)`=function(codes, data, importance=NULL, mapping) {
    cl <- mhca::cutreeApriori(
      mhca::mhclust(
        x = if(is.null(importance)) data else (data%*%diag(importance)),
        g = mapping,
        quick = T,
        gIntra = F))
    list(height=cl$height, merge=cl$merge, order=cl$order)
  }
)

