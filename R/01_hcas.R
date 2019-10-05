
wrapHClustFunc <- function(distances, method) (function(codes, data, mapping) {
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
  `Mahalanobis/Average`=function(codes, data, mapping) {
    #TODO: progress bar
    cl <- mhca::cutreeApriori(mhca::fixNonMonotHca(
      mhca::mhclust(
        x = data,
        g = mapping,
        quick = T,
        gIntra = F)))
    list(height=cl$height, merge=cl$merge, order=cl$order)
  }
)

