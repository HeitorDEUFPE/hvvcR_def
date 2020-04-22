#' @title Internal function for usage in bivariate function.
#' @name Entropy
#'
#' @seealso bivariate

Entropy <-function( vls ) {
  res<- vls/sum(vls)*log2(vls/sum(vls))
  res[vls == 0]<- 0
  return(-sum(res))
}
