#' @title Binary rarefaction (curve-fitting) for 50% dataset
#'
#' @description
#' This function runs the subsampling algorithm iteravely on half (several times over) the association dataset, and returns a fitted model describing the power law relationship 
#'

binera.50 <-  function(assoc.df, iter) {
  
  c <- curve.50(assoc.df, iter)
  model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=c)
  return(model1)
  
}