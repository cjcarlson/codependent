#' @title Binary rarefaction (curve-fitting)
#'
#' @description
#' This function runs the subsampling algorithm on the association dataset, and returns a fitted model describing the power law relationship
#'
#' @param assoc.df The raw dataset of associations (pairs of names in two columns, hosts and affiliates)
#' @param iter Number of iterations required for each subsample
#'


binera <- function(assoc.df, iter) {

  c <- curve.df(assoc.df, iter)
  model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=c)
  return(model1)

}
