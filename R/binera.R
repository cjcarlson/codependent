#' @title Binary rarefaction (curve-fitting)
#'
#' @description
#' This function runs the subsampling algorithm on the association dataset, and returns a fitted model describing the power law relationship
#'
#' @param assoc.df The raw dataset of associations (pairs of names in two columns, hosts and affiliates)
#' @param iter Number of iterations required for each subsample
#' @param plots Do you want plots? Maybe you do. The world is your oyster.
#' @param subSample Fraction of host species to subsample (default=NULL, use all data)
#'
#' @export


binera <- function(assoc.df, iter=100, plots=FALSE, subSample=NULL, koh=FALSE) {

  n.host <- n.par <- host <- pred <- 0 
  
  cu <- curve.df(assoc.df, iter, subSample=subSample)
  model1 <- stats::nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=cu)

  if(plots==TRUE) {
  pred.df <- data.frame(pred = predict(model1), host = cu$n.host)
  g <- ggplot2::ggplot(cu, aes(n.host, n.par)) + xlim(0,max(cu$n.host)*1.05) + ylim(0,max(cu$n.par)*1.05) + xlab('Hosts') + ylab('Affiliates') +
    geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .15, color='darkturquoise') + theme_bw() +
    geom_line(color='black',lwd=1,data = pred.df, aes(x=host, y=pred))
  
  if(koh==TRUE) {
    vals <- c(0:length(unique(assoc.df[,1])))
    dfvals <- sapply(length(unique(assoc.df[,1]))-vals, kohex, assoc.df=assoc.df)
    koh.df <- data.frame(vals, dfvals)
    g <- g + geom_line(color='red',lwd=1,linetype=2,data = koh.df, aes(x=vals, y=dfvals))
  }
  
  print(g)
  }
  return(model1)
}
