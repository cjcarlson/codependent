#' @title Binary rarefaction (curve-fitting)
#'
#' @description
#' This function runs the subsampling algorithm on the association dataset, and returns a fitted model describing the power law relationship
#'
#' @param assoc.df The raw dataset of associations (pairs of names in two columns, hosts and affiliates)
#' @param iter Number of iterations required for each subsample
#' @param plots Do you want plots? Maybe you do. The world is your oyster.
#'


binera <- function(assoc.df, iter, plots=FALSE) {

  c <- curve.df(assoc.df, iter)
  model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=c)
  
  pred.df <- data.frame(pred = predict(model1), host = c$n.host)
  
  if(plots==TRUE) {
  g <- ggplot2::ggplot(c, aes(n.host, n.par)) + xlim(0,max(c$n.host)*1.05) + ylim(0,max(c$n.par)*1.05) + xlab('Hosts') + ylab('Affiliates') + 
    geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .15, color='darkturquoise') + theme_bw() +
    geom_line(color='black',lwd=1,data = pred.df, aes(x=host, y=pred))
  print(g)
  }
  return(model1)
}
