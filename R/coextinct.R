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


coextinct <- function(assoc.df, iter=100, plots=FALSE, subSample=NULL) {
  
  assoc.df = unique(na.omit(assoc.df))
  n.host <- n.par <- host <- pred <- 0 
  
  cu <- curve.df(assoc.df, iter, subSample=subSample)
  cu.ex <- cu
  cu.ex[,1] <- (max(cu[,1])-cu[,1])/(max(cu[,1]))
  cu.ex[,2] <- (max(cu[,2])-cu[,2])/(max(cu[,2]))
  model1 <- stats::nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=cu)
  
  # OUR PREDICTIONS, AND OUR CURVES
  pred.df <- data.frame(pred = predict(model1, newdata=c(n.host=cu$n.host)),
                        host = c(cu$n.host)); for(i in 1:iter) {pred.df <- rbind(c(0,0),pred.df)}
  pred.ex <- pred.df
  pred.ex[,1] <- (max(pred.df[,1])-pred.df[,1])/(max(pred.df[,1]))
  pred.ex[,2] <- (max(pred.df[,2])-pred.df[,2])/(max(pred.df[,2]))
  
  # KOH CODE
  koh.list <- sapply(c(1:length(unique(assoc.df[,1]))), kohex, assoc.df=toju2018)
  koh.df <- data.frame(host.ext = c(1:length(unique(assoc.df[,1]))),
                       pred.ext = length(unique(assoc.df[,2]))-koh.list)
  koh.df[,1] <- koh.df[,1]/length(unique(assoc.df[,1]))
  koh.df[,2] <- koh.df[,2]/length(unique(assoc.df[,2]))
  
  # KOH APPROX CODE
  
  koh.apx.list <- sapply(c(1:length(unique(assoc.df[,1])))/length(unique(assoc.df[,1])),
                         koh.approx, assoc.df=toju2018)
  koh.apx.df <- data.frame(host.ext = c(1:length(unique(assoc.df[,1])))/length(unique(assoc.df[,1])),
                       pred.ext = koh.apx.list)
  
  # PLOTTING STUFF
  
  # real data 
  g <- ggplot2::ggplot(cu.ex, aes(n.host, n.par)) + xlim(0,1.01) + ylim(0,1.01) + xlab('Host Extinction') + ylab('Affiliate Extinction') +
      geom_point(shape = 16, size = 2.5, show.legend = FALSE, alpha = .15, color='darkturquoise') + theme_bw() 
  # fitted curve
  g <- g + geom_line(color='black',lwd=1,data = pred.ex, aes(x=host, y=pred))
  # koh estimator
  g <- g + geom_line(color='red',lwd=1,data = koh.df, aes(x=host.ext, y=pred.ext))
  # koh approx
  g <- g + geom_line(color='red',lwd=1,data = koh.apx.df, aes(x=host.ext, y=pred.ext), linetype=2)
  print(g)
  
  return(model1)

}
