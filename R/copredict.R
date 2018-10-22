#' 
#' 
#' @importFrom stats nls
#' @importFrom stats sd
#' @importFrom stats qt
#' @importFrom stats coef
#' @importFrom graphics hist
#' @importFrom graphics par
#' 
#' @title Extrapolating richness via curve-fitting
#'
#' @description
#' Fit a series of curves to subsamples of the data, return an extrapolated estimate and 95\% confidence interval estimate of affiliate richness at extrapolated host richness
#'
#' @param n.indep The independent host richness (the model predicts and extrapolates to this vallue)
#' @param assoc.df A data frame of host-affiliate intractions, host names in the first column and affiliate names in the second
#' @param iter1 Set the number of times to fit a curve
#' @param iter2 Set the number of points to subsample at each host richness within the generation of each curve
#' @param plot (boolean; default is TRUE) plot results?
#' 
#' @details 
#' 
#' @export


copredict <- function(n.indep, assoc.df, 
	iter1, iter2, plot=TRUE) {
	estlist <- sapply(1:iter1, function(x){
		q <- stats::coef(binera(assoc.df, iter2))
    q["b"] * (n.indep)^(q["z"])
	})
  est <- mean(estlist)
	if(plot){
	  graphics::hist(estlist, main='Extrapolated estimates')
	}
  error <- stats::qt(0.975, df=iter1-1) * sd(estlist)/sqrt(iter1)
  lci <- est - 1.96*error
  uci <- est + 1.96*error
#  print(paste(expression("Estimated number of species is"),est))
#  print(paste("The lower 95% CI is",lci))
#  print(paste("The upper 95% CI is",uci))
	return(c(mean=est, lowerCI=lci, upperCI=uci))
}
