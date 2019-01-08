#' 
#' 
#' @importFrom stats nls
#' @importFrom stats sd
#' @importFrom stats qt
#' @importFrom stats coef
#' @importFrom graphics hist
#' @importFrom graphics par
#' @importFrom nlstools confint2
#' 
#' 
#' @title Extrapolating richness via curve-fitting, using half the network
#'
#' @description
#'  Fit a series of curves to subsamples of the data, return an extrapolated estimate and 95\% confidence interval estimate of affiliate richness at extrapolated host richness
#'
#'
#' @param n.indep The independent host richness (the model predicts and extrapolates to this vallue)
#' @param assoc.df A data frame of host-affiliate intractions, host names in the first column and affiliate names in the second
#' @param iter1 Set the number of times to fit a curve
#' @param iter2 Set the number of points to subsample at each host richness within the generation of each curve
#' @param plot (boolean; default is TRUE) plot results?
#'
#' @details 
#' @export


copredict.ci <- function(assoc.df, n.indep, iter, plot=TRUE) {
  
  model <- binera(assoc.df, iter)
  q <- stats::coef(model)
  p.cis <- nlstools::confint2(model)
  
  h.count <- length(unique(assoc.df[,1]))
  p.count <- length(unique(assoc.df[,2]))

  est2 <- q["b"] * (h.count)^(q["z"])
  lci2 <- p.cis[1] * (h.count)^(p.cis[2])
  uci2 <- p.cis[3] * (h.count)^(p.cis[4])
  
  print(paste(expression("True number of species in entire network is "),p.count))
  print(paste(expression("Estimated number of species in entire network is "),est2))
  print(paste("The lower 95% CI is ",lci2))
  print(paste("The upper 95% CI is ",uci2))
  
  est <- q["b"] * (n.indep)^(q["z"])
  lci <- p.cis[1] * (n.indep)^(p.cis[2])
  uci <- p.cis[3] * (n.indep)^(p.cis[4])

  print(paste(expression("Extrapolated estimated number of species is "),est))
  print(paste("The lower 95% CI is ",lci))
  print(paste("The upper 95% CI is ",uci))

	ret <- data.frame(
		richness=c('True species richness', 'Estimated richness', 'Extrapolated richness'),
		estimate = c(length(unique(assoc.df[,2])), est2, est),
		lowerCI = c('--', lci2, lci), 
		upperCI = c('--', uci2, uci)) 
	return(ret)
}
