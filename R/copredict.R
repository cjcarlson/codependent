
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
#' @importFrom stats nls
#' @importFrom stats sd
#' @importFrom stats qt
#' @importFrom stats coef
#' @importFrom graphics hist
#' @importFrom graphics par
#' 
#' @export


copredict <- function(assoc.df, n.indep, iter, plot=TRUE) {
    
    model <- binera(assoc.df, iter)
    q <- stats::coef(model)
    est <- q["b"] * (n.indep)^(q["z"])
    p.cis <- nlstools::confint2(model)
    lci <- p.cis[1] * (n.indep)^(p.cis[2])
    uci <- p.cis[3] * (n.indep)^(p.cis[4])
    
	return(list(c(mean=est, lowerCI=lci, upperCI=uci),
	            q,
	            p.cis))
}
