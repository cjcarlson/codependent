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
#'
#' @export


copredict.ci <- function(n.indep, assoc.df, iter1, iter2) {

  estlist <- c()
  est100list <- c()
  df <- data.frame(blist = c(0), zlist=c(0))
  for (i in 1:iter1) {
    q <- coef(binera.50(assoc.df, iter2))
    est <- q["b"] * (n.indep)^(q["z"])
    est.100 <- q["b"] * (length(unique(assoc.df[,1])))^q["z"]
    df[i,] <- c(q["b"],q["z"])
    estlist <- c(estlist,est)
    est100list <- c(est100list,est.100)
    print(i)
  }


  e2 <- elnorm(est100list, method = "mvue", ci = TRUE, ci.type = "two-sided",
               ci.method = "exact", conf.level = 0.95)
  est2 <- exp(e2$parameters[1])
  lci2 <- exp(e2$interval$limits[1])
  uci2 <- exp(e2$interval$limits[2])

  print(paste(expression("True number of species in entire network is"),length(unique(assoc.df[,2]))))
  print(paste(expression("Estimated number of species in entire network is"),est2))
  print(paste("The lower 95% CI is",lci2))
  print(paste("The upper 95% CI is",uci2))


  par(mfrow=c(2,1))
  hist(est100list,main='100% estimates')
  hist(estlist,main='Extrapolated estimates')

  e1 <- elnorm(estlist, method = "mvue", ci = TRUE, ci.type = "two-sided",
               ci.method = "exact", conf.level = 0.95)
  est <- exp(e1$parameters[1])
  lci <- exp(e1$interval$limits[1])
  uci <- exp(e1$interval$limits[2])



  print(paste(expression("Extrapolated estimated number of species is"),est))
  print(paste("The lower 95% CI is",lci))
  print(paste("The upper 95% CI is",uci))



}
