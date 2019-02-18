
#' @title Asymptotic scaling estimator
#'
#' @description
#' Estimating z values asymptotically
#' 
#' 
#' @param hell ass
#' @details 
#'
#' @importFrom stats nls
#' @importFrom stats sd
#' @importFrom stats qt
#' @importFrom stats coef
#' @importFrom graphics hist
#' @importFrom graphics par
#' @importFrom nlstools confint2
#' 
#' @export


zstar <- function(assoc.df, nsamp=1, npts=100, plot=TRUE) {
  
  n.host <- length(unique(assoc.df[,1]))
  n.aff <- length(unique(assoc.df[,2]))
  zvals <- data.frame(nhost=c(1),z=c(1),est=c(1))
  zvals <- zvals[0,]
  
  for (i in 1:npts) {
    iter <- sample(5:n.host,1) #n.host
    host.list <- sample(assoc.df[,1],iter)
    net.sub <- assoc.df[assoc.df[,1] %in% host.list,]
    net.sub <- unique(net.sub[,1:2])
    cop <- tryCatch(codependent::copredict(net.sub, n.host, nsamp, plot=TRUE),error = function(e){NA})
    if(!is.na(cop)){
      zvals[i,] <- c(iter, as.numeric(cop[[2]][2]), as.numeric(cop[[1]][1]))
    }
  }
  
  #print(zvals)
  
  z.0 <- min(zvals$z) * 0.5
  model.0 <- lm(log(z - z.0) ~ nhost, data=zvals)
  start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=z.0)
  nl.fit <- nls(z ~ a * exp(b * nhost) + c, 
                data = zvals, start = start)
  
  pred <- predict(nl.fit)
  plot(zvals[,1:2])
  points(pred~zvals$nhost, col='red')
  
  z.est <- coef(nl.fit)[3]
  z.conf <- nlstools::confint2(nl.fit)[3,]
  return(list(z.est,z.conf))
  
}
