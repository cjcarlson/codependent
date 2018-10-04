#' @title Subsampling of 50\% o network
#'
#' @description
#' This function iteratively samples half the network and generates a data frame with sub-sample host and affilaite richness tallied together
#'
#' @param assoc.df The raw dataset of associations (pairs of names in two columns, hosts and affiliates)
#' @param iter Number of iterations required for each subsample
#' 


curve.50 <- function(assoc.df, iter){
  
  hostlist <- unique(assoc.df[,1])
  host.50 <- sample(hostlist, floor(length(hostlist)/2), replace = FALSE)
  
  assoc.df <- assoc.df[assoc.df[,1] %in% host.50,]
  
  u.hosts <- unique(assoc.df[,1])
  n.hosts <- length(u.hosts)
  
  results.df <- data.frame(n.host=0,n.par=0)
  results.df <- results.df[-1,]
  
  for (i in 1:n.hosts) {
    #for (i in 1:5) {
    for (j in 1:iter) {
      
      sub.hosts <- sample(u.hosts,i)
      sub.df <- assoc.df[assoc.df[,1] %in% sub.hosts,]
      u.par <- unique(sub.df[,2])
      n.par <- length(u.par)
      
      results.df[nrow(results.df)+1,] <- c(i, n.par)
    }
    #print(i)
  }
  return(results.df)
}
