#' @title Subsampling of entire network
#'
#' @description
#' This function iteratively samples the entire network and generates a data frame with sub-sample host and affilaite richness tallied together
#'
#' @param assoc.df The raw dataset of associations (pairs of names in two columns, hosts and affiliates)
#' @param iter Number of iterations required for each subsample
#'
#' @export

curve.df <- function(assoc.df, iter, subSample=0){

  # placeholder: Tad add subsample at this level because you didn't
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
  results.df <- data.frame(results.df)
  return(results.df)
}
