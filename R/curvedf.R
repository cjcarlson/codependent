#' @title Subsampling of entire network
#'
#' @description
#' This function iteratively samples the entire network and generates 
#' a data frame with sub-sample host and affilaite richness tallied together
#'
#' @param assoc.df The raw dataset of associations (pairs of names in 
#'  two columns, hosts and affiliates)
#'
#' @param iter Number of iterations required for each subsample
#' @param subSample Fraction of host species to subsample for richness estimation
#' @export

curve.df <- function(assoc.df, iter, subSample=NULL){

  if(!is.null(subSample)){
    hostlist <- unique(assoc.df[,1])
    hostCut <- sample(hostlist, round(subSample*hostlist,0), replace = FALSE)
    assoc.df <- assoc.df[assoc.df[,1] %in% hostCut,]
  }
  
  u.hosts <- unique(assoc.df[,1])
  n.hosts <- length(u.hosts)

  oneSp <- function(x, i=iter){
    df<- vapply(1:iter, FUN.VALUE=c(double(1), double(1)),
      function(a){
        sub.hosts <- sample(u.hosts, x)
        sub.df <- assoc.df[assoc.df[,1] %in% sub.hosts,]
        u.par <- unique(sub.df[,2])
        n.par <- length(u.par)
        return(c(x, n.par))
    })
    return(t(df))
  }
  
  results.df <- lapply(1:n.hosts, 
    oneSp, i=iter)
  ret <- data.frame(do.call(rbind, results.df))
  colnames(ret) <- c('n.host', 'n.par')
  return(ret)
}





