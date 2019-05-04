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


kohex <- function(assoc.df, E) {
  
  assoc.df = unique(na.omit(assoc.df))
  H <- length(unique(assoc.df[,1]))
  h <- H-E
  n <- length(unique(assoc.df[,2]))
  
  degdist <- c(table(assoc.df[,2]))
  sj <- function(num) {sum(degdist==num)}
  
  alpha <- function(j) {if(j+h<=H){choose((H-j),h)/choose(H,h)
    } else {0}}
  
  tau.f <- function(j) {alpha(j)*sj(j)}
  tau <- n - sum(sapply(c(1:H), tau.f))
  return(tau)
}


koh.approx <- function(assoc.df, E) {
  
  assoc.df = unique(na.omit(assoc.df))
  H <- length(unique(assoc.df[,1]))
  h <- H-E
  n <- length(unique(assoc.df[,2]))
  
  degdist <- c(table(assoc.df[,2]))
  sbar <- mean(degdist)
  
  A.est <- (0.35*E - 0.43)*E*log(sbar) + E
  return(A.est)
  
}


