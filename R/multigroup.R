#' @title Multigroup estimates
#'
#' @description
#' This function runs the new inclusion-exclusion algorithm correction designed by Colin Carlson and Laura Alexander and implemented in Carlson et al. (in prep)
#'
#' @param fake A data frame of the group membership of all the sample species
#' @param orders You gotta just quickly name all the groups
#' @param est.num.fake The numbers you extrapolated for each group
#'
#' @export


multigroup <- function(fake,
                  orders=c("A","B","C","D","E","F"),
                  est.num.fake) {

  estimates <- rbind(counted=colSums(fake[,-1]),
                     estimated=est.num.fake)

  colnames(estimates) <- orders

  colnames(fake) <- c('parasite',orders)
  combos <- list()
  #make list of all combos
  for (i in 1:length(orders)){
    combos[[i]] <- combn(orders,i)
  }

  #count how often each appears
  for (i in 1:length(combos)){
    lvl <- combos[[i]]
    nums <- NULL
    for (c in 1:ncol(lvl)){
      if (i == 1) r <- which(fake[,lvl[,c]] == i) #for each individual group
      if (i > 1) r <- which(rowSums(fake[,lvl[,c]]) == i) #for each combination

      n <- length(r)
      nums <- c(nums,n)
    }
    combos[[i]] <- rbind(combos[[i]],nums)
  }

  intersects <- matrix(ncol = 1, nrow = length(orders))
  intersects[1] <- sum(estimates[2,]) #first level is just the sum of all the independent estimates

  for (i in 2:length(orders)){ # the number of orders will be the number of sets
    clist <- combos[[i]] #gets all combinations of length i from combos list
    int <- NULL #empty vector of the intersects
    for (c in 1:ncol(clist)){ #each column is a different unique combination
      groups <- clist[1:i,c]
      collect <- NULL
      for (g in groups){
        value <- estimates["estimated",g]/estimates["counted",g] #for each Aest/numA, etc.
        collect <- c(collect, value)
      }
      colvalue <- as.numeric(clist[i+1,c])*sum(collect)/length(collect) #multiply by the intersect of that group
      int <- c(int, colvalue)   #collect them!
    }

    if(i %% 2 == 0) intersects[i] <- -sum(int) #if i is even, sum of cardinality blah blah blah it's subtracted
    if(i %% 2 == 1) intersects[i] <- sum(int) #if i is odd, added

  }

  return(sum(intersects))

}
