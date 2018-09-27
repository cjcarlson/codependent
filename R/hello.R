# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


curve.df <- function(assoc.df, iter){

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

# binary network rarefaction

binera <- function(assoc.df, iter) {

  c <- curve.df(assoc.df, iter)
  model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=c)
  return(model1)

}



copredict <- function(n.indep, assoc.df, iter1, iter2) {

  estlist <- c()
  for (i in 1:iter1) {
  q <- coef(binera(assoc.df, iter2))
  est <- q["b"] * (n.indep)^(q["z"])
  estlist <- c(estlist,est)
  print(i)
  }

  est <- mean(estlist)
  hist(estlist,main='Extrapolated estimates')
  error <- qt(0.975,df=iter1-1)*sd(estlist)/sqrt(iter1)
  lci <- est - 1.96*error
  uci <- est + 1.96*error
  print(paste(expression("Estimated number of species is"),est))
  print(paste("The lower 95% CI is",lci))
  print(paste("The upper 95% CI is",uci))


}


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
    print(i)
  }
  return(results.df)
}

binera.50 <-  function(assoc.df, iter) {
  
  c <- curve.50(assoc.df, iter)
  model1 <- nls(n.par~b*n.host^z,start = list(b = 1, z = 0.5),data=c)
  return(model1)
  
}


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
