#' @title Make the bivariate report.
#' @name bivariate
#'
#' @description Receives x (explanatory variable) and y (dependent variable) and
#'  calculates bivariate metrics according to the types of the input variables.
#' @param x Vector of explanatory variable.
#' @param target Vector of dependent variable.
#' @param targetClass Tells whether the dependent variable is numeric or binary.
#' @return Returns a tibble.
#' @seealso descriptive_numeric, descriptive_categoric
#' @export

bivariate <- function(x,target,targetClass = c("binary","numeric")){
  if(targetClass=="binary"){
    if(class(x)%in%c("character","factor")){
      n <- length(table(x))
      d <- cbind(table(x,target)[,1],
                 p0 = rep(0,n),
                 table(x,target)[,2],
                 p1 = rep(0,n),
                 rr = rep(0,n),
                 tx0 = rep(0,n),
                 tx1 = rep(0,n),
                 T.CHI = c(0,rep(NA,n-1)),
                 P.CHI = c(0,rep(NA,n-1)),
                 ent = rep(0,n),
                 gi = c(0,rep(NA,n-1)))
      for(i in 1:n){
        d[i,2] <- (d[i,1]/sum(d[,1]))
        d[i,4] <- (d[i,3]/sum(d[,3]))
        d[i,5] <- d[i,1]/sum(d[i,c(1,3)])
        d[i,6] <- d[i,3]/sum(d[i,c(1,3)])
        d[i,7] <- d[i,4]/d[i,2]
        d[1,8] <- chisq.test(table(x,target))$statistic
        d[1,9] <- chisq.test(table(x,target))$p.value
        d[i,10] <- Entropy(d[i,c(1,3)])
        d[1,11] <- InformationGain(table(x,target))
      }
      colnames(d) <- c("N0","P0","N1","P1","TX0","TX1","RR","Chi","Chi.P","Entropy","Information Gain")
      return(d)
    }
  }
}

bivariate(x = sample(c("A","B","C"),1000,replace = T,prob = c(0.2,0.3,0.5)),
          target = sample(c(0,1),1000,replace = T,prob = c(0.7,0.3)),
          targetClass = "binary")
