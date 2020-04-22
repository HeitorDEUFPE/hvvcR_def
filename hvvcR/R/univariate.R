#' @title Internal function for usage in descriptive_categoric function.
#' @name univariate
#'
#' @seealso descriptive_categoric


univariate <- function(x){
  if(class(x)%in%c("character","factor")){
    tab <- table(x)
    ptab <- prop.table(tab)
    resu <- data.frame(categories = names(tab),N = as.vector(tab),"%" = as.vector(ptab)*100,row.names = NULL)
    return(resu)
  }
  else{
    return(NULL)
  }
}
