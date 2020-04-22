#' @title Make the descriptive statistical analysis of categoric variables
#' @name descriptive_categoric
#'
#' @description Receives a data.frame or tibble and returns some descriptive measures for the categoric variables in the dataset.
#' @param data data.frame or tibble.
#' @param export TRUE of FALSE for export results in excel arquive .xlsx. FALSE by default.
#' @param NameExport Character with name of the excel arquive .xlsx file to be exported. By default the is DescritptiveCategoric.
#' @return Returns a tibble.
#' 
#' 
#' @seealso descriptive_numeric
#' @export

descriptive_categoric <-   function(data,export = FALSE,NameExport = "DescriptiveCategoric",labelNA = "Missing"){
  
  require(magrittr)
  require(dplyr)
  
  d1 <- data %>% 
    select_if(is.factor) %>% 
    mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
  
  d2 <- data %>% 
    select_if(is.character) %>% 
    mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
  
  remove(data)
  
  d <- cbind.data.frame(d1,d2)
  
  rm(d1,d2)
  
  # Missing values

  Names <- names(d)
  nvar <- ncol(d)
  
  cat("Number of categoric variables:",nvar,"\n")

  for(i in 1:nvar){

    cat("Variable ",i, "of ",nvar," ............... ",Names[i],"\n")
    
    if(i==1){
      resu <- cbind.data.frame(variable = Names[1],univariate(d[,1]))
      resu[nrow(resu)+1,] <- c(NA,NA,NA,NA)
    }
    if(i>1){
      resu <- rbind.data.frame(resu,
                               cbind.data.frame(variable = Names[i],univariate(d[,i])))
      resu[nrow(resu)+1,] <- c(NA,NA,NA,NA)
    }
  }
  
  colnames(resu) <- c("variable","categories","N","%")
  
  resu <- as_tibble(resu)
  
  if(export==TRUE){
    writexl::write_xlsx(resu,paste0(name.export,".xlsx"))
  }
  
  cat("\n########################\n")
  cat("#        Done !        #\n")
  cat("########################\n")
  
  invisible(resu)
  
}

