#' @title Make the bivariate report for all dataset.
#' @name bivariate_all
#'
#' @description Receives a data.frame(or tibble) and y (dependent variable) and
#'  calculates bivariate metrics according to the types of the input variables ind dataset.
#' @param data data.frame or tibble.
#' @param target Vector of dependent variable.
#' @param targetClass Tells whether the dependent variable is numeric or binary.
#' @param cutNum Discretize the explanotory numeric variable? TRUE by default.
#' @param cutNumMethod Method for discretization if cutNum = TRUE. Disponible "median", "quintile" or "decile".
#' @param labelNA Label of missing values assume. "Missing" by default.
#' @param export TRUE of FALSE for export results in excel arquive .xlsx. FALSE by default.
#' @param NameExport Character with name of the excel arquive .xlsx file to be exported. By default the is BivariateReport.
#' 
#' 
#' @return Returns a tibble.
#' @seealso descriptive_numeric, descriptive_categoric, bivariate
#' @export

bivariate_all <- function(data,target,
                      targetClass = c("binary","numeric"),
                      cutNum = TRUE,
                      cutNumMethod = c("median","quintile","decile"),
                      labelNA = "Missing",
                      export = FALSE,
                      NameExport = "BivariateReport"){
  if(targetClass=="binary"){
    
    Nametg <- names(table(target))
    
    # Factor variables
    d1 <- data %>% 
      select_if(is.factor) %>% 
      mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
    
    # Character variables
    d2 <- data %>% 
      select_if(is.character) %>% 
      mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
    
    # Numeric variables 
    d3 <- data %>% 
      select_if(is.numeric)
    
    d11 <- bind_cols(d1,d2)
    remove(d1,d2)
    
    nvarc <- ncol(d11)
    nvarn <- ncol(d3)
    Namesc <- names(d11)
    Namesn <- names(d3)
    
    cat("Number of categoric variables:",nvarc,"\n")
    
    for(i in 1:nvarc){
      cat("Variable ",i, "of ",nvarc," ............... ",Namesc[i],"\n")
      
      if(i==1){
        resuc <- data.frame(Variable = Namesc[i],bivariate(x = unslit(d11[,i]),target = target,
                                                           targetClass = targetClass,
                                                           cutNum = cutNum,
                                                           cutNumMethod = cutNumMethod,
                                                           labelNA = labelNA))
        resuc[nrow(resuc)+1,] <- NA
      }
      if(i>1){
        resuc <- bind_rows(resuc,data.frame(Variable = Namesc[i],bivariate(x = unlist(d11[,i]),target = target,
                                                                           targetClass = targetClass,
                                                                           cutNum = cutNum,
                                                                           cutNumMethod = cutNumMethod,
                                                                           labelNA = labelNA)))
        resuc[nrow(resuc)+1,] <- NA
      }
      
    }
    
    cat("Number of numeric variables:",nvarn,"\n")
    
    for(i in 1:nvarn){
      cat("Variable ",i, "of ",nvarn," ............... ",Namesn[i],"\n")
      
      if(i==1){
        resun <- data.frame(Variable = Namesn[i],bivariate(x = unlist(d3[,i]),target = target,
                                                           targetClass = targetClass,
                                                           cutNum = cutNum,
                                                           cutNumMethod = cutNumMethod,
                                                           labelNA = labelNA))
        resun[nrow(resun)+1,] <- NA
      }
      if(i>1){
        resun <- bind_rows(resun,data.frame(Variable = Namesn[i],bivariate(x = unlist(d3[,i]),target = target,
                                                                           targetClass = targetClass,
                                                                           cutNum = cutNum,
                                                                           cutNumMethod = cutNumMethod,
                                                                           labelNA = labelNA)))
        resun[nrow(resun)+1,] <- NA
      }
      
    }
    
    resuc <- as_tibble(resuc); resun <- as_tibble(resun);
    
    names(resuc) <- c("Variable","Categories",paste("N",Nametg[1]),paste("P",Nametg[1]),paste("N",Nametg[2]),
                      paste("P",Nametg[2]),paste("TX",Nametg[1]), paste("TX",Nametg[2]),
                      "Odds","Odds ratio", "L.I. OR", "L.S. OR", "OR p-value","X²","X² p-value","Entropy","InformationGain","K-S")
    
    names(resun) <- c("Variable","Categories",paste("N",Nametg[1]),paste("P",Nametg[1]),paste("N",Nametg[2]),
                      paste("P",Nametg[2]),paste("TX",Nametg[1]), paste("TX",Nametg[2]),
                      "Odds","Odds ratio", "L.I. OR", "L.S. OR", "OR p-value","X²","X² p-value","Entropy","InformationGain","K-S")
    
    if(export==TRUE){
      list("Categoric variables" = resuc,
           "Numeric variables" = resun) %>% 
        writexl::write_xlsx(paste0(name.export,".xlsx"),col_names = TRUE)
    }
    
    invisible(list("Categoric variables" = resuc,
                   "Numeric variables" = resun))
    
  }
  if(targetClass=="numeric"){
    
    Nametg <- names(table(target))
    
    # Factor variables
    d1 <- data %>% 
      select_if(is.factor) %>% 
      mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
    
    # Character variables
    d2 <- data %>% 
      select_if(is.character) %>% 
      mutate_all(.funs = ~fct_explicit_na(.x,na_level = labelNA))
    
    # Numeric variables 
    d3 <- data %>% 
      select_if(is.numeric)
    
    d11 <- bind_cols(d1,d2)
    remove(d1,d2)
    
    nvarc <- ncol(d11)
    nvarn <- ncol(d3)
    Namesc <- names(d11)
    Namesn <- names(d3)
    
    cat("Number of categoric variables:",nvarc,"\n")
    
    for(i in 1:nvarc){
      cat("Variable ",i, "of ",nvarc," ............... ",Namesc[i],"\n")
      
      if(i==1){
        resuc <- data.frame(Variable = Namesc[i],bivariate(x = unlist(d11[,i]),target = target,
                                                           targetClass = targetClass,
                                                           cutNum = cutNum,
                                                           cutNumMethod = cutNumMethod,
                                                           labelNA = labelNA))
        resuc[nrow(resuc)+1,] <- NA
      }
      if(i>1){
        resuc <- bind_rows(resuc,data.frame(Variable = Namesc[i],bivariate(x = unlist(d11[,i]),target = target,
                                                                           targetClass = targetClass,
                                                                           cutNum = cutNum,
                                                                           cutNumMethod = cutNumMethod,
                                                                           labelNA = labelNA)))
        resuc[nrow(resuc)+1,] <- NA
      }
      
    }
    
    cat("Number of numeric variables:",nvarn,"\n")
    
    for(i in 1:nvarn){
      cat("Variable ",i, "of ",nvarn," ............... ",Namesn[i],"\n")
      
      if(i==1){
        resun <- data.frame(Variable = Namesn[i],bivariate(x = unlist(d3[,i]),target = target,
                                                           targetClass = targetClass,
                                                           cutNum = cutNum,
                                                           cutNumMethod = cutNumMethod,
                                                           labelNA = labelNA))
        resun[nrow(resun)+1,] <- NA
      }
      if(i>1){
        resun <- bind_rows(resun,data.frame(Variable = Namesn[i],bivariate(x = unlist(d3[,i]),target = target,
                                                                           targetClass = targetClass,
                                                                           cutNum = cutNum,
                                                                           cutNumMethod = cutNumMethod,
                                                                           labelNA = labelNA)))
        resun[nrow(resun)+1,] <- NA
      }
      
    }
    
    resuc <- as_tibble(resuc); resun <- as_tibble(resun);
    
    names(resuc) <- c("Variable","Categories","min","1º quartile","Median","Mean","3º quartile","max","sd","N Missing", "% Missing", "F Statistic", "F p-value")

    names(resun) <- c("Variable","Correlation","Cor p-value","N Missing target","% Missing target","N Missing x", "% Missing x", "N Pairs","% Pairs")
    
    if(export==TRUE){
      list("Categoric variables" = resuc,
           "Numeric variables" = resun) %>% 
        writexl::write_xlsx(paste0(name.export,".xlsx"),col_names = TRUE)
    }
    
  }
  
  cat("\n########################\n")
  cat("#        Done !        #\n")
  cat("########################\n")
  
  
  invisible(list("Categoric variables" = resuc,
                 "Numeric variables" = resun))
  
  
}
