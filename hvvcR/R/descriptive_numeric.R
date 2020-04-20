#' @title Make the descriptive statistical analysis of numeric variables
#' @name descriptive_numeric
#'
#' @description Receives a data.frame or tibble and returns some descriptive measures for the numeric variables in the dataset.
#' @param data data.frame or tibble.
#' @param export TRUE of FALSE for export results in excel arquive .xlsx. FALSE by default.
#' @param NameExport Character with name of the excel arquive .xlsx file to be exported. By default the is DescritptiveNumeric.
#' @return Returns a tibble.
#' @seealso descriptive_categoric
#' @export

descriptive_numeric <- function(data,export = FALSE,NameExport = "DescritptiveNumeric"){
  require(magrittr)
  require(dplyr)
  # flag.classe vai servir para selecionar somente as variaveis
  # numericas da base
  flag.classe <- ifelse(lapply(data,class)=="numeric"|
                          lapply(data,class)=="integer",1,0)
  # Numero de variaveis numericas
  nvar <- sum(flag.classe)
  # Contador do for para as variaveis de interesse
  iv = 1
  # Declarando variaveis
  minimo = maximo = media = mediana = numeric(nvar)
  quartil1 = quartil3 = sd = cv = ic95 = numeric(nvar)
  icli = icls = numeric(nvar)
  # Nomes das variaveis
  nomes <- colnames(data)
  nomes <- nomes[flag.classe==1]
  
  cat("Number of numeric variables:",nvar,"\n")
  
  # Algumas funcoes so funcionam em listas =/
  datal <- data %>%
    dplyr::select(nomes)
  
  for(i in 1:nvar){
      cat("Variable ",i, "of ",nvar," ............... ",nomes[i],"\n")
      minimo[iv] = min(datal[[i]],na.rm = T)
      maximo[iv] = max(datal[[i]],na.rm = T)
      media[iv] = mean(datal[[i]],na.rm = T)
      mediana[iv] = median(datal[[i]],na.rm = T)
      quartil1[iv] = quantile(datal[[i]],probs = seq(0,1,0.25),na.rm = T)[2]
      quartil3[iv] = quantile(datal[[i]],probs = seq(0,1,0.25),na.rm = T)[4]
      sd[iv] = sd(datal[[i]],na.rm = T)
      cv[iv] = if_else(media[iv]!=0,sd[iv]/media[i],as.numeric(NA))
      t <- t.test(datal[[i]])$conf.int
      icli[iv] <- t[1]
      icls[iv] <- t[2]
      iv <- iv + 1
  }
  
  cat("\n########################\n")
  cat("#        Done !        #\n")
  cat("########################\n")
  
  resu <- data.frame(variable = nomes,minimo,quartil1,media,mediana,quartil3,
                     maximo,sd,cv,icli,icls)
  resu <- as_tibble(resu)
  if(export==TRUE){
    require(writexl)
    write_xlsx(x = resu,
               path = paste(NameExport,".xlsx",sep = ""),
               col_names = TRUE)
  }
  
  invisible(resu)

}