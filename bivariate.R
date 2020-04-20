bivariate <- function(x,target,
                      targetClass = c("binary","numeric"),
                      cutNum = TRUE,
                      cutNumMethod = c("median","quintile","decile"),
                      labelNA = "Missing"){
  if(targetClass=="binary"){
    
    nameTarget <- as.character(names(table(target)))

    if(class(x)%in%c("character","factor")){
      
      x <- fct_explicit_na(x,na_level = labelNA)
      
      n <- length(table(x))
      d <- cbind(table(x,target)[,1],
                 p0 = rep(0,n),
                 table(x,target)[,2],
                 p1 = rep(0,n),
                 tx0 = rep(0,n),
                 tx1 = rep(0,n),
                 rr = rep(0,n),
                 a = rep(0,n),
                 b = rep(0,n),
                 c = rep(0,n),
                 d = rep(0,n),
                 T.CHI = c(0,rep(NA,n-1)),
                 P.CHI = c(0,rep(NA,n-1)),
                 ent = rep(0,n),
                 gi = c(0,rep(NA,n-1)),
                 ks = c(0,rep(NA,n-1)))
      for(i in 1:n){
        d[i,2] <- (d[i,1]/sum(d[,1]))*100
        d[i,4] <- (d[i,3]/sum(d[,3]))*100
        d[i,5] <- d[i,1]/sum(d[i,c(1,3)])*100
        d[i,6] <- d[i,3]/sum(d[i,c(1,3)])*100
        d[i,7] <- d[i,4]/d[i,2]
        d[1,12] <- chisq.test(table(droplevels(x,labelNA)[x!=labelNA],target[x!=labelNA]))$statistic # Calculates without missing values
        d[1,13] <- chisq.test(table(droplevels(x,labelNA)[x!=labelNA],target[x!=labelNA]))$p.value
        d[i,14] <- Entropy(d[i,c(1,3)])
        d[1,15] <- InformationGain(table(x,target))
      }
      
      or <- epitools::epitab(x,target)$tab
      d[,8] <- or[,5]
      d[,9] <- or[,6]
      d[,10] <- or[,7]
      d[,11] <- or[,8]
      
      d <- data.frame(c = rownames(d),as.data.frame(d)) %>% as_tibble()
      colnames(d) <- c("Categories",paste("N",nameTarget[1]),
                       paste("P",nameTarget[1]),
                       paste("N",nameTarget[2]),
                       paste("P",nameTarget[2]),
                       paste("TX",nameTarget[1]),
                       paste("TX",nameTarget[2]),
                       "Odds","Odds ratio","L.I. OR","L.S. OR","OR p-value",
                       "X²","X² p-value","Entropy","Information Gain","K-S")
      return(d)
    }
    if(class(x)%in%c("numeric","integer")){
      if(cutNum==TRUE){
        if(cutNumMethod=="median"){
          z <- cut(x,breaks = quantile(x,probs = c(0,0.5,1),na.rm = TRUE),include.lowest = TRUE,dig.lab = 2)
          z <- fct_explicit_na(z,na_level = labelNA)
        }
        if(cutNumMethod=="quintile"){
          z <- cut(x,breaks = quantile(x,probs = seq(0,1,0.2),na.rm = TRUE),include.lowest = TRUE,dig.lab = 2,ordered_result = FALSE)
          z <- fct_explicit_na(z,na_level = labelNA)
        }
        if(cutNumMethod=="decile"){
          z <- cut(x,breaks = quantile(x,probs = seq(0,1,0.1),na.rm = TRUE),include.lowest = TRUE,dig.lab = 2,ordered_result = FALSE)
          z <- fct_explicit_na(z,na_level = labelNA)
        }
        n <- length(table(z))
        d <- cbind(table(z,target)[,1],
                   p0 = rep(0,n),
                   table(z,target)[,2],
                   p1 = rep(0,n),
                   tx0 = rep(0,n),
                   tx1 = rep(0,n),
                   rr = rep(0,n),
                   a = rep(0,n),
                   b = rep(0,n),
                   c = rep(0,n),
                   d = rep(0,n),
                   T.CHI = c(0,rep(NA,n-1)),
                   P.CHI = c(0,rep(NA,n-1)),
                   ent = rep(0,n),
                   gi = c(0,rep(NA,n-1)),
                   ks = c(0,rep(NA,n-1)))
        for(i in 1:n){
          d[i,2] <- (d[i,1]/sum(d[,1]))*100
          d[i,4] <- (d[i,3]/sum(d[,3]))*100
          d[i,5] <- d[i,1]/sum(d[i,c(1,3)])*100
          d[i,6] <- d[i,3]/sum(d[i,c(1,3)])*100
          d[i,7] <- d[i,4]/d[i,2]
          d[1,12] <- chisq.test(table(droplevels(z,labelNA)[z!=labelNA],target[z!=labelNA]))$statistic # Calculates without missing values
          d[1,13] <- chisq.test(table(droplevels(z,labelNA)[z!=labelNA],target[z!=labelNA]))$p.value
          d[i,14] <- Entropy(d[i,c(1,3)])
          d[1,15] <- InformationGain(table(z,target))
        }
        
        or <- epitools::epitab(z,target)$tab
        d[,8] <- or[,5]
        d[,9] <- or[,6]
        d[,10] <- or[,7]
        d[,11] <- or[,8]
        
        d[1,16] <- ks.test(x[target==nameTarget[1]],x[target==nameTarget[2]])$statistic
        
        d <- data.frame(c = rownames(d),as.data.frame(d)) %>% as_tibble()
        colnames(d) <- c("Categories",paste("N",nameTarget[1]),
                         paste("P",nameTarget[1]),
                         paste("N",nameTarget[2]),
                         paste("P",nameTarget[2]),
                         paste("TX",nameTarget[1]),
                         paste("TX",nameTarget[2]),
                         "Odds","Odds ratio","L.I. OR","L.S. OR","OR p-value",
                         "X²","X² p-value","Entropy","Information Gain","K-S")
        return(d)
      }
      else{
        d <- data.frame(X = x,y = target) %>% 
          group_by(y) %>% 
          summarise(Min = min(X,na.rm = T),
                    q1 = quantile(X,probs = 0.25,na.rm = T),
                    Median = median(X,na.rm = T),
                    Mean = mean(X,na.rm = T),
                    q3 = quantile(X,probs = 0.75,na.rm = T),
                    Max = max(X,na.rm = T),
                    Sd = sd(X,na.rm = T),
                    NNa = sum(is.na(X)),
                    PNa = NNa/sum(is.na(x))*100) %>% 
          ungroup() %>% 
          mutate(ft = if_else(row_number()==1,summary(aov(x~target))[[1]]$`F value`[1],NaN),
                 fp = if_else(row_number()==1,summary(aov(x~target))[[1]]$`Pr(>F)`[1],NaN),
                 kst = if_else(row_number()==1,ks.test(x[target==nameTarget[1]],x[target==nameTarget[2]])$statistic,NaN),
                 ksp = if_else(row_number()==1,ks.test(x[target==nameTarget[1]],x[target==nameTarget[2]])$p.value,NaN))
        
        colnames(d) <- c("Categories","min","1º quartile","Median","Mean","3º quartile","max","sd",
                         "N Missing","% Missing","F Statistic","F p-value","K-S statistic","K-S p-value")
        return(d)
      }
      
    }
  }
  
  if(targetClass=="numeric"){
    if(class(x)%in%c("character","factor")){
      
      x <- fct_explicit_na(x,na_level = labelNA)
      
      d <- data.frame(X = x,y = target) %>% 
        group_by(X) %>% 
        summarise(Min = min(y,na.rm = T),
                  q1 = quantile(y,probs = 0.25,na.rm = T),
                  Median = median(y,na.rm = T),
                  Mean = mean(y,na.rm = T),
                  q3 = quantile(y,probs = 0.75,na.rm = T),
                  Max = max(y,na.rm = T),
                  Sd = sd(y,na.rm = T),
                  NNa = sum(is.na(y)),
                  PNa = NNa/sum(is.na(target))*100) %>% 
        ungroup() %>% 
        mutate(ft = if_else(row_number()==1,summary(aov(target~x))[[1]]$`F value`[1],NaN),
               fp = if_else(row_number()==1,summary(aov(target~x))[[1]]$`Pr(>F)`[1],NaN))
      
      colnames(d) <- c("Categories","min","1º quartile","Median","Mean","3º quartile","max","sd",
                       "N Missing","% Missing","F Statistic","F p-value")
      return(d)
    }
    d <- data.frame(cor = cor(x,target,use = "na.or.complete"),
                       p.value = cor.test(x,target)$p.value,
                       nNAy = sum(is.na(target)),
                       pNAy = sum(is.na(target))/length(target)*100,  
                       nNAx = sum(is.na(x)),
                       pNAx = sum(is.na(x))/length(x)*100,
                       npairs = sum((is.na(x)==FALSE)*(is.na(target)==FALSE))) %>% 
      mutate(ppairs = npairs/length(target)*100)
    colnames(d) <- c("Correlation","Cor. p-value","N Missing target","% Missing target","N Missing x","% Missing x","N Pairs","% Pairs")
    d <- as_tibble(d)
    return(d)
  }
}

