#' @title Computing variance components and reliability indices of a self-report scale repeatedly administered within participants
#' @param data = data.frame
#' @param items = numeric string indicating the items of interest (e.g., 1:3 for items 1, 2 and 3)
#' @param item.labels = character string indicating the names of the items of interest
GTHEORYrel <- function(data,items,latent.lab,what=c("varComp","rel")){ require(lme4)
  
  # creating variable TIME
  data <- plyr::ddply(data,"ID",transform,TIME=seq_along(ID))
  
  # preparing long dataset with one row per item
  psymetr <- stack(data[,items])
  psymetr$ID <- data$ID
  psymetr$time <- as.factor(data$TIME)
  psymetr <- psymetr[,c(3,4,2,1)]
  colnames(psymetr) <- c("person","time","item","y")
  psymetr <- psymetr[order(psymetr$person,psymetr$time,psymetr$item),]
  
  # random-only GLMM specificiation
  mod1 <- lmer(y ~  1 + (1|person) + (1|time) + (1|item) + 
                 (1|person:time) + (1|person:item) + (1|time:item),data=psymetr)
  
  # variance decomposition
  SIGMAp <- lme4::VarCorr(mod1)[["person"]][1,1]
  SIGMAt <- lme4::VarCorr(mod1)[["time"]][1,1]
  SIGMAi <- lme4::VarCorr(mod1)[["item"]][1,1]
  SIGMAtp <- lme4::VarCorr(mod1)[["person:time"]][1,1]
  SIGMApi <- lme4::VarCorr(mod1)[["person:item"]][1,1]
  SIGMAti <- lme4::VarCorr(mod1)[["time:item"]][1,1]
  SIGMAres <- sigma(mod1)^2
  
  # printing variance components
  vars <- data.frame(Component=c("SIGMAp","SIGMAt","SIGMAi","SIGMAtp","SIGMApi","SIGMAti","SIGMAres","Total"),
                     VAR=c(SIGMAp,SIGMAt,SIGMAi,SIGMAtp,SIGMApi,SIGMAti,SIGMAres,
                           sum(SIGMAp,SIGMAt,SIGMAi,SIGMAtp,SIGMApi,SIGMAti,SIGMAres)))
  vars$VAR <- round(vars$VAR,2)
  vars$perc <- round(100*vars$VAR/vars[nrow(vars),2],2)
  colnames(vars)[2:3] <- c(latent.lab,paste(latent.lab,"%"))
  
  if(what=="varComp"){ return(vars)
    
  }else if(what=="rel"){ # reliability coeff. based on Cranfort et al. (2006)
    rel <- data.frame(measure=latent.lab,
                      # R1F = (varPERSON + varPERSON*ITEM/n.item) / (varPERSON + varPERSON*ITEM/n.item + varERROR/n.item)
                      R1F = (vars[1,2] + vars[5,2]/length(items)) / (vars[1,2] + vars[5,2]/length(items) + vars[7,2]/length(items)),
                      # RkF = (varPERSON + varPERSON*ITEM/n.item) / (varPERSON + varPERSON*ITEM/n.item + varERROR/(n.item*n.occasions))
                      RkF = (vars[1,2] + vars[5,2]/length(items))/ (vars[1,2] + vars[5,2]/length(items) +
                                                                      vars[7,2]/(length(items)*max(data$TIME))),
                      # Rc = varPERSON*TIME / (varPERSON*TIME + varERROR/n.items)
                      Rc = vars[4,2] / (vars[4,2] + vars[7,2]/length(items)))
    rel[,2:ncol(rel)] <- round(rel[,2:ncol(rel)],2)
    return(rel)
  }else { stop("Error: what argument can be either 'varComp' or 'rel'") }}