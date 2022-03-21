#' @title Generalized linear (mixed-effects) regression analysis
#' @param modelType = type of model: GLM, mixed-effects (GLMER), or cumulative link mixedm odel (CLMM)
#' @param long = long-form dataset (data.frame) considered if modelType = GLMER or CLMM
#' @param wide = wide-form dataset (data.frame) 
#' @param resp = name of the response variable (character)
#' @param fix.eff = character vector of names of the predictor(s)
#' @param cluster.ID = name of the cluster variable considered if modelType = GLMER or CLMM (default: "ID")
#' @param timeVar = name of the variable indexing time, considered if modelType = GLMER (defult: "ActivityDate")
#' @param REML = argument from the lme4::lmer() function, see ?lmer
#' @param ran.eff = character string indicating the random effect by using the lme4 syntax (defult: "(1|ID)")
#' @param family = character string indicating the name of the GLM(ER) family to be used in the models (default: "normal")
#' @param link =character string indicating the name of the GLM(ER) link function to be used in the models (default: "identity")
#' @param nAGQ = argument from the lme4::glmer() function, see ?glmer
#' @param mc.predictors = character vector of names of the predictors to be mean-centered
#' @param gmc.predictors = character vector of names of the predictors to be grand-mean-centered
#' @param outputs = character vector of the desired otucomes: "fit" for model diagnostics, "mComp" for model comparison, "coeff" for the coefficient table, "plotEff" for the effect plot(s), and "key.res" for the key results
#' @param coeff.models = name of the predictor(s) whose corresponding model(s) should be considered for the "coeff" output
#' @param transform = argument from sjPlot::tab_model(), see ?tab_model
#' @param coeff.models = model(s) to be considered for the "coeff" output
#' @param plot.model = name of the predictor(s) whose model(s) should be considered for the "plotEff" output
#' @param plot.pred = name of the predictor(s) to be plotted
#' @param show.data = argument from sjPlot::plot_model(), see ?plot_model
#' @param dot.size = argument from sjPlot::plot_model(), see ?plot_model
#' @param dodge = argument from sjPlot::plot_model(), see ?plot_model
#' @param dot.size = argument from sjPlot::plot_model(), see ?plot_model
#' @param ci.lv = argument from sjPlot::plot_model(), see ?plot_model
#' @param y.lim = numeric vector of length 2 indicating the limits of the y-axis
#' @param return.plot = booean. Should the plot be returned by the function? (default: FALSE)
#' @param key.model = name of the predictor(s) whose model(s) should be considered for the "key.res" output
#' @param key.predictor = name of the predictor to be considered by the "key.res" output
#' @param digits = number of digits for all numeric ouputs
#' @param messages = boolean indicating whether a message should be printed for each operation (defult: FALSE)
glmerAn <- function(modelType=c("GLM","GLMER"),long,wide,resp,fix.eff,cluster.ID="ID",timeVar="ActivityDate",REML=FALSE,
                    ran.eff="(1|ID)",family="normal",link="identity",nAGQ=1,mc.predictors=NA,gmc.predictors=NA,
                    outputs=c("fit","mComp","coeff","plotEff","key.results"),coeff.models=NA,transform=NULL,
                    plot.model=NA,plot.pred="all",show.data=FALSE,dot.size=1,dodge=0,ci.lv=.95,y.lim=NA,return.plot=FALSE,
                    key.model=NA,key.predictor=NA,digits=3,messages=TRUE){ 
  
  if(messages==TRUE){ cat("Running",modelType,"analysis of",resp,"...") }
  
  # data preparation ..................................................................................................................
  if(messages==TRUE){ cat("\n\nPreparing data...") }
  
  # participants' ID recoding as factor
  colnames(long)[which(colnames(long)==cluster.ID)] <- colnames(wide)[which(colnames(wide)==cluster.ID)] <- "ID"
  long$ID <- as.factor(as.character(long$ID))
  wide$ID <- as.factor(as.character(wide$ID))
  wide <- wide[order(wide$ID),] # sorting wide by ID
  colnames(long)[which(colnames(long)==timeVar)] <- "time" # time variable
  long <- long[order(long$ID,long$time),] # sorting long by ID and time
  if(nlevels(long$ID)!=nlevels(wide$ID)){ stop(message="Error: different No. of participants in long and wide datasets") }
  
  # listwise deletion
  if(modelType%in%c("GLMER","CLMM")){ memory <- long 
  for(Var in c(resp,fix.eff[which(grepl(":",fix.eff,fixed=TRUE)==FALSE)])){ colnames(long)[which(colnames(long)==Var)] <- "Var"
  long <- long[!is.na(long$Var),] # removing obs. with missing response in any resp or fix.eff variable
  colnames(long)[which(colnames(long)=="Var")] <- Var }
  long$ID <- as.factor(as.character(long$ID))
  wide <- wide[wide$ID%in%levels(long$ID),] # excluding wide IDs not included in long IDs
  if(messages==TRUE){ cat("\n - Excluding",nrow(memory)-nrow(long),"incomplete observations (",
                          round(100*(nrow(memory)-nrow(long))/nrow(memory),1),
                          "% ) in the response var. or in any of the predictors,\n   and",
                          nlevels(memory$ID)-nlevels(long$ID),"participants with no complete variables") }
  } else { memory <- wide
  for(Var in c(resp,fix.eff[which(grepl(":",fix.eff,fixed=TRUE)==FALSE)])){ colnames(wide)[which(colnames(wide)==Var)] <- "Var"
  wide <- wide[!is.na(wide$Var),]
  colnames(wide)[which(colnames(wide)=="Var")] <- Var }
  if(messages==TRUE){ cat("\n - Excluding",nrow(memory)-nrow(wide),"participants with no complete variables") }}
  
  # mean-centering predictors
  if(!is.na(gmc.predictors[1])){ if(messages==TRUE){ cat("\n - Grand-mean-centering",paste(gmc.predictors,collapse=" , ")) }
    for(Var in gmc.predictors){  colnames(wide)[which(colnames(wide)==Var)] <- "Var" 
    wide$Var.gmc <- wide$Var - mean(wide$Var,na.rm=TRUE) # computing gmc values
    colnames(wide) <- gsub("Var",Var,colnames(wide))
    long <- plyr::join(long,wide[,c("ID",paste(Var,"gmc",sep="."))],type="left",by="ID") # joining gmc to long dataset
    fix.eff <- gsub(Var,paste(Var,"gmc",sep="."),fix.eff) }} # updating labels in fix.eff (from Var to Var.gmc)
  if(!is.na(mc.predictors[1])){ if(messages==TRUE){ cat("\n - Mean-centering",paste(mc.predictors,collapse=" , ")) }
    suppressMessages(suppressWarnings(require(Rmisc))) 
    for(Var in mc.predictors){ colnames(long)[which(colnames(long)==Var)] <- "Var" 
    wide$Var.cm <- suppressWarnings(summarySE(long,measurevar="Var",groupvars="ID",na.rm=TRUE)[,3]) # cluster means
    long <- plyr::join(long,wide[,c("ID","Var.cm")],type="left",by="ID") # joining cm to long dataset
    long$Var.mc <- long$Var - long$Var.cm # computing mc values
    colnames(long) <- gsub("Var",Var,colnames(long)) 
    fix.eff <- gsub(Var,paste(Var,"mc",sep="."),fix.eff) }} # updating labels in fix.eff (from Var to Var.mc)
  long <- long[,!duplicated(colnames(long))] # removing duplicated columns (don't know why)
  
  # modeling .........................................................................................................................
  
  # creating model formulas
  formulas <- character()
  if(modelType=="GLM"){ ran.eff <- "1" }
  null.f <- paste(resp,"~",ran.eff) # creating null model formula
  for(i in 1:length(fix.eff)){ # creating other formulas
    if(i==1){ formulas[i] <- paste(resp,"~",fix.eff[1]) } else { formulas[i] <- paste(formulas[i-1],"+",fix.eff[i])  }}
  if(modelType%in%c("GLMER","CLMM")){ if(!is.na(ran.eff)){ formulas <- paste(formulas,"+",ran.eff)
  if(substr(ran.eff,2,2)!="1"){ ranSlope <- paste(fix.eff[which(grepl(ran.eff,fix.eff))])[1]
  null.f <- gsub(ranSlope,"1",null.f)  # removing random slope from models without the related predictor
  for(i in 1:length(formulas)){ 
    if(!(grepl(ranSlope,gsub(paste(ranSlope,"[|]",sep=""),"",formulas[i])))){ 
      formulas[i] <- gsub(paste(ranSlope,"[|]",sep=""),"1|",formulas[i]) }}}
  } else { stop(message="Error: GLMER model type without ran.eff specification") }}
  if(messages==TRUE){ cat("\n\nModel specification:\n - model M0 (null):",null.f)
    for(i in 1:length(formulas)){ cat("\n - model M",i,": ",formulas[i],sep="")}}
  
  # fitting models
  models <- list()
  if(modelType=="GLM"){ if(messages==TRUE){ cat("\n\nFitting GLM models of",resp,"on",nrow(wide),"participants \n   using the",
                                                family,"family with the",link,"link function...") }
    if(family=="normal" & link=="identity"){ null.m <- lm(as.formula(null.f),data=wide) # normal family
    for(i in 1:length(formulas)){ models[[i]] <- lm(formula=as.formula(formulas[i]),data=wide) }
    } else if (family=="gamma") { null.m <- glm(as.formula(null.f),data=wide,family=Gamma(link=link),nAGQ=nAGQ) # gamma family
    for(i in 1:length(formulas)){ models[[i]] <- glm(formula=as.formula(formulas[i]),data=wide,family=Gamma(link=link)) }
    } else if(family=="normal" & link!="identity"){  
      null.m <- glm(as.formula(null.f),data=wide,family=gaussian(link=link)) # normal with other link functions
      for(i in 1:length(formulas)){ models[[i]] <- glm(formula=as.formula(formulas[i]),data=wide,family=gaussian(link=link)) }
    } else if(family=="binomial"){ 
      null.m <- glm(as.formula(null.f),data=wide,family=binomial(link=link)) # logistic regression
      for(i in 1:length(formulas)){ models[[i]] <- glm(formula=as.formula(formulas[i]),data=wide,family=binomial(link=link))}
    } else { stop(message="Error: only normal, gamma, and binomial family are allowed, 
                      with identity, inverse, and log link functions") }
  } else if(modelType=="GLMER"){ suppressMessages(suppressWarnings(require(lme4)))
    if(messages==TRUE){ cat("\n\nFitting",modelType,"models of",resp,"on",nrow(long),"observations from",
                            nlevels(as.factor(as.character(long$ID))),"participants \n   using the",family,
                            "family with the",link,"link function using",ifelse(REML==FALSE,"ML","REML"),"estimator...") }
    if(family=="normal" & link=="identity"){ null.m <- lmer(as.formula(null.f),data=long,REML=REML) # normal  identity
    for(i in 1:length(formulas)){ models[[i]] <- lmer(formula=as.formula(formulas[i]),data=long,REML=REML) }
    } else if (family=="gamma") { null.m <- glmer(as.formula(null.f),data=long,family=Gamma(link=link),nAGQ=nAGQ) # gamma
    for(i in 1:length(formulas)){ models[[i]]<-glmer(formula=as.formula(formulas[i]),data=long,family=Gamma(link=link),nAGQ=nAGQ) }
    } else if(family=="normal" & link!="identity"){ 
      null.m <- glmer(as.formula(null.f),data=long,family=gaussian(link=link),nAGQ=nAGQ) # normal with other links
      for(i in 1:length(formulas)){ models[[i]] <- glmer(formula=as.formula(formulas[i]),data=long,
                                                         family=gaussian(link=link),nAGQ=nAGQ) }
    } else if(family=="binomial"){ null.m <- glmer(as.formula(null.f),data=long,family=binomial(link=link),nAGQ=nAGQ) # logistic
    for(i in 1:length(formulas)){ models[[i]] <- glmer(formula=as.formula(formulas[i]),data=long,
                                                       family=binomial(link=link),nAGQ=nAGQ)}
    } else { stop(message="Error: only normal, logistic, and gamma family are allowed, 
                               with identity, inverse, and log link functions") }
  } else if(modelType=="CLMM"){ suppressMessages(suppressWarnings(require(ordinal))) # cumulative link mixed models
    if(messages==TRUE){ cat("\n\nFitting",modelType,"models of",resp,"on",nrow(long),"observations from",
                            nlevels(as.factor(as.character(long$ID))),"participants \n   using Cumulative Link Mixed Models") }
    long[,resp] <- factor(long[,resp],ordered=TRUE) # response variable as ordered factor
    null.m <- suppressWarnings(clmm(as.formula(gsub("~","~ 1 +",null.f)),data=long)) # suppress formula warning (bugged)
    for(i in 1:length(formulas)){ models[[i]] <- suppressWarnings(clmm(formula=as.formula(formulas[i]),data=long,nAGQ=nAGQ)) }
  } else { stop(message="Error: modelType can only be 'GLM', 'GLMER', or 'CLMM'") }
  
  # outputs..........................................................................................................................
  if(messages==TRUE){ cat("\n\nGenerating models outputs...") }
  
  # model diagnostics
  if("fit"%in%outputs & modelType!="CLMM"){ if(messages==TRUE){ cat("\n\n - Plotting diagnostics of the most complex model:") }
    suppressMessages(suppressWarnings(require(sjPlot))); suppressMessages(suppressWarnings(require(ggplot2)))
    fit <- models[[length(models)]] # most complex model
    if(modelType=="GLMER"){ dat <- long } else { dat <- wide } # fitted dataset
    plots <- list()
    if(family=="normal" & link=="identity"){ p <- plot_model(fit,type="diag",dot.size=dot.size) # LM(ER) diagnostics (normal)
    if(modelType=="GLMER"){ p[[2]] <- p[[2]]$ID } 
    suppressMessages(plot_grid(p,tags=TRUE,margin=c(0,0,0,0)))
    } else { if(family=="binomial"){ # logistic regression
      dat$logit <- log(predict(fit,type="response")/(1-predict(fit,type="response"))) # computing logit
      for(Var in fix.eff[which(grepl(":",fix.eff,fixed=TRUE)==FALSE)]){ # plotting logit by each continuous predictor
        if(class(dat[,Var])=="numeric"){  plots[[length(plots)+1]] <- ggplot(dat,aes_string(x=Var,y="logit")) + 
          geom_point(size=dot.size) + suppressMessages(stat_smooth(method="loess")) + ggtitle("Logit by",Var) }}
    } else { plots[[length(plots)+1]] <- ggplot(data.frame(residuals=residuals(fit,type="deviance")), # deviance residuals QQ plot
                                                aes(sample=residuals)) + stat_qq(size=dot.size) + stat_qq_line() +
      labs(x="Theoretical qualtiles (normal distr.)",y="Sample quantiles") + ggtitle("Deviance Residuals' normal QQ plot") 
    plots[[length(plots)+1]] <- ggplot(fortify.merMod(fit), aes(.fitted, resid(fit,type="deviance"))) + geom_point() +
      suppressMessages(stat_smooth(method="loess"))+geom_hline(yintercept=0, col="red", linetype="dashed") +
      labs(x="Fitted values",y="Residuals") + ggtitle("Deviance residuals vs. fitted values") }
      p <- plot_model(fit,type="diag",dot.size=dot.size) # random effects with GLMER
      if(modelType=="GLMER"){ p <- p$ID }
      plots[[length(plots)+1]] <- p + ggtitle("Random effects' normal QQ plot") }
    if(family!="binomial"){ type <- ifelse(family=="gamma","deviance","pearson") # deviance residuals with gamma
    for(Var in fix.eff[which(grepl(":",fix.eff,fixed=TRUE)==FALSE)]){ # homoscedasticity: residuals by levels of categorical pred.
      if(class(dat[,Var])=="factor"){ Dat <- cbind(dat,residuals=resid(fit,type=type),Var=dat[,Var],Resp=dat[,resp])
      plots[[length(plots)+1]] <- ggplot(Dat,aes_string(x="Var",y="residuals")) + 
        ggtitle(paste(type,"residuals by",Var,"\n mean =",paste(round(with(Dat, tapply(Resp, Var, mean)),1),collapse=", "),
                      ", SD =",paste(round(with(Dat, tapply(Resp, Var, sd)),1),collapse=", "))) + xlab(Var) +
        geom_point(size=dot.size,position=position_jitter(),color="gray") + geom_violin(alpha=0.3) }}}
    suppressMessages(plot_grid(plots,tags=TRUE,margin=rep(0,4))) 
    cat("\n  Printing Variance Inflation Factors (VIF):\n")
    suppressMessages(suppressWarnings(require(car))) # variance inflation factors
    print(vif(fit)) }
  
  # model comparison - likelihood ratio test & Akaike weight
  if("mComp"%in%outputs | "key.results"%in%outputs){ 
    if(messages==TRUE){ cat("\n\n - Running likelihood ratio test:") } # likelihood ratio test
    suppressMessages(suppressWarnings(require(knitr))); suppressMessages(suppressWarnings(require(MuMIn))) 
    if(modelType=="CLMM"){ lrt <- as.data.frame(ordinal:::anova.clm(null.m,models[[1]])) # use anova.clm() to avoid env. issue
    } else { lrt <- as.data.frame(suppressMessages(anova(null.m,models[[1]]))) }
    for(i in 1:(length(models)-1)){ if(length(models)>=(i+1)){
      if(modelType=="CLMM"){ lrt <- rbind(lrt,as.data.frame(ordinal:::anova.clm(models[[i]],models[[i+1]]))[2,]) 
      } else { lrt <- rbind(lrt,as.data.frame(suppressMessages(anova(models[[i]],models[[i+1]])))[2,]) }}}
    rownames(lrt) <- c("Null model",fix.eff)
    for(i in nrow(lrt):2){ if(lrt[i,ncol(lrt)] < .05){ if(messages==TRUE){ cat("\n   Selected model:",rownames(lrt)[i]) }
      break }}
    if("mComp"%in%outputs){ print(kable(round(lrt,digits))) }
    AICs <- lrt[1:2,"AIC"] # Akaike weight
    if(messages==TRUE){ cat("\n\n - Computing Aw coefficients for each model vs. all previous models:") 
      cat("\n   - null model vs.",fix.eff[1],": =",round(Weights(AICs),digits)[1],
          "\n   -",fix.eff[1],"vs. null model =",round(Weights(AICs),digits)[2]) }
    if(length(models)>1){ for(i in 3:nrow(lrt)){ AICs <- c(AICs,lrt[i,"AIC"])
    if(messages==TRUE){ cat("\n   -",row.names(lrt)[i],"=",round(Weights(AICs),digits)[length(AICs)]) }}}
    if(messages==TRUE){ cat("\n   Selected model:",row.names(lrt[which(lrt$AIC==min(lrt$AIC)),])) }
    if("key.results"%in%outputs){ 
      key <- lrt[which(grepl(key.predictor,row.names(lrt))),] # key results
      if(nrow(key)>1){ key <- key[1,] }
      key.results <- data.frame(sig.LRT=key[,ncol(key)]<0.05,higher.Aw=key$AIC==min(AICs[1:(which(fix.eff==key.predictor)+1)])) }} 
  
  # estimated parameters from key.model
  if("key.results"%in%outputs){
    modSummary <- summary(models[[which(fix.eff==key.model)]])
    modSummary <- modSummary$coefficients
    if(modelType=="CLMM"){ modSummary <- modSummary[nlevels(long[,resp]):nrow(modSummary),] }
    key <- modSummary[which(grepl(key.predictor,row.names(modSummary))),3][1] # taking only first coeff for key.results
    key.results <- cbind(key.results,t.196=abs(key)>1.96,t=key) }
  if("coeff"%in%outputs){ if(messages==TRUE){ suppressMessages(suppressWarnings(require(knitr)))
    cat("\n\n - Printing estimated coefficients for models",paste(coeff.models,collapse=" , "))}
    suppressMessages(suppressWarnings(require(XML))); suppressMessages(suppressWarnings(require(sjPlot)))
    out <- data.frame(readHTMLTable(htmlParse(tab_model(models[which(fix.eff%in%coeff.models)],transform=transform,
                                                        show.icc=FALSE,show.p=FALSE,show.stat=TRUE,show.re.var=FALSE,
                                                        show.ci=FALSE,show.r2=FALSE,show.se=TRUE,collapse.se=TRUE,
                                                        string.est="B (SE)",string.stat="t")))[1])
    colnames(out) <- out[1,]
    out <- out[-1,] # sorting rows and columns
    out <- rbind(out[which(!substr(out$Predictors,1,3)%in%c("SD ","Cor")),],
                 out[which(substr(out$Predictors,1,3)=="SD "),],out[which(substr(out$Predictors,1,4)=="Cor "),])
    print(knitr::kable(out)) }
  
  # plotting effects
  if("plotEff"%in%outputs){ if(messages==TRUE){ cat("\n\n - Plotting effect(s) estimated by model",which(fix.eff==plot.model)) }
    suppressMessages(suppressWarnings(require(sjPlot))); suppressMessages(suppressWarnings(require(ggplot2)))
    if(plot.pred[1]=="all"){ fix.eff.plot <- fix.eff } else { fix.eff.plot <- plot.pred }
    if(length(fix.eff.plot)==1){ if(grepl(":",plot.pred)){ 
      terms <- c(strsplit(plot.pred,split=":")[[1]][1],strsplit(plot.pred,split=":")[[1]][2]) 
    } else { terms <- fix.eff[which(fix.eff==fix.eff.plot)] } 
      p <- plot_model(models[[which(fix.eff==fix.eff.plot)]],type="pred",terms=terms,ci.lv=ci.lv,dodge=dodge,
                      show.data=show.data,dot.size=dot.size,jitter=0.15) + ggtitle(paste(resp,"by",fix.eff.plot,fix.eff.plot))
      if(!is.na(y.lim[1])){ p <- p + ylim(y.lim) }
      if(return.plot==TRUE){ return(p) } else { print(p) } # returning or printing the plot 
    } else { plots <- list()
    for(i in 1:length(fix.eff.plot)){ if(grepl(":",fix.eff.plot[i],fixed=TRUE)==FALSE){ # predicted effects conditioned on ran.eff.
      p <- plot_model(models[[i]],type="pred",terms=fix.eff.plot[i],show.data=show.data,dot.size=dot.size,ci.lv=ci.lv,
                      colors="gray",jitter=0.15,dodge=dodge)
    } else { p <- plot_model(models[[i]],type="pred",terms=strsplit(fix.eff.plot[i],split=":")[[1]],ci.lv=ci.lv,dodge=dodge) }
      plots[[i]] <- p + ggtitle(paste(resp,"by",fix.eff.plot[i])) }
    plot_grid(plots,tags=TRUE,margin=rep(0,4)) }}
  
  # returning key results (sig. LRT, Aw higher than previous model, t > 1.96)
  if("key.results"%in%outputs){ return(key.results) }}